/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "hbset.h"
#include "hbvm.h"
#include "hbcomp.h"
#include "hbmemory.ch"

/* ------------------------------------------------------------------------- */
/* FM statistic module */
/* ------------------------------------------------------------------------- */

/* remove this 'undef' when number of memory leaks will be reduced to
   reasonable size */
/* #undef HB_FM_STATISTICS */


#ifdef HB_FM_STATISTICS

#define HB_MEMINFO_SIGNATURE  0xDEADBEAF
#define HB_MEMSTR_BLOCK_MAX   256

#ifndef HB_MEMFILER
#  define HB_MEMFILER  0xff
#endif

typedef struct _HB_MEMINFO
{
   struct _HB_MEMINFO * pPrevBlock;
   struct _HB_MEMINFO * pNextBlock;
   ULONG  ulSize;
   HB_U32 Signature;
} HB_MEMINFO, * PHB_MEMINFO;

#ifdef HB_ALLOC_ALIGNMENT
#  define HB_MEMINFO_SIZE     ( ( sizeof( HB_MEMINFO ) + HB_ALLOC_ALIGNMENT - 1 ) - \
                                ( sizeof( HB_MEMINFO ) + HB_ALLOC_ALIGNMENT - 1 ) % HB_ALLOC_ALIGNMENT )
#else
#  define HB_MEMINFO_SIZE     sizeof( HB_MEMINFO )
#endif

static PHB_MEMINFO s_pMemBlocks = NULL;
static long s_ulMemoryBlocks = 0;      /* memory blocks used */
static long s_ulMemoryMaxBlocks = 0;   /* maximum number of used memory blocks */
static long s_ulMemoryMaxConsumed = 0; /* memory size consumed */
static long s_ulMemoryConsumed = 0;    /* memory max size consumed */

#endif /* HB_FM_STATISTICS */

void * hb_xgrab( ULONG ulSize )        /* allocates fixed memory, exits on failure */
{
   void * pMem;

   if( ulSize == 0 )
      hb_errInternal( HB_EI_XGRABNULLSIZE, "hb_xgrab requested to allocate zero bytes", NULL, NULL );

#ifdef HB_FM_STATISTICS
   pMem = malloc( ulSize + HB_MEMINFO_SIZE + sizeof( HB_U32 ) );
   if( pMem )
   {
      if( s_pMemBlocks )
         s_pMemBlocks->pPrevBlock = ( PHB_MEMINFO ) pMem;
      ( ( PHB_MEMINFO ) pMem )->pNextBlock = s_pMemBlocks;
      ( ( PHB_MEMINFO ) pMem )->pPrevBlock = NULL;
      s_pMemBlocks = ( PHB_MEMINFO ) pMem;
      ( ( PHB_MEMINFO ) pMem )->ulSize = ulSize;
      ( ( PHB_MEMINFO ) pMem )->Signature = HB_MEMINFO_SIGNATURE;
      HB_PUT_LE_UINT32( ( ( HB_BYTE * ) pMem ) + HB_MEMINFO_SIZE + ulSize, HB_MEMINFO_SIGNATURE );

      s_ulMemoryConsumed += ulSize;
      if( s_ulMemoryMaxConsumed < s_ulMemoryConsumed )
         s_ulMemoryMaxConsumed = s_ulMemoryConsumed;
      s_ulMemoryBlocks++;
      if( s_ulMemoryMaxBlocks < s_ulMemoryBlocks )
         s_ulMemoryMaxBlocks = s_ulMemoryBlocks;
      pMem = ( HB_BYTE * ) pMem + HB_MEMINFO_SIZE;
   }
   else
#else
   pMem = malloc( ulSize );
   if( ! pMem )
#endif
      hb_errInternal( HB_EI_XGRABALLOC, "hb_xgrab can't allocate memory", NULL, NULL );

   return pMem;
}

void * hb_xrealloc( void * pMem, ULONG ulSize )       /* reallocates memory */
{
#ifdef HB_FM_STATISTICS
   PHB_MEMINFO pMemBlock;
   ULONG ulMemSize;
   void * pResult;

   if( ulSize == 0 )
   {
      if( pMem )
         hb_xfree( pMem );
      return NULL;
   }
   else if( ! pMem )
      return hb_xgrab( ulSize );

   pMemBlock = ( PHB_MEMINFO ) ( ( HB_BYTE * ) pMem - HB_MEMINFO_SIZE );
   ulMemSize = pMemBlock->ulSize;

   if( pMemBlock->Signature != HB_MEMINFO_SIGNATURE )
      hb_errInternal( HB_EI_XREALLOCINV, "hb_xrealloc called with an invalid pointer", NULL, NULL );

   if( HB_GET_LE_UINT32( ( ( HB_BYTE * ) pMem ) + ulMemSize ) != HB_MEMINFO_SIGNATURE )
      hb_errInternal( HB_EI_XMEMOVERFLOW, "Memory buffer overflow", NULL, NULL );

   HB_PUT_LE_UINT32( ( ( HB_BYTE * ) pMem ) + ulMemSize, 0 );

   pResult = realloc( pMemBlock, ulSize + HB_MEMINFO_SIZE + sizeof( HB_U32 ) );
   if( pResult )
   {
      if( s_pMemBlocks == pMemBlock )
         s_pMemBlocks = ( PHB_MEMINFO ) pResult;
      else
         ( ( PHB_MEMINFO ) pResult )->pPrevBlock->pNextBlock = ( PHB_MEMINFO ) pResult;

      if( ( ( PHB_MEMINFO ) pResult )->pNextBlock )
         ( ( PHB_MEMINFO ) pResult )->pNextBlock->pPrevBlock = ( PHB_MEMINFO ) pResult;
         s_ulMemoryConsumed += ( ulSize - ulMemSize );

      if( s_ulMemoryMaxConsumed < s_ulMemoryConsumed )
         s_ulMemoryMaxConsumed = s_ulMemoryConsumed;

      ( ( PHB_MEMINFO ) pResult )->ulSize = ulSize;  /* size of the memory block */
      HB_PUT_LE_UINT32( ( ( HB_BYTE * ) pResult ) + ulSize + HB_MEMINFO_SIZE, HB_MEMINFO_SIGNATURE );
      pResult = ( HB_BYTE * ) pResult + HB_MEMINFO_SIZE;
   }
#else
   void * pResult = realloc( pMem, ulSize );
#endif

   if( ! pResult && ulSize )
      hb_errInternal( HB_EI_XREALLOC, "hb_xrealloc can't reallocate memory", NULL, NULL );

   return pResult;
}

void hb_xfree( void * pMem )            /* frees fixed memory */
{
   if( pMem )
   {
#ifdef HB_FM_STATISTICS
      PHB_MEMINFO pMemBlock = ( PHB_MEMINFO ) ( ( HB_BYTE * ) pMem - HB_MEMINFO_SIZE );

      if( pMemBlock->Signature != HB_MEMINFO_SIGNATURE )
         hb_errInternal( HB_EI_XFREEINV, "hb_xfree called with an invalid pointer", NULL, NULL );

      if( HB_GET_LE_UINT32( ( ( HB_BYTE * ) pMem ) + pMemBlock->ulSize ) != HB_MEMINFO_SIGNATURE )
         hb_errInternal( HB_EI_XMEMOVERFLOW, "Memory buffer overflow", NULL, NULL );

      s_ulMemoryConsumed -= pMemBlock->ulSize;
      s_ulMemoryBlocks--;
      if( s_pMemBlocks == pMemBlock )
         s_pMemBlocks = pMemBlock->pNextBlock;
      else
         pMemBlock->pPrevBlock->pNextBlock = pMemBlock->pNextBlock;

      if( pMemBlock->pNextBlock )
         pMemBlock->pNextBlock->pPrevBlock = pMemBlock->pPrevBlock;

      pMemBlock->Signature = 0;
      HB_PUT_LE_UINT32( ( ( HB_BYTE * ) pMem ) + pMemBlock->ulSize, 0 );
      pMem = ( HB_BYTE * ) pMem - HB_MEMINFO_SIZE;
#endif
      free( pMem );
   }
   else
      hb_errInternal( HB_EI_XFREENULL, "hb_xfree called with a NULL pointer", NULL, NULL );
}

ULONG hb_xquery( int iMode )
{
   ULONG ulResult = 0;

#ifdef HB_FM_STATISTICS
   switch( iMode )
   {
      case HB_MEM_USED:
         ulResult = s_ulMemoryConsumed;
         break;

      case HB_MEM_USEDMAX:
         ulResult = s_ulMemoryMaxConsumed;
         break;
   }
#else
   HB_SYMBOL_UNUSED( iMode );
#endif
   return ulResult;
}

#ifdef HB_FM_STATISTICS
static char * hb_memToStr( char * szBuffer, void * pMem, ULONG ulSize )
{
   unsigned char * byMem = ( HB_BYTE * ) pMem;
   char * pDest = szBuffer;
   int iSize, i, iPrintable;

   if( ulSize > HB_MEMSTR_BLOCK_MAX )
      iSize = HB_MEMSTR_BLOCK_MAX;
   else
      iSize = ( int ) ulSize;

   iPrintable = 0;
   for( i = 0; i < iSize; ++i )
      if( ( byMem[ i ] & 0x7f ) >= 0x20 )
         iPrintable++;

   if( ( iPrintable * 100 ) / iSize > 70 ) /* more then 70% printable chars */
   {
      /* format as string of original chars */
      for( i = 0; i < iSize; ++i )
         if( ( byMem[ i ] & 0x7f ) >= 0x20 )
            * pDest++ = byMem[ i ];
         else
            * pDest++ = '.';
   }
   else
   {
     /* format as hex */
      for( i = 0; i < iSize; ++i )
      {
         int iLo = byMem[ i ] & 0x0f, iHi = byMem[ i ] >> 4;
         * pDest++ = '\\';
         * pDest++ = iHi <= 9 ? '0' + iHi : 'A' - 10 + iHi;
         * pDest++ = iLo <= 9 ? '0' + iLo : 'A' - 10 + iLo;
      }
   }
   * pDest = '\0';

   return szBuffer;
}
#endif

void hb_xexit( void )
{
#ifdef HB_FM_STATISTICS
   if( s_ulMemoryBlocks /* || hb_cmdargCheck( "INFO" ) */ )
   {
      char szBuffer[ HB_MAX( 3 * HB_MEMSTR_BLOCK_MAX + 1, 100 ) ];
      PHB_MEMINFO pMemBlock;
      int i;

      hb_conOutErr( hb_conNewLine(), 0 );
      hb_conOutErr( "----------------------------------------", 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
      hb_snprintf( szBuffer, sizeof( szBuffer ), "Total memory allocated: %lu bytes (%lu blocks)", s_ulMemoryMaxConsumed, s_ulMemoryMaxBlocks );
      hb_conOutErr( szBuffer, 0 );

      if( s_ulMemoryBlocks )
      {
         hb_conOutErr( hb_conNewLine(), 0 );
         hb_snprintf( szBuffer, sizeof( szBuffer ), "WARNING! Memory allocated but not released: %lu bytes (%lu blocks)", s_ulMemoryConsumed, s_ulMemoryBlocks );
         hb_conOutErr( szBuffer, 0 );
      }

      hb_conOutErr( hb_conNewLine(), 0 );

      for( i = 1, pMemBlock = s_pMemBlocks; pMemBlock; ++i, pMemBlock = pMemBlock->pNextBlock )
         HB_TRACE( HB_TR_ERROR, ( "Block %i %p (size %lu) \"%s\"", i,
                ( char * ) pMemBlock + HB_MEMINFO_SIZE, pMemBlock->ulSize,
                hb_memToStr( szBuffer, ( char * ) pMemBlock + HB_MEMINFO_SIZE,
                             pMemBlock->ulSize ) ) );
   }
#endif
}

HB_BOOL hb_xtraced( void )
{
   return HB_FALSE;
}

/* NOTE: Use as minimal calls from here, as possible.
         Don't allocate memory from this function. [vszakats] */
void hb_errInternal( HB_ERRCODE errCode, const char * szText, const char * szPar1, const char * szPar2 )
{
   char buffer[ 1024 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_errInternal(%d, %s, %s, %s)", errCode, szText, szPar1, szPar2));

   hb_conOutErr( hb_conNewLine(), 0 );
   hb_snprintf( buffer, sizeof( buffer ), "Unrecoverable error %d: ", errCode );
   hb_conOutErr( buffer, 0 );
   if( szText )
   {
      hb_snprintf( buffer, sizeof( buffer ), szText, szPar1, szPar2 );
      hb_conOutErr( buffer, 0 );
   }
   hb_conOutErr( hb_conNewLine(), 0 );
   exit( EXIT_FAILURE );
}

/* console */
void hb_conOutErr( const char * pStr, ULONG ulLen )
{
   if( ulLen == 0 )
      ulLen = strlen( pStr );

   fprintf( stderr, "%.*s", ( int ) ulLen, pStr );
}

const char * hb_conNewLine( void )
{
   return "\n";
}

int hb_charUpper( int iChar )
{
   return HB_TOUPPER( iChar );
}

int hb_charLower( int iChar )
{
   return HB_TOLOWER( iChar );
}

const char * hb_osEncodeCP( const char * szName, char ** pszFree, ULONG * pulSize )
{
   HB_SYMBOL_UNUSED( pulSize );
   HB_SYMBOL_UNUSED( pszFree );
   return szName;
}

const char * hb_osDecodeCP( const char * szName, char ** pszFree, ULONG * pulSize )
{
   HB_SYMBOL_UNUSED( pulSize );
   HB_SYMBOL_UNUSED( pszFree );
   return szName;
}


/* HB_TRACE */
static HB_TRACEINFO s_traceInfo;

void hb_traceset( int level, const char * file, int line, const char * proc )
{
   s_traceInfo.level = level;
   s_traceInfo.file = file;
   s_traceInfo.line = line;
   s_traceInfo.proc = proc;
}

PHB_TRACEINFO hb_traceinfo( void )
{
   return &s_traceInfo;
}


/* VM */
void hb_vmLock( void ) {}

void hb_vmUnlock( void ) {}

void hb_fsSetIOError( HB_BOOL fResult, HB_USHORT uiOperation )
{
   HB_SYMBOL_UNUSED( fResult );
   HB_SYMBOL_UNUSED( uiOperation );
}


/* file name conversion */

static int     s_iFileCase = HB_SET_CASE_MIXED;
static int     s_iDirCase  = HB_SET_CASE_MIXED;
static HB_BOOL s_fFnTrim   = HB_FALSE;
static char    s_cDirSep   = HB_OS_PATH_DELIM_CHR;

const char * hb_fsNameConv( const char * szFileName, char ** pszFree )
{
   if( s_fFnTrim || s_cDirSep != HB_OS_PATH_DELIM_CHR ||
       s_iFileCase != HB_SET_CASE_MIXED || s_iDirCase != HB_SET_CASE_MIXED )
   {
      PHB_FNAME pFileName;
      ULONG ulLen;

      if( pszFree )
      {
         szFileName = *pszFree = hb_strncpy( ( char * ) hb_xgrab( HB_PATH_MAX ),
                                             szFileName, HB_PATH_MAX - 1 );
      }

      if( s_cDirSep != HB_OS_PATH_DELIM_CHR )
      {
         char *p = ( char * ) szFileName;
         while( *p )
         {
            if( *p == s_cDirSep )
               *p = HB_OS_PATH_DELIM_CHR;
            p++;
         }
      }

      pFileName = hb_fsFNameSplit( szFileName );

      /* strip trailing and leading spaces */
      if( s_fFnTrim )
      {
         if( pFileName->szName )
         {
            ulLen = strlen( pFileName->szName );
            while( ulLen && pFileName->szName[ulLen - 1] == ' ' )
               --ulLen;
            while( ulLen && pFileName->szName[0] == ' ' )
            {
               ++pFileName->szName;
               --ulLen;
            }
            ( ( char * ) pFileName->szName )[ulLen] = '\0';
         }
         if( pFileName->szExtension )
         {
            ulLen = strlen( pFileName->szExtension );
            while( ulLen && pFileName->szExtension[ulLen - 1] == ' ' )
               --ulLen;
            while( ulLen && pFileName->szExtension[0] == ' ' )
            {
               ++pFileName->szExtension;
               --ulLen;
            }
            ( ( char * ) pFileName->szExtension )[ulLen] = '\0';
         }
      }

      /* FILECASE */
      if( s_iFileCase == HB_SET_CASE_LOWER )
      {
         if( pFileName->szName )
            hb_strlow( ( char * ) pFileName->szName );
         if( pFileName->szExtension )
            hb_strlow( ( char * ) pFileName->szExtension );
      }
      else if( s_iFileCase == HB_SET_CASE_UPPER )
      {
         if( pFileName->szName )
            hb_strupr( ( char * ) pFileName->szName );
         if( pFileName->szExtension )
            hb_strupr( ( char * ) pFileName->szExtension );
      }

      /* DIRCASE */
      if( pFileName->szPath )
      {
         if( s_iDirCase == HB_SET_CASE_LOWER )
            hb_strlow( ( char * ) pFileName->szPath );
         else if( s_iDirCase == HB_SET_CASE_UPPER )
            hb_strupr( ( char * ) pFileName->szPath );
      }

      hb_fsFNameMerge( ( char * ) szFileName, pFileName );
      hb_xfree( pFileName );
   }
   else if( pszFree )
      *pszFree = NULL;

   return szFileName;
}

int hb_setGetDirSeparator( void )
{
   return s_cDirSep;
}

void hb_compChkFileSwitches( int argc, char * argv[] )
{
   int i, n;

   for( i = 1; i < argc; ++i )
   {
      if( HB_ISOPTSEP( argv[i][0] ) && argv[i][1] == 'f' )
      {
         n = 0;
         switch( argv[i][2] )
         {
            case 'n':
               if( !argv[i][3] )
               {
                  s_iFileCase = HB_SET_CASE_MIXED;
                  n = 3;
               }
               else if( argv[i][3] == ':' )
               {
                  if( argv[i][4] == 'u' )
                  {
                     s_iFileCase = HB_SET_CASE_UPPER;
                     n = 5;
                  }
                  else if( argv[i][4] == 'l' )
                  {
                     s_iFileCase = HB_SET_CASE_LOWER;
                     n = 5;
                  }
               }
               else if( argv[i][3] == '-' )
               {
                  s_iFileCase = HB_SET_CASE_MIXED;
                  n = 4;
               }
               break;

            case 'd':
               if( !argv[i][3] )
               {
                  s_iDirCase = HB_SET_CASE_MIXED;
                  n = 3;
               }
               else if( argv[i][3] == ':' )
               {
                  if( argv[i][4] == 'u' )
                  {
                     s_iDirCase = HB_SET_CASE_UPPER;
                     n = 5;
                  }
                  else if( argv[i][4] == 'l' )
                  {
                     s_iDirCase = HB_SET_CASE_LOWER;
                     n = 5;
                  }
               }
               else if( argv[i][3] == '-' )
               {
                  s_iDirCase = HB_SET_CASE_MIXED;
                  n = 4;
               }
               break;

            case 'p':
               if( !argv[i][3] )
               {
                  s_cDirSep = HB_OS_PATH_DELIM_CHR;
                  n = 3;
               }
               else if( argv[i][3] == '-' )
               {
                  s_cDirSep = HB_OS_PATH_DELIM_CHR;
                  n = 4;
               }
               else if( argv[i][3] == ':' && argv[i][4] )
               {
                  s_cDirSep = argv[i][4];
                  n = 5;
               }
               break;

            case 's':
               if( !argv[i][3] )
               {
                  s_fFnTrim = HB_TRUE;
                  n = 3;
               }
               else if( argv[i][3] == '-' )
               {
                  s_fFnTrim = HB_FALSE;
                  n = 4;
               }
               break;
         }
         if( n )
         {
            argv[i] += n;
            if( argv[i][0] )
               --i;
            else
               argv[i] = ( char * ) "-";
         }
      }
   }
}
