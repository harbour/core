/*
 * Harbour Project source code:
 *
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
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
#include "hbapicdp.h"
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
#  define HB_MEMFILER         0xff
#endif

typedef struct _HB_MEMINFO
{
   struct _HB_MEMINFO * pPrevBlock;
   struct _HB_MEMINFO * pNextBlock;
   HB_SIZE nSize;
   HB_U32  Signature;
} HB_MEMINFO, * PHB_MEMINFO;

#ifdef HB_ALLOC_ALIGNMENT
#  define HB_MEMINFO_SIZE  ( ( sizeof( HB_MEMINFO ) + HB_ALLOC_ALIGNMENT - 1 ) - \
                             ( sizeof( HB_MEMINFO ) + HB_ALLOC_ALIGNMENT - 1 ) % HB_ALLOC_ALIGNMENT )
#else
#  define HB_MEMINFO_SIZE  sizeof( HB_MEMINFO )
#endif

static PHB_MEMINFO s_pMemBlocks         = NULL;
static HB_ISIZ     s_nMemoryBlocks      = 0; /* memory blocks used */
static HB_ISIZ     s_nMemoryMaxBlocks   = 0; /* maximum number of used memory blocks */
static HB_ISIZ     s_nMemoryMaxConsumed = 0; /* memory size consumed */
static HB_ISIZ     s_nMemoryConsumed    = 0; /* memory max size consumed */

#endif /* HB_FM_STATISTICS */

void * hb_xgrab( HB_SIZE nSize )        /* allocates fixed memory, exits on failure */
{
   void * pMem;

   if( nSize == 0 )
      hb_errInternal( HB_EI_XGRABNULLSIZE, "hb_xgrab requested to allocate zero bytes", NULL, NULL );

#ifdef HB_FM_STATISTICS
   pMem = malloc( nSize + HB_MEMINFO_SIZE + sizeof( HB_U32 ) );
   if( pMem )
   {
      if( s_pMemBlocks )
         s_pMemBlocks->pPrevBlock = ( PHB_MEMINFO ) pMem;
      ( ( PHB_MEMINFO ) pMem )->pNextBlock = s_pMemBlocks;
      ( ( PHB_MEMINFO ) pMem )->pPrevBlock = NULL;
      s_pMemBlocks = ( PHB_MEMINFO ) pMem;
      ( ( PHB_MEMINFO ) pMem )->nSize = nSize;
      ( ( PHB_MEMINFO ) pMem )->Signature = HB_MEMINFO_SIGNATURE;
      HB_PUT_LE_UINT32( ( ( HB_BYTE * ) pMem ) + HB_MEMINFO_SIZE + nSize, HB_MEMINFO_SIGNATURE );

      s_nMemoryConsumed += nSize;
      if( s_nMemoryMaxConsumed < s_nMemoryConsumed )
         s_nMemoryMaxConsumed = s_nMemoryConsumed;
      s_nMemoryBlocks++;
      if( s_nMemoryMaxBlocks < s_nMemoryBlocks )
         s_nMemoryMaxBlocks = s_nMemoryBlocks;
      pMem = ( HB_BYTE * ) pMem + HB_MEMINFO_SIZE;
   }
   else
#else
   pMem = malloc( nSize );
   if( ! pMem )
#endif
      hb_errInternal( HB_EI_XGRABALLOC, "hb_xgrab can't allocate memory", NULL, NULL );

   return pMem;
}

void * hb_xrealloc( void * pMem, HB_SIZE nSize )       /* reallocates memory */
{
#ifdef HB_FM_STATISTICS
   PHB_MEMINFO pMemBlock;
   HB_SIZE nMemSize;
   void * pResult;

   if( nSize == 0 )
   {
      if( pMem )
         hb_xfree( pMem );
      return NULL;
   }
   else if( ! pMem )
      return hb_xgrab( nSize );

   pMemBlock = ( PHB_MEMINFO ) ( ( HB_BYTE * ) pMem - HB_MEMINFO_SIZE );
   nMemSize = pMemBlock->nSize;

   if( pMemBlock->Signature != HB_MEMINFO_SIGNATURE )
      hb_errInternal( HB_EI_XREALLOCINV, "hb_xrealloc called with an invalid pointer", NULL, NULL );

   if( HB_GET_LE_UINT32( ( ( HB_BYTE * ) pMem ) + nMemSize ) != HB_MEMINFO_SIGNATURE )
      hb_errInternal( HB_EI_XMEMOVERFLOW, "Memory buffer overflow", NULL, NULL );

   HB_PUT_LE_UINT32( ( ( HB_BYTE * ) pMem ) + nMemSize, 0 );

   pResult = realloc( pMemBlock, nSize + HB_MEMINFO_SIZE + sizeof( HB_U32 ) );
   if( pResult )
   {
      if( s_pMemBlocks == pMemBlock )
         s_pMemBlocks = ( PHB_MEMINFO ) pResult;
      else
         ( ( PHB_MEMINFO ) pResult )->pPrevBlock->pNextBlock = ( PHB_MEMINFO ) pResult;

      if( ( ( PHB_MEMINFO ) pResult )->pNextBlock )
         ( ( PHB_MEMINFO ) pResult )->pNextBlock->pPrevBlock = ( PHB_MEMINFO ) pResult;
      s_nMemoryConsumed += ( nSize - nMemSize );

      if( s_nMemoryMaxConsumed < s_nMemoryConsumed )
         s_nMemoryMaxConsumed = s_nMemoryConsumed;

      ( ( PHB_MEMINFO ) pResult )->nSize = nSize;  /* size of the memory block */
      HB_PUT_LE_UINT32( ( ( HB_BYTE * ) pResult ) + nSize + HB_MEMINFO_SIZE, HB_MEMINFO_SIGNATURE );
      pResult = ( HB_BYTE * ) pResult + HB_MEMINFO_SIZE;
   }
#else
   void * pResult = realloc( pMem, nSize );
#endif

   if( ! pResult && nSize )
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

      if( HB_GET_LE_UINT32( ( ( HB_BYTE * ) pMem ) + pMemBlock->nSize ) != HB_MEMINFO_SIGNATURE )
         hb_errInternal( HB_EI_XMEMOVERFLOW, "Memory buffer overflow", NULL, NULL );

      s_nMemoryConsumed -= pMemBlock->nSize;
      s_nMemoryBlocks--;
      if( s_pMemBlocks == pMemBlock )
         s_pMemBlocks = pMemBlock->pNextBlock;
      else
         pMemBlock->pPrevBlock->pNextBlock = pMemBlock->pNextBlock;

      if( pMemBlock->pNextBlock )
         pMemBlock->pNextBlock->pPrevBlock = pMemBlock->pPrevBlock;

      pMemBlock->Signature = 0;
      HB_PUT_LE_UINT32( ( ( HB_BYTE * ) pMem ) + pMemBlock->nSize, 0 );
      pMem = ( HB_BYTE * ) pMem - HB_MEMINFO_SIZE;
#endif
      free( pMem );
   }
   else
      hb_errInternal( HB_EI_XFREENULL, "hb_xfree called with a NULL pointer", NULL, NULL );
}

HB_SIZE hb_xquery( int iMode )
{
   HB_SIZE nResult = 0;

#ifdef HB_FM_STATISTICS
   switch( iMode )
   {
      case HB_MEM_USED:
         nResult = s_nMemoryConsumed;
         break;

      case HB_MEM_USEDMAX:
         nResult = s_nMemoryMaxConsumed;
         break;
   }
#else
   HB_SYMBOL_UNUSED( iMode );
#endif
   return nResult;
}

#ifdef HB_FM_STATISTICS
static char * hb_memToStr( char * szBuffer, void * pMem, HB_SIZE nSize )
{
   unsigned char * byMem = ( HB_BYTE * ) pMem;
   char * pDest = szBuffer;
   int iSize, i, iPrintable;

   if( nSize > HB_MEMSTR_BLOCK_MAX )
      iSize = HB_MEMSTR_BLOCK_MAX;
   else
      iSize = ( int ) nSize;

   iPrintable = 0;
   for( i = 0; i < iSize; ++i )
      if( ( byMem[ i ] & 0x7f ) >= 0x20 )
         iPrintable++;

   if( ( iPrintable * 100 ) / iSize > 70 ) /* more then 70% printable chars */
   {
      /* format as string of original chars */
      for( i = 0; i < iSize; ++i )
         if( ( byMem[ i ] & 0x7f ) >= 0x20 )
            *pDest++ = byMem[ i ];
         else
            *pDest++ = '.';
   }
   else
   {
      /* format as hex */
      for( i = 0; i < iSize; ++i )
      {
         int iLo = byMem[ i ] & 0x0f, iHi = byMem[ i ] >> 4;
         *pDest++ = '\\';
         *pDest++ = iHi <= 9 ? '0' + iHi : 'A' - 10 + iHi;
         *pDest++ = iLo <= 9 ? '0' + iLo : 'A' - 10 + iLo;
      }
   }
   *pDest = '\0';

   return szBuffer;
}
#endif

void hb_xexit( void )
{
#ifdef HB_FM_STATISTICS
   if( s_nMemoryBlocks /* || hb_cmdargCheck( "INFO" ) */ )
   {
      char szBuffer[ HB_MAX( 3 * HB_MEMSTR_BLOCK_MAX + 1, 100 ) ];
      PHB_MEMINFO pMemBlock;
      int i;

      hb_conOutErr( hb_conNewLine(), 0 );
      hb_conOutErr( "----------------------------------------", 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
      hb_snprintf( szBuffer, sizeof( szBuffer ), "Total memory allocated: %" HB_PFS "u bytes (%" HB_PFS "u blocks)", s_nMemoryMaxConsumed, s_nMemoryMaxBlocks );
      hb_conOutErr( szBuffer, 0 );

      if( s_nMemoryBlocks )
      {
         hb_conOutErr( hb_conNewLine(), 0 );
         hb_snprintf( szBuffer, sizeof( szBuffer ), "WARNING! Memory allocated but not released: %" HB_PFS "u bytes (%" HB_PFS "u blocks)", s_nMemoryConsumed, s_nMemoryBlocks );
         hb_conOutErr( szBuffer, 0 );
      }

      hb_conOutErr( hb_conNewLine(), 0 );

      for( i = 1, pMemBlock = s_pMemBlocks; pMemBlock; ++i, pMemBlock = pMemBlock->pNextBlock )
         HB_TRACE( HB_TR_ERROR, ( "Block %i %p (size %" HB_PFS "u) \"%s\"", i,
                ( char * ) pMemBlock + HB_MEMINFO_SIZE, pMemBlock->nSize,
                hb_memToStr( szBuffer, ( char * ) pMemBlock + HB_MEMINFO_SIZE,
                             pMemBlock->nSize ) ) );
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

   HB_TRACE( HB_TR_DEBUG, ( "hb_errInternal(%d, %s, %s, %s)", errCode, szText, szPar1, szPar2 ) );

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
void hb_conOutErr( const char * pStr, HB_SIZE nLen )
{
   if( nLen == 0 )
      nLen = strlen( pStr );

   fprintf( stderr, "%.*s", ( int ) nLen, pStr );
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

PHB_CODEPAGE hb_vmCDP( void )
{
   return NULL;
}

HB_SIZE hb_cdpTextPos( PHB_CODEPAGE cdp, const char * pText, HB_SIZE nSize, HB_SIZE nIndex )
{
   HB_SYMBOL_UNUSED( cdp );
   HB_SYMBOL_UNUSED( pText );

   return nIndex >= nSize ? nSize : nIndex;
}

HB_BOOL hb_cdpCharEq( PHB_CODEPAGE cdp, const char * szText1, HB_SIZE nLen1, HB_SIZE * pnPos1,
                      const char * szText2, HB_SIZE nLen2, HB_SIZE * pnPos2 )
{
   HB_SYMBOL_UNUSED( cdp );

   if( *pnPos1 < nLen1 && *pnPos2 < nLen2 )
      return szText1[ ( *pnPos1 )++ ] == szText2[ ( *pnPos2 )++ ];
   else
      return HB_FALSE;
}

HB_BOOL hb_cdpCharCaseEq( PHB_CODEPAGE cdp, const char * szText1, HB_SIZE nLen1, HB_SIZE * pnPos1,
                          const char * szText2, HB_SIZE nLen2, HB_SIZE * pnPos2 )
{
   HB_SYMBOL_UNUSED( cdp );

   if( *pnPos1 < nLen1 && *pnPos2 < nLen2 )
   {
      HB_UCHAR uc1 = szText1[ ( *pnPos1 )++ ],
               uc2 = szText2[ ( *pnPos2 )++ ];
      return HB_TOUPPER( uc1 ) == HB_TOUPPER( uc2 );
   }
   else
      return HB_FALSE;
}

const char * hb_osEncodeCP( const char * szName, char ** pszFree, HB_SIZE * pnSize )
{
   HB_SYMBOL_UNUSED( pnSize );
   HB_SYMBOL_UNUSED( pszFree );
   return szName;
}

const char * hb_osDecodeCP( const char * szName, char ** pszFree, HB_SIZE * pnSize )
{
   HB_SYMBOL_UNUSED( pnSize );
   HB_SYMBOL_UNUSED( pszFree );
   return szName;
}

char * hb_osStrEncode( const char * pszName )
{
   return hb_strdup( pszName );
}

char * hb_osStrEncodeN( const char * pszName, HB_SIZE nLen )
{
   return hb_strndup( pszName, nLen );
}

char * hb_osStrDecode( const char * pszName )
{
   return hb_strdup( pszName );
}

char * hb_osStrDecode2( const char * pszName, char * pszBuffer, HB_SIZE nSize )
{
   return hb_strncpy( pszBuffer, pszName, nSize );
}

#if defined( HB_OS_WIN )
HB_WCHAR * hb_osStrU16Encode( const char * pszName )
{
   return hb_mbtowc( pszName );
}

HB_WCHAR * hb_osStrU16EncodeN( const char * pszName, HB_SIZE nLen )
{
   return hb_mbntowc( pszName, nLen );
}

char * hb_osStrU16Decode( const HB_WCHAR * pszNameW )
{
   return hb_wctomb( pszNameW );
}

char * hb_osStrU16Decode2( const HB_WCHAR * pszNameW, char * pszBuffer, HB_SIZE nSize )
{
   hb_wcntombcpy( pszBuffer, pszNameW, nSize );
   return pszBuffer;
}
#endif


/* HB_TRACE */
static HB_TRACEINFO s_traceInfo;

void hb_traceset( int level, const char * file, int line, const char * proc )
{
   s_traceInfo.level = level;
   s_traceInfo.file  = file;
   s_traceInfo.line  = line;
   s_traceInfo.proc  = proc;
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
      HB_SIZE nLen;

      if( pszFree )
      {
         szFileName = *pszFree = hb_strncpy( ( char * ) hb_xgrab( HB_PATH_MAX ),
                                             szFileName, HB_PATH_MAX - 1 );
      }

      if( s_cDirSep != HB_OS_PATH_DELIM_CHR )
      {
         char * p = ( char * ) szFileName;
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
            nLen = strlen( pFileName->szName );
            while( nLen && pFileName->szName[ nLen - 1 ] == ' ' )
               --nLen;
            while( nLen && pFileName->szName[ 0 ] == ' ' )
            {
               ++pFileName->szName;
               --nLen;
            }
            ( ( char * ) pFileName->szName )[ nLen ] = '\0';
         }
         if( pFileName->szExtension )
         {
            nLen = strlen( pFileName->szExtension );
            while( nLen && pFileName->szExtension[ nLen - 1 ] == ' ' )
               --nLen;
            while( nLen && pFileName->szExtension[ 0 ] == ' ' )
            {
               ++pFileName->szExtension;
               --nLen;
            }
            ( ( char * ) pFileName->szExtension )[ nLen ] = '\0';
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

#if defined( HB_OS_WIN )
HB_WCHAR * hb_fsNameConvU16( const char * szFileName )
{
   char * pszBuffer = NULL;
   HB_WCHAR * lpwFileName;

   if( s_fFnTrim || s_cDirSep != HB_OS_PATH_DELIM_CHR ||
       s_iFileCase != HB_SET_CASE_MIXED || s_iDirCase != HB_SET_CASE_MIXED )
   {
      PHB_FNAME pFileName;
      HB_SIZE nLen;

      szFileName = pszBuffer = hb_strncpy( ( char * ) hb_xgrab( HB_PATH_MAX ),
                                           szFileName, HB_PATH_MAX - 1 );

      if( s_cDirSep != HB_OS_PATH_DELIM_CHR )
      {
         char * p = ( char * ) szFileName;
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
            nLen = strlen( pFileName->szName );
            while( nLen && pFileName->szName[ nLen - 1 ] == ' ' )
               --nLen;
            while( nLen && pFileName->szName[ 0 ] == ' ' )
            {
               ++pFileName->szName;
               --nLen;
            }
            ( ( char * ) pFileName->szName )[ nLen ] = '\0';
         }
         if( pFileName->szExtension )
         {
            nLen = strlen( pFileName->szExtension );
            while( nLen && pFileName->szExtension[ nLen - 1 ] == ' ' )
               --nLen;
            while( nLen && pFileName->szExtension[ 0 ] == ' ' )
            {
               ++pFileName->szExtension;
               --nLen;
            }
            ( ( char * ) pFileName->szExtension )[ nLen ] = '\0';
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

   lpwFileName = hb_mbtowc( szFileName );
   if( pszBuffer )
      hb_xfree( pszBuffer );

   return lpwFileName;
}
#endif

int hb_setGetDirSeparator( void )
{
   return s_cDirSep;
}

void hb_compChkFileSwitches( int argc, char * argv[] )
{
   int i, n;

   for( i = 1; i < argc; ++i )
   {
      if( HB_ISOPTSEP( argv[ i ][ 0 ] ) && argv[ i ][ 1 ] == 'f' )
      {
         n = 0;
         switch( argv[ i ][ 2 ] )
         {
            case 'n':
               if( ! argv[ i ][ 3 ] )
               {
                  s_iFileCase = HB_SET_CASE_MIXED;
                  n = 3;
               }
               else if( argv[ i ][ 3 ] == ':' )
               {
                  if( argv[ i ][ 4 ] == 'u' )
                  {
                     s_iFileCase = HB_SET_CASE_UPPER;
                     n = 5;
                  }
                  else if( argv[ i ][ 4 ] == 'l' )
                  {
                     s_iFileCase = HB_SET_CASE_LOWER;
                     n = 5;
                  }
               }
               else if( argv[ i ][ 3 ] == '-' )
               {
                  s_iFileCase = HB_SET_CASE_MIXED;
                  n = 4;
               }
               break;

            case 'd':
               if( ! argv[ i ][ 3 ] )
               {
                  s_iDirCase = HB_SET_CASE_MIXED;
                  n = 3;
               }
               else if( argv[ i ][ 3 ] == ':' )
               {
                  if( argv[ i ][ 4 ] == 'u' )
                  {
                     s_iDirCase = HB_SET_CASE_UPPER;
                     n = 5;
                  }
                  else if( argv[ i ][ 4 ] == 'l' )
                  {
                     s_iDirCase = HB_SET_CASE_LOWER;
                     n = 5;
                  }
               }
               else if( argv[ i ][ 3 ] == '-' )
               {
                  s_iDirCase = HB_SET_CASE_MIXED;
                  n = 4;
               }
               break;

            case 'p':
               if( ! argv[ i ][ 3 ] )
               {
                  s_cDirSep = HB_OS_PATH_DELIM_CHR;
                  n = 3;
               }
               else if( argv[ i ][ 3 ] == '-' )
               {
                  s_cDirSep = HB_OS_PATH_DELIM_CHR;
                  n = 4;
               }
               else if( argv[ i ][ 3 ] == ':' && argv[ i ][ 4 ] )
               {
                  s_cDirSep = argv[ i ][ 4 ];
                  n = 5;
               }
               break;

            case 's':
               if( ! argv[ i ][ 3 ] )
               {
                  s_fFnTrim = HB_TRUE;
                  n = 3;
               }
               else if( argv[ i ][ 3 ] == '-' )
               {
                  s_fFnTrim = HB_FALSE;
                  n = 4;
               }
               break;
         }
         if( n )
         {
            argv[ i ] += n;
            if( argv[ i ][ 0 ] )
               --i;
            else
               argv[ i ] = ( char * ) "-";
         }
      }
   }
}
