/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour Portable Object (.HRB) file runner
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
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
/*
 * The following functions are added Feb 01,2002 by
 *       Alexander Kresin <alex@belacy.belgorod.su>
 *
 *  __HRBLOAD()
 *  __HRBDO()
 *  __HRBUNLOAD()
 *  __HRBGETFU()
 *  __HRBDOFU()
 */

#include "hbapi.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbvm.h"
#include "hbpcode.h"

/* TODO: Fill the error codes with valid ones (instead of 9999) */
/* TOFIX: Change this assembler hack to something standard and portable */
/* TODO: Change the fopen()/fread()/fclose() calls to hb_fs*() */
/* TOFIX: Fix the memory leak on error. */

/* NOTE: This is the assembler output from : hb_vmExecute( pcode, symbols ).  */

/* #if INTEL32 */

static BYTE prgFunction[] =
{
   0x68, 0x00, 0x00, 0x00, 0x00,  /* push offset pcode               */
   0x68, 0x00, 0x00, 0x00, 0x00,  /* push offset symbols             */
   0xE8, 0x00, 0x00, 0x00, 0x00,  /* call near relative hb_vmExecute */
   0x83, 0xC4, 0x08,              /* add esp, 8                      */
   0xC3                           /* ret near                        */
};

/* #elseif INTEL16 */
/* #elseif MOTOROLA */
/* #elseif ... */
/* #endif */

typedef union
{
   BYTE *   pAsmData;                           /* The assembler bytes      */
   PHB_FUNC pFunPtr;                            /* The (dynamic) harbour
                                                   function                 */
} ASM_CALL, * PASM_CALL;

typedef struct
{
   char *     szName;                           /* Name of the function     */
   PASM_CALL  pAsmCall;                         /* Assembler call           */
   BYTE *     pCode;                            /* P-code                   */
} HB_DYNF, * PHB_DYNF;

typedef struct
{
   ULONG ulSymbols;                             /* Number of symbols        */
   ULONG ulFuncs;                               /* Number of functions      */
   LONG ulSymStart;                             /* Startup Symbol           */
   PHB_SYMB pSymRead;                           /* Symbols read             */
   PHB_DYNF pDynFunc;                           /* Functions read           */
} HRB_BODY, * PHRB_BODY;


#define SYM_NOLINK  0                           /* Symbol does not have to
                                                                  be linked */
#define SYM_FUNC    1                           /* Defined function         */
#define SYM_EXTERN  2                           /* Prev. defined function   */

#define SYM_NOT_FOUND 0xFFFFFFFF                /* Symbol not found.
                                                   FindSymbol               */

PHRB_BODY hb_hrbLoad( char* szHrb );
void hb_hrbDo( PHRB_BODY pHrbBody, int argc, char * argv[] );
void hb_hrbUnLoad( PHRB_BODY pHrbBody );
static ULONG     hb_hrbFindSymbol( char * szName, PHB_DYNF pDynFunc, ULONG ulLoaded );
static PASM_CALL hb_hrbAsmCreateFun( PHB_SYMB pSymbols, BYTE * pCode ); /* Create a dynamic function*/
static void      hb_hrbAsmPatch( BYTE * pCode, ULONG ulOffset, void * Address );
static void      hb_hrbAsmPatchRelative( BYTE * pCode, ULONG ulOffset, void * Address, ULONG ulNext );

static FILE *    hb_hrbFileOpen( char * szFileName );
static void      hb_hrbFileRead( FILE * file, char * szFileName, char * cBuffer, int iSize, int iCount );
static BYTE      hb_hrbFileReadByte( FILE * file, char * szFileName );
static int       hb_hrbFileReadHead( FILE * file, char * szFileName );
static char *    hb_hrbFileReadId( FILE * file, char * szFileName );
static long      hb_hrbFileReadLong( FILE * file, char * szFileName );
static void      hb_hrbFileClose( FILE * file );

static ULONG     s_ulSymEntry = 0;              /* Link enhancement         */

/*
   __HRBRUN( <cFile> [, xParam1 [, xParamN ] ] ) -> return value.

   This program will get the data from the .HRB file and run the p-code
   contained in it.

   In due time it should also be able to collect the data from the
   binary/executable itself
*/

HB_FUNC( __HRBRUN )
{
   int argc = hb_pcount();

   if( argc >= 1 )
   {
      PHRB_BODY pHrbBody = hb_hrbLoad( hb_parc( 1 ) );

      if( pHrbBody )
      {
         int i;
         char **argv = NULL;

         if( argc > 1 )
         {
            argv = (char**) hb_xgrab( sizeof(char*) * (argc-1) );
            for( i=0; i<argc-1; i++ )
               argv[i] = hb_parc( i+2 );
         }
         hb_hrbDo( pHrbBody, argc-1, argv );
         if( argv )
            hb_xfree( argv );
         hb_retl( 1 );
         hb_hrbUnLoad( pHrbBody );
      }
      else
         hb_retl( 0 );
   }
   else
      hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBRUN", 0 );
}

HB_FUNC( __HRBLOAD )
{
   if( hb_pcount() >= 1 )
      hb_retnl( (LONG) hb_hrbLoad( hb_parc( 1 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBLOAD", 0 );
}

HB_FUNC( __HRBDO )
{
   int argc = hb_pcount();

   if( argc >= 1 )
   {
      int i;
      char **argv = NULL;
      PHRB_BODY pHrbBody = (PHRB_BODY) hb_parnl( 1 );

      if( pHrbBody )
      {
         if( argc > 1 )
         {
            argv = (char**) hb_xgrab( sizeof(char*) * (argc-1) );
            for( i=0; i<argc-1; i++ )
               argv[i] = hb_parc( i+2 );
         }
         hb_hrbDo( pHrbBody, argc-1, argv );
         if( argv )
            hb_xfree( argv );
      }
      else
         hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBDO", 0 );
   }
   else
      hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBDO", 0 );
}

HB_FUNC( __HRBUNLOAD )
{
   if( hb_pcount() >= 1 )
      hb_hrbUnLoad( (PHRB_BODY) hb_parnl( 1 ) );
   else
      hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBUNLOAD", 0 );
}

HB_FUNC( __HRBGETFU )
{
   if( hb_pcount() > 1 && ISNUM( 1 ) && ISCHAR( 2 ) )
   {
      PHRB_BODY pHrbBody = (PHRB_BODY) hb_parnl( 1 );
      ULONG ulPos = 0;

      if( pHrbBody )
      {
         char * szName = hb_strupr( hb_strdup( hb_parc( 2 ) ) );

         while( ulPos < pHrbBody->ulSymbols )
         {
            if( !strcmp( szName, pHrbBody->pSymRead[ ulPos ].szName ) )
               break;
            ulPos++;
         }
         if( ulPos < pHrbBody->ulSymbols )
            hb_retnl( (LONG) ( pHrbBody->pSymRead + ulPos ) );
         else
            hb_retnl( 0 );
         hb_xfree( szName );
      }
      else
         hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBGETFU", 0 );
   }
   else
      hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBGETFU", 0 );
}

HB_FUNC( __HRBDOFU )
{
   int argc = hb_pcount();
   if( argc >=1 )
   {
      int i;
      PHB_SYMB pSym = (PHB_SYMB) hb_parnl( 1 );

      if( pSym )
      {
         hb_vmPushSymbol( pSym );
         hb_vmPushNil();
         for( i = 0; i < argc-1; i++ ) /* Push other  params  */
            hb_vmPush( hb_param( i + 2, HB_IT_ANY ) );

         hb_vmDo( argc-1 );            /* Run function        */
      }
      else
         hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBDOFU", 0 );
   }
   else
      hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBDOFU", 0 );
}

PHRB_BODY hb_hrbLoad( char* szHrb )
{
   char szFileName[ _POSIX_PATH_MAX + 1 ];
   PHB_FNAME pFileName;
   FILE * file;
   BOOL bError = FALSE;
   PHRB_BODY pHrbBody = NULL;

   /* Create full filename */

   pFileName = hb_fsFNameSplit( szHrb );

   if( ! pFileName->szExtension )
      pFileName->szExtension = ".hrb";

   hb_fsFNameMerge( szFileName, pFileName );

   hb_xfree( pFileName );

   /* Open as binary */

   while ( ( file = hb_hrbFileOpen( szFileName ) ) == NULL )
   {
      USHORT uiAction = hb_errRT_BASE_Ext1( EG_OPEN, 9999, NULL, szFileName, 0, EF_CANDEFAULT | EF_CANRETRY, 1, hb_paramError( 1 ) );

      if( uiAction == E_DEFAULT || uiAction == E_BREAK )
         break;
   }

   if( file )
   {
      ULONG ulSize;                                /* Size of function         */
      ULONG ul, ulPos;

      PHB_SYMB pSymRead;                           /* Symbols read             */
      PHB_DYNF pDynFunc;                           /* Functions read           */
      PHB_DYNS pDynSym;
      int nVersion = hb_hrbFileReadHead( file, szFileName );

      if( !nVersion )
      {
         hb_hrbFileClose( file );
         return NULL;
      }
      pHrbBody = ( PHRB_BODY ) hb_xgrab( sizeof( HRB_BODY ) );
      pHrbBody->ulSymStart = -1;
      pHrbBody->ulSymbols = hb_hrbFileReadLong( file, szFileName );
      pSymRead = ( PHB_SYMB ) hb_xgrab( pHrbBody->ulSymbols * sizeof( HB_SYMB ) );

      for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )  /* Read symbols in .HRB     */
      {
         pSymRead[ ul ].szName  = hb_hrbFileReadId( file, szFileName );
         pSymRead[ ul ].cScope  = hb_hrbFileReadByte( file, szFileName );
         pSymRead[ ul ].value.pFunPtr = ( PHB_FUNC ) ( ULONG ) hb_hrbFileReadByte( file, szFileName );
         pSymRead[ ul ].pDynSym = NULL;

         if ( pHrbBody->ulSymStart == -1 &&
                    pSymRead[ ul ].cScope & HB_FS_FIRST &&
                    ! ( pSymRead[ ul ].cScope & HB_FS_INITEXIT ) )
             pHrbBody->ulSymStart = ul;
      }

      pHrbBody->ulFuncs = hb_hrbFileReadLong( file, szFileName );  /* Read number of functions */
      pDynFunc = ( PHB_DYNF ) hb_xgrab( pHrbBody->ulFuncs * sizeof( HB_DYNF ) );
      memset( pDynFunc, 0, pHrbBody->ulFuncs * sizeof( HB_DYNF ) );
      for( ul = 0; ul < pHrbBody->ulFuncs; ul++ )         /* Read symbols in .HRB     */
      {
         pDynFunc[ ul ].szName = hb_hrbFileReadId( file, szFileName );

         ulSize = hb_hrbFileReadLong( file, szFileName );      /* Read size of function    */
         pDynFunc[ ul ].pCode = ( BYTE * ) hb_xgrab( ulSize );
         hb_hrbFileRead( file, szFileName, ( char * ) pDynFunc[ ul ].pCode, 1, ulSize );
                                             /* Read the block           */
         pDynFunc[ ul ].pAsmCall = hb_hrbAsmCreateFun( pSymRead,
                                              pDynFunc[ ul ].pCode );
                                             /* Create matching dynamic  */
                                             /* function                 */
      }

      pHrbBody->pSymRead = pSymRead;
      pHrbBody->pDynFunc = pDynFunc;
      s_ulSymEntry = 0;
      for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )    /* Linker                   */
      {
         if( ( ( ULONG ) pSymRead[ ul ].value.pFunPtr ) == SYM_FUNC )
         {
            ulPos = hb_hrbFindSymbol( pSymRead[ ul ].szName, pDynFunc, pHrbBody->ulFuncs );
            if( ulPos != SYM_NOT_FOUND )
            {
               /* Exists and NOT static ?  */
/*                if(    hb_dynsymFind( pSymRead[ ul ].szName ) &&
                   !( pSymRead[ ul ].cScope & HB_FS_STATIC ) )
               {
                  hb_errRT_BASE( EG_ARG, 9999, "Duplicate symbol", pSymRead[ ul ].szName );
                  bError = TRUE;
                  break;
               }
*/
               pSymRead[ ul ].value.pFunPtr = pDynFunc[ ulPos ].pAsmCall->pFunPtr;
            }
            else
               pSymRead[ ul ].value.pFunPtr = ( PHB_FUNC ) SYM_EXTERN;
         }
         if( ( ( ULONG ) pSymRead[ ul ].value.pFunPtr ) == SYM_EXTERN )
         {                                   /* External function        */
            pDynSym = hb_dynsymFind( pSymRead[ ul ].szName );
            if( !pDynSym )
            {
               char szName[21];
               strncpy( szName,pSymRead[ ul ].szName,20 );
               hb_hrbUnLoad( pHrbBody );
               hb_errRT_BASE( EG_ARG, 9999, "Unknown or unregistered symbol", szName, 0 );
               bError = TRUE;
               break;
            }
            pSymRead[ ul ].value.pFunPtr = pDynSym->pFunPtr;
         }
      }
      if( bError )
      {
         hb_hrbUnLoad( pHrbBody );
         pHrbBody = NULL;
      }

      hb_hrbFileClose( file );
   }
   hb_vmProcessSymbols( pHrbBody->pSymRead, ( USHORT ) pHrbBody->ulSymbols );
   return pHrbBody;
}

void hb_hrbDo( PHRB_BODY pHrbBody, int argc, char * argv[] )
{
   PHB_ITEM pRetVal = NULL;
   ULONG ul;
   int i;

   /* Initialize static variables first
    */
   for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )    /* Check INIT functions     */
   {
      if( ( pHrbBody->pSymRead[ ul ].cScope & HB_FS_INITEXIT ) == HB_FS_INITEXIT )
      {
         /* call (_INITSTATICS) function. This function assigns
          * literal values to static variables only. There is no need
          * to pass any parameters to this function because they
          * cannot be used to initialize static variable.
          */
         pHrbBody->pSymRead[ ul ].value.pFunPtr();
      }
   }
   for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )    /* Check INIT functions     */
   {
      if( ( pHrbBody->pSymRead[ ul ].cScope & HB_FS_INITEXIT ) == HB_FS_INIT )
      {
         hb_vmPushSymbol( pHrbBody->pSymRead + ul );
         hb_vmPushNil();
         for( i = 0; i < argc; i++ ) /* Push other cmdline params*/
            hb_vmPushString( argv[i],strlen(argv[i]) );

         hb_vmDo( argc );            /* Run init function        */
      }
   }

   /* May not have a startup symbol, if first symbol was an INIT Symbol (was executed already).*/
   if ( pHrbBody->ulSymStart >= 0 )
   {
       hb_vmPushSymbol( &( pHrbBody->pSymRead[ pHrbBody->ulSymStart ] ) );
       hb_vmPushNil();
       for( i = 0; i < ( hb_pcount() - 1 ); i++ )
          hb_vmPush( hb_param( i + 2, HB_IT_ANY ) ); /* Push other cmdline params*/
       hb_vmDo( hb_pcount() - 1 );                   /* Run the thing !!!        */

       pRetVal = hb_itemNew( NULL );
       hb_itemCopy( pRetVal, &hb_stack.Return );
   }

   for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )    /* Check EXIT functions     */
   {
      if( ( pHrbBody->pSymRead[ ul ].cScope & HB_FS_INITEXIT ) == HB_FS_EXIT )
      {
         hb_vmPushSymbol( pHrbBody->pSymRead + ul );
         hb_vmPushNil();
         hb_vmDo( 0 );                   /* Run exit function        */
         pHrbBody->pSymRead[ ul ].cScope = pHrbBody->pSymRead[ ul ].cScope & ( ~HB_FS_EXIT );
                                         /* Exit function cannot be
                                            handled by main in hvm.c */
      }
   }

   if( pRetVal )
      hb_itemRelease( hb_itemReturn( pRetVal ) );
}

void hb_hrbUnLoad( PHRB_BODY pHrbBody )
{
   ULONG ul;

   for( ul = 0; ul < pHrbBody->ulFuncs; ul++ )
   {
      hb_xfree( pHrbBody->pDynFunc[ ul ].pAsmCall->pAsmData );
      hb_xfree( pHrbBody->pDynFunc[ ul ].pAsmCall );
      hb_xfree( pHrbBody->pDynFunc[ ul ].pCode );
      hb_xfree( pHrbBody->pDynFunc[ ul ].szName );
   }

   for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )
      hb_xfree( pHrbBody->pSymRead[ ul ].szName );

   hb_xfree( pHrbBody->pDynFunc );
   hb_xfree( pHrbBody->pSymRead );
   hb_xfree( pHrbBody );
}

static ULONG hb_hrbFindSymbol( char * szName, PHB_DYNF pDynFunc, ULONG ulLoaded )
{
   ULONG ulRet;

   HB_TRACE(HB_TR_DEBUG, ("hb_hrbFindSymbol(%s, %p, %lu)", szName, pDynFunc, ulLoaded));

   if( ( s_ulSymEntry < ulLoaded ) &&             /* Is it a normal list ?    */
       !strcmp( szName, pDynFunc[ s_ulSymEntry ].szName ) )
      ulRet = s_ulSymEntry++;
   else
   {
      BOOL bFound = FALSE;

      ulRet = 0;
      while( !bFound && ulRet < ulLoaded )
      {
         if( !strcmp( szName, pDynFunc[ ulRet ].szName ) )
            bFound = TRUE;
         else
            ulRet++;
      }
      if( !bFound )
         ulRet = SYM_NOT_FOUND;
   }
   return ulRet;
}

static int hb_hrbFileReadHead( FILE * file, char * szFileName )
{
   unsigned char szHead[] = { (unsigned char)192,'H','R','B' }, szBuf[4];
   char cInt[ 2 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_hrbFileReadHead(%p)", file ));

   hb_hrbFileRead( file, szFileName, ( char * ) szBuf, 1, 4 );
   if( strncmp( ( char * ) szHead, ( char * ) szBuf, 4 ) )
   {
      hb_errRT_BASE_Ext1( EG_CORRUPTION, 9999, NULL, szFileName, 0, EF_CANDEFAULT, 1, hb_paramError( 1 ) );
      return 0;
   }
   hb_hrbFileRead( file, szFileName, cInt, 2, 1 );

   return ( ( BYTE ) cInt[ 0 ] ) +
          ( ( BYTE ) cInt[ 1 ] ) * 0x100 ;

}

/* ReadId
   Read the next (zero terminated) identifier */
static char * hb_hrbFileReadId( FILE * file, char * szFileName )
{
   char * szTemp;                                /* Temporary buffer         */
   char * szIdx;
   char * szRet;
   BOOL  bCont = TRUE;

   HB_TRACE(HB_TR_DEBUG, ("hb_hrbFileReadId(%p, %s)", file, szFileName));

   szTemp = ( char * ) hb_xgrab( 256 );
   szIdx  = szTemp;
   do
   {
      hb_hrbFileRead( file, szFileName, szIdx, 1, 1 );
      if( *szIdx )
         szIdx++;
      else
         bCont = FALSE;
   } while( bCont );

   szRet = ( char * ) hb_xgrab( szIdx - szTemp + 1 );
   strcpy( szRet, szTemp );
   hb_xfree( szTemp );

   return szRet;
}


static BYTE hb_hrbFileReadByte( FILE * file, char * szFileName )
{
   BYTE bRet;

   HB_TRACE(HB_TR_DEBUG, ("hb_hrbFileReadByte(%p, %s)", file, szFileName));

   hb_hrbFileRead( file, szFileName, ( char * ) &bRet, 1, 1 );

   return bRet;
}


static long hb_hrbFileReadLong( FILE * file, char * szFileName )
{
   char cLong[ 4 ];                               /* Temporary long           */

   HB_TRACE(HB_TR_DEBUG, ("hb_hrbFileReadLong(%p, %s)", file, szFileName));

   hb_hrbFileRead( file, szFileName, cLong, 4, 1 );

   if( cLong[ 3 ] )                             /* Convert to long if ok    */
   {
      hb_errRT_BASE_Ext1( EG_READ, 9999, NULL, szFileName, 0, EF_NONE, 0 );
      return 0;
   }
   else
      return ( ( BYTE ) cLong[ 0 ] )             +
             ( ( BYTE ) cLong[ 1 ] ) * 0x100     +
             ( ( BYTE ) cLong[ 2 ] ) * 0x10000   +
             ( ( BYTE ) cLong[ 3 ] ) * 0x1000000 ;
}


/*  hb_hrbFileRead
    Controlled read from file. If errornous -> Break */
static void hb_hrbFileRead( FILE * file, char * szFileName, char * cBuffer, int iSize, int iCount )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hrbFileRead(%p, %s, %p, %d, %d)", file, szFileName, cBuffer, iSize, iCount));

   if( iCount != ( int ) fread( cBuffer, iSize, iCount, file ) )
      hb_errRT_BASE_Ext1( EG_READ, 9999, NULL, szFileName, 0, EF_NONE, 0 );
}


/*  hb_hrbFileOpen
    Open an .HRB file  */
static FILE * hb_hrbFileOpen( char * szFileName )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hrbFileOpen(%s)", szFileName));

   return fopen( szFileName, "rb" );
}


/*  hb_hrbFileClose
    Close an .HRB file  */
static void hb_hrbFileClose( FILE * file )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hrbFileClose(%p)", file));

   fclose( file );
}


/*
   Create dynamic function.

   This function is needed, since it will allow the existing strategy of
   function pointers to work properly.

   For each Harbour function a little program calling the virtual machine
   should be present (see : *.c)

   Since these programs no longer exists when using this system, they should
   be create dynamically at run-time.

   If a .PRG contains 10 functions, 10 dynamic functions are created which
   are all the same :-) except for 2 pointers.
*/
static PASM_CALL hb_hrbAsmCreateFun( PHB_SYMB pSymbols, BYTE * pCode )
{
   PASM_CALL asmRet;

   HB_TRACE(HB_TR_DEBUG, ("hb_hrbAsmCreateFun(%p, %p)", pSymbols, pCode));

   asmRet = ( PASM_CALL ) hb_xgrab( sizeof( ASM_CALL ) );
   asmRet->pAsmData = ( BYTE * ) hb_xgrab( sizeof( prgFunction ) );
   memcpy( asmRet->pAsmData, prgFunction, sizeof( prgFunction ) );
                                              /* Copy new assembler code in */
/* #if INTEL32 */

   hb_hrbAsmPatch( asmRet->pAsmData, 1, pSymbols );   /* Insert pointer to testsym */
   hb_hrbAsmPatch( asmRet->pAsmData, 6, pCode );      /* Insert pointer to testcode */
   hb_hrbAsmPatchRelative( asmRet->pAsmData, 11, hb_vmExecute, 15 );
                                      /* Insert pointer to hb_vmExecute() */

/* #elseif INTEL16 */
/* #elseif MOTOROLA */
/* #elseif ... */
/* #endif */
   return asmRet;
}

/* Patch an address of the dynamic function */
static void hb_hrbAsmPatch( BYTE * pCode, ULONG ulOffset, void * Address )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hrbAsmPatch(%p, %lu, %p)", pCode, ulOffset, Address));

/* #if 32 bits and low byte first */

   pCode[ ulOffset     ] = ( BYTE ) ( ( ( ULONG ) Address       ) & 0xFF );
   pCode[ ulOffset + 1 ] = ( BYTE ) ( ( ( ULONG ) Address >>  8 ) & 0xFF );
   pCode[ ulOffset + 2 ] = ( BYTE ) ( ( ( ULONG ) Address >> 16 ) & 0xFF );
   pCode[ ulOffset + 3 ] = ( BYTE ) ( ( ( ULONG ) Address >> 24 ) & 0xFF );

/* #elseif 16 bits and low byte first */
/* #elseif 32 bits and high byte first */
/* #elseif ... */
/* #endif */
}


/* Intel specific ?? Patch an address relative to the next instruction */
static void hb_hrbAsmPatchRelative( BYTE * pCode, ULONG ulOffset,
                                    void * Address, ULONG ulNext )
{
   ULONG ulBase;
   ULONG ulRelative;

   HB_TRACE(HB_TR_DEBUG, ("hb_hrbAsmPatchRelative(%p, %lu, %p, %lu)", pCode, ulOffset, Address, ulNext));

/* #if 32 bits and low byte first */
   ulBase = ( ULONG ) pCode + ulNext;
                                /* Relative to next instruction */
   ulRelative = ( ULONG ) Address - ulBase;

   pCode[ ulOffset     ] = ( BYTE ) ( ( ulRelative       ) & 0xFF );
   pCode[ ulOffset + 1 ] = ( BYTE ) ( ( ulRelative >>  8 ) & 0xFF );
   pCode[ ulOffset + 2 ] = ( BYTE ) ( ( ulRelative >> 16 ) & 0xFF );
   pCode[ ulOffset + 3 ] = ( BYTE ) ( ( ulRelative >> 24 ) & 0xFF );

/* #elseif 16 bits and low byte first */
/* #elseif 32 bits and high byte first */
/* #elseif ... */
/* #endif */
}
