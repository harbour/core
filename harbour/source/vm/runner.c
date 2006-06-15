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

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbvm.h"
#include "hbpcode.h"
#include "hb_io.h"

/* TODO: Fill the error codes with valid ones (instead of 9999) */
/* TOFIX: Fix the memory leak on error. */

typedef struct
{
   char *         szName;                       /* Name of the function     */
   PHB_PCODEFUNC  pCodeFunc;                    /* Dynamic function info    */
   BYTE *         pCode;                        /* P-code                   */
} HB_DYNF, * PHB_DYNF;

typedef struct
{
   ULONG          ulSymbols;                    /* Number of symbols        */
   ULONG          ulFuncs;                      /* Number of functions      */
   BOOL           fInit;                        /* should be INIT functions executed */
   BOOL           fExit;                        /* should be EXIT functions executed */
   LONG           ulSymStart;                   /* Startup Symbol           */
   PHB_SYMB       pSymRead;                     /* Symbols read             */
   PHB_DYNF       pDynFunc;                     /* Functions read           */
   PHB_SYMBOLS    pModuleSymbols;
} HRB_BODY, * PHRB_BODY;


#define SYM_NOLINK  0                           /* Symbol does not have to be linked */
#define SYM_FUNC    1                           /* Defined function         */
#define SYM_EXTERN  2                           /* Prev. defined function   */
#define SYM_NOT_FOUND 0xFFFFFFFF                /* Symbol not found.        */

HB_EXTERN_BEGIN
HB_EXPORT PHRB_BODY hb_hrbLoad( char* szHrbBody, ULONG ulBodySize );
HB_EXPORT PHRB_BODY hb_hrbLoadFromFile( char* szHrb );
HB_EXPORT void hb_hrbDo( PHRB_BODY pHrbBody, int argc, char * argv[] );
HB_EXPORT void hb_hrbUnLoad( PHRB_BODY pHrbBody );
HB_EXTERN_END

static void hb_hrbInit( PHRB_BODY pHrbBody, int argc, char * argv[] );

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
      PHRB_BODY pHrbBody = hb_hrbLoadFromFile( hb_parcx( 1 ) );

      if( pHrbBody )
      {
         char **argv = NULL;
         int i;

         if( argc > 1 )
         {
            argv = (char**) hb_xgrab( sizeof(char*) * (argc-1) );

            for( i=0; i<argc-1; i++ )
            {
               argv[i] = hb_parcx( i+2 );
            }
         }

         hb_hrbDo( pHrbBody, argc-1, argv );

         if( argv )
         {
            hb_xfree( argv );
         }

         hb_retl( 1 );

         hb_hrbUnLoad( pHrbBody );
      }
      else
      {
         hb_retl( 0 );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBRUN", 0 );
   }
}

HB_FUNC( __HRBLOAD )
{
   int argc = hb_pcount();

   if( argc >= 1 )
   {
      BYTE szHead[] = { (BYTE)192,'H','R','B' };
      char * fileOrBody = hb_parc( 1 );
      PHRB_BODY pHrbBody;

      /* If parameter string */
      if ( fileOrBody && hb_parclen( 1 ) > 4 && strncmp( ( char * ) szHead, ( char * ) fileOrBody, 4 ) == 0 )
      {
         pHrbBody = hb_hrbLoad( fileOrBody, hb_parclen( 1 ) );
      }
      else
      {
         pHrbBody = hb_hrbLoadFromFile( fileOrBody );
      }
      if ( pHrbBody )
      {
         char **argv = NULL;
         int i;

         if( argc > 1 )
         {
            argv = (char**) hb_xgrab( sizeof(char*) * (argc-1) );

            for( i=0; i<argc-1; i++ )
            {
               argv[i] = hb_parcx( i+2 );
            }
         }

         hb_hrbInit( pHrbBody, argc-1, argv );

         if( argv )
         {
            hb_xfree( argv );
         }
      }
      hb_retptr( ( void *) pHrbBody );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 9998, NULL, "__HRBLOAD", 0 );
   }
}

HB_FUNC( __HRBDO )
{
   int argc = hb_pcount();

   if( argc >= 1 )
   {
      int i;
      char **argv = NULL;
      PHRB_BODY pHrbBody = (PHRB_BODY) hb_parptr( 1 );

      if( pHrbBody )
      {
         if( argc > 1 )
         {
            argv = ( char ** ) hb_xgrab( sizeof( char * ) * ( argc - 1 ) );

            for( i=0; i < argc - 1; i++ )
            {
               argv[i] = hb_parcx( i+2 );
            }
         }

         hb_hrbDo( pHrbBody, argc-1, argv );

         if( argv )
         {
            hb_xfree( argv );
         }
      }
      else
      {
         hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBDO", 0 );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBDO", 0 );
   }
}

HB_FUNC( __HRBUNLOAD )
{
   if( hb_pcount() >= 1 )
   {
      hb_hrbUnLoad( (PHRB_BODY) hb_parptr( 1 ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBUNLOAD", 0 );
   }
}

HB_FUNC( __HRBGETFU )
{
   if( hb_pcount() > 1 && ISPOINTER( 1 ) && ISCHAR( 2 ) )
   {
      PHRB_BODY pHrbBody = (PHRB_BODY) hb_parptr( 1 );
      ULONG ulPos = 0;

      if( pHrbBody )
      {
         char * szName = hb_strupr( hb_strdup( hb_parcx( 2 ) ) );

         while( ulPos < pHrbBody->ulSymbols )
         {
            if( !strcmp( szName, pHrbBody->pSymRead[ ulPos ].szName ) )
            {
               break;
            }

            ulPos++;
         }

         if( ulPos < pHrbBody->ulSymbols )
         {
            hb_retptr( ( void *) ( pHrbBody->pSymRead + ulPos ) );
         }
         else
         {
            hb_retptr( NULL );
         }

         hb_xfree( szName );
      }
      else
      {
         hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBGETFU", 0 );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBGETFU", 0 );
   }
}

HB_FUNC( __HRBDOFU )
{
   int argc = hb_pcount();

   if( argc >=1 )
   {
      int i;
      PHB_SYMB pSym = (PHB_SYMB) hb_parptr( 1 );

      if( pSym )
      {
         hb_vmPushSymbol( pSym );
         hb_vmPushNil();

         for( i = 0; i < argc-1; i++ ) /* Push other  params  */
         {
            hb_vmPush( hb_param( i + 2, HB_IT_ANY ) );
         }

         hb_vmDo( argc-1 );            /* Run function        */
      }
      else
      {
         hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBDOFU", 0 );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBDOFU", 0 );
   }
}


static ULONG hb_hrbFindSymbol( char * szName, PHB_DYNF pDynFunc, ULONG ulLoaded )
{
   ULONG ulRet = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_hrbFindSymbol(%s, %p, %lu)", szName, pDynFunc, ulLoaded));

   while( ulRet < ulLoaded )
   {
      if( ! strcmp( szName, pDynFunc[ ulRet ].szName ) )
      {
         return ulRet;
      }
      ulRet++;
   }
   return SYM_NOT_FOUND;
}

static int hb_hrbReadHead( char * szBody, ULONG ulBodySize, ULONG * ulBodyOffset )
{
   BYTE szHead[] = { (BYTE)192,'H','R','B' };
   char cInt[ 2 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_hrbReadHead(%p,%i,%i)", szBody, ulBodySize, * ulBodyOffset ));

   if( ulBodySize < 6 || strncmp( ( char * ) szHead, ( char * ) szBody, 4 ) )
   {
      hb_errRT_BASE( EG_CORRUPTION, 9999, NULL, "__HRBLOAD", 0 );
      return 0;
   }

   cInt[0] = szBody[(*ulBodyOffset)+4];
   cInt[1] = szBody[(*ulBodyOffset)+5];

   * ulBodyOffset += 6;    // header + version offset

   return HB_PCODE_MKSHORT( cInt );
}

/* ReadId
   Read the next (zero terminated) identifier */
static char * hb_hrbReadId( char * szBody, ULONG ulBodySize, ULONG * ulBodyOffset )
{
   char * szIdx;

   HB_TRACE(HB_TR_DEBUG, ("hb_hrbReadId(%p,%lu,%p)", szBody, ulBodySize, ulBodyOffset));

   szIdx = &szBody[ *ulBodyOffset ];

   do
   {
      if ( *ulBodyOffset > ulBodySize )
      {
         hb_errRT_BASE( EG_CORRUPTION, 9999, NULL, "__HRBLOAD", 0 );
         szIdx = "";
         break;
      }
   }
   while( szBody[ ( *ulBodyOffset )++ ] );

   return hb_strdup( szIdx );
}


static LONG hb_hrbReadLong( char * szBody, ULONG ulBodySize, ULONG * ulBodyOffset )
{
   char cLong[ 4 ];                               /* Temporary long           */

   HB_TRACE(HB_TR_DEBUG, ("hb_hrbReadLong(%p,%i,%i)", szBody, ulBodySize, * ulBodyOffset));

   if ( (* ulBodyOffset + 4) > ulBodySize )
   {
      hb_errRT_BASE( EG_CORRUPTION, 9999, NULL, "__HRBLOAD", 0 );
      return 0;
   }

   memcpy( cLong, (char *) (szBody+(*ulBodyOffset)), 4 );

   * ulBodyOffset += 4;

   if( cLong[ 3 ] )                             /* Convert to long if ok    */
   {
      hb_errRT_BASE( EG_CORRUPTION, 9999, NULL, "__HRBLOAD", 0 );
      return 0;
   }
   else
   {
      return HB_PCODE_MKLONG( cLong );
   }
}

static void hb_hrbInitStatic( PHRB_BODY pHrbBody )
{
   if( ! pHrbBody->fInit && ! pHrbBody->fExit )
   {
      ULONG ul;

      pHrbBody->fInit = TRUE;
      /* Initialize static variables first */
      for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )    /* Check _INITSTATICS functions */
      {
         if( ( pHrbBody->pSymRead[ ul ].scope.value & HB_FS_INITEXIT ) == HB_FS_INITEXIT )
         {
            /* call (_INITSTATICS) function. This function assigns
             * literal values to static variables only. There is no need
             * to pass any parameters to this function because they
             * cannot be used to initialize static variable.
             */

            /* changed to call VM execution instead of direct function address call
             * pHrbBody->pSymRead[ ul ].value.pFunPtr();
             * [MLombardo]
             */

            hb_vmPushSymbol( &(pHrbBody->pSymRead[ ul ]) );
            hb_vmPushNil();
            hb_vmDo( 0 );

         }
      }
   }
}

static void hb_hrbInit( PHRB_BODY pHrbBody, int argc, char * argv[] )
{
   if ( pHrbBody->fInit )
   {
      ULONG ul;
      int i;

      pHrbBody->fInit = FALSE;
      pHrbBody->fExit = TRUE;

      for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )    /* Check INIT functions */
      {
         if( ( pHrbBody->pSymRead[ ul ].scope.value & HB_FS_INITEXIT ) == HB_FS_INIT )
         {
            hb_vmPushSymbol( pHrbBody->pSymRead + ul );
            hb_vmPushNil();
            for( i = 0; i < argc; i++ ) /* Push other cmdline params*/
               hb_vmPushString( argv[i], strlen( argv[i] ) );

            hb_vmDo( argc );            /* Run init function        */
         }
      }
   }
}

static void hb_hrbExit( PHRB_BODY pHrbBody )
{
   if ( pHrbBody->fExit )
   {
      ULONG ul;

      pHrbBody->fExit = FALSE;
      pHrbBody->fInit = TRUE;

      for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )
      {
         if( ( pHrbBody->pSymRead[ ul ].scope.value & HB_FS_INITEXIT ) == HB_FS_EXIT )
         {
            hb_vmPushSymbol( pHrbBody->pSymRead + ul );
            hb_vmPushNil();
            hb_vmDo( 0 );
         }
      }
   }
}

void hb_hrbUnLoad( PHRB_BODY pHrbBody )
{
   ULONG ul;

   hb_hrbExit( pHrbBody );

   if( pHrbBody->pModuleSymbols )
   {
      hb_vmFreeSymbols( pHrbBody->pModuleSymbols );
   }

   for( ul = 0; ul < pHrbBody->ulFuncs; ul++ )
   {
      PHB_DYNS pDyn;

      pDyn = hb_dynsymFind( pHrbBody->pDynFunc[ ul ].szName );
      if( pDyn && pDyn->pSymbol->value.pCodeFunc == pHrbBody->pDynFunc[ ul ].pCodeFunc )
      {
         pDyn->pSymbol->value.pCodeFunc = NULL;
      }

      hb_xfree( pHrbBody->pDynFunc[ ul ].pCodeFunc );
      hb_xfree( pHrbBody->pDynFunc[ ul ].pCode );
      hb_xfree( pHrbBody->pDynFunc[ ul ].szName );
   }

   hb_xfree( pHrbBody->pDynFunc );
   hb_xfree( pHrbBody );
}

PHRB_BODY hb_hrbLoad( char* szHrbBody, ULONG ulBodySize )
{
   PHRB_BODY pHrbBody = NULL;
   ULONG ulBodyOffset = 0;

   if( szHrbBody )
   {
      ULONG ulSize;                                /* Size of function         */
      ULONG ul, ulPos;

      PHB_SYMB pSymRead;                           /* Symbols read             */
      PHB_DYNF pDynFunc;                           /* Functions read           */
      PHB_DYNS pDynSym;

      int nVersion = hb_hrbReadHead( (char *) szHrbBody, (ULONG) ulBodySize, &ulBodyOffset );

      if( !nVersion )
      {
         return NULL;
      }

      pHrbBody = ( PHRB_BODY ) hb_xgrab( sizeof( HRB_BODY ) );

      pHrbBody->fInit = FALSE;
      pHrbBody->fExit = FALSE;
      pHrbBody->ulSymStart = -1;
      pHrbBody->ulFuncs = 0;
      pHrbBody->pSymRead = NULL;
      pHrbBody->pDynFunc = NULL;
      pHrbBody->ulSymbols = hb_hrbReadLong( (char *) szHrbBody, ulBodySize, &ulBodyOffset );
      pHrbBody->pModuleSymbols = NULL;

      pSymRead = ( PHB_SYMB ) hb_xgrab( pHrbBody->ulSymbols * sizeof( HB_SYMB ) );

      for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )  /* Read symbols in .HRB */
      {
         pSymRead[ ul ].szName = hb_hrbReadId( (char *) szHrbBody, ulBodySize, &ulBodyOffset );
         pSymRead[ ul ].scope.value = szHrbBody[ulBodyOffset++];
         pSymRead[ ul ].value.pCodeFunc = ( PHB_PCODEFUNC ) ( HB_PTRDIFF ) szHrbBody[ulBodyOffset++];
         pSymRead[ ul ].pDynSym = NULL;

         if( pHrbBody->ulSymStart == -1 &&
             ( pSymRead[ ul ].scope.value & HB_FS_FIRST ) != 0 &&
             ( pSymRead[ ul ].scope.value & HB_FS_INITEXIT ) == 0 )
         {
            pHrbBody->ulSymStart = ul;
         }
      }

      pHrbBody->ulFuncs = hb_hrbReadLong( (char *) szHrbBody, ulBodySize, &ulBodyOffset );  /* Read number of functions */

      pDynFunc = ( PHB_DYNF ) hb_xgrab( pHrbBody->ulFuncs * sizeof( HB_DYNF ) );
      memset( pDynFunc, 0, pHrbBody->ulFuncs * sizeof( HB_DYNF ) );

      for( ul = 0; ul < pHrbBody->ulFuncs; ul++ )
      {
         pDynFunc[ ul ].szName = hb_hrbReadId( (char *) szHrbBody, ulBodySize, &ulBodyOffset );
         ulSize = hb_hrbReadLong( (char *) szHrbBody, ulBodySize, &ulBodyOffset );      /* Read size of function    */
         pDynFunc[ ul ].pCode = ( BYTE * ) hb_xgrab( ulSize );

         /* Read the block */
         memcpy( ( char * ) pDynFunc[ ul ].pCode, (char *) (szHrbBody + ulBodyOffset), ulSize );
         ulBodyOffset += ulSize;
         pDynFunc[ ul ].pCodeFunc = (PHB_PCODEFUNC) hb_xgrab( sizeof( HB_PCODEFUNC ) );
         pDynFunc[ ul ].pCodeFunc->pCode    = pDynFunc[ ul ].pCode;
         pDynFunc[ ul ].pCodeFunc->pSymbols = pSymRead;
      }

      pHrbBody->pSymRead = pSymRead;
      pHrbBody->pDynFunc = pDynFunc;

      for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )    /* Linker */
      {
         if( pSymRead[ ul ].value.pCodeFunc == ( PHB_PCODEFUNC ) SYM_FUNC )
         {
            ulPos = hb_hrbFindSymbol( pSymRead[ ul ].szName, pDynFunc, pHrbBody->ulFuncs );

            if( ulPos == SYM_NOT_FOUND )
            {
               pSymRead[ ul ].value.pCodeFunc = ( PHB_PCODEFUNC ) SYM_EXTERN;
            }
            else
            {
               pSymRead[ ul ].value.pCodeFunc = ( PHB_PCODEFUNC ) pDynFunc[ ulPos ].pCodeFunc;
               pSymRead[ ul ].scope.value |= HB_FS_PCODEFUNC; /* | HB_FS_LOCAL; */
            }
         }

         /* External function */
         if( pSymRead[ ul ].value.pCodeFunc == ( PHB_PCODEFUNC ) SYM_EXTERN )
         {
            pDynSym = hb_dynsymFind( pSymRead[ ul ].szName );

            if( pDynSym )
            {
               pSymRead[ ul ].value.pFunPtr = pDynSym->pSymbol->value.pFunPtr;
               if( pDynSym->pSymbol->scope.value & HB_FS_PCODEFUNC )
               {
                  pSymRead[ ul ].scope.value |= HB_FS_PCODEFUNC;
               }
            }
            else
            {
               char szName[21];

               strncpy( szName, pSymRead[ ul ].szName, 20 );

               hb_hrbUnLoad( pHrbBody );
               hb_errRT_BASE( EG_ARG, 9999, "Unknown or unregistered symbol", szName, 0 );
               return NULL;
            }
         }
      }

      if( pHrbBody )
      {
         pHrbBody->pModuleSymbols = hb_vmRegisterSymbols( pHrbBody->pSymRead,
                  ( USHORT ) pHrbBody->ulSymbols, "pcode.hrb", 0, TRUE, FALSE );

         if( pHrbBody->pModuleSymbols->pModuleSymbols != pSymRead )
         {
            /*
             * Old unused symbol table has been recycled - free the one
             * we allocated and disactivate static initialization [druzus]
             */
            pHrbBody->pSymRead = pHrbBody->pModuleSymbols->pModuleSymbols;

            for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )
            {
               if( pSymRead[ ul ].szName )
                  hb_xfree( pSymRead[ ul ].szName );
            }
            hb_xfree( pSymRead );

            pHrbBody->fInit = TRUE;
         }
         else
         {
            /* mark symbol table as dynamically allocated so HVM will free it on exit */
            pHrbBody->pModuleSymbols->fAllocated = TRUE;

            /* initialize static variables */
            hb_hrbInitStatic( pHrbBody );
         }
      }
   }

   return pHrbBody;
}

PHRB_BODY hb_hrbLoadFromFile( char* szHrb )
{
   char szFileName[ _POSIX_PATH_MAX + 1 ];
   PHRB_BODY pHrbBody = NULL;
   PHB_FNAME pFileName;
   FHANDLE file;

   /* Create full filename */

   pFileName = hb_fsFNameSplit( szHrb );

   if( ! pFileName->szExtension )
   {
      pFileName->szExtension = ".hrb";
   }

   hb_fsFNameMerge( szFileName, pFileName );

   hb_xfree( pFileName );

   /* Open as binary */

   while ( ( file = hb_fsOpen( ( BYTE *)szFileName, FO_READ )) == 0 )
   {
      USHORT uiAction = hb_errRT_BASE_Ext1( EG_OPEN, 9999, NULL, szFileName, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY, HB_ERR_ARGS_BASEPARAMS );

      if( uiAction == E_DEFAULT || uiAction == E_BREAK )
      {
         break;
      }
   }

   if( file )
   {
      ULONG ulBodySize = hb_fsSeek( file, 0, FS_END );

      if( ulBodySize )
      {
         BYTE * pbyBuffer;

         pbyBuffer = ( BYTE * ) hb_xgrab( ulBodySize + sizeof( char ) + 1 );
         hb_fsSeek( file, 0, FS_SET );
         hb_fsReadLarge( file, pbyBuffer, ulBodySize );
         pbyBuffer[ ulBodySize ] = '\0';

         pHrbBody = hb_hrbLoad( (char*) pbyBuffer, (ULONG) ulBodySize );

         hb_xfree( pbyBuffer );
      }
      hb_fsClose( file );
   }
   return( pHrbBody );
}

void hb_hrbDo( PHRB_BODY pHrbBody, int argc, char * argv[] )
{
   PHB_ITEM pRetVal = NULL;
   int i;

   hb_hrbInit( pHrbBody, argc, argv );

   /* May not have a startup symbol, if first symbol was an INIT Symbol (was executed already).*/
   if ( pHrbBody->ulSymStart >= 0 )
   {
       hb_vmPushSymbol( &( pHrbBody->pSymRead[ pHrbBody->ulSymStart ] ) );
       hb_vmPushNil();

       for( i = 0; i < ( hb_pcount() - 1 ); i++ )
       {
          hb_vmPush( hb_param( i + 2, HB_IT_ANY ) ); /* Push other cmdline params*/
       }

       hb_vmDo( hb_pcount() - 1 );                   /* Run the thing !!!        */

       pRetVal = hb_itemNew( NULL );
       hb_itemMove( pRetVal, hb_stackReturnItem() );
   }

   hb_hrbExit( pHrbBody );

   if( pRetVal )
   {
      hb_itemRelease( hb_itemReturnForward( pRetVal ) );
   }
}
