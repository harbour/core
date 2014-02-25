/*
 * Harbour Project source code:
 * Harbour Portable Object (.hrb) file runner
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2002 Alexander Kresin <alex@belacy.belgorod.su>
 *    hb_hrbLoad()
 *    hb_hrbDo()
 *    hb_hrbUnload()
 *    hb_hrbGetFunSym()
 *
 * See COPYING.txt for licensing terms.
 *
 */

#include "hbvmint.h"
#include "hbapi.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbvm.h"
#include "hbpcode.h"
#include "hbset.h"
#include "hb_io.h"
#include "hbhrb.ch"

typedef struct
{
   char *        szName;                        /* Name of the function     */
   PHB_PCODEFUNC pCodeFunc;                     /* Dynamic function info    */
   HB_BYTE *     pCode;                         /* P-code                   */
} HB_DYNF, * PHB_DYNF;

typedef struct
{
   HB_ULONG    ulSymbols;                       /* Number of symbols        */
   HB_ULONG    ulFuncs;                         /* Number of functions      */
   HB_BOOL     fInit;                           /* should be INIT functions executed */
   HB_BOOL     fExit;                           /* should be EXIT functions executed */
   HB_LONG     lSymStart;                       /* Startup Symbol           */
   PHB_SYMB    pSymRead;                        /* Symbols read             */
   PHB_DYNF    pDynFunc;                        /* Functions read           */
   PHB_SYMBOLS pModuleSymbols;
} HRB_BODY, * PHRB_BODY;

static const char s_szHead[ 4 ] = { '\xC0', 'H', 'R', 'B' };


#define SYM_NOLINK     0            /* symbol does not have to be linked */
#define SYM_FUNC       1            /* function defined in this module   */
#define SYM_EXTERN     2            /* function defined in other module  */
#define SYM_DEFERRED   3            /* lately bound function             */
#define SYM_NOT_FOUND  0xFFFFFFFFUL /* Symbol not found.                 */

static HB_SIZE hb_hrbCheckSig( const char * szBody, HB_SIZE nBodySize )
{
   return ( nBodySize > sizeof( s_szHead ) &&
               memcmp( s_szHead, szBody, sizeof( s_szHead ) ) == 0 ) ?
            sizeof( s_szHead ) : 0;
}

static int hb_hrbReadHead( const char * szBody, HB_SIZE nBodySize, HB_SIZE * pnBodyOffset )
{
   const char * pVersion;
   HB_SIZE nSigSize;

   HB_TRACE( HB_TR_DEBUG, ( "hb_hrbReadHead(%p,%" HB_PFS "u,%p)", szBody, nBodySize, pnBodyOffset ) );

   nSigSize = hb_hrbCheckSig( szBody, nBodySize );

   if( nSigSize == 0 || nBodySize - nSigSize < 2 )
      return 0;

   pVersion = szBody + nSigSize;
   *pnBodyOffset += nSigSize + 2;

   return HB_PCODE_MKSHORT( pVersion );
}

static HB_BOOL hb_hrbReadValue( const char * szBody, HB_SIZE nBodySize, HB_SIZE * pnBodyOffset, HB_ULONG * pulValue )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hrbReadValue(%p,%" HB_PFS "u,%p,%p)", szBody, nBodySize, pnBodyOffset, pulValue ) );

   if( *pnBodyOffset + 4 < nBodySize )
   {
      *pulValue = HB_PCODE_MKLONG( szBody + *pnBodyOffset );
      *pnBodyOffset += 4;

      if( *pulValue <= 0x00FFFFFFUL )
         return HB_TRUE;
   }

   return HB_FALSE;
}

/* ReadId
   Read the next (zero terminated) identifier */
static char * hb_hrbReadId( const char * szBody, HB_SIZE nBodySize, HB_SIZE * pnBodyOffset )
{
   const char * szIdx;

   HB_TRACE( HB_TR_DEBUG, ( "hb_hrbReadId(%p,%" HB_PFS "u,%p)", szBody, nBodySize, pnBodyOffset ) );

   szIdx = &szBody[ *pnBodyOffset ];

   do
   {
      if( *pnBodyOffset > nBodySize )
         return NULL;
   }
   while( szBody[ ( *pnBodyOffset )++ ] );

   return hb_strdup( szIdx );
}

static HB_ULONG hb_hrbFindSymbol( const char * szName, PHB_DYNF pDynFunc, HB_ULONG ulLoaded )
{
   HB_ULONG ulRet;

   HB_TRACE( HB_TR_DEBUG, ( "hb_hrbFindSymbol(%s, %p, %lu)", szName, pDynFunc, ulLoaded ) );

   for( ulRet = 0; ulRet < ulLoaded; ++ulRet )
   {
      if( ! strcmp( szName, pDynFunc[ ulRet ].szName ) )
         return ulRet;
   }

   return SYM_NOT_FOUND;
}

static void hb_hrbInitStatic( PHRB_BODY pHrbBody )
{
   if( ! pHrbBody->fInit && ! pHrbBody->fExit )
   {
      HB_ULONG ul;

      pHrbBody->fInit = HB_TRUE;
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

            hb_vmPushSymbol( &( pHrbBody->pSymRead[ ul ] ) );
            hb_vmPushNil();
            hb_vmProc( 0 );
         }
      }
   }
}

static void hb_hrbInit( PHRB_BODY pHrbBody, int iPCount, PHB_ITEM * pParams )
{
   if( pHrbBody->fInit )
   {
      if( hb_vmRequestReenter() )
      {
         HB_ULONG ul;
         HB_BOOL fRepeat, fClipInit = HB_TRUE;
         int i;

         pHrbBody->fInit = HB_FALSE;
         pHrbBody->fExit = HB_TRUE;

         do
         {
            fRepeat = HB_FALSE;
            ul = pHrbBody->ulSymbols;
            while( ul-- )
            {
               /* Check INIT functions */
               if( ( pHrbBody->pSymRead[ ul ].scope.value & HB_FS_INITEXIT ) == HB_FS_INIT )
               {
                  if( strcmp( pHrbBody->pSymRead[ ul ].szName, "CLIPINIT$" ) ?
                      ! fClipInit : fClipInit )
                  {
                     hb_vmPushSymbol( pHrbBody->pSymRead + ul );
                     hb_vmPushNil();
                     for( i = 0; i < iPCount; i++ )
                        hb_vmPush( pParams[ i ] );
                     hb_vmProc( ( HB_USHORT ) iPCount );
                     if( hb_vmRequestQuery() != 0 )
                        break;
                  }
                  else if( fClipInit )
                     fRepeat = HB_TRUE;
               }
            }
            fClipInit = HB_FALSE;
         }
         while( fRepeat && hb_vmRequestQuery() == 0 );

         hb_vmRequestRestore();
      }
   }
}

static void hb_hrbExit( PHRB_BODY pHrbBody )
{
   if( pHrbBody->fExit )
   {
      if( hb_vmRequestReenter() )
      {
         HB_ULONG ul;

         pHrbBody->fExit = HB_FALSE;
         pHrbBody->fInit = HB_TRUE;

         for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )
         {
            if( ( pHrbBody->pSymRead[ ul ].scope.value & HB_FS_INITEXIT ) == HB_FS_EXIT )
            {
               hb_vmPushSymbol( pHrbBody->pSymRead + ul );
               hb_vmPushNil();
               hb_vmProc( 0 );
               if( hb_vmRequestQuery() != 0 )
                  break;
            }
         }

         hb_vmRequestRestore();
      }
   }
}

static void hb_hrbUnLoad( PHRB_BODY pHrbBody )
{
   HB_ULONG ul;

   hb_hrbExit( pHrbBody );

   if( pHrbBody->pModuleSymbols )
   {
      hb_vmFreeSymbols( pHrbBody->pModuleSymbols );
   }

   if( pHrbBody->pDynFunc )
   {
      for( ul = 0; ul < pHrbBody->ulFuncs; ul++ )
      {
         PHB_DYNS pDyn;

         if( pHrbBody->pDynFunc[ ul ].szName &&
             pHrbBody->pDynFunc[ ul ].pCodeFunc )
         {
            pDyn = hb_dynsymFind( pHrbBody->pDynFunc[ ul ].szName );
            if( pDyn && pDyn->pSymbol->value.pCodeFunc ==
                        pHrbBody->pDynFunc[ ul ].pCodeFunc )
            {
               pDyn->pSymbol->value.pCodeFunc = NULL;
            }
         }
         if( pHrbBody->pDynFunc[ ul ].pCodeFunc )
            hb_xfree( pHrbBody->pDynFunc[ ul ].pCodeFunc );
         if( pHrbBody->pDynFunc[ ul ].pCode )
            hb_xfree( pHrbBody->pDynFunc[ ul ].pCode );
         if( pHrbBody->pDynFunc[ ul ].szName )
            hb_xfree( pHrbBody->pDynFunc[ ul ].szName );
      }

      hb_xfree( pHrbBody->pDynFunc );
   }

   hb_xfree( pHrbBody );
}



static PHRB_BODY hb_hrbLoad( const char * szHrbBody, HB_SIZE nBodySize, HB_USHORT usMode, const char * szFileName )
{
   PHRB_BODY pHrbBody = NULL;

   if( szHrbBody )
   {
      HB_SIZE nBodyOffset = 0;
      HB_SIZE nSize;                              /* Size of function */
      HB_SIZE nPos;
      HB_ULONG ul;
      char * buffer, ch;
      HB_USHORT usBind = ( usMode & HB_HRB_BIND_MODEMASK );

      PHB_SYMB pSymRead;                           /* Symbols read     */
      PHB_DYNF pDynFunc;                           /* Functions read   */
      PHB_DYNS pDynSym;

      int iVersion = hb_hrbReadHead( szHrbBody, nBodySize, &nBodyOffset );

      if( iVersion == 0 )
      {
         hb_errRT_BASE( EG_CORRUPTION, 9995, NULL, HB_ERR_FUNCNAME, 0 );
         return NULL;
      }

      pHrbBody = ( PHRB_BODY ) hb_xgrab( sizeof( HRB_BODY ) );

      pHrbBody->fInit = HB_FALSE;
      pHrbBody->fExit = HB_FALSE;
      pHrbBody->lSymStart = -1;
      pHrbBody->ulFuncs = 0;
      pHrbBody->pSymRead = NULL;
      pHrbBody->pDynFunc = NULL;
      pHrbBody->pModuleSymbols = NULL;
      if( ! hb_hrbReadValue( szHrbBody, nBodySize, &nBodyOffset, &pHrbBody->ulSymbols ) ||
            pHrbBody->ulSymbols == 0 )
      {
         hb_hrbUnLoad( pHrbBody );
         hb_errRT_BASE( EG_CORRUPTION, 9996, NULL, HB_ERR_FUNCNAME, 0 );
         return NULL;
      }

      /* calculate the size of dynamic symbol table */
      nPos = nBodyOffset;
      nSize = 0;

      for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )  /* Read symbols in .hrb */
      {
         while( nBodyOffset < nBodySize )
         {
            ++nSize;
            if( szHrbBody[ nBodyOffset++ ] == 0 )
               break;
         }
         nBodyOffset += 2;
         if( nBodyOffset >= nBodySize )
         {
            hb_hrbUnLoad( pHrbBody );
            hb_errRT_BASE( EG_CORRUPTION, 9997, NULL, HB_ERR_FUNCNAME, 0 );
            return NULL;
         }
      }

      nBodyOffset = nPos;
      ul = pHrbBody->ulSymbols * sizeof( HB_SYMB );
      pSymRead = ( PHB_SYMB ) hb_xgrab( nSize + ul );
      buffer = ( ( char * ) pSymRead ) + ul;

      for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )  /* Read symbols in .hrb */
      {
         pSymRead[ ul ].szName = buffer;
         do
         {
            ch = *buffer++ = szHrbBody[ nBodyOffset++ ];
         }
         while( ch );
         pSymRead[ ul ].scope.value = ( HB_BYTE ) szHrbBody[ nBodyOffset++ ];
         pSymRead[ ul ].value.pCodeFunc = ( PHB_PCODEFUNC ) ( HB_PTRDIFF ) szHrbBody[ nBodyOffset++ ];
         pSymRead[ ul ].pDynSym = NULL;

         if( pHrbBody->lSymStart == -1 &&
             ( pSymRead[ ul ].scope.value & HB_FS_FIRST ) != 0 &&
             ( pSymRead[ ul ].scope.value & HB_FS_INITEXIT ) == 0 )
         {
            pHrbBody->lSymStart = ul;
         }
      }

      /* Read number of functions */
      if( ! hb_hrbReadValue( szHrbBody, nBodySize, &nBodyOffset, &pHrbBody->ulFuncs ) )
      {
         hb_xfree( pSymRead );
         hb_hrbUnLoad( pHrbBody );
         hb_errRT_BASE( EG_CORRUPTION, 9997, NULL, HB_ERR_FUNCNAME, 0 );
         return NULL;
      }

      pHrbBody->pSymRead = pSymRead;

      if( pHrbBody->ulFuncs )
      {
         pDynFunc = ( PHB_DYNF ) hb_xgrab( pHrbBody->ulFuncs * sizeof( HB_DYNF ) );
         memset( pDynFunc, 0, pHrbBody->ulFuncs * sizeof( HB_DYNF ) );
         pHrbBody->pDynFunc = pDynFunc;

         for( ul = 0; ul < pHrbBody->ulFuncs; ul++ )
         {
            HB_ULONG ulValue;

            /* Read name of function */
            pDynFunc[ ul ].szName = hb_hrbReadId( szHrbBody, nBodySize, &nBodyOffset );
            if( pDynFunc[ ul ].szName == NULL )
               break;

            /* Read size of function */
            if( ! hb_hrbReadValue( szHrbBody, nBodySize, &nBodyOffset, &ulValue ) )
               break;

            nSize = ( HB_SIZE ) ulValue;

            if( nBodyOffset + nSize > nBodySize )
               break;

            /* Copy function body */
            pDynFunc[ ul ].pCode = ( HB_BYTE * ) hb_xgrab( nSize );
            memcpy( ( char * ) pDynFunc[ ul ].pCode, szHrbBody + nBodyOffset, nSize );
            nBodyOffset += nSize;

            pDynFunc[ ul ].pCodeFunc = ( PHB_PCODEFUNC ) hb_xgrab( sizeof( HB_PCODEFUNC ) );
            pDynFunc[ ul ].pCodeFunc->pCode    = pDynFunc[ ul ].pCode;
            pDynFunc[ ul ].pCodeFunc->pSymbols = pSymRead;
         }

         if( ul < pHrbBody->ulFuncs )
         {
            hb_xfree( pSymRead );
            hb_hrbUnLoad( pHrbBody );
            hb_errRT_BASE( EG_CORRUPTION, 9998, NULL, HB_ERR_FUNCNAME, 0 );
            return NULL;
         }
      }

      /* End of PCODE loading, now linking */
      for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )
      {
         if( pSymRead[ ul ].value.pCodeFunc == ( PHB_PCODEFUNC ) SYM_FUNC )
         {
            nPos = hb_hrbFindSymbol( pSymRead[ ul ].szName, pHrbBody->pDynFunc, pHrbBody->ulFuncs );

            if( nPos == SYM_NOT_FOUND )
            {
               pSymRead[ ul ].value.pCodeFunc = ( PHB_PCODEFUNC ) SYM_EXTERN;
            }
            else
            {
               pSymRead[ ul ].value.pCodeFunc = ( PHB_PCODEFUNC ) pHrbBody->pDynFunc[ nPos ].pCodeFunc;
               pSymRead[ ul ].scope.value |= HB_FS_PCODEFUNC | HB_FS_LOCAL |
                  ( usBind == HB_HRB_BIND_FORCELOCAL ? HB_FS_STATIC : 0 );
            }
         }
         else if( pSymRead[ ul ].value.pCodeFunc == ( PHB_PCODEFUNC ) SYM_DEFERRED )
         {
            pSymRead[ ul ].value.pCodeFunc = ( PHB_PCODEFUNC ) SYM_EXTERN;
            pSymRead[ ul ].scope.value |= HB_FS_DEFERRED;
         }

         /* External function */
         if( pSymRead[ ul ].value.pCodeFunc == ( PHB_PCODEFUNC ) SYM_EXTERN )
         {
            pSymRead[ ul ].value.pCodeFunc = NULL;

            pDynSym = hb_dynsymFind( pSymRead[ ul ].szName );

            if( pDynSym )
            {
               pSymRead[ ul ].value.pFunPtr = pDynSym->pSymbol->value.pFunPtr;
               if( pDynSym->pSymbol->scope.value & HB_FS_PCODEFUNC )
               {
                  pSymRead[ ul ].scope.value |= HB_FS_PCODEFUNC;
               }
            }
            else if( ( pSymRead[ ul ].scope.value & HB_FS_DEFERRED ) == 0 )
            {
               if( ( usMode & HB_HRB_BIND_LAZY ) != 0 )
                  pSymRead[ ul ].scope.value |= HB_FS_DEFERRED;
               else
               {
                  char szName[ HB_SYMBOL_NAME_LEN + 1 ];

                  hb_strncpy( szName, pSymRead[ ul ].szName, sizeof( szName ) - 1 );
                  hb_xfree( pSymRead );
                  hb_hrbUnLoad( pHrbBody );
                  hb_errRT_BASE( EG_ARG, 6101, "Unknown or unregistered symbol", szName, 0 );
                  return NULL;
               }
            }
         }
      }

      if( hb_vmLockModuleSymbols() )
      {
         if( usBind == HB_HRB_BIND_LOCAL )
         {
            for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )
            {
               if( ( pSymRead[ ul ].scope.value &
                     ( HB_FS_LOCAL | HB_FS_STATIC ) ) == HB_FS_LOCAL )
               {
                  pDynSym = hb_dynsymFind( pSymRead[ ul ].szName );
                  if( pDynSym )
                  {
                     /* convert public function to static one */
                     pSymRead[ ul ].scope.value |= HB_FS_STATIC;
                  }
               }
            }
         }

         pHrbBody->pModuleSymbols = hb_vmRegisterSymbols( pHrbBody->pSymRead,
                        ( HB_USHORT ) pHrbBody->ulSymbols,
                        szFileName ? szFileName : "pcode.hrb", 0,
                        HB_TRUE, HB_FALSE, usBind == HB_HRB_BIND_OVERLOAD );

         if( pHrbBody->pModuleSymbols->pModuleSymbols != pSymRead )
         {
            /*
             * Old unused symbol table has been recycled - free the one
             * we allocated and disactivate static initialization [druzus]
             */
            pHrbBody->pSymRead = pHrbBody->pModuleSymbols->pModuleSymbols;
            hb_xfree( pSymRead );

            pHrbBody->fInit = HB_TRUE;
         }
         else
         {
            /* mark symbol table as dynamically allocated so HVM will free it on exit */
            pHrbBody->pModuleSymbols->fAllocated = HB_TRUE;

            /* initialize static variables */
            hb_hrbInitStatic( pHrbBody );
         }
         hb_vmUnlockModuleSymbols();
      }
      else
      {
         hb_xfree( pSymRead );
         hb_hrbUnLoad( pHrbBody );
         pHrbBody = NULL;
      }
   }

   return pHrbBody;
}

static PHRB_BODY hb_hrbLoadFromFile( const char * szHrb, HB_USHORT usMode )
{
   PHRB_BODY pHrbBody = NULL;
   PHB_ITEM pError = NULL;
   PHB_FILE pFile;

   /* Open as binary */
   do
   {
      pFile = hb_fileExtOpen( szHrb,
                              hb_stackSetStruct()->HB_SET_DEFEXTENSIONS ? ".hrb" : NULL,
                              FO_READ | FXO_SHARELOCK, NULL, pError );
      if( pFile == NULL )
      {
         pError = hb_errRT_FileError( pError, NULL, EG_OPEN, 6102, szHrb );
         if( hb_errLaunch( pError ) != E_RETRY )
            break;
      }
   }
   while( pFile == NULL );

   if( pError )
      hb_itemRelease( pError );

   if( pFile != NULL )
   {
      HB_SIZE nBodySize = hb_fileSize( pFile );

      if( nBodySize )
      {
         char * pbyBuffer;

         pbyBuffer = ( char * ) hb_xgrab( nBodySize + sizeof( char ) + 1 );
         hb_fileReadAt( pFile, pbyBuffer, nBodySize, 0 );
         pbyBuffer[ nBodySize ] = '\0';

         pHrbBody = hb_hrbLoad( ( const char * ) pbyBuffer, nBodySize, usMode, szHrb );
         hb_xfree( pbyBuffer );
      }
      hb_fileClose( pFile );
   }

   return pHrbBody;
}

static void hb_hrbDo( PHRB_BODY pHrbBody, int iPCount, PHB_ITEM * pParams )
{
   PHB_ITEM pRetVal = NULL;
   int i;

   hb_hrbInit( pHrbBody, iPCount, pParams );

   /* May not have a startup symbol, if first symbol was an INIT Symbol (was executed already). */
   if( pHrbBody->lSymStart >= 0 && hb_vmRequestQuery() == 0 )
   {
      hb_vmPushSymbol( &pHrbBody->pSymRead[ pHrbBody->lSymStart ] );
      hb_vmPushNil();

      for( i = 0; i < iPCount; i++ )
         hb_vmPush( pParams[ i ] );

      hb_vmProc( ( HB_USHORT ) iPCount );

      pRetVal = hb_itemNew( NULL );
      hb_itemMove( pRetVal, hb_stackReturnItem() );
   }

   if( pRetVal )
      hb_itemReturnRelease( pRetVal );
}

/* HRB module destructor */
static HB_GARBAGE_FUNC( hb_hrb_Destructor )
{
   PHRB_BODY * pHrbPtr = ( PHRB_BODY * ) Cargo;

   if( *pHrbPtr )
   {
      hb_hrbUnLoad( *pHrbPtr );
      *pHrbPtr = NULL;
   }
}

static const HB_GC_FUNCS s_gcHrbFuncs =
{
   hb_hrb_Destructor,
   hb_gcDummyMark
};

static PHRB_BODY hb_hrbParam( int iParam )
{
   PHRB_BODY * pHrbPtr = ( PHRB_BODY * ) hb_parptrGC( &s_gcHrbFuncs, iParam );

   return pHrbPtr ? *pHrbPtr : NULL;
}

static void hb_hrbReturn( PHRB_BODY pHrbBody )
{
   PHRB_BODY * pHrbPtr = ( PHRB_BODY * ) hb_gcAllocate( sizeof( PHRB_BODY ),
                                                        &s_gcHrbFuncs );

   *pHrbPtr = pHrbBody;
   hb_retptrGC( pHrbPtr );
}

/*
   hb_hrbRun( [ <nOptions>, ] <cHrb> [, <xparams,...> ] ) -> <retVal>

   This program will get the data from the .hrb file and run the p-code
   contained in it.

   In due time it should also be able to collect the data from the
   binary/executable itself
 */
HB_FUNC( HB_HRBRUN )
{
   HB_USHORT usMode = HB_HRB_BIND_DEFAULT;
   HB_USHORT nParam = 1;
   HB_SIZE nLen;

   if( HB_ISNUM( 1 ) )
   {
      usMode = ( HB_USHORT ) hb_parni( 1 );
      nParam++;
   }

   nLen = hb_parclen( nParam );

   if( nLen > 0 )
   {
      const char * fileOrBody = hb_parc( nParam );
      PHRB_BODY pHrbBody;

      if( hb_hrbCheckSig( fileOrBody, nLen ) != 0 )
         pHrbBody = hb_hrbLoad( fileOrBody, nLen, usMode, NULL );
      else
         pHrbBody = hb_hrbLoadFromFile( fileOrBody, usMode );

      if( pHrbBody )
      {
         int iPCount = hb_pcount() - nParam, i;
         PHB_ITEM * pParams = NULL;

         if( iPCount > 0 )
         {
            pParams = ( PHB_ITEM * ) hb_xgrab( sizeof( PHB_ITEM ) * iPCount );
            for( i = 0; i < iPCount; i++ )
               pParams[ i ] = hb_stackItemFromBase( i + 1 + nParam );
         }

         hb_hrbDo( pHrbBody, iPCount, pParams );

         if( pParams )
            hb_xfree( pParams );

         hb_hrbUnLoad( pHrbBody );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 6103, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* hb_hrbLoad( [ <nOptions>, ] <cHrb> [, <xparams,...> ] ) */

HB_FUNC( HB_HRBLOAD )
{
   HB_USHORT usMode = HB_HRB_BIND_DEFAULT;
   HB_USHORT nParam = 1;
   HB_SIZE nLen;

   if( HB_ISNUM( 1 ) )
   {
      usMode = ( HB_USHORT ) hb_parni( 1 );
      nParam++;
   }

   nLen = hb_parclen( nParam );

   if( nLen > 0 )
   {
      const char * fileOrBody = hb_parc( nParam );
      PHRB_BODY pHrbBody;

      if( hb_hrbCheckSig( fileOrBody, nLen ) != 0 )
         pHrbBody = hb_hrbLoad( fileOrBody, nLen, usMode, NULL );
      else
         pHrbBody = hb_hrbLoadFromFile( fileOrBody, usMode );

      if( pHrbBody )
      {
         int iPCount = hb_pcount() - nParam;
         PHB_ITEM * pParams = NULL;
         int i;

         if( iPCount > 0 )
         {
            pParams = ( PHB_ITEM * ) hb_xgrab( sizeof( PHB_ITEM ) * iPCount );

            for( i = 0; i < iPCount; i++ )
               pParams[ i ] = hb_stackItemFromBase( i + 1 + nParam );
         }

         hb_hrbInit( pHrbBody, iPCount, pParams );

         if( pParams )
            hb_xfree( pParams );
      }
      hb_hrbReturn( pHrbBody );
   }
   else
      hb_errRT_BASE( EG_ARG, 9998, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HRBDO )
{
   PHRB_BODY pHrbBody = hb_hrbParam( 1 );

   if( pHrbBody )
   {
      int iPCount = hb_pcount() - 1;
      PHB_ITEM * pParams = NULL;
      int i;

      if( iPCount > 0 )
      {
         pParams = ( PHB_ITEM * ) hb_xgrab( sizeof( PHB_ITEM ) * iPCount );

         for( i = 0; i < iPCount; i++ )
            pParams[ i ] = hb_stackItemFromBase( i + 2 );
      }

      hb_hrbDo( pHrbBody, iPCount, pParams );

      if( pParams )
         hb_xfree( pParams );
   }
   else
      hb_errRT_BASE( EG_ARG, 6104, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HRBUNLOAD )
{
   PHRB_BODY * pHrbPtr = ( PHRB_BODY * ) hb_parptrGC( &s_gcHrbFuncs, 1 );

   if( pHrbPtr )
   {
      PHRB_BODY pHrbBody = *pHrbPtr;

      if( pHrbBody )
      {
         *pHrbPtr = NULL;
         hb_hrbUnLoad( pHrbBody );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 6105, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HRBGETFUNSYM )
{
   PHRB_BODY pHrbBody = hb_hrbParam( 1 );
   const char * szName = hb_parc( 2 );

   if( pHrbBody && szName )
   {
      PHB_SYMB pSym;
      HB_ULONG nPos;

      for( nPos = 0, pSym = pHrbBody->pSymRead; nPos < pHrbBody->ulSymbols; ++pSym, ++nPos )
      {
         if( pSym->value.pFunPtr != NULL &&
             ( pSym->scope.value & HB_FS_INITEXIT ) == 0 &&
             hb_stricmp( szName, pSym->szName ) == 0 )
         {
            hb_itemPutSymbol( hb_stackReturnItem(), pSym );
            break;
         }
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 6106, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HRBGETFUNLIST )
{
   PHRB_BODY pHrbBody = hb_hrbParam( 1 );

   if( pHrbBody )
   {
      PHB_SYMB pSym;
      HB_ULONG nPos;
      PHB_ITEM paList = hb_itemArrayNew( 0 );
      PHB_ITEM pFuncName = hb_itemNew( NULL );
      int iType = hb_parni( 2 );

      for( nPos = 0, pSym = pHrbBody->pSymRead; nPos < pHrbBody->ulSymbols; ++pSym, ++nPos )
      {
         if( pSym->value.pFunPtr != NULL &&
             ( pSym->scope.value & HB_FS_INITEXIT ) == 0 )
         {
            if( iType == 0 ||
                ( ( iType & HB_HRB_FUNC_EXTERN ) &&
                  ( pSym->scope.value & HB_FS_LOCAL ) == 0 ) ||
                ( ( pSym->scope.value & HB_FS_LOCAL ) &&
                  ( ( ( iType & HB_HRB_FUNC_STATIC ) &&
                      ( pSym->scope.value & HB_FS_STATIC ) ) ||
                    ( ( iType & HB_HRB_FUNC_PUBLIC ) &&
                      ( pSym->scope.value & HB_FS_STATIC ) == 0 ) ) ) )
            {
               hb_arrayAdd( paList, hb_itemPutC( pFuncName, pSym->szName ) );
            }
         }
      }

      hb_itemRelease( pFuncName );
      hb_itemReturnRelease( paList );
   }
   else
      hb_errRT_BASE( EG_ARG, 6107, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HRBSIGNATURE )
{
   hb_retclen( s_szHead, sizeof( s_szHead ) );
}
