/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    HB_COMPILE() - compiler interface
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#include "hbcomp.h"

static void s_pp_msg( void * cargo, int iErrorFmt, int iLine,
                      const char * szModule, char cPrefix, int iValue,
                      const char * szText,
                      const char * szPar1, const char * szPar2 )
{
   HB_SYMBOL_UNUSED( cargo );

   /* ignore all warning messages and errors when break or quit request */
   if( cPrefix != 'W' && hb_vmRequestQuery() == 0 )
   {
      char szMsgBuf[ 512 ], szLine[ 512 ];
      PHB_ITEM pError;

      hb_snprintf( szMsgBuf, sizeof( szMsgBuf ), szText, szPar1, szPar2 );
      if( !szModule || *szModule == 0 || strcmp( szModule, "{SOURCE}.prg" ) == 0 )
         hb_snprintf( szLine, sizeof( szLine ),
                      "line:%i", iLine );
      else
         hb_snprintf( szLine, sizeof( szLine ),
                      iErrorFmt == HB_ERRORFMT_CLIPPER ? "%s(%i)" : "%s:%i",
                      szModule, iLine );
      pError = hb_errRT_New( ES_ERROR, "COMPILER", 1001, ( HB_ERRCODE ) iValue,
                             szMsgBuf, szLine, 0 /*OsCode*/, EF_NONE );
      hb_errLaunch( pError );
      hb_errRelease( pError );
   }
}

static int s_pp_openFile( void * cargo, char * szFileName,
                          HB_BOOL fBefore, HB_BOOL fSysFile, HB_BOOL fBinary,
                          HB_PATHNAMES * pIncludePaths,
                          HB_BOOL * pfNested, FILE ** file_ptr,
                          const char ** pBufPtr, HB_SIZE *pnLen, HB_BOOL *pfFree )
{
   HB_SYMBOL_UNUSED( fSysFile );
   HB_SYMBOL_UNUSED( fBinary );
   HB_SYMBOL_UNUSED( pIncludePaths );
   HB_SYMBOL_UNUSED( pfNested );
   HB_SYMBOL_UNUSED( file_ptr );

   if( !fBefore )
   {
      HB_COMP_DECL = ( HB_COMP_PTR ) cargo;
      PHB_ITEM pIncItem = ( PHB_ITEM ) HB_COMP_PARAM->cargo;

      if( pIncItem )
      {
         if( HB_IS_HASH( pIncItem ) )
         {
            PHB_ITEM pFileItem = hb_hashGetCItemPtr( pIncItem, szFileName );

            if( pFileItem )
            {
               HB_SIZE nLen = hb_itemGetCLen( pFileItem );
               if( nLen )
               {
                  *pBufPtr = hb_itemGetCPtr( pFileItem );
                  *pnLen   = nLen;
                  *pfFree  = HB_FALSE;
                  return HB_PP_OPEN_OK;
               }
            }
         }
      }
   }

   return HB_PP_OPEN_FILE;
}

static void hb_compGenArgList( int iFirst, int iLast,
                               int * pArgC, const char *** pArgV,
                               PHB_ITEM * pIncItem,
                               PHB_PP_OPEN_FUNC * pOpenFunc,
                               PHB_PP_MSG_FUNC * pMsgFunc )
{
   PHB_ITEM pParam;
   HB_SIZE ul, nLen;
   int argc = 1, i;
   const char ** argv;

   if( pMsgFunc )
   {
      * pMsgFunc = NULL;
      if( HB_ISLOG( iFirst ) )
      {
         if( hb_parl( iFirst ) )
            * pMsgFunc = s_pp_msg;
         ++iFirst;
      }
   }

   if( pIncItem && pOpenFunc )
   {
      *pOpenFunc = NULL;
      *pIncItem = hb_param( iFirst, HB_IT_HASH );
      if( *pIncItem )
      {
         ++iFirst;
         *pOpenFunc = s_pp_openFile;
      }
   }

   for( i = iFirst; i <= iLast; ++i )
   {
      pParam = hb_param( i, HB_IT_ARRAY | HB_IT_STRING );
      if( pParam )
      {
         if( HB_IS_ARRAY( pParam ) )
         {
            ul = hb_arrayLen( pParam );
            if( ul ) do
            {
               if( hb_arrayGetType( pParam, ul ) & HB_IT_STRING )
                  ++argc;
            }
            while( --ul );
         }
         else if( HB_IS_STRING( pParam ) )
            ++argc;
      }
   }

   argv = ( const char ** ) hb_xgrab( sizeof( char * ) * ( argc + 1 ) );
   argc = 0;
   for( i = iFirst; i <= iLast; ++i )
   {
      pParam = hb_param( i, HB_IT_ARRAY | HB_IT_STRING );
      if( pParam )
      {
         if( HB_IS_ARRAY( pParam ) )
         {
            nLen = hb_arrayLen( pParam );
            for( ul = 1; ul <= nLen; ++ul )
            {
               if( hb_arrayGetType( pParam, ul ) & HB_IT_STRING )
                  argv[ argc++ ] = hb_arrayGetCPtr( pParam, ul );
            }
         }
         else if( HB_IS_STRING( pParam ) )
            argv[ argc++ ] = hb_itemGetCPtr( pParam );
      }
   }
   argv[ argc ] = NULL;

   * pArgC = argc;
   * pArgV = argv;
}

HB_FUNC( HB_COMPILE )
{
   int argc;
   const char ** argv;
   PHB_ITEM pIncItem;
   PHB_PP_OPEN_FUNC pOpenFunc;
   PHB_PP_MSG_FUNC pMsgFunc;

   hb_compGenArgList( 1, hb_pcount(), &argc, &argv, &pIncItem, &pOpenFunc, &pMsgFunc );
   hb_retni( hb_compMainExt( argc, argv, NULL, NULL, NULL, 0, pIncItem, pOpenFunc, pMsgFunc ) );
   hb_xfree( argv );
}

HB_FUNC( HB_COMPILEBUF )
{
   int iResult, argc;
   const char ** argv;
   PHB_ITEM pIncItem;
   PHB_PP_OPEN_FUNC pOpenFunc;
   PHB_PP_MSG_FUNC pMsgFunc;
   HB_BYTE * pBuffer;
   HB_SIZE nLen;

   hb_compGenArgList( 1, hb_pcount(), &argc, &argv, &pIncItem, &pOpenFunc, &pMsgFunc );
   iResult = hb_compMainExt( argc, argv, &pBuffer, &nLen, NULL, 0, pIncItem, pOpenFunc, pMsgFunc );
   hb_xfree( argv );

   if( iResult == EXIT_SUCCESS && pBuffer )
      hb_retclen_buffer( ( char * ) pBuffer, nLen );
}

HB_FUNC( HB_COMPILEFROMBUF )
{
   int iResult, argc;
   const char ** argv;
   const char * szSource;
   PHB_ITEM pIncItem;
   PHB_PP_OPEN_FUNC pOpenFunc;
   PHB_PP_MSG_FUNC pMsgFunc;
   HB_BYTE * pBuffer;
   HB_SIZE nLen;

   szSource = hb_parc( 1 );
   if( szSource )
   {
      hb_compGenArgList( 2, hb_pcount(), &argc, &argv, &pIncItem, &pOpenFunc, &pMsgFunc );
      iResult = hb_compMainExt( argc, argv, &pBuffer, &nLen, szSource, 0, pIncItem, pOpenFunc, pMsgFunc );
      hb_xfree( argv );

      if( iResult == EXIT_SUCCESS && pBuffer )
         hb_retclen_buffer( ( char * ) pBuffer, nLen );
   }
}
