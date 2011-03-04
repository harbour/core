/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    .prg interface to preprocessor
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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


#include "hbpp.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "hbvm.h"

HB_EXTERN_BEGIN

static void hb_pp_ErrorMessage( void * cargo, const char * const szMsgTable[],
                                char cPrefix, int iCode,
                                const char * szParam1, const char * szParam2 )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_pp_ErrorGen(%p, %p, %c, %d, %s, %s)", cargo, szMsgTable, cPrefix, iCode, szParam1, szParam2));

   HB_SYMBOL_UNUSED( cargo );

   /* ignore all warning messages and errors when break or quit request */
   if( cPrefix != 'W' && hb_vmRequestQuery() == 0 )
   {
      char szMsgBuf[ 1024 ];
      PHB_ITEM pError;
      hb_snprintf( szMsgBuf, sizeof( szMsgBuf ), szMsgTable[ iCode - 1 ],
                   szParam1, szParam2 );
      pError = hb_errRT_New( ES_ERROR, "PP", 1001, ( HB_ERRCODE ) iCode, szMsgBuf,
                             NULL, 0, EF_NONE | EF_CANDEFAULT );
      hb_errLaunch( pError );
      hb_errRelease( pError );
   }
}

static void hb_pp_Disp( void * cargo, const char * szMessage )
{
   /* ignore stdout messages when PP used as library */
   HB_SYMBOL_UNUSED( cargo );
   HB_SYMBOL_UNUSED( szMessage );
}

static HB_BOOL hb_pp_CompilerSwitch( void * cargo, const char * szSwitch,
                                     int * piValue, HB_BOOL fSet )
{
   /* ignore all compiler switches */
   HB_SYMBOL_UNUSED( cargo );
   HB_SYMBOL_UNUSED( szSwitch );
   HB_SYMBOL_UNUSED( piValue );
   HB_SYMBOL_UNUSED( fSet );

   return HB_FALSE;
}

/* PP destructor */
static HB_GARBAGE_FUNC( hb_pp_Destructor )
{
   PHB_PP_STATE * pStatePtr = ( PHB_PP_STATE * ) Cargo;

   if( * pStatePtr )
   {
      hb_pp_free( * pStatePtr );
      * pStatePtr = NULL;
   }
}

HB_EXTERN_END

static const HB_GC_FUNCS s_gcPPFuncs =
{
   hb_pp_Destructor,
   hb_gcDummyMark
};

static void hb_pp_StdRules( PHB_ITEM ppItem )
{
   static HB_BOOL s_fInit = HB_TRUE;
   static PHB_DYNS s_pDynSym;

   if( s_fInit )
   {
      s_pDynSym = hb_dynsymFind( "__PP_STDRULES" );
      s_fInit = HB_FALSE;
   }

   if( s_pDynSym )
   {
      hb_vmPushDynSym( s_pDynSym );
      hb_vmPushNil();
      hb_vmPush( ppItem );
      hb_vmProc( 1 );
   }
}

PHB_PP_STATE hb_pp_Param( int iParam )
{
   PHB_PP_STATE * pStatePtr =
                  ( PHB_PP_STATE * ) hb_parptrGC( &s_gcPPFuncs, iParam );

   if( pStatePtr )
      return * pStatePtr;
   else
      return NULL;
}

/*
 * initialize new PP context and return pointer to it.
 * __PP_INIT( [<cIncludePath>], [<cStdChFile> ] [, <lArchDefs>] ) -> <pPP>
 * when <cStdChFile> is empty string ("") then no default rules are used
 * only the dynamically created #defines like __HARBOUR__, __DATE__, __TIME__
 */
HB_FUNC( __PP_INIT )
{
   PHB_PP_STATE * pStatePtr, pState = hb_pp_new();

   if( pState )
   {
      const char * szPath = hb_parc( 1 ), * szStdCh = hb_parc( 2 );
      HB_BOOL fArchDefs = hb_parldef( 3, 1 );
      PHB_ITEM ppItem;

      pStatePtr = ( PHB_PP_STATE * ) hb_gcAllocate( sizeof( PHB_PP_STATE ),
                                                    &s_gcPPFuncs );
      * pStatePtr = pState;
      ppItem = hb_itemPutPtrGC( NULL, ( void * ) pStatePtr );

      hb_pp_init( pState, HB_TRUE, 0, NULL, NULL, NULL,
                  hb_pp_ErrorMessage, hb_pp_Disp, NULL, NULL,
                  hb_pp_CompilerSwitch );

      if( szPath )
         hb_pp_addSearchPath( pState, szPath, HB_TRUE );

      if( !szStdCh )
         hb_pp_StdRules( ppItem );
      else if( *szStdCh )
         hb_pp_readRules( pState, szStdCh );

      hb_pp_initDynDefines( pState, fArchDefs );
      hb_pp_setStdBase( pState );

      hb_itemReturnRelease( ppItem );
   }
   else
      hb_ret();
}

/*
 * add new (or replace previous) include paths.
 * __PP_PATH( <pPP>, <cPath> [, <lClearPrev>] ) -> NIL
 */
HB_FUNC( __PP_PATH )
{
   PHB_PP_STATE pState = hb_pp_Param( 1 );

   if( pState )
      hb_pp_addSearchPath( pState, hb_parc( 2 ), hb_parl( 3 ) );
}

/*
 * reset the PP context (remove all rules added by user or preprocessed code)
 * __PP_RESET( <pPP> ) -> NIL
 */
HB_FUNC( __PP_RESET )
{
   PHB_PP_STATE pState = hb_pp_Param( 1 );

   if( pState )
      hb_pp_reset( pState );
}

/*
 * preprocess and execute new preprocessor directive
 * __PP_ADDRULE( <pPP>, <cDirective> ) -> <lOK>
 */
HB_FUNC( __PP_ADDRULE )
{
   PHB_PP_STATE pState = hb_pp_Param( 1 );

   if( pState )
   {
      const char * szText = hb_parc( 2 );
      HB_SIZE nLen = hb_parclen( 2 );

      if( szText )
      {
         while( nLen && ( szText[ 0 ] == ' ' || szText[ 0 ] == '\t' ) )
         {
            ++szText;
            --nLen;
         }
      }

      if( szText && nLen && szText[ 0 ] == '#' )
      {
         hb_pp_parseLine( pState, szText, &nLen );

         /* probably for parsing #included files the old code was making
            sth like that */
         do
         {
            if( hb_vmRequestQuery() != 0 )
               return;
         }
         while( hb_pp_nextLine( pState, NULL ) );

         hb_retl( HB_TRUE );
         return;
      }
   }
   hb_retl( HB_FALSE );
}

/*
 * preprocess given code and return result
 * __PP_PROCESS( <pPP>, <cCode> ) -> <cPreprocessedCode>
 */
HB_FUNC( __PP_PROCESS )
{
   PHB_PP_STATE pState = hb_pp_Param( 1 );

   if( pState )
   {
      HB_SIZE nLen = hb_parclen( 2 );

      if( nLen )
      {
         char * szText = hb_pp_parseLine( pState, hb_parc( 2 ), &nLen );
         hb_retclen( szText, nLen );
         return;
      }
   }

   hb_retc_null();
}
