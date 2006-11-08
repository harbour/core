/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * 
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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


#include "hbpp.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "hbvm.h"

static PHB_PP_STATE s_pp_state = NULL;

static void hb_pp_ErrorMessage( char * szMsgTable[], char cPrefix, int iCode,
                                const char * szParam1, const char * szParam2 )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_pp_ErrorGen(%p, %c, %d, %s, %s)", szMsgTable, cPrefix, iCode, szParam1, szParam2));

   /* ignore warning messages */
   if( cPrefix != 'W' )
   {
      char szMsgBuf[ 1024 ];
      PHB_ITEM pError;
      sprintf( szMsgBuf, szMsgTable[ iCode - 1 ], szParam1, szParam2 );
      pError = hb_errRT_New( ES_ERROR, "PP", 9999, ( ULONG ) iCode, szMsgBuf,
                             NULL, 0, EF_NONE | EF_CANDEFAULT );
      hb_errLaunch( pError );
      hb_errRelease( pError );
   }
}

static void hb_pp_Disp( const char * szMessage )
{
   /* ignore stdout messages when PP used as library */
   HB_SYMBOL_UNUSED( szMessage );
}

static PHB_PP_STATE hb_pp_stateParam( int * piParam )
{
   PHB_ITEM pItem = hb_parptr( 1 );

   if( pItem )
   {
      * piParam = 2;
      return ( PHB_PP_STATE ) hb_itemGetPtr( pItem );
   }
   else
   {
      * piParam = 1;
      if( !s_pp_state )
      {
         s_pp_state = hb_pp_new();
         hb_pp_init( s_pp_state, NULL, TRUE, NULL, NULL,
                     hb_pp_ErrorMessage, hb_pp_Disp, NULL, NULL );
      }
      return s_pp_state;
   }
}

HB_FUNC( __PP_INIT )
{
   PHB_PP_STATE pState;
   int iParam;

   pState = hb_pp_stateParam( &iParam );
   if( pState )
   {
      hb_pp_reset( pState );

      if( ISCHAR( iParam ) )
         hb_pp_addSearchPath( pState, hb_parc( iParam ), TRUE );

      hb_retptr( pState );
   }
   else
      hb_ret();
}

HB_FUNC( __PP_FREE )
{
   PHB_ITEM pItem = hb_parptr( 1 );

   if( pItem )
   {
      PHB_PP_STATE pState = ( PHB_PP_STATE ) hb_itemGetPtr( pItem );
      if( pState )
         hb_pp_free( pState );
   }
   else if( s_pp_state )
   {
      hb_pp_free( s_pp_state );
      s_pp_state = NULL;
   }
}

HB_FUNC( __PP_PATH )
{
   PHB_PP_STATE pState;
   int iParam;

   pState = hb_pp_stateParam( &iParam );
   if( pState )
   {
      hb_pp_addSearchPath( pState, hb_parc( iParam ), hb_parl( iParam + 1 ) );
   }
}

HB_FUNC( __PPADDRULE )
{
   PHB_PP_STATE pState;
   int iParam;

   pState = hb_pp_stateParam( &iParam );
   if( pState )
   {
      char * szText = hb_parc( iParam );
      ULONG ulLen = hb_parclen( iParam );

      if( szText )
      {
         while( ulLen && ( szText[ 0 ] == ' ' || szText[ 0 ] == '\t' ) )
         {
            ++szText;
            --ulLen;
         }
      }

      if( szText && ulLen && szText[ 0 ] == '#' )
      {
         hb_pp_reset( pState );
         hb_pp_parseLine( pState, szText, &ulLen );

         /* probably for #included files parsing the old code was making
            sth like that */
         do
         {
            if( hb_vmRequestQuery() != 0 )
               return;
         }
         while( hb_pp_nextLine( pState, NULL ) );

         hb_retl( TRUE );
         return;
      }
   }
   hb_retl( FALSE );
}

HB_FUNC( __PREPROCESS )
{
   PHB_PP_STATE pState;
   int iParam;

   pState = hb_pp_stateParam( &iParam );
   if( pState )
   {
      char * szText = hb_parc( iParam );
      ULONG ulLen = hb_parclen( iParam );

      if( szText && ulLen )
      {
         szText = hb_pp_parseLine( pState, szText, &ulLen );
         hb_retclen( szText, ulLen );
         return;
      }
   }

   hb_retc( NULL );
}
