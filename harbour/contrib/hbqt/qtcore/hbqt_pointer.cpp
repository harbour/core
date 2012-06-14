/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
/*----------------------------------------------------------------------*/

#include "hbvmint.h"

#include "hbqt.h"

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicls.h"
#include "hbinit.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "hbapicls.h"
#include "hbstack.h"
#include "hbvm.h"

#if QT_VERSION >= 0x040500

/*----------------------------------------------------------------------*/

void * hbqt_par_ptr( int iParam )
{
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_par_ptr( %d )", iParam ) );

   return hbqt_bindGetQtObject( hb_param( iParam, HB_IT_OBJECT ) );
}

void * hbqt_get_ptr( PHB_ITEM pObj )
{
   return hbqt_bindGetQtObject( pObj );
}

void hbqt_errRT_ARG( void )
{
   hb_errRT_BASE( EG_ARG, 9999, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

void hbqt_par_detach_ptrGC( int iParam )
{
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_par_detach_ptrGC( %d )", iParam ) );
   if( HB_ISOBJECT( iParam ) )
   {
      hbqt_bindSetOwner( hbqt_bindGetQtObject( hb_param( iParam, HB_IT_OBJECT ) ), HB_FALSE );
   }
}

/* TODO: Delete this. */
HB_FUNC( HBQT_ISEQUAL )
{
   hb_retl( hbqt_par_ptr( 1 ) == hbqt_par_ptr( 2 ) );
}

HB_FUNC( __HBQT_ERROR )
{
   HB_TRACE( HB_TR_DEBUG, ( "In __HBQT_ERROR" ));
   hb_errRT_BASE( EG_ARG, 9999, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( __HBQT_SETUTF8 )
{
   PHB_CODEPAGE cdp = hb_cdpFindExt( "UTF8" );
   if( cdp )
   {
      hb_vmSetCDP( cdp );
   }
}

PHB_ITEM hbqt_defineClassBegin( const char * pszClsName, PHB_ITEM s_oClass, const char * pszParentClsStr )
{
   static PHB_DYNS s__CLSLOCKDEF = NULL;

   PHB_ITEM oClass = NULL;

   if( s__CLSLOCKDEF == NULL )
      s__CLSLOCKDEF = hb_dynsymGetCase( "__CLSLOCKDEF" );

   hb_vmPushDynSym( s__CLSLOCKDEF );
   hb_vmPushNil();
   hb_vmPushItemRef( s_oClass );
   hb_vmDo( 1 );

   if( hb_itemGetL( hb_stackReturnItem() ) && pszParentClsStr )
   {
      static PHB_DYNS s___HBCLASS = NULL;

      char * pszSingleClsNameBuf = ( char * ) hb_xgrab( strlen( pszParentClsStr ) + 1 );

      if( s___HBCLASS == NULL )
         s___HBCLASS = hb_dynsymGetCase( "HBCLASS" );

      PHB_ITEM pClsName = hb_itemNew( NULL );
      PHB_ITEM pSuper = hb_itemNew( NULL );
      PHB_ITEM pSym_ClsFunc = hb_itemNew( NULL );

      hb_itemPutC( pClsName, pszClsName );

      HB_SIZE nPos = 0;
      HB_SIZE nStart = 0;

      /* array with parent classes (at least ONE) */
      hb_arrayNew( pSuper, 0 );

      HB_TRACE( HB_TR_DEBUG, ("%s: dCB 3", pszClsName ) );

      while( pszParentClsStr[ nPos++ ] )
      {
         if( ! pszParentClsStr[ nPos ] || ( pszParentClsStr[ nPos ] == ',' && pszParentClsStr[ nPos + 1 ] == ' ' ) )
         {
            PHB_ITEM pItem = hb_itemNew( NULL );

            memcpy( pszSingleClsNameBuf, pszParentClsStr + nStart, nPos - nStart );
            pszSingleClsNameBuf[ nPos - nStart ] = '\0';

            hb_itemPutC( pItem, pszSingleClsNameBuf );
            hb_arrayAdd( pSuper, hb_itemPutSymbol( pItem, hb_dynsymGetCase( pszSingleClsNameBuf )->pSymbol ) );

            hb_itemRelease( pItem );

            nStart = nPos + 2;
         }
      }

      hb_xfree( pszSingleClsNameBuf );

      hb_itemPutSymbol( pSym_ClsFunc, hb_dynsymGetCase( pszClsName )->pSymbol );

      hb_vmPushDynSym( s___HBCLASS );
      hb_vmPushNil();
      hb_vmDo( 0 );

      /* TODO: change this hack */
      char test[ HB_SYMBOL_NAME_LEN + 1 ];
      hb_snprintf( test, sizeof( test ), "HB_%s", pszClsName );

      hb_itemPutSymbol( pSym_ClsFunc, hb_dynsymGetCase( test )->pSymbol );

      hb_objSendMsg( hb_stackReturnItem(), "NEW", 3, pClsName, pSuper, pSym_ClsFunc );

      oClass = hb_itemNew( hb_stackReturnItem() );

      hb_itemRelease( pSym_ClsFunc );
      hb_itemRelease( pSuper );
      hb_itemRelease( pClsName );

      hb_objSendMsg( oClass, "CREATE", 0 );
      hb_objSendMsg( oClass, "INSTANCE", 0 );
   }

   return oClass;
}

void hbqt_defineClassEnd( PHB_ITEM s_oClass, PHB_ITEM oClass )
{
   if( s_oClass && oClass )
   {
      static PHB_DYNS s__CLSUNLOCKDEF = NULL;

      if( s__CLSUNLOCKDEF == NULL )
         s__CLSUNLOCKDEF = hb_dynsymGetCase( "__CLSUNLOCKDEF" );

      hb_vmPushDynSym( s__CLSUNLOCKDEF );
      hb_vmPushNil();
      hb_vmPushItemRef( s_oClass );
      hb_vmPush( oClass );
      hb_vmDo( 2 );

      hb_itemRelease( oClass );
   }
}

HB_BOOL hbqt_obj_isDerivedFrom( PHB_ITEM pItem, const char * pszClsName )
{
   return hb_clsIsParent( hb_objGetClass( pItem ), pszClsName );
}

/* Checks that the param iParam is an object and a subclass of pszClsName */
HB_BOOL hbqt_par_isDerivedFrom( int iParam, const char * pszClsName )
{
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_par_isDerivedFrom( %i, %s )", iParam, pszClsName ) );

   if( ( pItem = hb_param( iParam, HB_IT_OBJECT ) ) != NULL )
   {
      if( hbqt_bindGetQtObject( pItem ) == NULL )
         hb_errRT_BASE( EG_ARG, 9999, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      else
         return hbqt_obj_isDerivedFrom( pItem, pszClsName );
   }
   return HB_FALSE;
}

int hbqt_QtConnect( QObject *sender, const char * pszSignal, QObject *receiver, const char * pszSlot )
{
   HB_TRACE( HB_TR_DEBUG, ( "_Connect %s with slot %s", pszSignal, pszSlot ) );

   int nResult = 1;

   if( sender && receiver )
   {
      QString signal = pszSignal;
      QByteArray theSignal = QMetaObject::normalizedSignature( signal.toAscii() );
      QString slot = pszSlot;
      QByteArray theSlot = QMetaObject::normalizedSignature( slot.toAscii() );

      if( QMetaObject::checkConnectArgs( theSignal, theSlot ) )
      {
         int signalId = sender->metaObject()->indexOfSignal( theSignal );
         if( signalId != -1 )
         {
            int slotId = receiver->metaObject()->indexOfMethod( theSlot );
            if( slotId != -1 )
            {
               if( QMetaObject::connect( sender, signalId, receiver, slotId, Qt::AutoConnection ) )
               {
                  nResult = 0;
                  HB_TRACE( HB_TR_DEBUG, ( "SIGNAL2SLOT ok" ) );
               }
               else
                  nResult = 8;
            }
            else
               nResult = 7;
         }
         else
            nResult = 6;
      }
      else
         nResult = 5;
   }
   else
      nResult = 9;  // Qt objects not active

   HB_TRACE( HB_TR_DEBUG, ( "_Connect returns: %d", nResult ) );
   return nResult;
}

HB_FUNC( HBQT_CONNECT )
{
   HB_BOOL ret = HB_FALSE;

   if( hb_pcount() == 4 && HB_ISCHAR( 2 ) && HB_ISCHAR( 4 ) && hbqt_par_isDerivedFrom( 1, "QOBJECT" ) && hbqt_par_isDerivedFrom( 3, "QOBJECT" ) )
   {
      void * pText01 = NULL;
      void * pText02 = NULL;
      if( hbqt_QtConnect( ( QObject* ) hbqt_par_ptr( 1 ), hb_parstr_utf8( 2, &pText01, NULL ), ( QObject* ) hbqt_par_ptr( 3 ), hb_parstr_utf8( 4, &pText02, NULL ) ) == 0 )
      {
         ret = HB_TRUE;
      }
      hb_strfree( pText01 );
      hb_strfree( pText02 );
   }
   hb_retl( ret );
}

/*----------------------------------------------------------------------*/

#endif
