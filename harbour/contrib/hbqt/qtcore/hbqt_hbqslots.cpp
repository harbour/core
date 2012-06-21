/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta (marcosgambeta at gmail dot com)
 * Copyright 2009 Pritpal Bedi (pritpal@vouchcac.com)
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
 * Copyright 2010 Francesco Perillo ()
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

#include "hbqt.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbvm.h"

#if QT_VERSION >= 0x040500

#include "hbqt_hbqslots.h"

HB_EXPORT HBQSlots * hbqt_bindGetReceiverSlotsByHbObject( PHB_ITEM pObject );

static QList<QByteArray> s_argCombinations;
static QList<PHBQT_SLOT_FUNC> s_pCallback;

void hbqt_slots_register_callback( QByteArray sig, PHBQT_SLOT_FUNC pCallback )
{
   HB_TRACE( HB_TR_DEBUG, ( "callback %s", ( char * ) sig.data() ) );

   if( ! sig.isEmpty() && pCallback )
   {
      int iIndex = s_argCombinations.indexOf( sig );

      if( iIndex == -1 )
      {
         s_argCombinations << sig;
         s_pCallback << pCallback;
      }
      else
         s_pCallback[ iIndex ] = pCallback;
   }
}

void hbqt_slots_unregister_callback( QByteArray sig )
{
   if( ! sig.isEmpty() )
   {
      int iIndex = s_argCombinations.indexOf( sig );

      if( iIndex > -1 )
      {
         s_argCombinations.removeAt( iIndex );
         s_pCallback.removeAt( iIndex );
      }
   }
}

/*----------------------------------------------------------------------*/

HBQSlots::HBQSlots() : QObject()
{
}

HBQSlots::~HBQSlots()
{
   HB_TRACE( HB_TR_DEBUG, ( "HBQSlots::~HBQSlots()" ) );
}

int HBQSlots::hbConnect( PHB_ITEM pObj, char * pszSignal, PHB_ITEM bBlock )
{
   HB_TRACE( HB_TR_DEBUG, ( "HBQSlots::hbConnect( %s )", pszSignal ) );

   int nResult = 1;

   if( true )
   {
      QObject * object = ( QObject * ) hbqt_get_ptr( pObj );
      if( object )
      {
         if( hb_itemType( bBlock ) & HB_IT_BLOCK )
         {
            int i = object->property( pszSignal ).toInt();
            if( i == 0 )
            {
               QString signal = pszSignal;
               QByteArray theSignal = QMetaObject::normalizedSignature( signal.toAscii() );

               if( QMetaObject::checkConnectArgs( theSignal, theSignal ) )
               {
                  int signalId = object->metaObject()->indexOfSignal( theSignal );
                  if( signalId != -1 )
                  {
                     int slotId = object->metaObject()->indexOfMethod( theSignal );
                     if( slotId != -1 )
                     {
                        if( QMetaObject::connect( object, signalId, this, slotId + QObject::staticMetaObject.methodCount(), Qt::AutoConnection ) )
                        {
                           nResult = 0;

                           HB_TRACE( HB_TR_DEBUG, ( "HBQSlots::hbConnect( %p, %s, %i )", object, pszSignal, signalId ) );
                           hbqt_bindAddSlot( pObj, signalId, bBlock );
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
         }
         else
         {
            nResult = 3;
         }
      }
      else
         nResult = 2;
   }
   HB_TRACE( HB_TR_DEBUG, ( "HBQT_SLOTS_CONNECT returns: %d", nResult ) );
   return nResult;
}

int HBQSlots::hbDisconnect( PHB_ITEM pObj, char * pszSignal )
{
   HB_TRACE( HB_TR_DEBUG, ( "HBQSlots::hbDisconnect( %s )", pszSignal ) );

   int nResult = 1;

   QObject * object = ( QObject * ) hbqt_get_ptr( pObj );
   if( object )
   {
      QString signal = pszSignal;
      QByteArray theSignal = signal.toAscii();

      int signalId = object->metaObject()->indexOfSignal( QMetaObject::normalizedSignature( theSignal ) );
      if( signalId != -1 )
      {
         if( QMetaObject::disconnect( object, signalId, 0, 0 ) )
         {
            HB_TRACE( HB_TR_DEBUG, ( "HBQSlots::hbDisconnect( %s ) %i", pszSignal, i ) );
            nResult = 0;
         }
         else
            nResult = 5;
      }
      else
         nResult = 4;

      if( nResult == 0 )
      {
         HB_TRACE( HB_TR_DEBUG, ( "HBQSlots::hbDisConnect( %s ) signalId=%i, %p", pszSignal, signalId, object ) );
         hbqt_bindDelSlot( pObj, signalId, NULL );
      }
   }
   else
      nResult = 2;

   return nResult;
}


int HBQSlots::qt_metacall( QMetaObject::Call c, int id, void ** arguments )
{
   id = QObject::qt_metacall( c, id, arguments );
   if( id < 0 || c != QMetaObject::InvokeMetaMethod )
      return id;

   QObject * object = sender();
   if( object )
   {
      QByteArray paramString;
      const QMetaMethod meta = object->metaObject()->method( id );
      QList<QByteArray> arrayOfTypes = meta.parameterTypes();
      int parameterCount = arrayOfTypes.size();
      QStringList pList;

      if( parameterCount > 0 )
      {
         char szParams[ 20 ];
         hb_snprintf( szParams, sizeof( szParams ), "PARAM_%d", id );
         paramString = object->property( szParams ).toByteArray();

         char szPList[ 20 ];
         hb_snprintf( szPList, sizeof( szPList ), "PLIST_%d", id );
         pList = object->property( szPList ).toStringList();

         if( paramString.isNull() )
         {
            QStringList parList;
            HB_TRACE( HB_TR_DEBUG, ( "SlotsProxy signature %s", meta.signature() ) );

            for( int i = 0; i < parameterCount; i++ )
            {
               QByteArray s = arrayOfTypes.at( i ).trimmed();
               if( s.contains( "::" ) )
               {
                  parList += "int";
                  pList += "int";
               }
               else if( s.endsWith( "*" ) )
               {
                  parList += s;
                  s.chop( 1 );
                  pList += "HB_" + s.toUpper();
               }
               else
               {
                  parList += s;
                  pList += "HB_" + s.toUpper();
               }
            }
            paramString = parList.join( "$" ).toAscii();
            object->setProperty( szParams, paramString );

            object->setProperty( szPList, pList );

            HB_TRACE( HB_TR_DEBUG, ( "    %p   SlotsProxy pList %s ", object, ( char * ) pList.join( "$" ).toAscii().data() ) );
         }
      }

      if( hb_vmRequestReenter() )
      {
         PHB_ITEM hbObject = hbqt_bindGetHbObjectByQtObject( object );
         if( hbObject )
         {
            PHB_ITEM p = hbqt_bindGetSlots( hbObject, id );
            hb_itemRelease( hbObject );
            if( p )
            {
               PHB_ITEM codeBlock = hb_arrayGetItemPtr( p, 1 );
               if( HB_IS_BLOCK( codeBlock ) )
               {
                  if( parameterCount == 0 )
                  {
                     HB_TRACE( HB_TR_DEBUG, ( "Firing Signal( %p )", object ) );
                     hb_evalBlock0( codeBlock );
                     HB_TRACE( HB_TR_DEBUG, ( "Fired ( %p )", object ) );
                  }
                  else
                  {
                     int paramId = s_argCombinations.indexOf( paramString );
                     PHBQT_SLOT_FUNC pCallback = s_pCallback.at( paramId );
                     if( pCallback )
                     {
                        HB_TRACE( HB_TR_DEBUG, ( "Firing Signal( %p, %s )", object, paramString.data() ) );
                        pCallback( ( PHB_ITEM * ) codeBlock, arguments, pList );
                        HB_TRACE( HB_TR_DEBUG, ( "Fired ( %p )", object ) );
                     }
                  }
               }
               hb_itemRelease( p );
            }
         }
         hb_vmRequestRestore();
      }
   }
   return -1;
}

/*----------------------------------------------------------------------*/

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

int hbqt_QtDisconnect( QObject * sender, const char * pszSignal, QObject * receiver, const char * pszSlot )
{
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_QtDisconnect %s with slot %s", pszSignal, pszSlot ) );

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
               if( QMetaObject::disconnect( sender, signalId, receiver, slotId ) )
               {
                  nResult = 0;
                  HB_TRACE( HB_TR_DEBUG, ( "hbqt_QtDisconnect_OK( %p, %s, %s )", sender, pszSignal, pszSlot ) );
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

   return nResult;
}

/*  HBQT_CONNECT handles now two types of connection, one Qt based, one HB based:
    Qt:  HBQT_CONNECT( object, signal, object, string )
    HB:  HBQT_CONNECT( object, signal, codeblock )
*/
HB_FUNC( HBQT_CONNECT )
{
   int ret = -1;

   if( hb_pcount() == 4 && HB_ISCHAR( 2 ) && HB_ISCHAR( 4 ) && hbqt_par_isDerivedFrom( 1, "QOBJECT" ) && hbqt_par_isDerivedFrom( 3, "QOBJECT" ) )
   {
      void * pText01 = NULL;
      void * pText02 = NULL;
      ret =  hbqt_QtConnect( ( QObject* ) hbqt_par_ptr( 1 ), hb_parstr_utf8( 2, &pText01, NULL ), ( QObject* ) hbqt_par_ptr( 3 ), hb_parstr_utf8( 4, &pText02, NULL ) );
      hb_strfree( pText01 );
      hb_strfree( pText02 );
   }
   else if( hb_pcount() == 3 && HB_ISCHAR( 2 ) && HB_ISBLOCK( 3 ) && hbqt_par_isDerivedFrom( 1, "QOBJECT" ) )
   {
      HBQSlots * receiverSlots = hbqt_bindGetReceiverSlotsByHbObject( hb_param( 1, HB_IT_OBJECT ) );
      if( receiverSlots )
      {
         void * pText01 = NULL;
         ret = receiverSlots->hbConnect( hb_param( 1, HB_IT_OBJECT ), ( char * ) hb_parstr_utf8( 2, &pText01, NULL ), hb_param( 3, HB_IT_BLOCK ) );
         hb_strfree( pText01 );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 9999, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );

   hb_retni( ret );
}

HB_FUNC( HBQT_DISCONNECT )
{
   int ret = -1;

   HB_TRACE( HB_TR_DEBUG, ( "enters HBQT_DISCONNECT" ) );
   if( hb_pcount() == 2 && HB_ISCHAR( 2 ) && hbqt_par_isDerivedFrom( 1, "QOBJECT" )  )
   {
      HBQSlots * receiverSlots = hbqt_bindGetReceiverSlotsByHbObject( hb_param( 1, HB_IT_OBJECT ) );
      if( receiverSlots )
      {
         void * pText01 = NULL;
         ret = receiverSlots->hbDisconnect( hb_param( 1, HB_IT_OBJECT ), ( char * ) hb_parstr_utf8( 2, &pText01, NULL ) );
         hb_strfree( pText01 );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 9999, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );

   HB_TRACE( HB_TR_DEBUG, ( "exits HBQT_DISCONNECT" ) );
   hb_retni( ret );
}

static void hbqt_lib_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );
   HB_TRACE( HB_TR_DEBUG, ( "Slots: hbqt_lib_init" ) );
}

static void hbqt_lib_exit( void* cargo )
{
   HB_SYMBOL_UNUSED( cargo );
   HB_TRACE( HB_TR_DEBUG, ( "Exiting slots lib" ) );
}

HB_CALL_ON_STARTUP_BEGIN( _hbqtslots_init_ )
   hb_vmAtInit( hbqt_lib_init, NULL );
   hb_vmAtExit( hbqt_lib_exit, NULL );
HB_CALL_ON_STARTUP_END( _hbqtslots_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hbqtslots_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hbqtslots_init_ )
   #include "hbiniseg.h"
#endif

#endif

