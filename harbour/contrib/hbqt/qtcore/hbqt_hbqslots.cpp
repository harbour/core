/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta (marcosgambeta at gmail dot com)
 * Copyright 2009 Pritpal Bedi (pritpal@vouchcac.com)
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
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

#include "hbapiitm.h"
#include "hbstack.h"
#include "hbvm.h"
#include "hbapierr.h"

#if QT_VERSION >= 0x040500

#include "hbqt_hbqslots.h"

/*----------------------------------------------------------------------*/

#include <QtCore/QStringList>

#if 0

#include <QtCore/QProcess>
#include <QtCore/QUrl>
#include <QtCore/QDate>
#include <QtCore/QDateTime>
#include <QtCore/QTime>
#include <QtCore/QPointer>
#include <QtCore/QByteArray>
#include <QtCore/QModelIndex>
#include <QtCore/QRectF>

static void hbqt_SlotsExecPointer( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( *reinterpret_cast< void*( * )>( arguments[ 1 ] ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecPointerPointer( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( *reinterpret_cast< void*( * )>( arguments[ 1 ] ) );
   hb_vmPushPointer( *reinterpret_cast< void*( * )>( arguments[ 2 ] ) );
   hb_vmSend( 2 );
}

static void hbqt_SlotsExecPointerInt( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( *reinterpret_cast< void*( * )>( arguments[ 1 ] ) );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) );
   hb_vmSend( 2 );
}

static void hbqt_SlotsExecBool( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushLogical( *reinterpret_cast< bool( * ) >( arguments[ 1 ] ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecInt( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 1 ] ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecIntInt( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 1 ] ) );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) );
   hb_vmSend( 2 );
}

static void hbqt_SlotsExecIntIntInt( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 1 ] ) );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 3 ] ) );
   hb_vmSend( 3 );
}

static void hbqt_SlotsExecIntIntIntInt( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 1 ] ) );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 3 ] ) );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 4 ] ) );
   hb_vmSend( 4 );
}

static void hbqt_SlotsExecString( PHB_ITEM * codeBlock, void ** arguments )
{
   QString text = *reinterpret_cast< QString( * ) >( arguments[ 1 ] );
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushString( text.toAscii().data(), text.toAscii().length() );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecModel( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QModelIndex( ( *reinterpret_cast< QModelIndex( * ) >( arguments[ 1 ] ) ) ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecModelModel( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QModelIndex( ( *reinterpret_cast< QModelIndex( * ) >( arguments[ 1 ] ) ) ) );
   hb_vmPushPointer( new QModelIndex( ( *reinterpret_cast< QModelIndex( * ) >( arguments[ 2 ] ) ) ) );
   hb_vmSend( 2 );
}

static void hbqt_SlotsExecStringList( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QStringList( ( *reinterpret_cast<QStringList( * )>( arguments[ 1 ] ) ) ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQUrl( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QUrl( ( *reinterpret_cast< QUrl( * )>( arguments[ 1 ] ) ) ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQDate( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QDate( ( *reinterpret_cast< QDate( * ) >( arguments[ 1 ] ) ) ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQDateTime( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QDateTime( ( *reinterpret_cast< QDateTime( * ) >( arguments[ 1 ] ) ) ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQTime( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QTime( ( *reinterpret_cast< QTime( * ) >( arguments[ 1 ] ) ) ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQRectF( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QRectF( ( *reinterpret_cast< QRectF( * )>( arguments[ 1 ] ) ) ) );
   hb_vmSend( 1 );
}
#endif
/*----------------------------------------------------------------------*/

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
      {
         s_pCallback[ iIndex ] = pCallback;
      }
   }
}

void hbqt_slots_unregister_callback( QByteArray sig )
{
   if( ! sig.isEmpty() )
   {
      int iIndex = s_argCombinations.indexOf( sig );

      if( iIndex > -1 )
      {
         s_pCallback.removeAt( iIndex );
      }
   }
}

static int connect_signal( QString signal, QObject * object, HBQSlots * t_slots )
{
   HB_TRACE( HB_TR_DEBUG, ( "connect_signal: %s", ( char * ) signal.toAscii().data() ) );

   QByteArray theSignal = signal.toAscii();

   theSignal = QMetaObject::normalizedSignature( theSignal );

   HB_TRACE( HB_TR_DEBUG, ( "    normalized: %s", ( char * ) theSignal.data() ) );

   if( ! QMetaObject::checkConnectArgs( theSignal, theSignal ) )
   {
      HB_TRACE( HB_TR_ALWAYS, ( "NOT    checkConnectArgs: %s", ( char * ) theSignal.data() ) );
      return -1;
   }

   int signalId = object->metaObject()->indexOfSignal( theSignal );
   if( signalId == -1 )
   {
      HB_TRACE( HB_TR_ALWAYS, ( "NOT    indexOfSignal: %s", ( char * ) theSignal.data() ) );
      return -1;
   }

   int slotId = object->metaObject()->indexOfMethod( theSignal );
   if( slotId == -1 )
   {
      HB_TRACE( HB_TR_ALWAYS, ( "NOT    indexOfMethod: %s", ( char * ) theSignal.data() ) );
      return -1;
   }

   if( QMetaObject::connect( object, signalId, t_slots, slotId + QObject::staticMetaObject.methodCount(), Qt::AutoConnection ) )
   {
      HB_TRACE( HB_TR_DEBUG, ( "YES    connected: %i %i %s", signalId, slotId, ( char * ) theSignal.data() ) );
      return slotId;
   }
   return -1;
}

/*----------------------------------------------------------------------*/

static bool disconnect_signal( QObject * object, QString signal )
{
   QByteArray theSignal = signal.toAscii();

   theSignal = QMetaObject::normalizedSignature( theSignal ) ;

   int signalId = object->metaObject()->indexOfSignal( theSignal );
   if( signalId == -1 )
   {
      return false;
   }
   return QMetaObject::disconnect( object, signalId, 0, 0 );
}

/*----------------------------------------------------------------------*/

HBQSlots::HBQSlots( QObject* parent ) : QObject( parent )
{
}

HBQSlots::~HBQSlots()
{
   int i;

   for( i = 0; i < listBlock.size(); i++ )
   {
      if( listBlock[ i ] != NULL )
      {
         hb_itemRelease( listBlock.at( i ) );
         listBlock[ i ] = NULL;
      }
   }
   /* QUESTION: Should there be all remaining active slots disconnected at this point? */

   /*           Should be disconnected, but this is a responsibility of programmer as object is only known to the application */
   listBlock.clear();
}

int HBQSlots::qt_metacall( QMetaObject::Call c, int id, void **arguments )
{
   id = QObject::qt_metacall( c, id, arguments );

   if( id < 0 || c != QMetaObject::InvokeMetaMethod )
      return id;

   // Q_ASSERT(id < slotList.size());

   /* Register core slot handlers */
   if( s_argCombinations.size() == 0 )
   {
      #if 0
      hbqt_slots_register_callback( "int"                      , hbqt_SlotsExecInt            );
      hbqt_slots_register_callback( "int$int"                  , hbqt_SlotsExecIntInt         );
      hbqt_slots_register_callback( "int$int$int"              , hbqt_SlotsExecIntIntInt      );
      hbqt_slots_register_callback( "int$int$int$int"          , hbqt_SlotsExecIntIntIntInt   );
      hbqt_slots_register_callback( "bool"                     , hbqt_SlotsExecBool           );
      hbqt_slots_register_callback( "pointer"                  , hbqt_SlotsExecPointer        );
      hbqt_slots_register_callback( "pointer$pointer"          , hbqt_SlotsExecPointerPointer );
      hbqt_slots_register_callback( "pointer$int"              , hbqt_SlotsExecPointerInt     );
      hbqt_slots_register_callback( "QDate"                    , hbqt_SlotsExecQDate          );
      hbqt_slots_register_callback( "QDateTime"                , hbqt_SlotsExecQDateTime      );
      hbqt_slots_register_callback( "QModelIndex"              , hbqt_SlotsExecModel          );
      hbqt_slots_register_callback( "QModelIndex$QModelIndex"  , hbqt_SlotsExecModelModel     );
      hbqt_slots_register_callback( "QRectF"                   , hbqt_SlotsExecQRectF         );
      hbqt_slots_register_callback( "QString"                  , hbqt_SlotsExecString         );
      hbqt_slots_register_callback( "QStringList"              , hbqt_SlotsExecStringList     );
      hbqt_slots_register_callback( "QTime"                    , hbqt_SlotsExecQTime          );
      hbqt_slots_register_callback( "QUrl"                     , hbqt_SlotsExecQUrl           );
      #endif
   }

   QObject * object = sender();
   const QMetaMethod meta = object->metaObject()->method( id );
   QList<QByteArray> arrayOfTypes = meta.parameterTypes();
   int parameterCount = arrayOfTypes.size();
   QStringList parList;

   HB_TRACE( HB_TR_DEBUG, ( "SlotsProxy signature %s", meta.signature() ) );

   for( int i = 0; i < parameterCount; i++ )
   {
      if( arrayOfTypes.at( i ).contains( "::" ) ) // if includes :: is a enum -> int
      {
         parList += "int";
      }
      else
      {
         if( arrayOfTypes.at( i ).contains( "*" ) )  // if includes * is a pointer -> pointer
         {
            parList += "pointer";
         }
         else
         {
            parList += arrayOfTypes.at( i ); //
         }
      }
   }

   QByteArray paramString = parList.join( "$" ).toAscii();
   HB_TRACE( HB_TR_DEBUG, ( "       SlotsProxy parList %s ", ( char * ) paramString.data() ) );

   if( object )
   {
      char cSlotName[ 20 ];
      sprintf( cSlotName, "SLOT_%d", id );
      int i = object->property( cSlotName ).toInt();

      if( i > 0 && i <= this->listBlock.size() && hb_vmRequestReenter() )
      {
         if( parameterCount == 0 )
         {
            hb_vmEvalBlockV( this->listBlock.at( i - 1 ), 0 );
         }
         else
         {
            int paramId = s_argCombinations.indexOf( paramString );
            PHBQT_SLOT_FUNC pCallback;

            HB_TRACE( HB_TR_DEBUG, ( "  params=%s,  paramId=%d", ( char *) paramString.data(), paramId ) );

            pCallback = s_pCallback.at( paramId );
            if( pCallback )
            {
               pCallback( ( PHB_ITEM * ) this->listBlock.at( i - 1 ), arguments );
            }
         }
         hb_vmRequestRestore();
      }
   }
   return -1;
}

/*----------------------------------------------------------------------*/
/*
 * Harbour function to connect signals with slots
 */
HB_FUNC( __HBQT_SLOTS_CONNECT )
{
   HB_BOOL   bRet    = HB_FALSE;
   HBQSlots * t_slots = hbqt_par_HBQSlots( 1 );

   if( t_slots )
   {
      QObject * object = ( QObject * ) hbqt_pPtrFromObj( 2 );               /* get sender    */
      if( object )
      {
         int i = object->property( hb_parcx( 3 ) ).toInt();
         if ( i == 0 )
         {
            QString signal = hb_parcx( 3 );                                 /* get signal    */
            int idSignal = connect_signal( signal, object, t_slots ) ;
            if( idSignal >= 0 )
            {
               PHB_ITEM pBlock = hb_itemNew( hb_param( 4, HB_IT_BLOCK ) );  /* get codeblock */
               t_slots->listBlock << pBlock;

               char cSlotName[ 20 ];
               sprintf( cSlotName, "SLOT_%d", idSignal );

               object->setProperty( cSlotName, ( int ) t_slots->listBlock.size() );
               object->setProperty( hb_parcx(3), ( int ) t_slots->listBlock.size() );
               bRet = HB_TRUE;
            }
         }
      }
   }

   if( ! bRet )
   {
      hb_errRT_BASE( EG_ARG, 1100, NULL, "CONNECT", HB_ERR_ARGS_BASEPARAMS );
   }

   hb_retl( bRet );
}

/*
 * Harbour function to disconnect signals
 */
HB_FUNC( __HBQT_SLOTS_DISCONNECT )
{
   HB_BOOL   bRet    = HB_FALSE;
   HBQSlots * t_slots = hbqt_par_HBQSlots( 1 );

   if( t_slots )
   {
      QObject * object = ( QObject* ) hbqt_pPtrFromObj( 2 );
      if( object )
      {
         const char * slot = hb_parcx( 3 );

         int i = object->property( slot ).toInt();

         if( i > 0 && i <= t_slots->listBlock.size() )
         {
            object->setProperty( slot, QVariant() );

            bRet = ( disconnect_signal( object, ( QString ) slot ) == true );

            hb_itemRelease( t_slots->listBlock.at( i - 1 ) );
            t_slots->listBlock[ i - 1 ] = NULL;
         }
      }
   }
   hb_retl( bRet );
}

HB_FUNC( __HBQT_SLOTS_NEW )
{
   void * pObj = NULL;

   pObj = ( HBQSlots * ) new HBQSlots();

   hb_retptrGC( hbqt_gcAllocate_HBQSlots( pObj, true ) );
}

#endif
