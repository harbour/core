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

#include <QtCore/QPointer>

/*----------------------------------------------------------------------*/

#include <QtCore/QProcess>
#include <QtCore/QUrl>
#include <QtCore/QDate>
#include <QtCore/QDateTime>
#include <QtCore/QTime>
#include <QtCore/QPointer>
#include <QtCore/QByteArray>
#include <QtCore/QModelIndex>
/* TOFIX: QtGui components should not be accessed from this component */
#include <QtGui/QItemSelection>
#include <QtGui/QTextCursor>
#include <QtGui/QTextCharFormat>

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

static void hbqt_SlotsExecQRectInt( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QRect( *reinterpret_cast< QRect( * ) >( arguments[ 1 ] ) ) );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) );
   hb_vmSend( 2 );
}

static void hbqt_SlotsExecString( PHB_ITEM * codeBlock, void ** arguments )
{
   /* TODO: how to convert to this type with size */
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

static void hbqt_SlotsExecItemSelItemSel( PHB_ITEM * codeBlock, void ** arguments)
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QItemSelection( ( *reinterpret_cast< QItemSelection( * )>( arguments[ 1 ] ) ) ) );
   hb_vmPushPointer( new QItemSelection( ( *reinterpret_cast< QItemSelection( * )>( arguments[ 2 ] ) ) ) );
   hb_vmSend( 2 );
}

static void hbqt_SlotsExecTextCharFormat( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QTextCharFormat( ( *reinterpret_cast<QTextCharFormat( * )>( arguments[ 1 ] ) ) ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecFont( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QFont( ( *reinterpret_cast< QFont( * )>( arguments[ 1 ] ) ) ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQTextCursor( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QTextCursor( ( *reinterpret_cast< QTextCursor( * )>( arguments[ 1 ] ) ) ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecStringList( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QStringList( ( *reinterpret_cast<QStringList( * )>( arguments[ 1 ] ) ) ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQPoint( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QPoint( ( * reinterpret_cast< QPoint( * )>( arguments[ 1 ] ) ) ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQRectF( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QRectF( ( *reinterpret_cast< QRectF( * )>( arguments[ 1 ] ) ) ) );
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


/*----------------------------------------------------------------------*/

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
// HB_TRACE( HB_TR_DEBUG, ( "   signalId %d  ", signalId ) );
   return QMetaObject::disconnect( object, signalId, 0, 0 );
}

/*----------------------------------------------------------------------*/

static void hbqt_SlotsProxy( HBQSlots * t_slots, int id, QObject * object, void ** arguments )
{
   // array of function calls
   // static void *( arrayFunc[23])(PHB_ITEM *, void ** ) {
   //
   #if 0
   static void * ( arrayFunc[ 23 ] )
   {
      hbqt_SlotsExecInt ,
      hbqt_SlotsExecIntInt ,
      hbqt_SlotsExecIntIntInt ,
      hbqt_SlotsExecIntIntIntInt ,
      hbqt_SlotsExecBool ,
      hbqt_SlotsExecQRectInt ,
      hbqt_SlotsExecString ,
      hbqt_SlotsExecModel ,
      hbqt_SlotsExecModelModel ,
      hbqt_SlotsExecItemSelItemSel ,
      hbqt_SlotsExecTextCharFormat ,
      hbqt_SlotsExecFont ,
      hbqt_SlotsExecQTextCursor ,
      hbqt_SlotsExecStringList ,
      hbqt_SlotsExecPointer ,
      hbqt_SlotsExecPointerPointer ,
      hbqt_SlotsExecPointerInt ,
      hbqt_SlotsExecQDate ,
      hbqt_SlotsExecQDateTime ,
      hbqt_SlotsExecQPoint ,
      hbqt_SlotsExecQRectF ,
      hbqt_SlotsExecQTime ,
      hbqt_SlotsExecQUrl
   }
   #endif

   static QList< QByteArray > argCombinations;

   if( argCombinations.size() == 0 )
   {
      argCombinations << "int" <<
                         "int$int" <<
                         "int$int$int" <<
                         "int$int$int$int" <<
                         "bool" <<
                         "QRect$int" <<
                         "QString" <<
                         "QModel" <<
                         "QModel$QModel" <<
                         "QItemSelection$QItemSelection" <<
                         "QTextCharFormat" <<
                         "QFont" <<
                         "QTextCursor" <<
                         "QStringList" <<
                         "pointer" <<
                         "pointer$pointer" <<
                         "pointer$int"<<
                         "QDate" <<
                         "QDateTime" <<
                         "QPoint" <<
                         "QRectF" <<
                         "QTime" <<
                         "QUrl" <<
                         "QModelIndex$QModelIndex" ;
   }

   const QMetaMethod meta = object->metaObject()->method( id );

   HB_TRACE( HB_TR_DEBUG, ( "SlotsProxy signature %s", meta.signature() ) );

   QList<QByteArray> arrayOfTypes = meta.parameterTypes();

   // HB_TRACE( HB_TR_DEBUG, ( "       SlotsProxy objectName %s ", qPrintable( object->objectName() ) ) );
   // HB_TRACE( HB_TR_DEBUG, ( "       SlotsProxy className %s ", (char * ) object->metaObject()->className() ) );
   int parameterCount = arrayOfTypes.size();
   QStringList parList;

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

   QByteArray paramString = parList.join("$").toAscii();
   HB_TRACE( HB_TR_DEBUG, ( "       SlotsProxy parList %s ", (char * ) paramString.data() ) );

   if( object )
   {
      char cSlotName[ 20 ];
      sprintf( cSlotName, "SLOT_%d", id );
      int i = object->property( cSlotName ).toInt();
      // HB_TRACE( HB_TR_DEBUG, ( "SlotsProxy %s=%d", cSlotName, i ) );

      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         if( parameterCount == 0 )
         {
            hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 0 );
         }
         else
         {
            int paramId = argCombinations.indexOf( paramString );
            HB_TRACE( HB_TR_DEBUG, ( "  params=%s,  paramId=%d", ( char *) paramString.data(), paramId ) );

            switch( paramId )
            {
            case 0:   // if( paramString == "int" )
                hbqt_SlotsExecInt( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
                break;
            case 1:   // if( paramString == "int$int" )
                hbqt_SlotsExecIntInt( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
                break;
            case 2:   // if( paramString == "int$int$int" )
                hbqt_SlotsExecIntIntInt( (PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
                break;
            case 3:   // if( paramString == "int$int$int$int" )
               hbqt_SlotsExecIntIntIntInt( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 4:  //  if( paramString == "bool" )
               hbqt_SlotsExecBool( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 5:   // if( paramString == "QRect$int" )
               hbqt_SlotsExecQRectInt( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 6:   // if( paramString == "QString" )
               hbqt_SlotsExecString( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 7:   // if( paramString == "QModel" )
               hbqt_SlotsExecModel( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 8:   // if( paramString == "QModel$QModel" )
               hbqt_SlotsExecModelModel( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 9:   // if( paramString == "QItemSelection$QItemSelection" )
               hbqt_SlotsExecItemSelItemSel( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 10:   // if( paramString == "QTextCharFormat" )
               hbqt_SlotsExecTextCharFormat( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 11:   // if( paramString == "QFont" )
               hbqt_SlotsExecFont( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 12:   // if( paramString == "QTextCursor" )
               hbqt_SlotsExecQTextCursor( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 13:   // if( paramString == "QStringList" )
               hbqt_SlotsExecStringList( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 14:   // if( paramString == "pointer" )
               hbqt_SlotsExecPointer( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 15:   // if( paramString == "pointer$pointer" )
               hbqt_SlotsExecPointerPointer( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 16:   // if( paramString == "pointer$int" )
               hbqt_SlotsExecPointerInt( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 17:   // if( paramString == "QDate" )
               hbqt_SlotsExecQDate( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 18:   // if( paramString == "QDateTime" )
               hbqt_SlotsExecQDateTime( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 19:   // if( paramString == "QPoint" )
               hbqt_SlotsExecQPoint( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 20:   // if( paramString == "QRectF" )
               hbqt_SlotsExecQRectF( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 21:   // if( paramString == "QTime" )
               hbqt_SlotsExecQTime( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 22:   // if( paramString == "QUrl" )
               hbqt_SlotsExecQUrl( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 23:   // if( paramString == "QModel$QModel" )
               hbqt_SlotsExecModelModel( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            }
         }
         hb_vmRequestRestore();
      }
   }
}

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

bool HBQSlots::hbIsConnected( PHB_ITEM pObj, const char * slot )
{
   HB_SYMBOL_UNUSED( pObj );

   QObject * object = ( QObject * ) hbqt_pPtrFromObj( 1 );
   return isConnected( object, slot );
}

bool HBQSlots::isConnected( QObject * object, const char * slot )
{
   int i;

   for( i = 0; i < listBlock.size(); i++ )
   {
      if( listBlock[ i ] != NULL && listObj[ i ] == object )
      {
         if( object->property( slot ).toInt() == i + 1 )
         {
            return true;
         }
      }
   }
   return false;
}

bool HBQSlots::hbConnect( PHB_ITEM pObj, const char * slot, PHB_ITEM bBlock )
{
   HB_SYMBOL_UNUSED( pObj   );
   HB_SYMBOL_UNUSED( bBlock );

   QObject * object = ( QObject * ) hbqt_pPtrFromObj( 1 );                   /* get sender    */

   if( object )
   {
      if( !isConnected( object, slot ) )
      {
HB_TRACE( HB_TR_DEBUG, ( "AAA 3 %s  %p", slot, object ) );
         bool bConnected = connect_signal( ( QString ) slot, object, this );
HB_TRACE( HB_TR_DEBUG, ( "AAA 4" ) );
         if( bConnected )
         {
            PHB_ITEM pBlock = hb_itemNew( bBlock );                        /* get codeblock */
HB_TRACE( HB_TR_DEBUG, ( "AAA 5" ) );
            listBlock << pBlock;
            listObj   << object;

            object->setProperty( slot, ( int ) listBlock.size() );

            return true;
         }
      }
   }
   return false;
}

bool HBQSlots::hbDisconnect( PHB_ITEM pObj, const char * signal )
{
   HB_SYMBOL_UNUSED( pObj );

   QObject * object = ( QObject* ) hbqt_pPtrFromObj( 1 );

   if( object )
   {
      int i = object->property( signal ).toInt();

      if( i > 0 && i <= listBlock.size() )
      {
         hb_itemRelease( listBlock.at( i - 1 ) );
         listBlock[ i - 1 ] = NULL;
         listObj[ i - 1 ] = NULL;

         bool bRet = disconnect_signal( object, ( QString ) signal );

         HB_TRACE( HB_TR_DEBUG, ( "      QT_SLOTS_DISCONNECT: %s    %s", bRet ? "YES" : "NO", signal ) );

         return bRet;
      }
   }
   return false;
}

bool HBQSlots::hbClear()
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
   listBlock.clear();
   return true;
}

int HBQSlots::qt_metacall( QMetaObject::Call c, int id, void **arguments )
{
    id = QObject::qt_metacall( c, id, arguments );
    if( id < 0 || c != QMetaObject::InvokeMetaMethod )
    {
       return id;
    }
    // Q_ASSERT(id < slotList.size());

    hbqt_SlotsProxy( this, id, sender(), arguments );
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
   if ( !bRet )
        hb_errRT_BASE( EG_ARG, 1100, NULL, "CONNECT", HB_ERR_ARGS_BASEPARAMS );
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
