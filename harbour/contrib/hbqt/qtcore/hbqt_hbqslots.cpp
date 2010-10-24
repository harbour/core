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
#include <QtGui/QTextBlock>
#include <QtGui/QSessionManager>

#if 0    /* Disabled until dynamic registration protocol is not implemented */
#include <QtNetwork/QUrlInfo>
#include <QtNetwork/QNetworkProxy>
#include <QtNetwork/QHttpResponseHeader>
#include <QtNetwork/QNetworkRequest>
#endif

/*----------------------------------------------------------------------*/

#define SIG_int                                   "int"                              //  0
#define SIG_int_int                               "int$int"                          //  1
#define SIG_int_int_int                           "int$int$int"                      //  2
#define SIG_int_int_int_int                       "int$int$int$int"                  //  3
#define SIG_bool                                  "bool"                             //  4
#define SIG_QRect_int                             "QRect$int"                        //  5
#define SIG_QString                               "QString"                          //  6
#define SIG_QModelIndex                           "QModelIndex"                      //  7
#define SIG_QModelIndex_QModelIndex               "QModelIndex$QModelIndex"          //  8
#define SIG_QItemSelection_QItemSelection         "QItemSelection$QItemSelection"    //  9
#define SIG_QTextCharFormat                       "QTextCharFormat"                  // 10
#define SIG_QFont                                 "QFont"                            // 11
#define SIG_QTextCursor                           "QTextCursor"                      // 12
#define SIG_QStringList                           "QStringList"                      // 13
#define SIG_pointer                               "pointer"                          // 14
#define SIG_pointer_pointer                       "pointer$pointer"                  // 15
#define SIG_pointer_int                           "pointer$int"                      // 16
#define SIG_QDate                                 "QDate"                            // 17
#define SIG_QDateTime                             "QDateTime"                        // 18
#define SIG_QPoint                                "QPoint"                           // 19
#define SIG_QRectF                                "QRectF"                           // 20
#define SIG_QTime                                 "QTime"                            // 21
#define SIG_QUrl                                  "QUrl"                             // 22
#define SIG_QModelIndex_int_int                   "QModelIndex$int$int"              // 23
#define SIG_qint64                                "qint64"                           // 24
#define SIG_qint64_qint64                         "qint64$qint64"                    // 25
#define SIG_QTextBlock                            "QTextBlock"                       // 26
#define SIG_QSizeF                                "QSizeF"                           // 27
#define SIG_QColor                                "QColor"                           // 28
#define SIG_double                                "double"                           // 29
#define SIG_QModelIndexList                       "QModelIndexList"                  // 30
#define SIG_QRect                                 "QRect"                            // 31
#define SIG_int_QString                           "int$QString"                      // 32
#define SIG_QString_quint16_pointer               "QString$quint16$pointer"          // 33
#define SIG_int_bool                              "int$bool"                         // 34
#define SIG_pointer_QString                       "pointer$QString"                  // 35
#define SIG_QString_QString_QString               "QString$QString$QString"          // 36
#define SIG_int_int_QRect                         "int$int$QRect"                    // 37
#define SIG_QUrlInfo                              "QUrlInfo"                         // 38
#define SIG_QNetworkRequest                       "QNetworkRequest"                  // 39
#define SIG_QNetworkProxy_pointer                 "QNetworkProxy$pointer"            // 40
#define SIG_QHttpResponseHeader                   "QHttpResponseHeader"              // 41

#define S_G_int                                    0
#define S_G_int_int                                1
#define S_G_int_int_int                            2
#define S_G_int_int_int_int                        3
#define S_G_bool                                   4
#define S_G_QRect_int                              5
#define S_G_QString                                6
#define S_G_QModelIndex                            7
#define S_G_QModelIndex_QModelIndex                8
#define S_G_QItemSelection_QItemSelection          9
#define S_G_QTextCharFormat                       10
#define S_G_QFont                                 11
#define S_G_QTextCursor                           12
#define S_G_QStringList                           13
#define S_G_pointer                               14
#define S_G_pointer_pointer                       15
#define S_G_pointer_int                           16
#define S_G_QDate                                 17
#define S_G_QDateTime                             18
#define S_G_QPoint                                19
#define S_G_QRectF                                20
#define S_G_QTime                                 21
#define S_G_QUrl                                  22
#define S_G_QModelIndex_int_int                   23
#define S_G_qint64                                24
#define S_G_qint64_qint64                         25
#define S_G_QTextBlock                            26
#define S_G_QSizeF                                27
#define S_G_QColor                                28
#define S_G_double                                29
#define S_G_QModelIndexList                       30
#define S_G_QRect                                 31
#define S_G_int_QString                           32
#define S_G_QString_quint16_pointer               33
#define S_G_int_bool                              34
#define S_G_pointer_QString                       35
#define S_G_QString_QString_QString               36
#define S_G_int_int_QRect                         37
#define S_G_QUrlInfo                              38
#define S_G_QNetworkRequest                       39
#define S_G_QNetworkProxy_pointer                 40
#define S_G_QHttpResponseHeader                   41

/*----------------------------------------------------------------------*/

static void hbqt_fireSignal( int paramId, PHB_ITEM * codeBlock, void ** arguments )
{
   int iArgs;

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );

   switch( paramId )
   {
   case S_G_int:
      iArgs = 1;
      hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 1 ] ) );
      break;
   case S_G_int_int:
      iArgs = 2;
      hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 1 ] ) );
      hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) );
      break;
   case S_G_int_int_int:
      iArgs = 3;
      hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 1 ] ) );
      hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) );
      hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 3 ] ) );
      break;
   case S_G_int_int_int_int:
      iArgs = 4;
      hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 1 ] ) );
      hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) );
      hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 3 ] ) );
      hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 4 ] ) );
      break;
   case S_G_bool:
      iArgs = 1;
      hb_vmPushLogical( *reinterpret_cast< bool( * ) >( arguments[ 1 ] ) );
      break;
   case S_G_QRect_int:
      iArgs = 2;
      hb_vmPushPointer( new QRect( *reinterpret_cast< QRect( * ) >( arguments[ 1 ] ) ) );
      hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) );
      break;
   case S_G_QString:
      {
         iArgs = 1;
         QString text = *reinterpret_cast< QString( * ) >( arguments[ 1 ] );
         hb_vmPushString( text.toAscii().data(), text.toAscii().length() );
      }
      break;
   case S_G_QModelIndex:
      iArgs = 1;
      hb_vmPushPointer( new QModelIndex( ( *reinterpret_cast< QModelIndex( * ) >( arguments[ 1 ] ) ) ) );
      break;
   case S_G_QModelIndex_QModelIndex:
      iArgs = 2;
      hb_vmPushPointer( new QModelIndex( ( *reinterpret_cast< QModelIndex( * ) >( arguments[ 1 ] ) ) ) );
      hb_vmPushPointer( new QModelIndex( ( *reinterpret_cast< QModelIndex( * ) >( arguments[ 2 ] ) ) ) );
      break;
   case S_G_QItemSelection_QItemSelection:
      iArgs = 2;
      hb_vmPushPointer( new QItemSelection( ( *reinterpret_cast< QItemSelection( * )>( arguments[ 1 ] ) ) ) );
      hb_vmPushPointer( new QItemSelection( ( *reinterpret_cast< QItemSelection( * )>( arguments[ 2 ] ) ) ) );
      break;
   case S_G_QTextCharFormat:
      iArgs = 1;
      hb_vmPushPointer( new QTextCharFormat( ( *reinterpret_cast<QTextCharFormat( * )>( arguments[ 1 ] ) ) ) );
      break;
   case S_G_QFont:
      iArgs = 1;
      hb_vmPushPointer( new QFont( ( *reinterpret_cast< QFont( * )>( arguments[ 1 ] ) ) ) );
      break;
   case S_G_QTextCursor:
      iArgs = 1;
      hb_vmPushPointer( new QTextCursor( ( *reinterpret_cast< QTextCursor( * )>( arguments[ 1 ] ) ) ) );
      break;
   case S_G_QStringList:
      iArgs = 1;
      hb_vmPushPointer( new QStringList( ( *reinterpret_cast<QStringList( * )>( arguments[ 1 ] ) ) ) );
      break;
   case S_G_pointer:
      iArgs = 1;
      hb_vmPushPointer( *reinterpret_cast< void*( * )>( arguments[ 1 ] ) );
      break;
   case S_G_pointer_pointer:
      iArgs = 2;
      hb_vmPushPointer( *reinterpret_cast< void*( * )>( arguments[ 1 ] ) );
      hb_vmPushPointer( *reinterpret_cast< void*( * )>( arguments[ 2 ] ) );
      break;
   case S_G_pointer_int:
      iArgs = 2;
      hb_vmPushPointer( *reinterpret_cast< void*( * )>( arguments[ 1 ] ) );
      hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) );
      break;
   case S_G_QDate:
      iArgs = 1;
      hb_vmPushPointer( new QDate( ( *reinterpret_cast< QDate( * ) >( arguments[ 1 ] ) ) ) );
      break;
   case S_G_QDateTime:
      iArgs = 1;
      hb_vmPushPointer( new QDateTime( ( *reinterpret_cast< QDateTime( * ) >( arguments[ 1 ] ) ) ) );
      break;
   case S_G_QPoint:
      iArgs = 1;
      hb_vmPushPointer( new QPoint( ( * reinterpret_cast< QPoint( * )>( arguments[ 1 ] ) ) ) );
      break;
   case S_G_QRectF:
      iArgs = 1;
      hb_vmPushPointer( new QRectF( ( *reinterpret_cast< QRectF( * )>( arguments[ 1 ] ) ) ) );
      break;
   case S_G_QTime:
      iArgs = 1;
      hb_vmPushPointer( new QTime( ( *reinterpret_cast< QTime( * ) >( arguments[ 1 ] ) ) ) );
      break;
   case S_G_QUrl:
      iArgs = 1;
      hb_vmPushPointer( new QUrl( ( *reinterpret_cast< QUrl( * )>( arguments[ 1 ] ) ) ) );
      break;

   case S_G_QModelIndex_int_int:
      iArgs = 3;
      hb_vmPushPointer( new QModelIndex( ( *reinterpret_cast< QModelIndex( * ) >( arguments[ 1 ] ) ) ) );
      hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) );
      hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 3 ] ) );
      break;
   case S_G_qint64:
      iArgs = 1;
      hb_vmPushInteger( *reinterpret_cast< qint64( * ) >( arguments[ 1 ] ) );
      break;
   case S_G_qint64_qint64:
      iArgs = 2;
      hb_vmPushInteger( *reinterpret_cast< qint64( * ) >( arguments[ 1 ] ) );
      hb_vmPushInteger( *reinterpret_cast< qint64( * ) >( arguments[ 2 ] ) );
      break;
   case S_G_QTextBlock:
      iArgs = 1;
      hb_vmPushPointer( new QTextBlock( ( *reinterpret_cast< QTextBlock( * ) >( arguments[ 1 ] ) ) ) );
      break;
   case S_G_QSizeF:
      iArgs = 1;
      hb_vmPushPointer( new QSizeF( ( *reinterpret_cast< QSizeF( * ) >( arguments[ 1 ] ) ) ) );
      break;
   case S_G_QColor:
      iArgs = 1;
      hb_vmPushPointer( new QColor( ( *reinterpret_cast< QColor( * ) >( arguments[ 1 ] ) ) ) );
      break;
   case S_G_double:
      iArgs = 1;
      hb_vmPushDouble( *reinterpret_cast< double( * ) >( arguments[ 1 ] ), 4 );
      break;
   case S_G_QModelIndexList:
      iArgs = 1;
      hb_vmPushPointer( new QModelIndexList( ( *reinterpret_cast< QModelIndexList( * ) >( arguments[ 1 ] ) ) ) );
      break;
   case S_G_QRect:
      iArgs = 1;
      hb_vmPushPointer( new QRect( ( *reinterpret_cast< QRect( * ) >( arguments[ 1 ] ) ) ) );
      break;
   case S_G_int_QString:
      {
         iArgs = 2;
         hb_vmPushInteger( *reinterpret_cast< qint64( * ) >( arguments[ 1 ] ) );
         QString text = *reinterpret_cast< QString( * ) >( arguments[ 2 ] );
         hb_vmPushString( text.toAscii().data(), text.toAscii().length() );
      }
      break;
   case S_G_QString_quint16_pointer:
      {
         iArgs = 3;
         QString text = *reinterpret_cast< QString( * ) >( arguments[ 1 ] );
         hb_vmPushString( text.toAscii().data(), text.toAscii().length() );
         hb_vmPushInteger( *reinterpret_cast< quint64( * ) >( arguments[ 2 ] ) );
         hb_vmPushPointer( *reinterpret_cast< void*( * )>( arguments[ 3 ] ) );
      }
      break;
   case S_G_int_bool:
      iArgs = 2;
      hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 1 ] ) );
      hb_vmPushLogical( *reinterpret_cast< bool( * ) >( arguments[ 2 ] ) );
      break;
   case S_G_pointer_QString:
      {
         iArgs = 2;
         hb_vmPushPointer( *reinterpret_cast< void*( * )>( arguments[ 1 ] ) );
         QString text = *reinterpret_cast< QString( * ) >( arguments[ 2 ] );
         hb_vmPushString( text.toAscii().data(), text.toAscii().length() );
      }
      break;
   case S_G_QString_QString_QString:
      {
         iArgs = 3;
         QString text;
         text = *reinterpret_cast< QString( * ) >( arguments[ 1 ] );
         hb_vmPushString( text.toAscii().data(), text.toAscii().length() );
         text = *reinterpret_cast< QString( * ) >( arguments[ 2 ] );
         hb_vmPushString( text.toAscii().data(), text.toAscii().length() );
         text = *reinterpret_cast< QString( * ) >( arguments[ 3 ] );
         hb_vmPushString( text.toAscii().data(), text.toAscii().length() );
      }
      break;
   case S_G_int_int_QRect:
      iArgs = 3;
      hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 1 ] ) );
      hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) );
      hb_vmPushPointer( new QRect( ( *reinterpret_cast< QRect( * ) >( arguments[ 1 ] ) ) ) );
      break;
   #if 0   /* Disabled until dynamic registration is implemented */
   case S_G_QUrlInfo:
      iArgs = 1;
      hb_vmPushPointer( new QUrlInfo( ( *reinterpret_cast< QUrlInfo( * ) >( arguments[ 1 ] ) ) ) );
      break;
   case S_G_QNetworkRequest:
      iArgs = 1;
      hb_vmPushPointer( new QNetworkRequest( ( *reinterpret_cast< QNetworkRequest( * ) >( arguments[ 1 ] ) ) ) );
      break;
   case S_G_QNetworkProxy_pointer:
      iArgs = 2;
      hb_vmPushPointer( new QNetworkProxy( ( *reinterpret_cast< QNetworkProxy( * ) >( arguments[ 1 ] ) ) ) );
      hb_vmPushPointer( *reinterpret_cast< void*( * )>( arguments[ 2 ] ) );
      break;
   case S_G_QHttpResponseHeader:
      iArgs = 1;
      hb_vmPushPointer( new QHttpResponseHeader( ( *reinterpret_cast< QHttpResponseHeader( * ) >( arguments[ 1 ] ) ) ) );
      break;
   #endif
   }

   hb_vmSend( iArgs );
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
   static QList< QByteArray > argCombinations;

   if( argCombinations.size() == 0 )
   {
      argCombinations      <<
           SIG_int                              <<
           SIG_int_int                          <<
           SIG_int_int_int                      <<
           SIG_int_int_int_int                  <<
           SIG_bool                             <<
           SIG_QRect_int                        <<
           SIG_QString                          <<
           SIG_QModelIndex                      <<
           SIG_QModelIndex_QModelIndex          <<
           SIG_QItemSelection_QItemSelection    <<
           SIG_QTextCharFormat                  <<
           SIG_QFont                            <<
           SIG_QTextCursor                      <<
           SIG_QStringList                      <<
           SIG_pointer                          <<
           SIG_pointer_pointer                  <<
           SIG_pointer_int                      <<
           SIG_QDate                            <<
           SIG_QDateTime                        <<
           SIG_QPoint                           <<
           SIG_QRectF                           <<
           SIG_QTime                            <<
           SIG_QUrl                             <<
           SIG_QModelIndex_int_int              <<
           SIG_qint64                           <<
           SIG_qint64_qint64                    <<
           SIG_QTextBlock                       <<
           SIG_QSizeF                           <<
           SIG_QColor                           <<
           SIG_double                           <<
           SIG_QModelIndexList                  <<
           SIG_QRect                            <<
           SIG_QRect_int                        <<
           SIG_int_QString                      <<
           SIG_QString_quint16_pointer          <<
           SIG_int_bool                         <<
           SIG_pointer_QString                  <<
           SIG_QString_QString_QString          <<
           SIG_int_int_QRect                    <<
           SIG_QUrlInfo                         <<
           SIG_QNetworkRequest                  <<
           SIG_QNetworkProxy_pointer            <<
           SIG_QHttpResponseHeader              <<
                           "xxxyyyzzz"           ;            /* Just for line break */
   }

   const QMetaMethod meta = object->metaObject()->method( id );

   HB_TRACE( HB_TR_DEBUG, ( "SlotsProxy signature %s", meta.signature() ) );

   QList<QByteArray> arrayOfTypes = meta.parameterTypes();

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
            parList += arrayOfTypes.at( i );
         }
      }
   }

   QByteArray paramString = parList.join( "$" ).toAscii();
   HB_TRACE( HB_TR_DEBUG, ( "       SlotsProxy parList %s ", (char * ) paramString.data() ) );

   if( object )
   {
      char cSlotName[ 20 ];
      sprintf( cSlotName, "SLOT_%d", id );
      int i = object->property( cSlotName ).toInt();

      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         if( parameterCount == 0 )
         {
            hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 0 );
         }
         else
         {
            int paramId = argCombinations.indexOf( paramString );

            HB_TRACE( HB_TR_DEBUG, ( "  params=%s,  paramId=%d", ( char * ) paramString.data(), paramId ) );

            hbqt_fireSignal( paramId, ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments );
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
