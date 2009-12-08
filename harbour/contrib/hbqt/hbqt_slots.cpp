/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
/*----------------------------------------------------------------------*/

#include "hbapi.h"
#include "hbvm.h"
#include "hbapiitm.h"
#include "hbstack.h"

#include "hbqt.h"

#if QT_VERSION >= 0x040500

#include "hbqt_slots.h"

#include <QWidget>
#include <QString>
#include <QList>
#include <QKeyEvent>
#include <QAction>
#include <QObject>
#include <QEvent>
#include <QMessageBox>
#include <QFileDialog>

/*----------------------------------------------------------------------*/

typedef struct
{
   Events * t_events;
} HB_EVENTS, * PHB_EVENTS;

static HB_TSD_NEW( s_events, sizeof( HB_EVENTS ), NULL, NULL );

#define HB_GETQTEVENTFILTER()       ( ( PHB_EVENTS ) hb_stackGetTSD( &s_events ) )

typedef struct
{
   Slots * t_slots;
} HB_SLOTS, * PHB_SLOTS;

static HB_TSD_NEW( s_slots, sizeof( HB_SLOTS ), NULL, NULL );

#define HB_GETQTEVENTSLOTS()       ( ( PHB_SLOTS ) hb_stackGetTSD( &s_slots ) )

/*----------------------------------------------------------------------*/

static void qt_setEventFilter()
{
   Events * t_events = HB_GETQTEVENTFILTER()->t_events;
   if( ! t_events )
      HB_GETQTEVENTFILTER()->t_events = new Events();
}

static Events * qt_getEventFilter( void )
{
   return HB_GETQTEVENTFILTER()->t_events;
}

static void qt_setEventSlots()
{
   Slots * t_slots = HB_GETQTEVENTSLOTS()->t_slots;
   if( ! t_slots )
      HB_GETQTEVENTSLOTS()->t_slots = new Slots();
}

static Slots * qt_getEventSlots( void )
{
   return HB_GETQTEVENTSLOTS()->t_slots;
}

HB_FUNC( QT_SETEVENTFILTER )
{
   qt_setEventFilter();
}

HB_FUNC( QT_SETEVENTSLOTS )
{
   qt_setEventSlots();
}

HB_FUNC( QT_QEVENTFILTER )
{
   hb_retptr( qt_getEventFilter() );
}

/*----------------------------------------------------------------------*/

Slots::Slots( QObject* parent ) : QObject( parent )
{
}

Slots::~Slots()
{
   listBlock.clear();
}

static void hbqt_SlotsExec( QObject * object, const char * event )
{
   if( object )
   {
      Slots * t_slots = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 1, pObject );
         hb_itemRelease( pObject );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecBool( QObject * object, const char* event, bool bBool )
{
   if( object )
   {
      Slots * t_slots = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pBool = hb_itemPutL( NULL, bBool );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 2, pObject, pBool );
         hb_itemRelease( pObject );
         hb_itemRelease( pBool );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecInt( QObject * object, const char * event, int iValue )
{
   if( object )
   {
      Slots * t_slots = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pState = hb_itemPutNI( NULL, iValue );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 2, pObject, pState );
         hb_itemRelease( pObject );
         hb_itemRelease( pState );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecIntInt( QObject * object, const char * event, int iValue1, int iValue2 )
{
   if( object )
   {
      Slots * t_slots = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pValue1 = hb_itemPutNI( NULL, iValue1 );
         PHB_ITEM pValue2 = hb_itemPutNI( NULL, iValue2 );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 3, pObject, pValue1, pValue2 );
         hb_itemRelease( pObject );
         hb_itemRelease( pValue1 );
         hb_itemRelease( pValue2 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecIntIntInt( QObject * object, const char * event, int iValue1, int iValue2, int iValue3 )
{
   if( object )
   {
      Slots * t_slots = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pValue1 = hb_itemPutNI( NULL, iValue1 );
         PHB_ITEM pValue2 = hb_itemPutNI( NULL, iValue2 );
         PHB_ITEM pValue3 = hb_itemPutNI( NULL, iValue3 );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 4, pObject, pValue1, pValue2, pValue3 );
         hb_itemRelease( pObject );
         hb_itemRelease( pValue1 );
         hb_itemRelease( pValue2 );
         hb_itemRelease( pValue3 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecIntIntIntInt( QObject * object, const char * event, int iValue1, int iValue2, int iValue3, int iValue4 )
{
   if( object )
   {
      Slots * t_slots = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pValue1 = hb_itemPutNI( NULL, iValue1 );
         PHB_ITEM pValue2 = hb_itemPutNI( NULL, iValue2 );
         PHB_ITEM pValue3 = hb_itemPutNI( NULL, iValue3 );
         PHB_ITEM pValue4 = hb_itemPutNI( NULL, iValue4 );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 5, pObject, pValue1, pValue2, pValue3, pValue4 );
         hb_itemRelease( pObject );
         hb_itemRelease( pValue1 );
         hb_itemRelease( pValue2 );
         hb_itemRelease( pValue3 );
         hb_itemRelease( pValue4 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecString( QObject * object, const char * event, const QString & string )
{
   if( object )
   {
      Slots * t_slots = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pString = hb_itemPutC( NULL, string.toAscii().data() );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 2, pObject, pString );
         hb_itemRelease( pObject );
         hb_itemRelease( pString );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecModel( QObject * object, const char * event, const QModelIndex & index )
{
   if( object )
   {
      Slots * t_slots = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pState = hb_itemPutPtr( NULL, ( QModelIndex * ) new QModelIndex( index ) );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 2, pObject, pState );
         hb_itemRelease( pObject );
         delete ( ( QModelIndex * ) hb_itemGetPtr( pState ) );
         hb_itemRelease( pState );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecTextCharFormat( QObject * object, const char * event, const QTextCharFormat & f )
{
   if( object )
   {
      Slots * t_slots = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QTextCharFormat( f ) );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 2, pObject, p1 );
         hb_itemRelease( pObject );
         delete ( ( QTextCharFormat * ) hb_itemGetPtr( p1 ) );
         hb_itemRelease( p1 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecFont( QObject * object, const char * event, const QFont & font )
{
   if( object )
   {
      Slots * t_slots = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QFont( font ) );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 2, pObject, p1 );
         hb_itemRelease( pObject );
         delete ( ( QFont * ) hb_itemGetPtr( p1 ) );
         hb_itemRelease( p1 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecStringList( QObject * object, const char * event, const QStringList & stringList )
{
   if( object )
   {
      Slots * t_slots = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QStringList( stringList ) );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 2, pObject, p1 );
         hb_itemRelease( pObject );
         delete ( ( QStringList * ) hb_itemGetPtr( p1 ) );
         hb_itemRelease( p1 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecPointer( QObject * object, const char * event, void * p1 )
{
   if( object )
   {
      Slots * t_slots = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pP1 = hb_itemPutPtr( NULL, p1 );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 2, pObject, pP1 );
         hb_itemRelease( pObject );
         hb_itemRelease( pP1 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecPointerInt( QObject * object, const char * event, void * p1, int iInt )
{
   if( object )
   {
      Slots * t_slots = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pP1 = hb_itemPutPtr( NULL, p1 );
         PHB_ITEM pI1 = hb_itemPutNI( NULL, iInt );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 3, pObject, pP1, pI1 );
         hb_itemRelease( pObject );
         hb_itemRelease( pP1 );
         hb_itemRelease( pI1 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecPointerPointer( QObject * object, const char * event, void * p1, void * p2 )
{
   if( object )
   {
      Slots * t_slots = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pP1 = hb_itemPutPtr( NULL, p1 );
         PHB_ITEM pP2 = hb_itemPutPtr( NULL, p2 );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 3, pObject, pP1, pP2 );
         hb_itemRelease( pObject );
         hb_itemRelease( pP1 );
         hb_itemRelease( pP2 );
         hb_vmRequestRestore();
      }
   }
}

/* Generic Key and Mouse Events emitted by subclass objects */
void Slots::keyPressEvent( QKeyEvent * event )                                                           { hbqt_SlotsExecPointer(        qobject_cast<QObject *>( sender() ), "keyPressEvent()", event ); }
void Slots::keyReleaseEvent( QKeyEvent * event )                                                         { hbqt_SlotsExecPointer(        qobject_cast<QObject *>( sender() ), "keyReleaseEvent()", event ); }
void Slots::mouseMoveEvent( QMouseEvent * event )                                                        { hbqt_SlotsExecPointer(        qobject_cast<QObject *>( sender() ), "mouseMoveEvent()", event ); }
void Slots::mouseDoubleClickEvent( QMouseEvent * event )                                                 { hbqt_SlotsExecPointer(        qobject_cast<QObject *>( sender() ), "mouseDoubleClickEvent()", event ); }
void Slots::mousePressEvent( QMouseEvent * event )                                                       { hbqt_SlotsExecPointer(        qobject_cast<QObject *>( sender() ), "mousePressEvent()", event ); }
void Slots::mouseReleaseEvent( QMouseEvent * event )                                                     { hbqt_SlotsExecPointer(        qobject_cast<QObject *>( sender() ), "mouseReleaseEvent()", event ); }
void Slots::wheelEvent( QWheelEvent * event )                                                            { hbqt_SlotsExecPointer(        qobject_cast<QObject *>( sender() ), "wheelEvent()", event ); }
void Slots::resizeEvent( QResizeEvent * event )                                                          { hbqt_SlotsExecPointer(        qobject_cast<QObject *>( sender() ), "resizeEvent()", event ); }
void Slots::triggered( bool checked )                                                                    { hbqt_SlotsExecBool(           qobject_cast<QObject *>( sender() ), "triggered(bool)", checked ); }
void Slots::hovered( QAction * action )                                                                  { hbqt_SlotsExecPointer(        qobject_cast<QObject *>( sender() ), "hovered(action)", action ); }
void Slots::clicked()                                                                                    { hbqt_SlotsExec(               qobject_cast<QObject *>( sender() ), "clicked()" ); }
void Slots::returnPressed()                                                                              { hbqt_SlotsExec(               qobject_cast<QObject *>( sender() ), "returnPressed()" ); }
void Slots::viewportEntered()                                                                            { hbqt_SlotsExec(               qobject_cast<QObject *>( sender() ), "viewportEntered()" ); }
void Slots::pressed()                                                                                    { hbqt_SlotsExec(               qobject_cast<QObject *>( sender() ), "pressed()" ); }
void Slots::released()                                                                                   { hbqt_SlotsExec(               qobject_cast<QObject *>( sender() ), "released()" ); }
void Slots::triggered()                                                                                  { hbqt_SlotsExec(               qobject_cast<QObject *>( sender() ), "triggered()" ); }
void Slots::hovered()                                                                                    { hbqt_SlotsExec(               qobject_cast<QObject *>( sender() ), "hovered()" ); }
void Slots::stateChanged( int state )                                                                    { hbqt_SlotsExecInt(            qobject_cast<QObject *>( sender() ), "stateChanged(int)", state ); }
void Slots::activated( int index )                                                                       { hbqt_SlotsExecInt(            qobject_cast<QObject *>( sender() ), "activated(int)", index ); }
void Slots::currentIndexChanged( int index )                                                             { hbqt_SlotsExecInt(            qobject_cast<QObject *>( sender() ), "currentIndexChanged(int)", index ); }
void Slots::currentChanged( int index )                                                                  { hbqt_SlotsExecInt(            qobject_cast<QObject *>( sender() ), "currentChanged(int)", index ); }
void Slots::highlighted( int index )                                                                     { hbqt_SlotsExecInt(            qobject_cast<QObject *>( sender() ), "highlighted(int)", index ); }
void Slots::clicked( const QModelIndex & index )                                                         { hbqt_SlotsExecModel(          qobject_cast<QObject *>( sender() ), "clicked(QModelIndex)", index ); }
void Slots::doubleClicked( const QModelIndex & index )                                                   { hbqt_SlotsExecModel(          qobject_cast<QObject *>( sender() ), "doubleClicked(QModelIndex)", index ); }
void Slots::entered( const QModelIndex & index )                                                         { hbqt_SlotsExecModel(          qobject_cast<QObject *>( sender() ), "entered(QModelIndex)", index ); }
void Slots::actionTriggered( int action )                                                                { hbqt_SlotsExecInt(            qobject_cast<QObject *>( sender() ), "actionTriggered(int)", action ); }
void Slots::rangeChanged( int min, int max )                                                             { hbqt_SlotsExecIntInt(         qobject_cast<QObject *>( sender() ), "rangeChanged(int)", min, max ); }
void Slots::sliderMoved( int value )                                                                     { hbqt_SlotsExecInt(            qobject_cast<QObject *>( sender() ), "sliderMoved(int)", value ); }
void Slots::sliderPressed()                                                                              { hbqt_SlotsExec(               qobject_cast<QObject *>( sender() ), "sliderPressed()" ); }
void Slots::sliderReleased()                                                                             { hbqt_SlotsExec(               qobject_cast<QObject *>( sender() ), "sliderReleased()" ); }
void Slots::valueChanged( int value )                                                                    { hbqt_SlotsExecInt(            qobject_cast<QObject *>( sender() ), "valueChanged(int)", value ); }
void Slots::cursorPositionChanged( int iOld, int iNew )                                                  { hbqt_SlotsExecIntInt(         qobject_cast<QObject *>( sender() ), "cursorPositionChanged(int,int)", iOld, iNew ); }
void Slots::editingFinished()                                                                            { hbqt_SlotsExec(               qobject_cast<QObject *>( sender() ), "editingFinished()" ); }
void Slots::selectionChanged()                                                                           { hbqt_SlotsExec(               qobject_cast<QObject *>( sender() ), "selectionChanged()" ); }
void Slots::textChanged( const QString & text )                                                          { hbqt_SlotsExecString(         qobject_cast<QObject *>( sender() ), "textChanged(QString)", text ); }
void Slots::textEdited( const QString & text )                                                           { hbqt_SlotsExecString(         qobject_cast<QObject *>( sender() ), "textEdited(QString)", text ); }
/*  TreeViewobject */
void Slots::currentItemChanged( QTreeWidgetItem * current, QTreeWidgetItem * previous )                  { hbqt_SlotsExecPointerPointer( qobject_cast<QObject *>( sender() ), "currentItemChanged(QTWItem)", current, previous ); }
void Slots::itemActivated( QTreeWidgetItem * item, int column )                                          { hbqt_SlotsExecPointerInt(     qobject_cast<QObject *>( sender() ), "itemActivated(QTWItem)", item, column ); }
void Slots::itemChanged( QTreeWidgetItem * item, int column )                                            { hbqt_SlotsExecPointerInt(     qobject_cast<QObject *>( sender() ), "itemChanged(QTWItem)", item, column ); }
void Slots::itemClicked( QTreeWidgetItem * item, int column )                                            { hbqt_SlotsExecPointerInt(     qobject_cast<QObject *>( sender() ), "itemClicked(QTWItem)", item, column ); }
void Slots::itemDoubleClicked( QTreeWidgetItem * item, int column )                                      { hbqt_SlotsExecPointerInt(     qobject_cast<QObject *>( sender() ), "itemDoubleClicked(QTWItem)", item, column ); }
void Slots::itemEntered( QTreeWidgetItem * item, int column )                                            { hbqt_SlotsExecPointerInt(     qobject_cast<QObject *>( sender() ), "itemEntered(QTWItem)", item, column ); }
void Slots::itemPressed( QTreeWidgetItem * item, int column )                                            { hbqt_SlotsExecPointerInt(     qobject_cast<QObject *>( sender() ), "itemPressed(QTWItem)", item, column ); }
void Slots::itemExpanded( QTreeWidgetItem * item )                                                       { hbqt_SlotsExecPointer(        qobject_cast<QObject *>( sender() ), "itemExpanded(QTWItem)", item ); }
void Slots::itemCollapsed( QTreeWidgetItem * item )                                                      { hbqt_SlotsExecPointer(        qobject_cast<QObject *>( sender() ), "itemCollapsed(QTWItem)", item ); }
void Slots::itemSelectionChanged()                                                                       { hbqt_SlotsExec(               qobject_cast<QObject *>( sender() ), "itemSelectionChanged()" ); }
/* QDialog (s)*/
void Slots::currentFontChanged( const QFont & font )                                                     { hbqt_SlotsExecFont(           qobject_cast<QObject *>( sender() ), "currentFontChanged(QFont)", font ); }
void Slots::fontSelected( const QFont & font )                                                           { hbqt_SlotsExecFont(           qobject_cast<QObject *>( sender() ), "fontSelected(QFont)", font ); }
void Slots::accepted()                                                                                   { hbqt_SlotsExec(               qobject_cast<QObject *>( sender() ), "accepted()" ); }
void Slots::finished( int result )                                                                       { hbqt_SlotsExecInt(            qobject_cast<QObject *>( sender() ), "finished(int)", result ); }
void Slots::rejected()                                                                                   { hbqt_SlotsExec(               qobject_cast<QObject *>( sender() ), "rejected()" ); }
void Slots::currentChanged( const QString & path )                                                       { hbqt_SlotsExecString(         qobject_cast<QObject *>( sender() ), "currentChanged(QString)", path ); }
void Slots::directoryEntered( const QString & directory )                                                { hbqt_SlotsExecString(         qobject_cast<QObject *>( sender() ), "directoryEntered(QString)", directory ); }
void Slots::fileSelected( const QString & file )                                                         { hbqt_SlotsExecString(         qobject_cast<QObject *>( sender() ), "fileSelected(QString)", file ); }
void Slots::filesSelected( const QStringList & selected )                                                { hbqt_SlotsExecStringList(     qobject_cast<QObject *>( sender() ), "filesSelected(QStringList)", selected ); }
void Slots::filterSelected( const QString & filter )                                                     { hbqt_SlotsExecString(         qobject_cast<QObject *>( sender() ), "filterSelected(QString)", filter ); }
/* QPrintDialog */
void Slots::accepted( QPrinter * printer )                                                               { hbqt_SlotsExecPointer(        qobject_cast<QObject *>( sender() ), "accepted(QPrinter)", printer ); }
/* QTextEdit */
void Slots::copyAvailable( bool yes )                                                                    { hbqt_SlotsExecBool(           qobject_cast<QObject *>( sender() ), "copyAvailable(bool)", yes ); }
void Slots::currentCharFormatChanged( const QTextCharFormat & f )                                        { hbqt_SlotsExecTextCharFormat( qobject_cast<QObject *>( sender() ), "currentCharFormatChanged(QTextCharFormat)", f ); }
void Slots::cursorPositionChanged()                                                                      { hbqt_SlotsExec(               qobject_cast<QObject *>( sender() ), "cursorPositionChanged()" ); }
void Slots::redoAvailable( bool available )                                                              { hbqt_SlotsExecBool(           qobject_cast<QObject *>( sender() ), "redoAvailable(bool)", available ); }
void Slots::textChanged()                                                                                { hbqt_SlotsExec(               qobject_cast<QObject *>( sender() ), "textChanged()" ); }
void Slots::undoAvailable( bool available )                                                              { hbqt_SlotsExecBool(           qobject_cast<QObject *>( sender() ), "undoAvailable(available)", available ); }
void Slots::timeout()                                                                                    { hbqt_SlotsExec(               qobject_cast<QObject *>( sender() ), "timeout()" ); }
void Slots::scrollContentsBy( int x, int y )                                                             { hbqt_SlotsExecIntInt(         qobject_cast<QObject *>( sender() ), "scrollContentsBy(int,int)", x, y ); }
void Slots::geometriesChanged()                                                                          { hbqt_SlotsExec(               qobject_cast<QObject *>( sender() ), "geometriesChanged()" ); }
void Slots::sectionAutoResize( int logicalIndex, QHeaderView::ResizeMode mode )                          { hbqt_SlotsExecIntInt(         qobject_cast<QObject *>( sender() ), "sectionAutoResize(int,int)", logicalIndex, mode ); }
void Slots::sectionClicked( int logicalIndex )                                                           { hbqt_SlotsExecInt(            qobject_cast<QObject *>( sender() ), "sectionClicked(int)", logicalIndex ); }
void Slots::sectionCountChanged( int oldCount, int newCount )                                            { hbqt_SlotsExecIntInt(         qobject_cast<QObject *>( sender() ), "sectionCountChanged(int,int)", oldCount, newCount ); }
void Slots::sectionDoubleClicked( int logicalIndex )                                                     { hbqt_SlotsExecInt(            qobject_cast<QObject *>( sender() ), "sectionDoubleClicked(int)", logicalIndex ); }
void Slots::sectionEntered( int logicalIndex )                                                           { hbqt_SlotsExecInt(            qobject_cast<QObject *>( sender() ), "sectionEntered(int)", logicalIndex ); }
void Slots::sectionHandleDoubleClicked( int logicalIndex )                                               { hbqt_SlotsExecInt(            qobject_cast<QObject *>( sender() ), "sectionHandleDoubleClicked(int)", logicalIndex ); }
void Slots::sectionMoved( int logicalIndex, int oldVisualIndex, int newVisualIndex )                     { hbqt_SlotsExecIntIntInt(      qobject_cast<QObject *>( sender() ), "sectionMoved(int,int,int)", logicalIndex, oldVisualIndex, newVisualIndex ); }
void Slots::sectionPressed( int logicalIndex )                                                           { hbqt_SlotsExecInt(            qobject_cast<QObject *>( sender() ), "sectionPressed(int)", logicalIndex ); }
void Slots::sectionResized( int logicalIndex, int oldSize, int newSize )                                 { hbqt_SlotsExecIntIntInt(      qobject_cast<QObject *>( sender() ), "sectionResized(int,int,int)", logicalIndex, oldSize, newSize ); }
void Slots::sortIndicatorChanged( int logicalIndex, Qt::SortOrder order )                                { hbqt_SlotsExecIntInt(         qobject_cast<QObject *>( sender() ), "sortIndicatorChanged(int,int)", logicalIndex, order ); }
void Slots::buttonClicked( int id )                                                                      { hbqt_SlotsExecInt(            qobject_cast<QObject *>( sender() ), "buttonClicked(int)", id ); }
void Slots::buttonPressed( int id )                                                                      { hbqt_SlotsExecInt(            qobject_cast<QObject *>( sender() ), "buttonPressed(int)", id ); }
void Slots::buttonReleased( int id )                                                                     { hbqt_SlotsExecInt(            qobject_cast<QObject *>( sender() ), "buttonReleased(int)", id ); }
void Slots::linkActivated( const QString & link )                                                        { hbqt_SlotsExecString(         qobject_cast<QObject *>( sender() ), "linkActivated(QString)", link ); }
void Slots::linkHovered( const QString & link )                                                          { hbqt_SlotsExecString(         qobject_cast<QObject *>( sender() ), "linkHovered(QString)", link ); }
void Slots::cellActivated( int row, int column )                                                         { hbqt_SlotsExecIntInt(         qobject_cast<QObject *>( sender() ), "cellActivated(int,int)", row, column ); }
void Slots::cellChanged( int row, int column )                                                           { hbqt_SlotsExecIntInt(         qobject_cast<QObject *>( sender() ), "cellChanged(int,int)", row, column ); }
void Slots::cellClicked( int row, int column )                                                           { hbqt_SlotsExecIntInt(         qobject_cast<QObject *>( sender() ), "cellClicked(int,int)", row, column ); }
void Slots::cellDoubleClicked( int row, int column )                                                     { hbqt_SlotsExecIntInt(         qobject_cast<QObject *>( sender() ), "cellDoubleClicked(int,int)", row, column ); }
void Slots::cellEntered( int row, int column )                                                           { hbqt_SlotsExecIntInt(         qobject_cast<QObject *>( sender() ), "cellEntered(int,int)", row, column ); }
void Slots::cellPressed( int row, int column )                                                           { hbqt_SlotsExecIntInt(         qobject_cast<QObject *>( sender() ), "cellEntered(int,int)", row, column ); }
void Slots::currentCellChanged( int currentRow, int currentColumn, int previousRow, int previousColumn ) { hbqt_SlotsExecIntIntIntInt(   qobject_cast<QObject *>( sender() ), "currentCellChanged(int,int,int,int)", currentRow, currentColumn, previousRow, previousColumn ); }
void Slots::tabCloseRequested( int index )                                                               { hbqt_SlotsExecInt(            qobject_cast<QObject *>( sender() ), "tabCloseRequested(int)", index ); }
void Slots::paintRequested( QPrinter * printer )                                                         { hbqt_SlotsExecPointer(        qobject_cast<QObject *>( sender() ), "paintRequested(QPrinter)", printer ); }

/*----------------------------------------------------------------------*/
/*
 * Harbour function to connect signals with slots
 */
HB_FUNC( QT_CONNECT_SIGNAL )
{
   QObject * object = ( QObject * ) hbqt_gcpointer( 1 );         /* get sender    */

   if( object == NULL )
   {
       hb_retl( HB_FALSE );
       return;
   }

   QString   signal    = hb_parcx( 2 );                             /* get signal    */
   PHB_ITEM  codeblock = hb_itemNew( hb_param( 3, HB_IT_BLOCK ) );  /* get codeblock */
   bool      ret;

   qt_setEventSlots();
   Slots   * t_slots       = qt_getEventSlots();

   if(      signal == ( QString ) "clicked()" )                                 ret = object->connect( object, SIGNAL( clicked() )                                                 , t_slots, SLOT( clicked() )                                                 , Qt::AutoConnection );
   else if( signal == ( QString ) "returnPressed()" )                           ret = object->connect( object, SIGNAL( returnPressed() )                                           , t_slots, SLOT( returnPressed() )                                           , Qt::AutoConnection );
   else if( signal == ( QString ) "triggered()" )                               ret = object->connect( object, SIGNAL( triggered() )                                               , t_slots, SLOT( triggered() )                                               , Qt::AutoConnection );
   else if( signal == ( QString ) "hovered()" )                                 ret = object->connect( object, SIGNAL( hovered() )                                                 , t_slots, SLOT( hovered() )                                                 , Qt::AutoConnection );
   else if( signal == ( QString ) "viewportEntered()" )                         ret = object->connect( object, SIGNAL( viewportEntered() )                                         , t_slots, SLOT( viewportEntered() )                                         , Qt::AutoConnection );
   else if( signal == ( QString ) "pressed()" )                                 ret = object->connect( object, SIGNAL( pressed() )                                                 , t_slots, SLOT( pressed() )                                                 , Qt::AutoConnection );
   else if( signal == ( QString ) "released()" )                                ret = object->connect( object, SIGNAL( released() )                                                , t_slots, SLOT( released() )                                                , Qt::AutoConnection );
   else if( signal == ( QString ) "stateChanged(int)" )                         ret = object->connect( object, SIGNAL( stateChanged( int ) )                                       , t_slots, SLOT( stateChanged( int ) )                                       , Qt::AutoConnection );
   else if( signal == ( QString ) "activated(int)" )                            ret = object->connect( object, SIGNAL( activated( int ) )                                          , t_slots, SLOT( activated( int ) )                                          , Qt::AutoConnection );
   else if( signal == ( QString ) "currentIndexChanged(int)" )                  ret = object->connect( object, SIGNAL( currentIndexChanged( int ) )                                , t_slots, SLOT( currentIndexChanged( int ) )                                , Qt::AutoConnection );
   else if( signal == ( QString ) "highlighted(int)" )                          ret = object->connect( object, SIGNAL( highlighted( int ) )                                        , t_slots, SLOT( highlighted( int ) )                                        , Qt::AutoConnection );
   else if( signal == ( QString ) "triggered(bool)" )                           ret = object->connect( object, SIGNAL( triggered( bool ) )                                         , t_slots, SLOT( triggered( bool ) )                                         , Qt::AutoConnection );
   else if( signal == ( QString ) "clicked(QModelIndex)" )                      ret = object->connect( object, SIGNAL( clicked( const QModelIndex & ) )                            , t_slots, SLOT( clicked( const QModelIndex & ) )                            , Qt::AutoConnection );
   else if( signal == ( QString ) "doubleClicked(QModelIndex)" )                ret = object->connect( object, SIGNAL( doubleClicked( const QModelIndex & ) )                      , t_slots, SLOT( doubleClicked( const QModelIndex & ) )                      , Qt::AutoConnection );
   else if( signal == ( QString ) "entered(QModelIndex)" )                      ret = object->connect( object, SIGNAL( entered( const QModelIndex & ) )                            , t_slots, SLOT( entered( const QModelIndex & ) )                            , Qt::AutoConnection );
   else if( signal == ( QString ) "hovered(action)" )                           ret = object->connect( object, SIGNAL( hovered( QAction * ) )                                      , t_slots, SLOT( hovered( QAction * ) )                                      , Qt::AutoConnection );
   else if( signal == ( QString ) "currentChanged(int)" )                       ret = object->connect( object, SIGNAL( currentChanged( int ) )                                     , t_slots, SLOT( currentChanged( int ) )                                     , Qt::AutoConnection );
   else if( signal == ( QString ) "actionTriggered(int)" )                      ret = object->connect( object, SIGNAL( actionTriggered(int) )                                      , t_slots, SLOT( actionTriggered(int) )                                      , Qt::AutoConnection );
   else if( signal == ( QString ) "rangeChanged(int,int)" )                     ret = object->connect( object, SIGNAL( rangeChanged(int,int) )                                     , t_slots, SLOT( rangeChanged(int,int) )                                     , Qt::AutoConnection );
   else if( signal == ( QString ) "sliderMoved(int)" )                          ret = object->connect( object, SIGNAL( sliderMoved(int) )                                          , t_slots, SLOT( sliderMoved(int) )                                          , Qt::AutoConnection );
   else if( signal == ( QString ) "sliderPressed()" )                           ret = object->connect( object, SIGNAL( sliderPressed() )                                           , t_slots, SLOT( sliderPressed() )                                           , Qt::AutoConnection );
   else if( signal == ( QString ) "sliderReleased()" )                          ret = object->connect( object, SIGNAL( sliderReleased() )                                          , t_slots, SLOT( sliderReleased() )                                          , Qt::AutoConnection );
   else if( signal == ( QString ) "valueChanged(int)" )                         ret = object->connect( object, SIGNAL( valueChanged(int) )                                         , t_slots, SLOT( valueChanged(int) )                                         , Qt::AutoConnection );
   else if( signal == ( QString ) "cursorPositionChanged(int,int)" )            ret = object->connect( object, SIGNAL( cursorPositionChanged(int,int) )                            , t_slots, SLOT( cursorPositionChanged(int,int) )                            , Qt::AutoConnection );
   else if( signal == ( QString ) "editingFinished()" )                         ret = object->connect( object, SIGNAL( editingFinished() )                                         , t_slots, SLOT( editingFinished() )                                         , Qt::AutoConnection );
   else if( signal == ( QString ) "returnPressed()" )                           ret = object->connect( object, SIGNAL( returnPressed() )                                           , t_slots, SLOT( returnPressed() )                                           , Qt::AutoConnection );
   else if( signal == ( QString ) "selectionChanged()" )                        ret = object->connect( object, SIGNAL( selectionChanged() )                                        , t_slots, SLOT( selectionChanged() )                                        , Qt::AutoConnection );
   else if( signal == ( QString ) "textChanged(QString)" )                      ret = object->connect( object, SIGNAL( textChanged( const QString &) )                             , t_slots, SLOT( textChanged( const QString & ) )                            , Qt::AutoConnection );
   else if( signal == ( QString ) "textEdited(QString)" )                       ret = object->connect( object, SIGNAL( textEdited( const QString &) )                              , t_slots, SLOT( textEdited( const QString & ) )                             , Qt::AutoConnection );
   /* QTreeViewWidget */
   else if( signal == ( QString ) "currentItemChanged(QTWItem)" )               ret = object->connect( object, SIGNAL( currentItemChanged( QTreeWidgetItem *, QTreeWidgetItem * ) ), t_slots, SLOT( currentItemChanged( QTreeWidgetItem *, QTreeWidgetItem * ) ), Qt::AutoConnection );
   else if( signal == ( QString ) "itemActivated(QTWItem)" )                    ret = object->connect( object, SIGNAL( itemActivated( QTreeWidgetItem *, int ) )                   , t_slots, SLOT( itemActivated( QTreeWidgetItem *, int ) )                   , Qt::AutoConnection );
   else if( signal == ( QString ) "itemChanged(QTWItem)" )                      ret = object->connect( object, SIGNAL( itemChanged( QTreeWidgetItem *, int ) )                     , t_slots, SLOT( itemChanged( QTreeWidgetItem *, int ) )                     , Qt::AutoConnection );
   else if( signal == ( QString ) "itemClicked(QTWItem)" )                      ret = object->connect( object, SIGNAL( itemClicked( QTreeWidgetItem *, int ) )                     , t_slots, SLOT( itemClicked( QTreeWidgetItem *, int ) )                     , Qt::AutoConnection );
   else if( signal == ( QString ) "itemCollapsed(QTWItem)" )                    ret = object->connect( object, SIGNAL( itemCollapsed( QTreeWidgetItem * ) )                        , t_slots, SLOT( itemCollapsed( QTreeWidgetItem * ) )                        , Qt::AutoConnection );
   else if( signal == ( QString ) "itemDoubleClicked(QTWItem)" )                ret = object->connect( object, SIGNAL( itemDoubleClicked( QTreeWidgetItem *, int ) )               , t_slots, SLOT( itemDoubleClicked( QTreeWidgetItem *, int ) )               , Qt::AutoConnection );
   else if( signal == ( QString ) "itemEntered(QTWItem)" )                      ret = object->connect( object, SIGNAL( itemEntered( QTreeWidgetItem *, int ) )                     , t_slots, SLOT( itemEntered( QTreeWidgetItem *, int ) )                     , Qt::AutoConnection );
   else if( signal == ( QString ) "itemExpanded(QTWItem)" )                     ret = object->connect( object, SIGNAL( itemExpanded( QTreeWidgetItem * ) )                         , t_slots, SLOT( itemExpanded( QTreeWidgetItem * ) )                         , Qt::AutoConnection );
   else if( signal == ( QString ) "itemPressed(QTWItem)" )                      ret = object->connect( object, SIGNAL( itemPressed( QTreeWidgetItem *, int ) )                     , t_slots, SLOT( itemPressed( QTreeWidgetItem *, int ) )                     , Qt::AutoConnection );
   else if( signal == ( QString ) "itemSelectionChanged()" )                    ret = object->connect( object, SIGNAL( itemSelectionChanged() )                                    , t_slots, SLOT( itemSelectionChanged() )                                    , Qt::AutoConnection );
   /* QDialog (s) QFontDialog, QFileDialog */
   else if( signal == ( QString ) "currentFontChanged(QFont)" )                 ret = object->connect( object, SIGNAL( currentFontChanged( const QFont & ) )                       , t_slots, SLOT( currentFontChanged( const QFont & ) )                       , Qt::AutoConnection );
   else if( signal == ( QString ) "fontSelected(QFont)" )                       ret = object->connect( object, SIGNAL( fontSelected( const QFont & ) )                             , t_slots, SLOT( fontSelected( const QFont & ) )                             , Qt::AutoConnection );
   else if( signal == ( QString ) "accepted()" )                                ret = object->connect( object, SIGNAL( accepted() )                                                , t_slots, SLOT( accepted() )                                                , Qt::AutoConnection );
   else if( signal == ( QString ) "finished(int)" )                             ret = object->connect( object, SIGNAL( finished( int ) )                                           , t_slots, SLOT( finished( int ) )                                           , Qt::AutoConnection );
   else if( signal == ( QString ) "rejected()" )                                ret = object->connect( object, SIGNAL( rejected() )                                                , t_slots, SLOT( rejected() )                                                , Qt::AutoConnection );
   else if( signal == ( QString ) "currentChanged(QString)" )                   ret = object->connect( object, SIGNAL( currentChanged( const QString & ) )                         , t_slots, SLOT( currentChanged( const QString & ) )                         , Qt::AutoConnection );
   else if( signal == ( QString ) "directoryEntered(QString)" )                 ret = object->connect( object, SIGNAL( directoryEntered( const QString & ) )                       , t_slots, SLOT( directoryEntered( const QString & ) )                       , Qt::AutoConnection );
   else if( signal == ( QString ) "fileSelected(QString)" )                     ret = object->connect( object, SIGNAL( fileSelected( const QString & ) )                           , t_slots, SLOT( fileSelected( const QString & ) )                           , Qt::AutoConnection );
   else if( signal == ( QString ) "filesSelected(QStringList)" )                ret = object->connect( object, SIGNAL( filesSelected( const QStringList & ) )                      , t_slots, SLOT( filesSelected( const QStringList & ) )                      , Qt::AutoConnection );
   else if( signal == ( QString ) "filterSelected(QString)" )                   ret = object->connect( object, SIGNAL( filterSelected( const QString & ) )                         , t_slots, SLOT( filterSelected( const QString & ) )                         , Qt::AutoConnection );
   else if( signal == ( QString ) "accepted(QPrinter)" )                        ret = object->connect( object, SIGNAL( accepted( QPrinter * ) )                                    , t_slots, SLOT( accepted( QPrinter * ) )                                    , Qt::AutoConnection );
   else if( signal == ( QString ) "copyAvailable(bool)" )                       ret = object->connect( object, SIGNAL( copyAvailable( bool ) )                                     , t_slots, SLOT( copyAvailable( bool ) )                                     , Qt::AutoConnection );
   else if( signal == ( QString ) "currentCharFormatChanged(QTextCharFormat)" ) ret = object->connect( object, SIGNAL( currentCharFormatChanged( const QTextCharFormat & ) )       , t_slots, SLOT( currentCharFormatChanged( const QTextCharFormat & ) )       , Qt::AutoConnection );
   else if( signal == ( QString ) "cursorPositionChanged()" )                   ret = object->connect( object, SIGNAL( cursorPositionChanged() )                                   , t_slots, SLOT( cursorPositionChanged() )                                   , Qt::AutoConnection );
   else if( signal == ( QString ) "redoAvailable(bool)" )                       ret = object->connect( object, SIGNAL( redoAvailable( bool ) )                                     , t_slots, SLOT( redoAvailable( bool ) )                                     , Qt::AutoConnection );
   else if( signal == ( QString ) "textChanged()" )                             ret = object->connect( object, SIGNAL( textChanged() )                                             , t_slots, SLOT( textChanged() )                                             , Qt::AutoConnection );
   else if( signal == ( QString ) "undoAvailable(available)" )                  ret = object->connect( object, SIGNAL( undoAvailable( bool ) )                                     , t_slots, SLOT( undoAvailable( bool ) )                                     , Qt::AutoConnection );
   else if( signal == ( QString ) "timeout()" )                                 ret = object->connect( object, SIGNAL( timeout() )                                                 , t_slots, SLOT( timeout() )                                                 , Qt::AutoConnection );
   /* Generic purpose mechanism to receive key and mouse events off subclasses */
   else if( signal == ( QString ) "keyPressEvent()" )                           ret = object->connect( object, SIGNAL( sg_keyPressEvent( QKeyEvent * ) )                           , t_slots, SLOT( keyPressEvent( QKeyEvent * ) )                              , Qt::AutoConnection );
   else if( signal == ( QString ) "keyReleaseEvent()" )                         ret = object->connect( object, SIGNAL( sg_keyReleaseEvent( QKeyEvent * ) )                         , t_slots, SLOT( keyReleaseEvent( QKeyEvent * ) )                            , Qt::AutoConnection );
   else if( signal == ( QString ) "mouseMoveEvent()" )                          ret = object->connect( object, SIGNAL( sg_mouseMoveEvent( QMouseEvent * ) )                        , t_slots, SLOT( mouseMoveEvent( QMouseEvent * ) )                           , Qt::AutoConnection );
   else if( signal == ( QString ) "mouseDoubleClickEvent()" )                   ret = object->connect( object, SIGNAL( sg_mouseDoubleClickEvent( QMouseEvent * ) )                 , t_slots, SLOT( mouseDoubleClickEvent( QMouseEvent * ) )                    , Qt::AutoConnection );
   else if( signal == ( QString ) "mousePressEvent()" )                         ret = object->connect( object, SIGNAL( sg_mousePressEvent( QMouseEvent * ) )                       , t_slots, SLOT( mousePressEvent( QMouseEvent * ) )                          , Qt::AutoConnection );
   else if( signal == ( QString ) "mouseReleaseEvent()" )                       ret = object->connect( object, SIGNAL( sg_mouseReleaseEvent( QMouseEvent * ) )                     , t_slots, SLOT( mouseReleaseEvent( QMouseEvent * ) )                        , Qt::AutoConnection );
   else if( signal == ( QString ) "wheelEvent()" )                              ret = object->connect( object, SIGNAL( sg_wheelEvent( QWheelEvent * ) )                            , t_slots, SLOT( wheelEvent( QWheelEvent * ) )                               , Qt::AutoConnection );
   else if( signal == ( QString ) "resizeEvent()" )                             ret = object->connect( object, SIGNAL( sg_resizeEvent( QResizeEvent * ) )                          , t_slots, SLOT( resizeEvent( QResizeEvent * ) )                             , Qt::AutoConnection );
   else if( signal == ( QString ) "scrollContentsBy(int,int)" )                 ret = object->connect( object, SIGNAL( sg_scrollContentsBy( int, int ) )                           , t_slots, SLOT( scrollContentsBy( int, int ) )                              , Qt::AutoConnection );
   else if( signal == ( QString ) "geometriesChanged()" )                       ret = object->connect( object, SIGNAL( geometriesChanged() )                                       , t_slots, SLOT( geometriesChanged() )                                       , Qt::AutoConnection );
   else if( signal == ( QString ) "sectionAutoResize(int,int)" )                ret = object->connect( object, SIGNAL( sectionAutoResize( int, QHeaderView::ResizeMode ) )         , t_slots, SLOT( sectionAutoResize( int, QHeaderView::ResizeMode ) )         , Qt::AutoConnection );
   else if( signal == ( QString ) "sectionClicked(int)" )                       ret = object->connect( object, SIGNAL( sectionClicked( int ) )                                     , t_slots, SLOT( sectionClicked( int ) )                                     , Qt::AutoConnection );
   else if( signal == ( QString ) "sectionCountChanged(int,int)" )              ret = object->connect( object, SIGNAL( sectionCountChanged( int, int ) )                           , t_slots, SLOT( sectionCountChanged( int, int ) )                           , Qt::AutoConnection );
   else if( signal == ( QString ) "sectionDoubleClicked(int)" )                 ret = object->connect( object, SIGNAL( sectionDoubleClicked( int ) )                               , t_slots, SLOT( sectionDoubleClicked( int ) )                               , Qt::AutoConnection );
   else if( signal == ( QString ) "sectionEntered(int)" )                       ret = object->connect( object, SIGNAL( sectionEntered( int ) )                                     , t_slots, SLOT( sectionEntered( int ) )                                     , Qt::AutoConnection );
   else if( signal == ( QString ) "sectionHandleDoubleClicked(int)" )           ret = object->connect( object, SIGNAL( sectionHandleDoubleClicked( int ) )                         , t_slots, SLOT( sectionHandleDoubleClicked( int ) )                         , Qt::AutoConnection );
   else if( signal == ( QString ) "sectionMoved(int,int,int)" )                 ret = object->connect( object, SIGNAL( sectionMoved( int, int, int ) )                             , t_slots, SLOT( sectionMoved( int, int, int ) )                             , Qt::AutoConnection );
   else if( signal == ( QString ) "sectionPressed(int)" )                       ret = object->connect( object, SIGNAL( sectionPressed( int ) )                                     , t_slots, SLOT( sectionPressed( int ) )                                     , Qt::AutoConnection );
   else if( signal == ( QString ) "sectionResized(int,int,int)" )               ret = object->connect( object, SIGNAL( sectionResized( int, int, int ) )                           , t_slots, SLOT( sectionResized( int, int, int ) )                           , Qt::AutoConnection );
   else if( signal == ( QString ) "sortIndicatorChanged(int,int)" )             ret = object->connect( object, SIGNAL( sortIndicatorChanged( int, Qt::SortOrder ) )                , t_slots, SLOT( sortIndicatorChanged( int, Qt::SortOrder ) )                , Qt::AutoConnection );
   else if( signal == ( QString ) "buttonClicked(int)" )                        ret = object->connect( object, SIGNAL( buttonClicked( int ) )                                      , t_slots, SLOT( buttonClicked( int ) )                                      , Qt::AutoConnection );
   else if( signal == ( QString ) "buttonPressed(int)" )                        ret = object->connect( object, SIGNAL( buttonPressed( int ) )                                      , t_slots, SLOT( buttonPressed( int ) )                                      , Qt::AutoConnection );
   else if( signal == ( QString ) "buttonReleased(int)" )                       ret = object->connect( object, SIGNAL( buttonReleased( int ) )                                     , t_slots, SLOT( buttonReleased( int ) )                                     , Qt::AutoConnection );
   else if( signal == ( QString ) "linkActivated(QString)" )                    ret = object->connect( object, SIGNAL( linkActivated( const QString & ) )                          , t_slots, SLOT( linkActivated( const QString & ) )                          , Qt::AutoConnection );
   else if( signal == ( QString ) "linkHovered(QString)" )                      ret = object->connect( object, SIGNAL( linkHovered( const QString & ) )                            , t_slots, SLOT( linkHovered( const QString & ) )                            , Qt::AutoConnection );
   else if( signal == ( QString ) "cellActivated(int,int)" )                    ret = object->connect( object, SIGNAL( cellActivated( int, int ) )                                 , t_slots, SLOT( cellActivated( int, int ) )                                 , Qt::AutoConnection );
   else if( signal == ( QString ) "cellChanged(int,int)" )                      ret = object->connect( object, SIGNAL( cellChanged( int, int ) )                                   , t_slots, SLOT( cellChanged( int, int ) )                                   , Qt::AutoConnection );
   else if( signal == ( QString ) "cellClicked(int,int)" )                      ret = object->connect( object, SIGNAL( cellClicked( int, int ) )                                   , t_slots, SLOT( cellClicked( int, int ) )                                   , Qt::AutoConnection );
   else if( signal == ( QString ) "cellDoubleClicked(int,int)" )                ret = object->connect( object, SIGNAL( cellDoubleClicked( int, int ) )                             , t_slots, SLOT( cellDoubleClicked( int, int ) )                             , Qt::AutoConnection );
   else if( signal == ( QString ) "cellEntered(int,int)" )                      ret = object->connect( object, SIGNAL( cellEntered( int, int ) )                                   , t_slots, SLOT( cellEntered( int, int ) )                                   , Qt::AutoConnection );
   else if( signal == ( QString ) "cellPressed(int,int)" )                      ret = object->connect( object, SIGNAL( cellPressed( int, int ) )                                   , t_slots, SLOT( cellPressed( int, int ) )                                   , Qt::AutoConnection );
   else if( signal == ( QString ) "currentCellChanged(int,int,int,int)" )       ret = object->connect( object, SIGNAL( currentCellChanged( int, int, int, int ) )                  , t_slots, SLOT( currentCellChanged( int, int, int, int ) )                  , Qt::AutoConnection );
   else if( signal == ( QString ) "tabCloseRequested(int)" )                    ret = object->connect( object, SIGNAL( tabCloseRequested( int ) )                                  , t_slots, SLOT( tabCloseRequested( int ) )                                  , Qt::AutoConnection );
   else if( signal == ( QString ) "paintRequested(QPrinter)" )                  ret = object->connect( object, SIGNAL( paintRequested( QPrinter * ) )                              , t_slots, SLOT( paintRequested( QPrinter * ) )                              , Qt::AutoConnection );
   else ret = false;

   if( ret == true )
   {
      t_slots->listBlock  << codeblock;
      object->setProperty( hb_parcx( 2 ), ( int ) t_slots->listBlock.size() );
   }

   hb_retl( ret == true );
}

static bool disconnect_signal( QObject * object, const char * signal )
{
   if(      signal == ( QString ) "clicked()" )                                 return object->disconnect( SIGNAL( clicked() ) );
   else if( signal == ( QString ) "returnPressed()" )                           return object->disconnect( SIGNAL( returnPressed() ) );
   else if( signal == ( QString ) "triggered()" )                               return object->disconnect( SIGNAL( triggered() ) );
   else if( signal == ( QString ) "hovered()" )                                 return object->disconnect( SIGNAL( hovered() ) );
   else if( signal == ( QString ) "viewportEntered()" )                         return object->disconnect( SIGNAL( viewportEntered() ) );
   else if( signal == ( QString ) "pressed()" )                                 return object->disconnect( SIGNAL( pressed() ) );
   else if( signal == ( QString ) "released()" )                                return object->disconnect( SIGNAL( released() ) );
   else if( signal == ( QString ) "stateChanged(int)" )                         return object->disconnect( SIGNAL( stateChanged( int ) ) );
   else if( signal == ( QString ) "activated(int)" )                            return object->disconnect( SIGNAL( activated( int ) ) );
   else if( signal == ( QString ) "currentIndexChanged(int)" )                  return object->disconnect( SIGNAL( currentIndexChanged( int ) ) );
   else if( signal == ( QString ) "highlighted(int)" )                          return object->disconnect( SIGNAL( highlighted( int ) ) );
   else if( signal == ( QString ) "triggered(bool)" )                           return object->disconnect( SIGNAL( triggered( bool ) ) );
   else if( signal == ( QString ) "clicked(QModelIndex)" )                      return object->disconnect( SIGNAL( clicked( const QModelIndex & ) ) );
   else if( signal == ( QString ) "doubleClicked(QModelIndex)" )                return object->disconnect( SIGNAL( doubleClicked( const QModelIndex & ) ) );
   else if( signal == ( QString ) "entered(QModelIndex)" )                      return object->disconnect( SIGNAL( entered( const QModelIndex & ) ) );
   else if( signal == ( QString ) "hovered(action)" )                           return object->disconnect( SIGNAL( hovered( QAction * ) ) );
   else if( signal == ( QString ) "currentChanged(int)" )                       return object->disconnect( SIGNAL( currentChanged( int ) ) );
   else if( signal == ( QString ) "actionTriggered(int)" )                      return object->disconnect( SIGNAL( actionTriggered(int) ) );
   else if( signal == ( QString ) "rangeChanged(int,int)" )                     return object->disconnect( SIGNAL( rangeChanged(int,int) ) );
   else if( signal == ( QString ) "sliderMoved(int)" )                          return object->disconnect( SIGNAL( sliderMoved(int) ) );
   else if( signal == ( QString ) "sliderPressed()" )                           return object->disconnect( SIGNAL( sliderPressed() ) );
   else if( signal == ( QString ) "sliderReleased()" )                          return object->disconnect( SIGNAL( sliderReleased() ) );
   else if( signal == ( QString ) "valueChanged(int)" )                         return object->disconnect( SIGNAL( valueChanged(int) ) );
   else if( signal == ( QString ) "cursorPositionChanged(int,int)" )            return object->disconnect( SIGNAL( cursorPositionChanged(int,int) ) );
   else if( signal == ( QString ) "editingFinished()" )                         return object->disconnect( SIGNAL( editingFinished() ) );
   else if( signal == ( QString ) "returnPressed()" )                           return object->disconnect( SIGNAL( returnPressed() ) );
   else if( signal == ( QString ) "selectionChanged()" )                        return object->disconnect( SIGNAL( selectionChanged() ) );
   else if( signal == ( QString ) "textChanged(QString)" )                      return object->disconnect( SIGNAL( textChanged( const QString &) ) );
   else if( signal == ( QString ) "textEdited(QString)" )                       return object->disconnect( SIGNAL( textEdited( const QString &) ) );
   else if( signal == ( QString ) "currentItemChanged(QTWItem)" )               return object->disconnect( SIGNAL( currentItemChanged( QTreeWidgetItem *, QTreeWidgetItem * ) ) );
   else if( signal == ( QString ) "itemActivated(QTWItem)" )                    return object->disconnect( SIGNAL( itemActivated( QTreeWidgetItem *, int ) ) );
   else if( signal == ( QString ) "itemChanged(QTWItem)" )                      return object->disconnect( SIGNAL( itemChanged( QTreeWidgetItem *, int ) ) );
   else if( signal == ( QString ) "itemClicked(QTWItem)" )                      return object->disconnect( SIGNAL( itemClicked( QTreeWidgetItem *, int ) ) );
   else if( signal == ( QString ) "itemCollapsed(QTWItem)" )                    return object->disconnect( SIGNAL( itemCollapsed( QTreeWidgetItem * ) ) );
   else if( signal == ( QString ) "itemDoubleClicked(QTWItem)" )                return object->disconnect( SIGNAL( itemDoubleClicked( QTreeWidgetItem *, int ) ) );
   else if( signal == ( QString ) "itemEntered(QTWItem)" )                      return object->disconnect( SIGNAL( itemEntered( QTreeWidgetItem *, int ) ) );
   else if( signal == ( QString ) "itemExpanded(QTWItem)" )                     return object->disconnect( SIGNAL( itemExpanded( QTreeWidgetItem * ) ) );
   else if( signal == ( QString ) "itemPressed(QTWItem)" )                      return object->disconnect( SIGNAL( itemPressed( QTreeWidgetItem *, int ) ) );
   else if( signal == ( QString ) "itemSelectionChanged()" )                    return object->disconnect( SIGNAL( itemSelectionChanged() ) );
   else if( signal == ( QString ) "iconChanged()" )                             return object->disconnect( SIGNAL( iconChanged() ) );
   else if( signal == ( QString ) "titleChanged(QString)" )                     return object->disconnect( SIGNAL( titleChanged( const QString & ) ) );
   else if( signal == ( QString ) "urlChanged(QUrl)" )                          return object->disconnect( SIGNAL( urlChanged( const QUrl & ) ) );
   else if( signal == ( QString ) "currentFontChanged(QFont)" )                 return object->disconnect( SIGNAL( currentFontChanged( const QFont & ) ) );
   else if( signal == ( QString ) "fontSelected(QFont)" )                       return object->disconnect( SIGNAL( fontSelected( const QFont & ) ) );
   else if( signal == ( QString ) "accepted()" )                                return object->disconnect( SIGNAL( accepted() ) );
   else if( signal == ( QString ) "finished(int)" )                             return object->disconnect( SIGNAL( finished( int ) ) );
   else if( signal == ( QString ) "rejected()" )                                return object->disconnect( SIGNAL( rejected() ) );
   else if( signal == ( QString ) "currentChanged(QString)" )                   return object->disconnect( SIGNAL( currentChanged( const QString & ) ) );
   else if( signal == ( QString ) "directoryEntered(QString)" )                 return object->disconnect( SIGNAL( directoryEntered( const QString & ) ) );
   else if( signal == ( QString ) "fileSelected(QString)" )                     return object->disconnect( SIGNAL( fileSelected( const QString & ) ) );
   else if( signal == ( QString ) "filesSelected(QStringList)" )                return object->disconnect( SIGNAL( filesSelected( const QStringList & ) ) );
   else if( signal == ( QString ) "filterSelected(QString)" )                   return object->disconnect( SIGNAL( filterSelected( const QString & ) ) );
   else if( signal == ( QString ) "accepted(QPrinter)" )                        return object->disconnect( SIGNAL( accepted( QPrinter * ) ) );
   else if( signal == ( QString ) "copyAvailable(bool)" )                       return object->disconnect( SIGNAL( copyAvailable( bool ) ) );
   else if( signal == ( QString ) "currentCharFormatChanged(QTextCharFormat)" ) return object->disconnect( SIGNAL( currentCharFormatChanged( const QTextCharFormat & ) ) );
   else if( signal == ( QString ) "cursorPositionChanged()" )                   return object->disconnect( SIGNAL( cursorPositionChanged() ) );
   else if( signal == ( QString ) "redoAvailable(bool)" )                       return object->disconnect( SIGNAL( redoAvailable( bool ) ) );
   else if( signal == ( QString ) "textChanged()" )                             return object->disconnect( SIGNAL( textChanged() ) );
   else if( signal == ( QString ) "undoAvailable(available)" )                  return object->disconnect( SIGNAL( undoAvailable( bool ) ) );
   else if( signal == ( QString ) "timeout()" )                                 return object->disconnect( SIGNAL( timeout() ) );
   else if( signal == ( QString ) "keyPressEvent()" )                           return object->disconnect( SIGNAL( sg_keyPressEvent( QKeyEvent * ) ) );
   else if( signal == ( QString ) "keyReleaseEvent()" )                         return object->disconnect( SIGNAL( sg_keyReleaseEvent( QKeyEvent * ) ) );
   else if( signal == ( QString ) "mouseMoveEvent()" )                          return object->disconnect( SIGNAL( sg_mouseMoveEvent( QMouseEvent * ) ) );
   else if( signal == ( QString ) "mouseDoubleClickEvent()" )                   return object->disconnect( SIGNAL( sg_mouseDoubleClickEvent( QMouseEvent * ) ) );
   else if( signal == ( QString ) "mousePressEvent()" )                         return object->disconnect( SIGNAL( sg_mousePressEvent( QMouseEvent * ) ) );
   else if( signal == ( QString ) "mouseReleaseEvent()" )                       return object->disconnect( SIGNAL( sg_mouseReleaseEvent( QMouseEvent * ) ) );
   else if( signal == ( QString ) "wheelEvent()" )                              return object->disconnect( SIGNAL( sg_wheelEvent( QWheelEvent * ) ) );
   else if( signal == ( QString ) "resizeEvent()" )                             return object->disconnect( SIGNAL( sg_resizeEvent( QResizeEvent * ) ) );
   else if( signal == ( QString ) "scrollContentsBy(int,int)" )                 return object->disconnect( SIGNAL( sg_scrollContentsBy( int, int ) ) );
   else if( signal == ( QString ) "geometriesChanged()" )                       return object->disconnect( SIGNAL( geometriesChanged() ) );
   else if( signal == ( QString ) "sectionAutoResize(int,int)" )                return object->disconnect( SIGNAL( sectionAutoResize( int, QHeaderView::ResizeMode ) ) );
   else if( signal == ( QString ) "sectionClicked(int)" )                       return object->disconnect( SIGNAL( sectionClicked( int ) ) );
   else if( signal == ( QString ) "sectionCountChanged(int,int)" )              return object->disconnect( SIGNAL( sectionCountChanged( int, int ) ) );
   else if( signal == ( QString ) "sectionDoubleClicked(int)" )                 return object->disconnect( SIGNAL( sectionDoubleClicked( int ) ) );
   else if( signal == ( QString ) "sectionEntered(int)" )                       return object->disconnect( SIGNAL( sectionEntered( int ) ) );
   else if( signal == ( QString ) "sectionHandleDoubleClicked(int)" )           return object->disconnect( SIGNAL( sectionHandleDoubleClicked( int ) ) );
   else if( signal == ( QString ) "sectionMoved(int,int,int)" )                 return object->disconnect( SIGNAL( sectionMoved( int, int, int ) ) );
   else if( signal == ( QString ) "sectionPressed(int)" )                       return object->disconnect( SIGNAL( sectionPressed( int ) ) );
   else if( signal == ( QString ) "sectionResized(int,int,int)" )               return object->disconnect( SIGNAL( sectionResized( int, int, int ) ) );
   else if( signal == ( QString ) "sortIndicatorChanged(int,int)" )             return object->disconnect( SIGNAL( sortIndicatorChanged( int, Qt::SortOrder ) ) );
   else if( signal == ( QString ) "buttonClicked(int)" )                        return object->disconnect( SIGNAL( buttonClicked( int ) ) );
   else if( signal == ( QString ) "buttonPressed(int)" )                        return object->disconnect( SIGNAL( buttonPressed( int ) ) );
   else if( signal == ( QString ) "buttonReleased(int)" )                       return object->disconnect( SIGNAL( buttonReleased( int ) ) );
   else if( signal == ( QString ) "linkActivated(QString)" )                    return object->disconnect( SIGNAL( linkActivated( const QString & ) ) );
   else if( signal == ( QString ) "linkHovered(QString)" )                      return object->disconnect( SIGNAL( linkHovered( const QString & ) ) );
   else if( signal == ( QString ) "cellActivated(int,int)" )                    return object->disconnect( SIGNAL( cellActivated( int, int ) ) );
   else if( signal == ( QString ) "cellChanged(int,int)" )                      return object->disconnect( SIGNAL( cellChanged( int, int ) ) );
   else if( signal == ( QString ) "cellClicked(int,int)" )                      return object->disconnect( SIGNAL( cellClicked( int, int ) ) );
   else if( signal == ( QString ) "cellDoubleClicked(int,int)" )                return object->disconnect( SIGNAL( cellDoubleClicked( int, int ) ) );
   else if( signal == ( QString ) "cellEntered(int,int)" )                      return object->disconnect( SIGNAL( cellEntered( int, int ) ) );
   else if( signal == ( QString ) "cellPressed(int,int)" )                      return object->disconnect( SIGNAL( cellPressed( int, int ) ) );
   else if( signal == ( QString ) "currentCellChanged(int,int,int,int)" )       return object->disconnect( SIGNAL( currentCellChanged( int, int, int, int ) ) );
   else if( signal == ( QString ) "tabCloseRequested(int)" )                    return object->disconnect( SIGNAL( tabCloseRequested( int ) ) );
   else if( signal == ( QString ) "paintRequested(QPrinter)" )                  return object->disconnect( SIGNAL( paintRequested( QPrinter * ) ) );

   return false;
}

/*
 * harbour function to disconnect signals
 */
HB_FUNC( QT_DISCONNECT_SIGNAL )
{
   QObject * object = ( QObject* ) hbqt_gcpointer( 1 );
   bool bFreed = false;

   if( object )
   {
      Slots * t_slots = qt_getEventSlots();
      const char * signal = hb_parcx( 2 );
      int i = object->property( signal ).toInt();

      if( i > 0 && i <= t_slots->listBlock.size() )
      {
         hb_itemRelease( t_slots->listBlock.at( i - 1 ) );
         t_slots->listBlock[ i - 1 ] = NULL;
         bFreed = disconnect_signal( object, signal );
#if defined( __HB_DEBUG__ )
hbqt_debug( "      QT_DISCONNECT_SIGNAL: %s    %s", bFreed ? "YES" : "NO", signal );
#endif
      }
   }
   hb_retl( bFreed );
}

HB_FUNC( QT_SLOTS_DESTROY )
{
   qt_getEventSlots()->~Slots();
}

/*----------------------------------------------------------------------*/

Events::Events( QObject * parent ) : QObject( parent )
{
}

Events::~Events()
{
   listBlock.clear();
}

bool Events::eventFilter( QObject * object, QEvent * event )
{
   QEvent::Type eventtype = event->type();

   if( ( int ) eventtype == 0 )
      return false;

   char prop[ 10 ];
   hb_snprintf( prop, sizeof( prop ), "%s%i%s", "P", eventtype, "P" );

   int found = object->property( prop ).toInt();

   if( found == 0 )
      return false;

   bool ret = true;

   if( found <= listBlock.size() && listObj.at( found - 1 ) == object && hb_vmRequestReenter() )
   {
      PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
      PHB_ITEM pEvent  = hb_itemPutPtr( NULL, event  );
      ret = hb_itemGetL( hb_vmEvalBlockV( ( PHB_ITEM ) listBlock.at( found - 1 ), 2, pObject, pEvent ) );
      hb_itemRelease( pObject );
      hb_itemRelease( pEvent  );

      hb_vmRequestRestore();

      if( eventtype == QEvent::Close )
         event->ignore();
   }

   return ret;
}

HB_FUNC( QT_EVENTS_DESTROY )
{
   qt_getEventFilter()->~Events();
}

HB_FUNC( QT_CONNECT_EVENT )
{
   QObject * object = ( QObject* ) hbqt_gcpointer( 1 );          /* get sender    */

   if( object )
   {
      int       type      = hb_parni( 2 );
      PHB_ITEM  codeblock = hb_itemNew( hb_param( 3, HB_IT_BLOCK | HB_IT_BYREF ) );
      Events  * t_events  = qt_getEventFilter();

      char prop[ 20 ];
      hb_snprintf( prop, sizeof( prop ), "%s%i%s", "P", type, "P" );    /* Make it a unique identifier */

      t_events->listBlock << codeblock;
      t_events->listObj   << object;

      object->setProperty( prop, ( int ) t_events->listBlock.size() );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( QT_DISCONNECT_EVENT )
{
   HB_BOOL   bRet   = HB_FALSE;
   QObject * object = ( QObject* ) hbqt_gcpointer( 1 );

   if( object )
   {
      int       type     = hb_parni( 2 );
      Events  * t_events = qt_getEventFilter();

      char prop[ 20 ];
      hb_snprintf( prop, sizeof( prop ), "%s%i%s", "P", type, "P" );    /* Make it a unique identifier */

      int i = object->property( prop ).toInt();
      if( i > 0 && i <= t_events->listBlock.size() )
      {
         hb_itemRelease( t_events->listBlock.at( i - 1 ) );
         t_events->listBlock[ i - 1 ] = NULL;
         t_events->listObj[ i - 1 ]   = NULL;
         object->setProperty( prop, QVariant() );
         bRet = HB_TRUE;
#if defined( __HB_DEBUG__ )
hbqt_debug( "      QT_DISCONNECT_EVENT: %i", type );
#endif
      }
   }

   hb_retl( bRet );
}

/*----------------------------------------------------------------------*/

#endif
