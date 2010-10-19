/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum DockOption { AnimatedDocks, AllowNestedDocks, AllowTabbedDocks, ForceTabbedDocks, VerticalTabs }
 *  flags DockOptions
 */

/*
 *  Constructed[ 49/49 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QMainWindow>
#include <QtCore/QSettings>


/*
 * QMainWindow ( QWidget * parent = 0, Qt::WindowFlags flags = 0 )
 * ~QMainWindow ()
 */

HB_FUNC( HBQT_QMAINWINDOW_SAVESETTINGS )
{
   QSettings qSet( hbqt_par_QString( 1 ), QSettings::IniFormat );

   qSet.setValue( hbqt_par_QString( 2 ), hbqt_par_QMainWindow( 3 )->saveState() );
}

HB_FUNC( HBQT_QMAINWINDOW_RESTSETTINGS )
{
   QSettings qSet( hbqt_par_QString( 1 ), QSettings::IniFormat );

   hbqt_par_QMainWindow( 3 )->restoreState( qSet.value( hbqt_par_QString( 2 ) ).toByteArray() );
}

typedef struct
{
   QPointer< QMainWindow > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMainWindow;

HBQT_GC_FUNC( hbqt_gcRelease_QMainWindow )
{
   QMainWindow  * ph = NULL ;
   HBQT_GC_T_QMainWindow * p = ( HBQT_GC_T_QMainWindow * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QMainWindow   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QMainWindow   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QMainWindow          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QMainWindow    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QMainWindow    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QMainWindow( void * pObj, bool bNew )
{
   HBQT_GC_T_QMainWindow * p = ( HBQT_GC_T_QMainWindow * ) hb_gcAllocate( sizeof( HBQT_GC_T_QMainWindow ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QMainWindow >( ( QMainWindow * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMainWindow;
   p->type = HBQT_TYPE_QMainWindow;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QMainWindow  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QMainWindow", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QMAINWINDOW )
{
   QMainWindow * pObj = NULL;

   pObj = new QMainWindow( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QMainWindow( ( void * ) pObj, true ) );
}

/*
 * void addDockWidget ( Qt::DockWidgetArea area, QDockWidget * dockwidget )
 */
HB_FUNC( QT_QMAINWINDOW_ADDDOCKWIDGET )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->addDockWidget( ( Qt::DockWidgetArea ) hb_parni( 2 ), hbqt_par_QDockWidget( 3 ) );
   }
}

/*
 * void addDockWidget ( Qt::DockWidgetArea area, QDockWidget * dockwidget, Qt::Orientation orientation )
 */
HB_FUNC( QT_QMAINWINDOW_ADDDOCKWIDGET_1 )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->addDockWidget( ( Qt::DockWidgetArea ) hb_parni( 2 ), hbqt_par_QDockWidget( 3 ), ( Qt::Orientation ) hb_parni( 4 ) );
   }
}

/*
 * void addToolBar ( Qt::ToolBarArea area, QToolBar * toolbar )
 */
HB_FUNC( QT_QMAINWINDOW_ADDTOOLBAR )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->addToolBar( ( Qt::ToolBarArea ) hb_parni( 2 ), hbqt_par_QToolBar( 3 ) );
   }
}

/*
 * void addToolBar ( QToolBar * toolbar )
 */
HB_FUNC( QT_QMAINWINDOW_ADDTOOLBAR_1 )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->addToolBar( hbqt_par_QToolBar( 2 ) );
   }
}

/*
 * QToolBar * addToolBar ( const QString & title )               // NOT implemented
 */
HB_FUNC( QT_QMAINWINDOW_ADDTOOLBAR_2 )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QToolBar( ( p )->addToolBar( hb_parstr_utf8( 2, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/*
 * void addToolBarBreak ( Qt::ToolBarArea area = Qt::TopToolBarArea )
 */
HB_FUNC( QT_QMAINWINDOW_ADDTOOLBARBREAK )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->addToolBarBreak( ( HB_ISNUM( 2 ) ? ( Qt::ToolBarArea ) hb_parni( 2 ) : ( Qt::ToolBarArea ) Qt::TopToolBarArea ) );
   }
}

/*
 * QWidget * centralWidget () const
 */
HB_FUNC( QT_QMAINWINDOW_CENTRALWIDGET )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->centralWidget(), false ) );
   }
}

/*
 * Qt::DockWidgetArea corner ( Qt::Corner corner ) const
 */
HB_FUNC( QT_QMAINWINDOW_CORNER )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retni( ( Qt::DockWidgetArea ) ( p )->corner( ( Qt::Corner ) hb_parni( 2 ) ) );
   }
}

/*
 * virtual QMenu * createPopupMenu ()
 */
HB_FUNC( QT_QMAINWINDOW_CREATEPOPUPMENU )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->createPopupMenu(), false ) );
   }
}

/*
 * DockOptions dockOptions () const
 */
HB_FUNC( QT_QMAINWINDOW_DOCKOPTIONS )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retni( ( QMainWindow::DockOptions ) ( p )->dockOptions() );
   }
}

/*
 * Qt::DockWidgetArea dockWidgetArea ( QDockWidget * dockwidget ) const
 */
HB_FUNC( QT_QMAINWINDOW_DOCKWIDGETAREA )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retni( ( Qt::DockWidgetArea ) ( p )->dockWidgetArea( hbqt_par_QDockWidget( 2 ) ) );
   }
}

/*
 * bool documentMode () const
 */
HB_FUNC( QT_QMAINWINDOW_DOCUMENTMODE )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retl( ( p )->documentMode() );
   }
}

/*
 * QSize iconSize () const
 */
HB_FUNC( QT_QMAINWINDOW_ICONSIZE )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->iconSize() ), true ) );
   }
}

/*
 * void insertToolBar ( QToolBar * before, QToolBar * toolbar )
 */
HB_FUNC( QT_QMAINWINDOW_INSERTTOOLBAR )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->insertToolBar( hbqt_par_QToolBar( 2 ), hbqt_par_QToolBar( 3 ) );
   }
}

/*
 * void insertToolBarBreak ( QToolBar * before )
 */
HB_FUNC( QT_QMAINWINDOW_INSERTTOOLBARBREAK )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->insertToolBarBreak( hbqt_par_QToolBar( 2 ) );
   }
}

/*
 * bool isAnimated () const
 */
HB_FUNC( QT_QMAINWINDOW_ISANIMATED )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retl( ( p )->isAnimated() );
   }
}

/*
 * bool isDockNestingEnabled () const
 */
HB_FUNC( QT_QMAINWINDOW_ISDOCKNESTINGENABLED )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retl( ( p )->isDockNestingEnabled() );
   }
}

/*
 * QMenuBar * menuBar () const
 */
HB_FUNC( QT_QMAINWINDOW_MENUBAR )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QMenuBar( ( p )->menuBar(), false ) );
   }
}

/*
 * QWidget * menuWidget () const
 */
HB_FUNC( QT_QMAINWINDOW_MENUWIDGET )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->menuWidget(), false ) );
   }
}

/*
 * void removeDockWidget ( QDockWidget * dockwidget )
 */
HB_FUNC( QT_QMAINWINDOW_REMOVEDOCKWIDGET )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->removeDockWidget( hbqt_par_QDockWidget( 2 ) );
   }
}

/*
 * void removeToolBar ( QToolBar * toolbar )
 */
HB_FUNC( QT_QMAINWINDOW_REMOVETOOLBAR )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->removeToolBar( hbqt_par_QToolBar( 2 ) );
   }
}

/*
 * void removeToolBarBreak ( QToolBar * before )
 */
HB_FUNC( QT_QMAINWINDOW_REMOVETOOLBARBREAK )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->removeToolBarBreak( hbqt_par_QToolBar( 2 ) );
   }
}

/*
 * bool restoreDockWidget ( QDockWidget * dockwidget )
 */
HB_FUNC( QT_QMAINWINDOW_RESTOREDOCKWIDGET )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retl( ( p )->restoreDockWidget( hbqt_par_QDockWidget( 2 ) ) );
   }
}

/*
 * bool restoreState ( const QByteArray & state, int version = 0 )
 */
HB_FUNC( QT_QMAINWINDOW_RESTORESTATE )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retl( ( p )->restoreState( *hbqt_par_QByteArray( 2 ), hb_parni( 3 ) ) );
   }
}

/*
 * QByteArray saveState ( int version = 0 ) const
 */
HB_FUNC( QT_QMAINWINDOW_SAVESTATE )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->saveState( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * void setCentralWidget ( QWidget * widget )                        [*D=1*]
 */
HB_FUNC( QT_QMAINWINDOW_SETCENTRALWIDGET )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 2 );
      ( p )->setCentralWidget( hbqt_par_QWidget( 2 ) );
   }
}

/*
 * void setCorner ( Qt::Corner corner, Qt::DockWidgetArea area )
 */
HB_FUNC( QT_QMAINWINDOW_SETCORNER )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->setCorner( ( Qt::Corner ) hb_parni( 2 ), ( Qt::DockWidgetArea ) hb_parni( 3 ) );
   }
}

/*
 * void setDockOptions ( DockOptions options )
 */
HB_FUNC( QT_QMAINWINDOW_SETDOCKOPTIONS )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->setDockOptions( ( QMainWindow::DockOptions ) hb_parni( 2 ) );
   }
}

/*
 * void setDocumentMode ( bool enabled )
 */
HB_FUNC( QT_QMAINWINDOW_SETDOCUMENTMODE )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->setDocumentMode( hb_parl( 2 ) );
   }
}

/*
 * void setIconSize ( const QSize & iconSize )
 */
HB_FUNC( QT_QMAINWINDOW_SETICONSIZE )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->setIconSize( *hbqt_par_QSize( 2 ) );
   }
}

/*
 * void setMenuBar ( QMenuBar * menuBar )                            [*D=1*]
 */
HB_FUNC( QT_QMAINWINDOW_SETMENUBAR )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 2 );
      ( p )->setMenuBar( hbqt_par_QMenuBar( 2 ) );
   }
}

/*
 * void setMenuWidget ( QWidget * menuBar )                          [*D=1*]
 */
HB_FUNC( QT_QMAINWINDOW_SETMENUWIDGET )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 2 );
      ( p )->setMenuWidget( hbqt_par_QWidget( 2 ) );
   }
}

/*
 * void setStatusBar ( QStatusBar * statusbar )                      [*D=1*]
 */
HB_FUNC( QT_QMAINWINDOW_SETSTATUSBAR )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 2 );
      ( p )->setStatusBar( hbqt_par_QStatusBar( 2 ) );
   }
}

/*
 * void setTabPosition ( Qt::DockWidgetAreas areas, QTabWidget::TabPosition tabPosition )
 */
HB_FUNC( QT_QMAINWINDOW_SETTABPOSITION )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->setTabPosition( ( Qt::DockWidgetAreas ) hb_parni( 2 ), ( QTabWidget::TabPosition ) hb_parni( 3 ) );
   }
}

/*
 * void setTabShape ( QTabWidget::TabShape tabShape )
 */
HB_FUNC( QT_QMAINWINDOW_SETTABSHAPE )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->setTabShape( ( QTabWidget::TabShape ) hb_parni( 2 ) );
   }
}

/*
 * void setToolButtonStyle ( Qt::ToolButtonStyle toolButtonStyle )
 */
HB_FUNC( QT_QMAINWINDOW_SETTOOLBUTTONSTYLE )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->setToolButtonStyle( ( Qt::ToolButtonStyle ) hb_parni( 2 ) );
   }
}

/*
 * void setUnifiedTitleAndToolBarOnMac ( bool set )
 */
HB_FUNC( QT_QMAINWINDOW_SETUNIFIEDTITLEANDTOOLBARONMAC )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->setUnifiedTitleAndToolBarOnMac( hb_parl( 2 ) );
   }
}

/*
 * void splitDockWidget ( QDockWidget * first, QDockWidget * second, Qt::Orientation orientation )
 */
HB_FUNC( QT_QMAINWINDOW_SPLITDOCKWIDGET )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->splitDockWidget( hbqt_par_QDockWidget( 2 ), hbqt_par_QDockWidget( 3 ), ( Qt::Orientation ) hb_parni( 4 ) );
   }
}

/*
 * QStatusBar * statusBar () const
 */
HB_FUNC( QT_QMAINWINDOW_STATUSBAR )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStatusBar( ( p )->statusBar(), false ) );
   }
}

/*
 * QTabWidget::TabPosition tabPosition ( Qt::DockWidgetArea area ) const
 */
HB_FUNC( QT_QMAINWINDOW_TABPOSITION )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retni( ( QTabWidget::TabPosition ) ( p )->tabPosition( ( Qt::DockWidgetArea ) hb_parni( 2 ) ) );
   }
}

/*
 * QTabWidget::TabShape tabShape () const
 */
HB_FUNC( QT_QMAINWINDOW_TABSHAPE )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retni( ( QTabWidget::TabShape ) ( p )->tabShape() );
   }
}

/*
 * QList<QDockWidget *> tabifiedDockWidgets ( QDockWidget * dockwidget ) const
 */
HB_FUNC( QT_QMAINWINDOW_TABIFIEDDOCKWIDGETS )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QDockWidget *>( ( p )->tabifiedDockWidgets( hbqt_par_QDockWidget( 2 ) ) ), true ) );
   }
}

/*
 * void tabifyDockWidget ( QDockWidget * first, QDockWidget * second )
 */
HB_FUNC( QT_QMAINWINDOW_TABIFYDOCKWIDGET )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->tabifyDockWidget( hbqt_par_QDockWidget( 2 ), hbqt_par_QDockWidget( 3 ) );
   }
}

/*
 * Qt::ToolBarArea toolBarArea ( QToolBar * toolbar ) const
 */
HB_FUNC( QT_QMAINWINDOW_TOOLBARAREA )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retni( ( Qt::ToolBarArea ) ( p )->toolBarArea( hbqt_par_QToolBar( 2 ) ) );
   }
}

/*
 * bool toolBarBreak ( QToolBar * toolbar ) const
 */
HB_FUNC( QT_QMAINWINDOW_TOOLBARBREAK )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retl( ( p )->toolBarBreak( hbqt_par_QToolBar( 2 ) ) );
   }
}

/*
 * Qt::ToolButtonStyle toolButtonStyle () const
 */
HB_FUNC( QT_QMAINWINDOW_TOOLBUTTONSTYLE )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retni( ( Qt::ToolButtonStyle ) ( p )->toolButtonStyle() );
   }
}

/*
 * bool unifiedTitleAndToolBarOnMac () const
 */
HB_FUNC( QT_QMAINWINDOW_UNIFIEDTITLEANDTOOLBARONMAC )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      hb_retl( ( p )->unifiedTitleAndToolBarOnMac() );
   }
}

/*
 * void setAnimated ( bool enabled )
 */
HB_FUNC( QT_QMAINWINDOW_SETANIMATED )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->setAnimated( hb_parl( 2 ) );
   }
}

/*
 * void setDockNestingEnabled ( bool enabled )
 */
HB_FUNC( QT_QMAINWINDOW_SETDOCKNESTINGENABLED )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
   {
      ( p )->setDockNestingEnabled( hb_parl( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
