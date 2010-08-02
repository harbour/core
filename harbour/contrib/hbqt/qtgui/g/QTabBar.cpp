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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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
#include "hbqtgui_garbage.h"
#include "hbqtgui.h"
#include "hbqtcore_garbage.h"
#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum ButtonPosition { LeftSide, RightSide }
 *  enum SelectionBehavior { SelectLeftTab, SelectRightTab, SelectPreviousTab }
 *  enum Shape { RoundedNorth, RoundedSouth, RoundedWest, RoundedEast, ..., TriangularEast }
 */

#include <QtCore/QPointer>

#include <QtGui/QTabBar>
#include <QtCore/QVariant>
#include <QtGui/QIcon>

/*
 * QTabBar ( QWidget * parent = 0 )
 * ~QTabBar ()
 */

typedef struct
{
   QPointer< QTabBar > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QTabBar;

QT_G_FUNC( hbqt_gcRelease_QTabBar )
{
   QTabBar  * ph = NULL ;
   QGC_POINTER_QTabBar * p = ( QGC_POINTER_QTabBar * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QTabBar   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QTabBar   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QTabBar          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTabBar    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTabBar    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTabBar( void * pObj, bool bNew )
{
   QGC_POINTER_QTabBar * p = ( QGC_POINTER_QTabBar * ) hb_gcAllocate( sizeof( QGC_POINTER_QTabBar ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTabBar >( ( QTabBar * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTabBar;
   p->type = HBQT_TYPE_QTabBar;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTabBar  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTabBar", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTABBAR )
{
   QTabBar * pObj = NULL;

   pObj =  new QTabBar( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTabBar( ( void * ) pObj, true ) );
}

/*
 * int addTab ( const QString & text )
 */
HB_FUNC( QT_QTABBAR_ADDTAB )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retni( ( p )->addTab( QTabBar::tr( hb_parc( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_ADDTAB FP=hb_retni( ( p )->addTab( QTabBar::tr( hb_parc( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * int count () const
 */
HB_FUNC( QT_QTABBAR_COUNT )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retni( ( p )->count() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_COUNT FP=hb_retni( ( p )->count() ); p is NULL" ) );
   }
}

/*
 * int currentIndex () const
 */
HB_FUNC( QT_QTABBAR_CURRENTINDEX )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retni( ( p )->currentIndex() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_CURRENTINDEX FP=hb_retni( ( p )->currentIndex() ); p is NULL" ) );
   }
}

/*
 * bool documentMode () const
 */
HB_FUNC( QT_QTABBAR_DOCUMENTMODE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retl( ( p )->documentMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_DOCUMENTMODE FP=hb_retl( ( p )->documentMode() ); p is NULL" ) );
   }
}

/*
 * bool drawBase () const
 */
HB_FUNC( QT_QTABBAR_DRAWBASE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retl( ( p )->drawBase() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_DRAWBASE FP=hb_retl( ( p )->drawBase() ); p is NULL" ) );
   }
}

/*
 * Qt::TextElideMode elideMode () const
 */
HB_FUNC( QT_QTABBAR_ELIDEMODE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retni( ( Qt::TextElideMode ) ( p )->elideMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_ELIDEMODE FP=hb_retni( ( Qt::TextElideMode ) ( p )->elideMode() ); p is NULL" ) );
   }
}

/*
 * bool expanding () const
 */
HB_FUNC( QT_QTABBAR_EXPANDING )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retl( ( p )->expanding() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_EXPANDING FP=hb_retl( ( p )->expanding() ); p is NULL" ) );
   }
}

/*
 * QSize iconSize () const
 */
HB_FUNC( QT_QTABBAR_ICONSIZE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->iconSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_ICONSIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->iconSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * int insertTab ( int index, const QString & text )
 */
HB_FUNC( QT_QTABBAR_INSERTTAB )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retni( ( p )->insertTab( hb_parni( 2 ), QTabBar::tr( hb_parc( 3 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_INSERTTAB FP=hb_retni( ( p )->insertTab( hb_parni( 2 ), QTabBar::tr( hb_parc( 3 ) ) ) ); p is NULL" ) );
   }
}

/*
 * bool isMovable () const
 */
HB_FUNC( QT_QTABBAR_ISMOVABLE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retl( ( p )->isMovable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_ISMOVABLE FP=hb_retl( ( p )->isMovable() ); p is NULL" ) );
   }
}

/*
 * bool isTabEnabled ( int index ) const
 */
HB_FUNC( QT_QTABBAR_ISTABENABLED )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retl( ( p )->isTabEnabled( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_ISTABENABLED FP=hb_retl( ( p )->isTabEnabled( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void moveTab ( int from, int to )
 */
HB_FUNC( QT_QTABBAR_MOVETAB )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->moveTab( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_MOVETAB FP=( p )->moveTab( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void removeTab ( int index )
 */
HB_FUNC( QT_QTABBAR_REMOVETAB )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->removeTab( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_REMOVETAB FP=( p )->removeTab( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * SelectionBehavior selectionBehaviorOnRemove () const
 */
HB_FUNC( QT_QTABBAR_SELECTIONBEHAVIORONREMOVE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retni( ( QTabBar::SelectionBehavior ) ( p )->selectionBehaviorOnRemove() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_SELECTIONBEHAVIORONREMOVE FP=hb_retni( ( QTabBar::SelectionBehavior ) ( p )->selectionBehaviorOnRemove() ); p is NULL" ) );
   }
}

/*
 * void setDocumentMode ( bool set )
 */
HB_FUNC( QT_QTABBAR_SETDOCUMENTMODE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setDocumentMode( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_SETDOCUMENTMODE FP=( p )->setDocumentMode( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDrawBase ( bool drawTheBase )
 */
HB_FUNC( QT_QTABBAR_SETDRAWBASE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setDrawBase( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_SETDRAWBASE FP=( p )->setDrawBase( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setElideMode ( Qt::TextElideMode )
 */
HB_FUNC( QT_QTABBAR_SETELIDEMODE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setElideMode( ( Qt::TextElideMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_SETELIDEMODE FP=( p )->setElideMode( ( Qt::TextElideMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setExpanding ( bool enabled )
 */
HB_FUNC( QT_QTABBAR_SETEXPANDING )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setExpanding( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_SETEXPANDING FP=( p )->setExpanding( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setIconSize ( const QSize & size )
 */
HB_FUNC( QT_QTABBAR_SETICONSIZE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setIconSize( *hbqt_par_QSize( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_SETICONSIZE FP=( p )->setIconSize( *hbqt_par_QSize( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMovable ( bool movable )
 */
HB_FUNC( QT_QTABBAR_SETMOVABLE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setMovable( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_SETMOVABLE FP=( p )->setMovable( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSelectionBehaviorOnRemove ( SelectionBehavior behavior )
 */
HB_FUNC( QT_QTABBAR_SETSELECTIONBEHAVIORONREMOVE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setSelectionBehaviorOnRemove( ( QTabBar::SelectionBehavior ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_SETSELECTIONBEHAVIORONREMOVE FP=( p )->setSelectionBehaviorOnRemove( ( QTabBar::SelectionBehavior ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setShape ( Shape shape )
 */
HB_FUNC( QT_QTABBAR_SETSHAPE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setShape( ( QTabBar::Shape ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_SETSHAPE FP=( p )->setShape( ( QTabBar::Shape ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTabButton ( int index, ButtonPosition position, QWidget * widget )
 */
HB_FUNC( QT_QTABBAR_SETTABBUTTON )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setTabButton( hb_parni( 2 ), ( QTabBar::ButtonPosition ) hb_parni( 3 ), hbqt_par_QWidget( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_SETTABBUTTON FP=( p )->setTabButton( hb_parni( 2 ), ( QTabBar::ButtonPosition ) hb_parni( 3 ), hbqt_par_QWidget( 4 ) ); p is NULL" ) );
   }
}

/*
 * void setTabData ( int index, const QVariant & data )
 */
HB_FUNC( QT_QTABBAR_SETTABDATA )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setTabData( hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_SETTABDATA FP=( p )->setTabData( hb_parni( 2 ), *hbqt_par_QVariant( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setTabEnabled ( int index, bool enabled )
 */
HB_FUNC( QT_QTABBAR_SETTABENABLED )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setTabEnabled( hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_SETTABENABLED FP=( p )->setTabEnabled( hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setTabText ( int index, const QString & text )
 */
HB_FUNC( QT_QTABBAR_SETTABTEXT )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setTabText( hb_parni( 2 ), QTabBar::tr( hb_parc( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_SETTABTEXT FP=( p )->setTabText( hb_parni( 2 ), QTabBar::tr( hb_parc( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * void setTabTextColor ( int index, const QColor & color )
 */
HB_FUNC( QT_QTABBAR_SETTABTEXTCOLOR )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setTabTextColor( hb_parni( 2 ), *hbqt_par_QColor( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_SETTABTEXTCOLOR FP=( p )->setTabTextColor( hb_parni( 2 ), *hbqt_par_QColor( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setTabToolTip ( int index, const QString & tip )
 */
HB_FUNC( QT_QTABBAR_SETTABTOOLTIP )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setTabToolTip( hb_parni( 2 ), QTabBar::tr( hb_parc( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_SETTABTOOLTIP FP=( p )->setTabToolTip( hb_parni( 2 ), QTabBar::tr( hb_parc( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * void setTabWhatsThis ( int index, const QString & text )
 */
HB_FUNC( QT_QTABBAR_SETTABWHATSTHIS )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setTabWhatsThis( hb_parni( 2 ), QTabBar::tr( hb_parc( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_SETTABWHATSTHIS FP=( p )->setTabWhatsThis( hb_parni( 2 ), QTabBar::tr( hb_parc( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * void setTabsClosable ( bool closable )
 */
HB_FUNC( QT_QTABBAR_SETTABSCLOSABLE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setTabsClosable( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_SETTABSCLOSABLE FP=( p )->setTabsClosable( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setUsesScrollButtons ( bool useButtons )
 */
HB_FUNC( QT_QTABBAR_SETUSESSCROLLBUTTONS )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setUsesScrollButtons( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_SETUSESSCROLLBUTTONS FP=( p )->setUsesScrollButtons( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * Shape shape () const
 */
HB_FUNC( QT_QTABBAR_SHAPE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retni( ( QTabBar::Shape ) ( p )->shape() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_SHAPE FP=hb_retni( ( QTabBar::Shape ) ( p )->shape() ); p is NULL" ) );
   }
}

/*
 * int tabAt ( const QPoint & position ) const
 */
HB_FUNC( QT_QTABBAR_TABAT )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retni( ( p )->tabAt( *hbqt_par_QPoint( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_TABAT FP=hb_retni( ( p )->tabAt( *hbqt_par_QPoint( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QWidget * tabButton ( int index, ButtonPosition position ) const
 */
HB_FUNC( QT_QTABBAR_TABBUTTON )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->tabButton( hb_parni( 2 ), ( QTabBar::ButtonPosition ) hb_parni( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_TABBUTTON FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->tabButton( hb_parni( 2 ), ( QTabBar::ButtonPosition ) hb_parni( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QVariant tabData ( int index ) const
 */
HB_FUNC( QT_QTABBAR_TABDATA )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->tabData( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_TABDATA FP=hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->tabData( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QIcon tabIcon ( int index ) const
 */
HB_FUNC( QT_QTABBAR_TABICON )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->tabIcon( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_TABICON FP=hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->tabIcon( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRect tabRect ( int index ) const
 */
HB_FUNC( QT_QTABBAR_TABRECT )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->tabRect( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_TABRECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->tabRect( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QString tabText ( int index ) const
 */
HB_FUNC( QT_QTABBAR_TABTEXT )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retc( ( p )->tabText( hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_TABTEXT FP=hb_retc( ( p )->tabText( hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QColor tabTextColor ( int index ) const
 */
HB_FUNC( QT_QTABBAR_TABTEXTCOLOR )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->tabTextColor( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_TABTEXTCOLOR FP=hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->tabTextColor( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QString tabToolTip ( int index ) const
 */
HB_FUNC( QT_QTABBAR_TABTOOLTIP )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retc( ( p )->tabToolTip( hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_TABTOOLTIP FP=hb_retc( ( p )->tabToolTip( hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString tabWhatsThis ( int index ) const
 */
HB_FUNC( QT_QTABBAR_TABWHATSTHIS )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retc( ( p )->tabWhatsThis( hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_TABWHATSTHIS FP=hb_retc( ( p )->tabWhatsThis( hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool tabsClosable () const
 */
HB_FUNC( QT_QTABBAR_TABSCLOSABLE )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retl( ( p )->tabsClosable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_TABSCLOSABLE FP=hb_retl( ( p )->tabsClosable() ); p is NULL" ) );
   }
}

/*
 * bool usesScrollButtons () const
 */
HB_FUNC( QT_QTABBAR_USESSCROLLBUTTONS )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      hb_retl( ( p )->usesScrollButtons() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_USESSCROLLBUTTONS FP=hb_retl( ( p )->usesScrollButtons() ); p is NULL" ) );
   }
}

/*
 * void setCurrentIndex ( int index )
 */
HB_FUNC( QT_QTABBAR_SETCURRENTINDEX )
{
   QTabBar * p = hbqt_par_QTabBar( 1 );
   if( p )
      ( p )->setCurrentIndex( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABBAR_SETCURRENTINDEX FP=( p )->setCurrentIndex( hb_parni( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
