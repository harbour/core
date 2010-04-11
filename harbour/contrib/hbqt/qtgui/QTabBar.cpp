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

#include "../hbqt.h"

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
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   QPointer< QTabBar > pq;
} QGC_POINTER_QTabBar;

QT_G_FUNC( hbqt_gcRelease_QTabBar )
{
   QGC_POINTER_QTabBar * p = ( QGC_POINTER_QTabBar * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QTabBar   /.\\   pq=%p", p->ph, (void *)(p->pq) ) );
            delete ( ( QTabBar * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QTabBar   \\./   pq=%p", p->ph, (void *)(p->pq) ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QTabBar          pq=%p", p->ph, (void *)(p->pq) ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTabBar    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTabBar    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTabBar( void * pObj, bool bNew )
{
   QGC_POINTER_QTabBar * p = ( QGC_POINTER_QTabBar * ) hb_gcAllocate( sizeof( QGC_POINTER_QTabBar ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTabBar;

   if( bNew )
   {
      new( & p->pq ) QPointer< QTabBar >( ( QTabBar * ) pObj );
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
   void * pObj = NULL;

   pObj = ( QTabBar* ) new QTabBar( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTabBar( pObj, true ) );
}

/*
 * int addTab ( const QString & text )
 */
HB_FUNC( QT_QTABBAR_ADDTAB )
{
   hb_retni( hbqt_par_QTabBar( 1 )->addTab( QTabBar::tr( hb_parc( 2 ) ) ) );
}

/*
 * int count () const
 */
HB_FUNC( QT_QTABBAR_COUNT )
{
   hb_retni( hbqt_par_QTabBar( 1 )->count() );
}

/*
 * int currentIndex () const
 */
HB_FUNC( QT_QTABBAR_CURRENTINDEX )
{
   hb_retni( hbqt_par_QTabBar( 1 )->currentIndex() );
}

/*
 * bool documentMode () const
 */
HB_FUNC( QT_QTABBAR_DOCUMENTMODE )
{
   hb_retl( hbqt_par_QTabBar( 1 )->documentMode() );
}

/*
 * bool drawBase () const
 */
HB_FUNC( QT_QTABBAR_DRAWBASE )
{
   hb_retl( hbqt_par_QTabBar( 1 )->drawBase() );
}

/*
 * Qt::TextElideMode elideMode () const
 */
HB_FUNC( QT_QTABBAR_ELIDEMODE )
{
   hb_retni( ( Qt::TextElideMode ) hbqt_par_QTabBar( 1 )->elideMode() );
}

/*
 * bool expanding () const
 */
HB_FUNC( QT_QTABBAR_EXPANDING )
{
   hb_retl( hbqt_par_QTabBar( 1 )->expanding() );
}

/*
 * QSize iconSize () const
 */
HB_FUNC( QT_QTABBAR_ICONSIZE )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QTabBar( 1 )->iconSize() ), true ) );
}

/*
 * int insertTab ( int index, const QString & text )
 */
HB_FUNC( QT_QTABBAR_INSERTTAB )
{
   hb_retni( hbqt_par_QTabBar( 1 )->insertTab( hb_parni( 2 ), QTabBar::tr( hb_parc( 3 ) ) ) );
}

/*
 * bool isMovable () const
 */
HB_FUNC( QT_QTABBAR_ISMOVABLE )
{
   hb_retl( hbqt_par_QTabBar( 1 )->isMovable() );
}

/*
 * bool isTabEnabled ( int index ) const
 */
HB_FUNC( QT_QTABBAR_ISTABENABLED )
{
   hb_retl( hbqt_par_QTabBar( 1 )->isTabEnabled( hb_parni( 2 ) ) );
}

/*
 * void moveTab ( int from, int to )
 */
HB_FUNC( QT_QTABBAR_MOVETAB )
{
   hbqt_par_QTabBar( 1 )->moveTab( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void removeTab ( int index )
 */
HB_FUNC( QT_QTABBAR_REMOVETAB )
{
   hbqt_par_QTabBar( 1 )->removeTab( hb_parni( 2 ) );
}

/*
 * SelectionBehavior selectionBehaviorOnRemove () const
 */
HB_FUNC( QT_QTABBAR_SELECTIONBEHAVIORONREMOVE )
{
   hb_retni( ( QTabBar::SelectionBehavior ) hbqt_par_QTabBar( 1 )->selectionBehaviorOnRemove() );
}

/*
 * void setDocumentMode ( bool set )
 */
HB_FUNC( QT_QTABBAR_SETDOCUMENTMODE )
{
   hbqt_par_QTabBar( 1 )->setDocumentMode( hb_parl( 2 ) );
}

/*
 * void setDrawBase ( bool drawTheBase )
 */
HB_FUNC( QT_QTABBAR_SETDRAWBASE )
{
   hbqt_par_QTabBar( 1 )->setDrawBase( hb_parl( 2 ) );
}

/*
 * void setElideMode ( Qt::TextElideMode )
 */
HB_FUNC( QT_QTABBAR_SETELIDEMODE )
{
   hbqt_par_QTabBar( 1 )->setElideMode( ( Qt::TextElideMode ) hb_parni( 2 ) );
}

/*
 * void setExpanding ( bool enabled )
 */
HB_FUNC( QT_QTABBAR_SETEXPANDING )
{
   hbqt_par_QTabBar( 1 )->setExpanding( hb_parl( 2 ) );
}

/*
 * void setIconSize ( const QSize & size )
 */
HB_FUNC( QT_QTABBAR_SETICONSIZE )
{
   hbqt_par_QTabBar( 1 )->setIconSize( *hbqt_par_QSize( 2 ) );
}

/*
 * void setMovable ( bool movable )
 */
HB_FUNC( QT_QTABBAR_SETMOVABLE )
{
   hbqt_par_QTabBar( 1 )->setMovable( hb_parl( 2 ) );
}

/*
 * void setSelectionBehaviorOnRemove ( SelectionBehavior behavior )
 */
HB_FUNC( QT_QTABBAR_SETSELECTIONBEHAVIORONREMOVE )
{
   hbqt_par_QTabBar( 1 )->setSelectionBehaviorOnRemove( ( QTabBar::SelectionBehavior ) hb_parni( 2 ) );
}

/*
 * void setShape ( Shape shape )
 */
HB_FUNC( QT_QTABBAR_SETSHAPE )
{
   hbqt_par_QTabBar( 1 )->setShape( ( QTabBar::Shape ) hb_parni( 2 ) );
}

/*
 * void setTabButton ( int index, ButtonPosition position, QWidget * widget )
 */
HB_FUNC( QT_QTABBAR_SETTABBUTTON )
{
   hbqt_par_QTabBar( 1 )->setTabButton( hb_parni( 2 ), ( QTabBar::ButtonPosition ) hb_parni( 3 ), hbqt_par_QWidget( 4 ) );
}

/*
 * void setTabData ( int index, const QVariant & data )
 */
HB_FUNC( QT_QTABBAR_SETTABDATA )
{
   hbqt_par_QTabBar( 1 )->setTabData( hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
}

/*
 * void setTabEnabled ( int index, bool enabled )
 */
HB_FUNC( QT_QTABBAR_SETTABENABLED )
{
   hbqt_par_QTabBar( 1 )->setTabEnabled( hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setTabText ( int index, const QString & text )
 */
HB_FUNC( QT_QTABBAR_SETTABTEXT )
{
   hbqt_par_QTabBar( 1 )->setTabText( hb_parni( 2 ), QTabBar::tr( hb_parc( 3 ) ) );
}

/*
 * void setTabTextColor ( int index, const QColor & color )
 */
HB_FUNC( QT_QTABBAR_SETTABTEXTCOLOR )
{
   hbqt_par_QTabBar( 1 )->setTabTextColor( hb_parni( 2 ), *hbqt_par_QColor( 3 ) );
}

/*
 * void setTabToolTip ( int index, const QString & tip )
 */
HB_FUNC( QT_QTABBAR_SETTABTOOLTIP )
{
   hbqt_par_QTabBar( 1 )->setTabToolTip( hb_parni( 2 ), QTabBar::tr( hb_parc( 3 ) ) );
}

/*
 * void setTabWhatsThis ( int index, const QString & text )
 */
HB_FUNC( QT_QTABBAR_SETTABWHATSTHIS )
{
   hbqt_par_QTabBar( 1 )->setTabWhatsThis( hb_parni( 2 ), QTabBar::tr( hb_parc( 3 ) ) );
}

/*
 * void setTabsClosable ( bool closable )
 */
HB_FUNC( QT_QTABBAR_SETTABSCLOSABLE )
{
   hbqt_par_QTabBar( 1 )->setTabsClosable( hb_parl( 2 ) );
}

/*
 * void setUsesScrollButtons ( bool useButtons )
 */
HB_FUNC( QT_QTABBAR_SETUSESSCROLLBUTTONS )
{
   hbqt_par_QTabBar( 1 )->setUsesScrollButtons( hb_parl( 2 ) );
}

/*
 * Shape shape () const
 */
HB_FUNC( QT_QTABBAR_SHAPE )
{
   hb_retni( ( QTabBar::Shape ) hbqt_par_QTabBar( 1 )->shape() );
}

/*
 * int tabAt ( const QPoint & position ) const
 */
HB_FUNC( QT_QTABBAR_TABAT )
{
   hb_retni( hbqt_par_QTabBar( 1 )->tabAt( *hbqt_par_QPoint( 2 ) ) );
}

/*
 * QWidget * tabButton ( int index, ButtonPosition position ) const
 */
HB_FUNC( QT_QTABBAR_TABBUTTON )
{
   hb_retptrGC( hbqt_gcAllocate_QWidget( hbqt_par_QTabBar( 1 )->tabButton( hb_parni( 2 ), ( QTabBar::ButtonPosition ) hb_parni( 3 ) ), false ) );
}

/*
 * QVariant tabData ( int index ) const
 */
HB_FUNC( QT_QTABBAR_TABDATA )
{
   hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( hbqt_par_QTabBar( 1 )->tabData( hb_parni( 2 ) ) ), true ) );
}

/*
 * QIcon tabIcon ( int index ) const
 */
HB_FUNC( QT_QTABBAR_TABICON )
{
   hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( hbqt_par_QTabBar( 1 )->tabIcon( hb_parni( 2 ) ) ), true ) );
}

/*
 * QRect tabRect ( int index ) const
 */
HB_FUNC( QT_QTABBAR_TABRECT )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QTabBar( 1 )->tabRect( hb_parni( 2 ) ) ), true ) );
}

/*
 * QString tabText ( int index ) const
 */
HB_FUNC( QT_QTABBAR_TABTEXT )
{
   hb_retc( hbqt_par_QTabBar( 1 )->tabText( hb_parni( 2 ) ).toAscii().data() );
}

/*
 * QColor tabTextColor ( int index ) const
 */
HB_FUNC( QT_QTABBAR_TABTEXTCOLOR )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( hbqt_par_QTabBar( 1 )->tabTextColor( hb_parni( 2 ) ) ), true ) );
}

/*
 * QString tabToolTip ( int index ) const
 */
HB_FUNC( QT_QTABBAR_TABTOOLTIP )
{
   hb_retc( hbqt_par_QTabBar( 1 )->tabToolTip( hb_parni( 2 ) ).toAscii().data() );
}

/*
 * QString tabWhatsThis ( int index ) const
 */
HB_FUNC( QT_QTABBAR_TABWHATSTHIS )
{
   hb_retc( hbqt_par_QTabBar( 1 )->tabWhatsThis( hb_parni( 2 ) ).toAscii().data() );
}

/*
 * bool tabsClosable () const
 */
HB_FUNC( QT_QTABBAR_TABSCLOSABLE )
{
   hb_retl( hbqt_par_QTabBar( 1 )->tabsClosable() );
}

/*
 * bool usesScrollButtons () const
 */
HB_FUNC( QT_QTABBAR_USESSCROLLBUTTONS )
{
   hb_retl( hbqt_par_QTabBar( 1 )->usesScrollButtons() );
}

/*
 * void setCurrentIndex ( int index )
 */
HB_FUNC( QT_QTABBAR_SETCURRENTINDEX )
{
   hbqt_par_QTabBar( 1 )->setCurrentIndex( hb_parni( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
