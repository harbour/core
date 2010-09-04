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

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 28/30 [ 93.33% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  }
 *  }
 */

#include <QtCore/QPointer>

#include <QtGui/QToolBar>
#include <QtGui/QIcon>


/* QToolBar ( const QString & title, QWidget * parent = 0 )
 * QToolBar ( QWidget * parent = 0 )
 * ~QToolBar ()
 */

typedef struct
{
   QPointer< QToolBar > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QToolBar;

HBQT_GC_FUNC( hbqt_gcRelease_QToolBar )
{
   QToolBar  * ph = NULL ;
   HBQT_GC_T_QToolBar * p = ( HBQT_GC_T_QToolBar * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QToolBar   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QToolBar   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QToolBar          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QToolBar    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QToolBar    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QToolBar( void * pObj, bool bNew )
{
   HBQT_GC_T_QToolBar * p = ( HBQT_GC_T_QToolBar * ) hb_gcAllocate( sizeof( HBQT_GC_T_QToolBar ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QToolBar >( ( QToolBar * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QToolBar;
   p->type = HBQT_TYPE_QToolBar;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QToolBar  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QToolBar", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTOOLBAR )
{
   QToolBar * pObj = NULL;

   if( hb_param( 1, HB_IT_STRING ) )
      pObj =  new QToolBar( hbqt_par_QString( 1 ), hbqt_par_QWidget( 2 ) ) ;
   else
      pObj =  new QToolBar( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QToolBar( ( void * ) pObj, true ) );
}

/*
 * QAction * actionAt ( const QPoint & p ) const
 */
HB_FUNC( QT_QTOOLBAR_ACTIONAT )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionAt( *hbqt_par_QPoint( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_ACTIONAT FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionAt( *hbqt_par_QPoint( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * actionAt ( int x, int y ) const
 */
HB_FUNC( QT_QTOOLBAR_ACTIONAT_1 )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_ACTIONAT_1 FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * void addAction ( QAction * action )
 */
HB_FUNC( QT_QTOOLBAR_ADDACTION )
{
   HBQT_GC_T_QToolBar * q = ( HBQT_GC_T_QToolBar * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 2 );

   HB_TRACE( HB_TR_DEBUG, ( "Entering function QT_QTOOLBAR_ADDACTION()" ) );
   if( p && p->ph && q && q->ph )
   {
      HB_TRACE( HB_TR_DEBUG, ( "QT_QTOOLBAR_ADDACTION() Qt oject: %p is attached to: %p", ( void * ) p->ph, ( void * ) q->ph ) );
      p->bNew = HB_FALSE;
      ( q->ph )->addAction( ( QAction * ) p->ph );
   }
}

/*
 * QAction * addAction ( const QString & text )
 */
HB_FUNC( QT_QTOOLBAR_ADDACTION_1 )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( QToolBar::tr( hb_parc( 2 ) ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_ADDACTION_1 FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( QToolBar::tr( hb_parc( 2 ) ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * addAction ( const QIcon & icon, const QString & text )
 */
HB_FUNC( QT_QTOOLBAR_ADDACTION_2 )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ), QToolBar::tr( hb_parc( 3 ) ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_ADDACTION_2 FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ), QToolBar::tr( hb_parc( 3 ) ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * addAction ( const QString & text, const QObject * receiver, const char * member )
 */
HB_FUNC( QT_QTOOLBAR_ADDACTION_3 )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( QToolBar::tr( hb_parc( 2 ) ), hbqt_par_QObject( 3 ), hbqt_par_char( 4 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_ADDACTION_3 FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( QToolBar::tr( hb_parc( 2 ) ), hbqt_par_QObject( 3 ), hbqt_par_char( 4 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * addAction ( const QIcon & icon, const QString & text, const QObject * receiver, const char * member )
 */
HB_FUNC( QT_QTOOLBAR_ADDACTION_4 )
{
   HBQT_GC_T_QToolBar * q = ( HBQT_GC_T_QToolBar * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 4 );

   HB_TRACE( HB_TR_DEBUG, ("QTOOLBAR_ADDACTION_4" ) );
   if ( p && p->ph && q && q->ph )
   {
      HB_TRACE( HB_TR_DEBUG, ( "QT_QTOOLBAR_ADDITEM_4: %p is attached to: %p", (void *) p->ph, (void *) q->ph ) ) ;
      p->bNew = HB_FALSE;
      if ( q && q->ph )
         hb_retptrGC( hbqt_gcAllocate_QAction( ( q->ph )->addAction( QIcon( hbqt_par_QString( 2 ) ), QToolBar::tr( hb_parc( 3 ) ), (QObject *) ( p->ph), hbqt_par_char( 5 ) ), false ) );
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "F=QT_QTOOLBAR_ADDACTION_4 FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( QIcon( hbqt_par_QString( 2 ) ), QToolBar::tr( hb_parc( 3 ) ), hbqt_par_QObject( 4 ), hbqt_par_char( 5 ) ), false ) ); p is NULL" ) );
      }
   }
}

/*
 * QAction * addSeparator ()
 */
HB_FUNC( QT_QTOOLBAR_ADDSEPARATOR )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addSeparator(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_ADDSEPARATOR FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addSeparator(), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * addWidget ( QWidget * widget )
 */
HB_FUNC( QT_QTOOLBAR_ADDWIDGET )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addWidget( hbqt_par_QWidget( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_ADDWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addWidget( hbqt_par_QWidget( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * Qt::ToolBarAreas allowedAreas () const
 */
HB_FUNC( QT_QTOOLBAR_ALLOWEDAREAS )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retni( ( Qt::ToolBarAreas ) ( p )->allowedAreas() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_ALLOWEDAREAS FP=hb_retni( ( Qt::ToolBarAreas ) ( p )->allowedAreas() ); p is NULL" ) );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QTOOLBAR_CLEAR )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      ( p )->clear();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_CLEAR FP=( p )->clear(); p is NULL" ) );
   }
}

/*
 * QSize iconSize () const
 */
HB_FUNC( QT_QTOOLBAR_ICONSIZE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->iconSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_ICONSIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->iconSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * QAction * insertSeparator ( QAction * before )
 */
HB_FUNC( QT_QTOOLBAR_INSERTSEPARATOR )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->insertSeparator( hbqt_par_QAction( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_INSERTSEPARATOR FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->insertSeparator( hbqt_par_QAction( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * insertWidget ( QAction * before, QWidget * widget )
 */
HB_FUNC( QT_QTOOLBAR_INSERTWIDGET )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->insertWidget( hbqt_par_QAction( 2 ), hbqt_par_QWidget( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_INSERTWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->insertWidget( hbqt_par_QAction( 2 ), hbqt_par_QWidget( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * bool isAreaAllowed ( Qt::ToolBarArea area ) const
 */
HB_FUNC( QT_QTOOLBAR_ISAREAALLOWED )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retl( ( p )->isAreaAllowed( ( Qt::ToolBarArea ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_ISAREAALLOWED FP=hb_retl( ( p )->isAreaAllowed( ( Qt::ToolBarArea ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isFloatable () const
 */
HB_FUNC( QT_QTOOLBAR_ISFLOATABLE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retl( ( p )->isFloatable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_ISFLOATABLE FP=hb_retl( ( p )->isFloatable() ); p is NULL" ) );
   }
}

/*
 * bool isFloating () const
 */
HB_FUNC( QT_QTOOLBAR_ISFLOATING )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retl( ( p )->isFloating() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_ISFLOATING FP=hb_retl( ( p )->isFloating() ); p is NULL" ) );
   }
}

/*
 * bool isMovable () const
 */
HB_FUNC( QT_QTOOLBAR_ISMOVABLE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retl( ( p )->isMovable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_ISMOVABLE FP=hb_retl( ( p )->isMovable() ); p is NULL" ) );
   }
}

/*
 * Qt::Orientation orientation () const
 */
HB_FUNC( QT_QTOOLBAR_ORIENTATION )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retni( ( Qt::Orientation ) ( p )->orientation() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_ORIENTATION FP=hb_retni( ( Qt::Orientation ) ( p )->orientation() ); p is NULL" ) );
   }
}

/*
 * void setAllowedAreas ( Qt::ToolBarAreas areas )
 */
HB_FUNC( QT_QTOOLBAR_SETALLOWEDAREAS )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      ( p )->setAllowedAreas( ( Qt::ToolBarAreas ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_SETALLOWEDAREAS FP=( p )->setAllowedAreas( ( Qt::ToolBarAreas ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFloatable ( bool floatable )
 */
HB_FUNC( QT_QTOOLBAR_SETFLOATABLE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      ( p )->setFloatable( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_SETFLOATABLE FP=( p )->setFloatable( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMovable ( bool movable )
 */
HB_FUNC( QT_QTOOLBAR_SETMOVABLE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      ( p )->setMovable( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_SETMOVABLE FP=( p )->setMovable( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setOrientation ( Qt::Orientation orientation )
 */
HB_FUNC( QT_QTOOLBAR_SETORIENTATION )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      ( p )->setOrientation( ( Qt::Orientation ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_SETORIENTATION FP=( p )->setOrientation( ( Qt::Orientation ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * QAction * toggleViewAction () const
 */
HB_FUNC( QT_QTOOLBAR_TOGGLEVIEWACTION )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->toggleViewAction(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_TOGGLEVIEWACTION FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->toggleViewAction(), false ) ); p is NULL" ) );
   }
}

/*
 * Qt::ToolButtonStyle toolButtonStyle () const
 */
HB_FUNC( QT_QTOOLBAR_TOOLBUTTONSTYLE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retni( ( Qt::ToolButtonStyle ) ( p )->toolButtonStyle() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_TOOLBUTTONSTYLE FP=hb_retni( ( Qt::ToolButtonStyle ) ( p )->toolButtonStyle() ); p is NULL" ) );
   }
}

/*
 * QWidget * widgetForAction ( QAction * action ) const
 */
HB_FUNC( QT_QTOOLBAR_WIDGETFORACTION )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widgetForAction( hbqt_par_QAction( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_WIDGETFORACTION FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widgetForAction( hbqt_par_QAction( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * void setIconSize ( const QSize & iconSize )
 */
HB_FUNC( QT_QTOOLBAR_SETICONSIZE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      ( p )->setIconSize( *hbqt_par_QSize( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_SETICONSIZE FP=( p )->setIconSize( *hbqt_par_QSize( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setToolButtonStyle ( Qt::ToolButtonStyle toolButtonStyle )
 */
HB_FUNC( QT_QTOOLBAR_SETTOOLBUTTONSTYLE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
      ( p )->setToolButtonStyle( ( Qt::ToolButtonStyle ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBAR_SETTOOLBUTTONSTYLE FP=( p )->setToolButtonStyle( ( Qt::ToolButtonStyle ) hb_parni( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
