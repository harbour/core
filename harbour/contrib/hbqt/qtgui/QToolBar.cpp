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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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

#include "hbapi.h"
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtGui/QToolBar>
#include <QtGui/QIcon>


/* QToolBar ( const QString & title, QWidget * parent = 0 )
 * QToolBar ( QWidget * parent = 0 )
 * ~QToolBar ()
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QToolBar > pq;
} QGC_POINTER_QToolBar;

QT_G_FUNC( release_QToolBar )
{
   QGC_POINTER_QToolBar * p = ( QGC_POINTER_QToolBar * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QToolBar                     p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QToolBar                    ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QToolBar * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QToolBar * ) p->ph )->~QToolBar();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QToolBar * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QToolBar                    Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO release_QToolBar                    Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_QToolBar                    Object Already deleted!" ) );
   }
}

void * hbqt_gcAllocate_QToolBar( void * pObj )
{
   QGC_POINTER_QToolBar * p = ( QGC_POINTER_QToolBar * ) hb_gcAllocate( sizeof( QGC_POINTER_QToolBar ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = release_QToolBar;
   new( & p->pq ) QPointer< QToolBar >( ( QToolBar * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QToolBar                    %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QTOOLBAR )
{
   void * pObj = NULL;

   if( hb_param( 1, HB_IT_STRING ) )
      pObj = ( QToolBar* ) new QToolBar( hbqt_par_QString( 1 ), hbqt_par_QWidget( 2 ) ) ;
   else
      pObj = ( QToolBar* ) new QToolBar( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QToolBar( pObj ) );
}
/*
 * QAction * actionAt ( const QPoint & p ) const
 */
HB_FUNC( QT_QTOOLBAR_ACTIONAT )
{
   hb_retptr( ( QAction* ) hbqt_par_QToolBar( 1 )->actionAt( *hbqt_par_QPoint( 2 ) ) );
}

/*
 * QAction * actionAt ( int x, int y ) const
 */
HB_FUNC( QT_QTOOLBAR_ACTIONAT_1 )
{
   hb_retptr( ( QAction* ) hbqt_par_QToolBar( 1 )->actionAt( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * void addAction ( QAction * action )
 */
HB_FUNC( QT_QTOOLBAR_ADDACTION )
{
   hbqt_par_QToolBar( 1 )->addAction( hbqt_par_QAction( 2 ) );
}

/*
 * QAction * addAction ( const QString & text )
 */
HB_FUNC( QT_QTOOLBAR_ADDACTION_1 )
{
   hb_retptr( ( QAction* ) hbqt_par_QToolBar( 1 )->addAction( hbqt_par_QString( 2 ) ) );
}

/*
 * QAction * addAction ( const QIcon & icon, const QString & text )
 */
HB_FUNC( QT_QTOOLBAR_ADDACTION_2 )
{
   hb_retptr( ( QAction* ) hbqt_par_QToolBar( 1 )->addAction( QIcon( hbqt_par_QString( 2 ) ), hbqt_par_QString( 3 ) ) );
}

/*
 * QAction * addAction ( const QString & text, const QObject * receiver, const char * member )
 */
HB_FUNC( QT_QTOOLBAR_ADDACTION_3 )
{
   hb_retptr( ( QAction* ) hbqt_par_QToolBar( 1 )->addAction( hbqt_par_QString( 2 ), hbqt_par_QObject( 3 ), hbqt_par_char( 4 ) ) );
}

/*
 * QAction * addAction ( const QIcon & icon, const QString & text, const QObject * receiver, const char * member )
 */
HB_FUNC( QT_QTOOLBAR_ADDACTION_4 )
{
   hb_retptr( ( QAction* ) hbqt_par_QToolBar( 1 )->addAction( QIcon( hbqt_par_QString( 2 ) ), hbqt_par_QString( 3 ), hbqt_par_QObject( 4 ), hbqt_par_char( 5 ) ) );
}

/*
 * QAction * addSeparator ()
 */
HB_FUNC( QT_QTOOLBAR_ADDSEPARATOR )
{
   hb_retptr( ( QAction* ) hbqt_par_QToolBar( 1 )->addSeparator() );
}

/*
 * QAction * addWidget ( QWidget * widget )
 */
HB_FUNC( QT_QTOOLBAR_ADDWIDGET )
{
   hb_retptr( ( QAction* ) hbqt_par_QToolBar( 1 )->addWidget( hbqt_par_QWidget( 2 ) ) );
}

/*
 * Qt::ToolBarAreas allowedAreas () const
 */
HB_FUNC( QT_QTOOLBAR_ALLOWEDAREAS )
{
   hb_retni( ( Qt::ToolBarAreas ) hbqt_par_QToolBar( 1 )->allowedAreas() );
}

/*
 * void clear ()
 */
HB_FUNC( QT_QTOOLBAR_CLEAR )
{
   hbqt_par_QToolBar( 1 )->clear();
}

/*
 * QSize iconSize () const
 */
HB_FUNC( QT_QTOOLBAR_ICONSIZE )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QToolBar( 1 )->iconSize() ) ) );
}

/*
 * QAction * insertSeparator ( QAction * before )
 */
HB_FUNC( QT_QTOOLBAR_INSERTSEPARATOR )
{
   hb_retptr( ( QAction* ) hbqt_par_QToolBar( 1 )->insertSeparator( hbqt_par_QAction( 2 ) ) );
}

/*
 * QAction * insertWidget ( QAction * before, QWidget * widget )
 */
HB_FUNC( QT_QTOOLBAR_INSERTWIDGET )
{
   hb_retptr( ( QAction* ) hbqt_par_QToolBar( 1 )->insertWidget( hbqt_par_QAction( 2 ), hbqt_par_QWidget( 3 ) ) );
}

/*
 * bool isAreaAllowed ( Qt::ToolBarArea area ) const
 */
HB_FUNC( QT_QTOOLBAR_ISAREAALLOWED )
{
   hb_retl( hbqt_par_QToolBar( 1 )->isAreaAllowed( ( Qt::ToolBarArea ) hb_parni( 2 ) ) );
}

/*
 * bool isFloatable () const
 */
HB_FUNC( QT_QTOOLBAR_ISFLOATABLE )
{
   hb_retl( hbqt_par_QToolBar( 1 )->isFloatable() );
}

/*
 * bool isFloating () const
 */
HB_FUNC( QT_QTOOLBAR_ISFLOATING )
{
   hb_retl( hbqt_par_QToolBar( 1 )->isFloating() );
}

/*
 * bool isMovable () const
 */
HB_FUNC( QT_QTOOLBAR_ISMOVABLE )
{
   hb_retl( hbqt_par_QToolBar( 1 )->isMovable() );
}

/*
 * Qt::Orientation orientation () const
 */
HB_FUNC( QT_QTOOLBAR_ORIENTATION )
{
   hb_retni( ( Qt::Orientation ) hbqt_par_QToolBar( 1 )->orientation() );
}

/*
 * void setAllowedAreas ( Qt::ToolBarAreas areas )
 */
HB_FUNC( QT_QTOOLBAR_SETALLOWEDAREAS )
{
   hbqt_par_QToolBar( 1 )->setAllowedAreas( ( Qt::ToolBarAreas ) hb_parni( 2 ) );
}

/*
 * void setFloatable ( bool floatable )
 */
HB_FUNC( QT_QTOOLBAR_SETFLOATABLE )
{
   hbqt_par_QToolBar( 1 )->setFloatable( hb_parl( 2 ) );
}

/*
 * void setMovable ( bool movable )
 */
HB_FUNC( QT_QTOOLBAR_SETMOVABLE )
{
   hbqt_par_QToolBar( 1 )->setMovable( hb_parl( 2 ) );
}

/*
 * void setOrientation ( Qt::Orientation orientation )
 */
HB_FUNC( QT_QTOOLBAR_SETORIENTATION )
{
   hbqt_par_QToolBar( 1 )->setOrientation( ( Qt::Orientation ) hb_parni( 2 ) );
}

/*
 * QAction * toggleViewAction () const
 */
HB_FUNC( QT_QTOOLBAR_TOGGLEVIEWACTION )
{
   hb_retptr( ( QAction* ) hbqt_par_QToolBar( 1 )->toggleViewAction() );
}

/*
 * Qt::ToolButtonStyle toolButtonStyle () const
 */
HB_FUNC( QT_QTOOLBAR_TOOLBUTTONSTYLE )
{
   hb_retni( ( Qt::ToolButtonStyle ) hbqt_par_QToolBar( 1 )->toolButtonStyle() );
}

/*
 * QWidget * widgetForAction ( QAction * action ) const
 */
HB_FUNC( QT_QTOOLBAR_WIDGETFORACTION )
{
   hb_retptr( ( QWidget* ) hbqt_par_QToolBar( 1 )->widgetForAction( hbqt_par_QAction( 2 ) ) );
}

/*
 * void setIconSize ( const QSize & iconSize )
 */
HB_FUNC( QT_QTOOLBAR_SETICONSIZE )
{
   hbqt_par_QToolBar( 1 )->setIconSize( *hbqt_par_QSize( 2 ) );
}

/*
 * void setToolButtonStyle ( Qt::ToolButtonStyle toolButtonStyle )
 */
HB_FUNC( QT_QTOOLBAR_SETTOOLBUTTONSTYLE )
{
   hbqt_par_QToolBar( 1 )->setToolButtonStyle( ( Qt::ToolButtonStyle ) hb_parni( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
