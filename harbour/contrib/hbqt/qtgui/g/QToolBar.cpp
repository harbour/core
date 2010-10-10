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
 *  Constructed[ 28/28 [ 100.00% ] ]
 *
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
      pObj = new QToolBar( hbqt_par_QString( 1 ), hbqt_par_QWidget( 2 ) ) ;
   else
      pObj = new QToolBar( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QToolBar( ( void * ) pObj, true ) );
}

/*
 * QAction * actionAt ( const QPoint & p ) const
 */
HB_FUNC( QT_QTOOLBAR_ACTIONAT )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionAt( *hbqt_par_QPoint( 2 ) ), false ) );
   }
}

/*
 * QAction * actionAt ( int x, int y ) const
 */
HB_FUNC( QT_QTOOLBAR_ACTIONAT_1 )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->actionAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   }
}

/*
 * void addAction ( QAction * action )   [*D=1*]
 */
HB_FUNC( QT_QTOOLBAR_ADDACTION )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 2 );
      ( p )->addAction( hbqt_par_QAction( 2 ) );
   }
}

/*
 * QAction * addAction ( const QString & text )
 */
HB_FUNC( QT_QTOOLBAR_ADDACTION_1 )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( hb_parstr_utf8( 2, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/*
 * QAction * addAction ( const QIcon & icon, const QString & text )
 */
HB_FUNC( QT_QTOOLBAR_ADDACTION_2 )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( ( HB_ISCHAR( 2 ) ? QIcon( hbqt_par_QString( 2 ) ) : *hbqt_par_QIcon( 2 )), hb_parstr_utf8( 3, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/*
 * QAction * addAction ( const QString & text, const QObject * receiver, const char * member )
 */
HB_FUNC( QT_QTOOLBAR_ADDACTION_3 )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( hb_parstr_utf8( 2, &pText, NULL ), hbqt_par_QObject( 3 ), ( const char * ) hb_parc( 4 ) ), false ) );
      hb_strfree( pText );
   }
}

/*
 * QAction * addAction ( const QIcon & icon, const QString & text, const QObject * receiver, const char * member )   [*D=3*]
 */
HB_FUNC( QT_QTOOLBAR_ADDACTION_4 )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 4 );
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( ( HB_ISCHAR( 2 ) ? QIcon( hbqt_par_QString( 2 ) ) : *hbqt_par_QIcon( 2 )), hb_parstr_utf8( 3, &pText, NULL ), hbqt_par_QObject( 4 ), ( const char * ) hb_parc( 5 ) ), false ) );
      hb_strfree( pText );
   }
}

/*
 * QAction * addSeparator ()
 */
HB_FUNC( QT_QTOOLBAR_ADDSEPARATOR )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addSeparator(), false ) );
   }
}

/*
 * QAction * addWidget ( QWidget * widget )
 */
HB_FUNC( QT_QTOOLBAR_ADDWIDGET )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addWidget( hbqt_par_QWidget( 2 ) ), false ) );
   }
}

/*
 * Qt::ToolBarAreas allowedAreas () const
 */
HB_FUNC( QT_QTOOLBAR_ALLOWEDAREAS )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      hb_retni( ( Qt::ToolBarAreas ) ( p )->allowedAreas() );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QTOOLBAR_CLEAR )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      ( p )->clear();
   }
}

/*
 * QSize iconSize () const
 */
HB_FUNC( QT_QTOOLBAR_ICONSIZE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->iconSize() ), true ) );
   }
}

/*
 * QAction * insertSeparator ( QAction * before )
 */
HB_FUNC( QT_QTOOLBAR_INSERTSEPARATOR )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->insertSeparator( hbqt_par_QAction( 2 ) ), false ) );
   }
}

/*
 * QAction * insertWidget ( QAction * before, QWidget * widget )
 */
HB_FUNC( QT_QTOOLBAR_INSERTWIDGET )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->insertWidget( hbqt_par_QAction( 2 ), hbqt_par_QWidget( 3 ) ), false ) );
   }
}

/*
 * bool isAreaAllowed ( Qt::ToolBarArea area ) const
 */
HB_FUNC( QT_QTOOLBAR_ISAREAALLOWED )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      hb_retl( ( p )->isAreaAllowed( ( Qt::ToolBarArea ) hb_parni( 2 ) ) );
   }
}

/*
 * bool isFloatable () const
 */
HB_FUNC( QT_QTOOLBAR_ISFLOATABLE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      hb_retl( ( p )->isFloatable() );
   }
}

/*
 * bool isFloating () const
 */
HB_FUNC( QT_QTOOLBAR_ISFLOATING )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      hb_retl( ( p )->isFloating() );
   }
}

/*
 * bool isMovable () const
 */
HB_FUNC( QT_QTOOLBAR_ISMOVABLE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      hb_retl( ( p )->isMovable() );
   }
}

/*
 * Qt::Orientation orientation () const
 */
HB_FUNC( QT_QTOOLBAR_ORIENTATION )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      hb_retni( ( Qt::Orientation ) ( p )->orientation() );
   }
}

/*
 * void setAllowedAreas ( Qt::ToolBarAreas areas )
 */
HB_FUNC( QT_QTOOLBAR_SETALLOWEDAREAS )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      ( p )->setAllowedAreas( ( Qt::ToolBarAreas ) hb_parni( 2 ) );
   }
}

/*
 * void setFloatable ( bool floatable )
 */
HB_FUNC( QT_QTOOLBAR_SETFLOATABLE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      ( p )->setFloatable( hb_parl( 2 ) );
   }
}

/*
 * void setMovable ( bool movable )
 */
HB_FUNC( QT_QTOOLBAR_SETMOVABLE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      ( p )->setMovable( hb_parl( 2 ) );
   }
}

/*
 * void setOrientation ( Qt::Orientation orientation )
 */
HB_FUNC( QT_QTOOLBAR_SETORIENTATION )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      ( p )->setOrientation( ( Qt::Orientation ) hb_parni( 2 ) );
   }
}

/*
 * QAction * toggleViewAction () const
 */
HB_FUNC( QT_QTOOLBAR_TOGGLEVIEWACTION )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->toggleViewAction(), false ) );
   }
}

/*
 * Qt::ToolButtonStyle toolButtonStyle () const
 */
HB_FUNC( QT_QTOOLBAR_TOOLBUTTONSTYLE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      hb_retni( ( Qt::ToolButtonStyle ) ( p )->toolButtonStyle() );
   }
}

/*
 * QWidget * widgetForAction ( QAction * action ) const
 */
HB_FUNC( QT_QTOOLBAR_WIDGETFORACTION )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widgetForAction( hbqt_par_QAction( 2 ) ), false ) );
   }
}

/*
 * void setIconSize ( const QSize & iconSize )
 */
HB_FUNC( QT_QTOOLBAR_SETICONSIZE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      ( p )->setIconSize( *hbqt_par_QSize( 2 ) );
   }
}

/*
 * void setToolButtonStyle ( Qt::ToolButtonStyle toolButtonStyle )
 */
HB_FUNC( QT_QTOOLBAR_SETTOOLBUTTONSTYLE )
{
   QToolBar * p = hbqt_par_QToolBar( 1 );
   if( p )
   {
      ( p )->setToolButtonStyle( ( Qt::ToolButtonStyle ) hb_parni( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
