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
 *  enum ToolButtonPopupMode { DelayedPopup, MenuButtonPopup, InstantPopup }
 */

#include <QtCore/QPointer>

#include <QtGui/QToolButton>


/*
 * QToolButton ( QWidget * parent = 0 )
 * ~QToolButton ()
 */

typedef struct
{
   QPointer< QToolButton > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QToolButton;

QT_G_FUNC( hbqt_gcRelease_QToolButton )
{
   QToolButton  * ph = NULL ;
   QGC_POINTER_QToolButton * p = ( QGC_POINTER_QToolButton * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QToolButton   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QToolButton   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QToolButton          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QToolButton    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QToolButton    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QToolButton( void * pObj, bool bNew )
{
   QGC_POINTER_QToolButton * p = ( QGC_POINTER_QToolButton * ) hb_gcAllocate( sizeof( QGC_POINTER_QToolButton ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QToolButton >( ( QToolButton * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QToolButton;
   p->type = HBQT_TYPE_QToolButton;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QToolButton  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QToolButton", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTOOLBUTTON )
{
   QToolButton * pObj = NULL;

   pObj =  new QToolButton( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QToolButton( ( void * ) pObj, true ) );
}

/*
 * Qt::ArrowType arrowType () const
 */
HB_FUNC( QT_QTOOLBUTTON_ARROWTYPE )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      hb_retni( ( Qt::ArrowType ) ( p )->arrowType() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBUTTON_ARROWTYPE FP=hb_retni( ( Qt::ArrowType ) ( p )->arrowType() ); p is NULL" ) );
   }
}

/*
 * bool autoRaise () const
 */
HB_FUNC( QT_QTOOLBUTTON_AUTORAISE )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      hb_retl( ( p )->autoRaise() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBUTTON_AUTORAISE FP=hb_retl( ( p )->autoRaise() ); p is NULL" ) );
   }
}

/*
 * QAction * defaultAction () const
 */
HB_FUNC( QT_QTOOLBUTTON_DEFAULTACTION )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->defaultAction(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBUTTON_DEFAULTACTION FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->defaultAction(), false ) ); p is NULL" ) );
   }
}

/*
 * QMenu * menu () const
 */
HB_FUNC( QT_QTOOLBUTTON_MENU )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->menu(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBUTTON_MENU FP=hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->menu(), false ) ); p is NULL" ) );
   }
}

/*
 * ToolButtonPopupMode popupMode () const
 */
HB_FUNC( QT_QTOOLBUTTON_POPUPMODE )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      hb_retni( ( QToolButton::ToolButtonPopupMode ) ( p )->popupMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBUTTON_POPUPMODE FP=hb_retni( ( QToolButton::ToolButtonPopupMode ) ( p )->popupMode() ); p is NULL" ) );
   }
}

/*
 * void setArrowType ( Qt::ArrowType type )
 */
HB_FUNC( QT_QTOOLBUTTON_SETARROWTYPE )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      ( p )->setArrowType( ( Qt::ArrowType ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBUTTON_SETARROWTYPE FP=( p )->setArrowType( ( Qt::ArrowType ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setAutoRaise ( bool enable )
 */
HB_FUNC( QT_QTOOLBUTTON_SETAUTORAISE )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      ( p )->setAutoRaise( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBUTTON_SETAUTORAISE FP=( p )->setAutoRaise( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMenu ( QMenu * menu )
 */
HB_FUNC( QT_QTOOLBUTTON_SETMENU )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      ( p )->setMenu( hbqt_par_QMenu( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBUTTON_SETMENU FP=( p )->setMenu( hbqt_par_QMenu( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPopupMode ( ToolButtonPopupMode mode )
 */
HB_FUNC( QT_QTOOLBUTTON_SETPOPUPMODE )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      ( p )->setPopupMode( ( QToolButton::ToolButtonPopupMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBUTTON_SETPOPUPMODE FP=( p )->setPopupMode( ( QToolButton::ToolButtonPopupMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * Qt::ToolButtonStyle toolButtonStyle () const
 */
HB_FUNC( QT_QTOOLBUTTON_TOOLBUTTONSTYLE )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      hb_retni( ( Qt::ToolButtonStyle ) ( p )->toolButtonStyle() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBUTTON_TOOLBUTTONSTYLE FP=hb_retni( ( Qt::ToolButtonStyle ) ( p )->toolButtonStyle() ); p is NULL" ) );
   }
}

/*
 * void setDefaultAction ( QAction * action )
 */
HB_FUNC( QT_QTOOLBUTTON_SETDEFAULTACTION )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      ( p )->setDefaultAction( hbqt_par_QAction( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBUTTON_SETDEFAULTACTION FP=( p )->setDefaultAction( hbqt_par_QAction( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setToolButtonStyle ( Qt::ToolButtonStyle style )
 */
HB_FUNC( QT_QTOOLBUTTON_SETTOOLBUTTONSTYLE )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      ( p )->setToolButtonStyle( ( Qt::ToolButtonStyle ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBUTTON_SETTOOLBUTTONSTYLE FP=( p )->setToolButtonStyle( ( Qt::ToolButtonStyle ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void showMenu ()
 */
HB_FUNC( QT_QTOOLBUTTON_SHOWMENU )
{
   QToolButton * p = hbqt_par_QToolButton( 1 );
   if( p )
      ( p )->showMenu();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTOOLBUTTON_SHOWMENU FP=( p )->showMenu(); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
