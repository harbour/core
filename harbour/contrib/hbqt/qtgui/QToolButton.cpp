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
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   QPointer< QToolButton > pq;
} QGC_POINTER_QToolButton;

QT_G_FUNC( hbqt_gcRelease_QToolButton )
{
   QGC_POINTER_QToolButton * p = ( QGC_POINTER_QToolButton * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QToolButton   /.\\   pq=%p", p->ph, (void *)(p->pq) ) );
            delete ( ( QToolButton * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QToolButton   \\./   pq=%p", p->ph, (void *)(p->pq) ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QToolButton          pq=%p", p->ph, (void *)(p->pq) ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QToolButton    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QToolButton    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QToolButton( void * pObj, bool bNew )
{
   QGC_POINTER_QToolButton * p = ( QGC_POINTER_QToolButton * ) hb_gcAllocate( sizeof( QGC_POINTER_QToolButton ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QToolButton;

   if( bNew )
   {
      new( & p->pq ) QPointer< QToolButton >( ( QToolButton * ) pObj );
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
   void * pObj = NULL;

   pObj = ( QToolButton* ) new QToolButton( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QToolButton( pObj, true ) );
}

/*
 * Qt::ArrowType arrowType () const
 */
HB_FUNC( QT_QTOOLBUTTON_ARROWTYPE )
{
   hb_retni( ( Qt::ArrowType ) hbqt_par_QToolButton( 1 )->arrowType() );
}

/*
 * bool autoRaise () const
 */
HB_FUNC( QT_QTOOLBUTTON_AUTORAISE )
{
   hb_retl( hbqt_par_QToolButton( 1 )->autoRaise() );
}

/*
 * QAction * defaultAction () const
 */
HB_FUNC( QT_QTOOLBUTTON_DEFAULTACTION )
{
   hb_retptrGC( hbqt_gcAllocate_QAction( hbqt_par_QToolButton( 1 )->defaultAction(), false ) );
}

/*
 * QMenu * menu () const
 */
HB_FUNC( QT_QTOOLBUTTON_MENU )
{
   hb_retptrGC( hbqt_gcAllocate_QMenu( hbqt_par_QToolButton( 1 )->menu(), false ) );
}

/*
 * ToolButtonPopupMode popupMode () const
 */
HB_FUNC( QT_QTOOLBUTTON_POPUPMODE )
{
   hb_retni( ( QToolButton::ToolButtonPopupMode ) hbqt_par_QToolButton( 1 )->popupMode() );
}

/*
 * void setArrowType ( Qt::ArrowType type )
 */
HB_FUNC( QT_QTOOLBUTTON_SETARROWTYPE )
{
   hbqt_par_QToolButton( 1 )->setArrowType( ( Qt::ArrowType ) hb_parni( 2 ) );
}

/*
 * void setAutoRaise ( bool enable )
 */
HB_FUNC( QT_QTOOLBUTTON_SETAUTORAISE )
{
   hbqt_par_QToolButton( 1 )->setAutoRaise( hb_parl( 2 ) );
}

/*
 * void setMenu ( QMenu * menu )
 */
HB_FUNC( QT_QTOOLBUTTON_SETMENU )
{
   hbqt_par_QToolButton( 1 )->setMenu( hbqt_par_QMenu( 2 ) );
}

/*
 * void setPopupMode ( ToolButtonPopupMode mode )
 */
HB_FUNC( QT_QTOOLBUTTON_SETPOPUPMODE )
{
   hbqt_par_QToolButton( 1 )->setPopupMode( ( QToolButton::ToolButtonPopupMode ) hb_parni( 2 ) );
}

/*
 * Qt::ToolButtonStyle toolButtonStyle () const
 */
HB_FUNC( QT_QTOOLBUTTON_TOOLBUTTONSTYLE )
{
   hb_retni( ( Qt::ToolButtonStyle ) hbqt_par_QToolButton( 1 )->toolButtonStyle() );
}

/*
 * void setDefaultAction ( QAction * action )
 */
HB_FUNC( QT_QTOOLBUTTON_SETDEFAULTACTION )
{
   hbqt_par_QToolButton( 1 )->setDefaultAction( hbqt_par_QAction( 2 ) );
}

/*
 * void setToolButtonStyle ( Qt::ToolButtonStyle style )
 */
HB_FUNC( QT_QTOOLBUTTON_SETTOOLBUTTONSTYLE )
{
   hbqt_par_QToolButton( 1 )->setToolButtonStyle( ( Qt::ToolButtonStyle ) hb_parni( 2 ) );
}

/*
 * void showMenu ()
 */
HB_FUNC( QT_QTOOLBUTTON_SHOWMENU )
{
   hbqt_par_QToolButton( 1 )->showMenu();
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
