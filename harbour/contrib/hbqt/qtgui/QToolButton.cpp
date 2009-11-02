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

/*
 *  enum ToolButtonPopupMode { DelayedPopup, MenuButtonPopup, InstantPopup }
 */

#include <QtCore/QPointer>

#include <QtGui/QToolButton>


/*
 * QToolButton ( QWidget * parent = 0 )
 * ~QToolButton ()
 */

QT_G_FUNC( release_QToolButton )
{
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "release_QToolButton                 %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      const QMetaObject * m = ( ( QObject * ) ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         ( ( QToolButton * ) ph )->~QToolButton();
         ph = NULL;
      }
      else
      {
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "  Object Name Missing: QToolButton" );  OutputDebugString( str );
#endif
      }
   }
   else
   {
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "! ph____QToolButton" );  OutputDebugString( str );
#endif
   }
}

HB_FUNC( QT_QTOOLBUTTON )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );
   QPointer< QToolButton > pObj = NULL;
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "   GC:  new QToolButton                 %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif

   pObj = ( QToolButton* ) new QToolButton( hbqt_par_QWidget( 1 ) ) ;

#if defined(__debug__)
hb_snprintf( str, sizeof(str), "   GC:                                  %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif
   p->ph = pObj;
   p->func = release_QToolButton;

   hb_retptrGC( p );
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
   hb_retptr( ( QAction* ) hbqt_par_QToolButton( 1 )->defaultAction() );
}

/*
 * QMenu * menu () const
 */
HB_FUNC( QT_QTOOLBUTTON_MENU )
{
   hb_retptr( ( QMenu* ) hbqt_par_QToolButton( 1 )->menu() );
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
