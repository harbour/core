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
 *  enum StyleOptionType { Type }
 *  enum StyleOptionVersion { Version }
 *  enum ToolButtonFeature { None, Arrow, Menu, PopupDelay, HasMenu, MenuButtonPopup }
 *  flags ToolButtonFeatures
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionToolButton>


/*
 * QStyleOptionToolButton ()
 * QStyleOptionToolButton ( const QStyleOptionToolButton & other )
 */

QT_G_FUNC( release_QStyleOptionToolButton )
{
#if defined(__debug__)
   hb_snprintf( str, sizeof(str), "release_QStyleOptionToolButton" );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      delete ( ( QStyleOptionToolButton * ) ph );
      ph = NULL;
   }
}

HB_FUNC( QT_QSTYLEOPTIONTOOLBUTTON )
{
   void * pObj = NULL;

   pObj = ( QStyleOptionToolButton* ) new QStyleOptionToolButton() ;

   hb_retptr( pObj );
}
/*
 * Qt::ArrowType arrowType
 */
HB_FUNC( QT_QSTYLEOPTIONTOOLBUTTON_ARROWTYPE )
{
   hb_retni( ( Qt::ArrowType ) hbqt_par_QStyleOptionToolButton( 1 )->arrowType );
}

/*
 * ToolButtonFeatures features
 */
HB_FUNC( QT_QSTYLEOPTIONTOOLBUTTON_FEATURES )
{
   hb_retni( ( QStyleOptionToolButton::ToolButtonFeatures ) hbqt_par_QStyleOptionToolButton( 1 )->features );
}

/*
 * QFont font
 */
HB_FUNC( QT_QSTYLEOPTIONTOOLBUTTON_FONT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QFont( hbqt_par_QStyleOptionToolButton( 1 )->font ), release_QFont ) );
}

/*
 * QIcon icon
 */
HB_FUNC( QT_QSTYLEOPTIONTOOLBUTTON_ICON )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QIcon( hbqt_par_QStyleOptionToolButton( 1 )->icon ), release_QIcon ) );
}

/*
 * QSize iconSize
 */
HB_FUNC( QT_QSTYLEOPTIONTOOLBUTTON_ICONSIZE )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QSize( hbqt_par_QStyleOptionToolButton( 1 )->iconSize ), release_QSize ) );
}

/*
 * QPoint pos
 */
HB_FUNC( QT_QSTYLEOPTIONTOOLBUTTON_POS )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QPoint( hbqt_par_QStyleOptionToolButton( 1 )->pos ), release_QPoint ) );
}

/*
 * QString text
 */
HB_FUNC( QT_QSTYLEOPTIONTOOLBUTTON_TEXT )
{
   hb_retc( hbqt_par_QStyleOptionToolButton( 1 )->text.toLatin1().data() );
}

/*
 * Qt::ToolButtonStyle toolButtonStyle
 */
HB_FUNC( QT_QSTYLEOPTIONTOOLBUTTON_TOOLBUTTONSTYLE )
{
   hb_retni( ( Qt::ToolButtonStyle ) hbqt_par_QStyleOptionToolButton( 1 )->toolButtonStyle );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
