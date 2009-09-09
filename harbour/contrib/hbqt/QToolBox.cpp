/*
 * $Id$
 */

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
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/


#include <QtGui/QToolBox>


/*
 * QToolBox ( QWidget * parent = 0, Qt::WindowFlags f = 0 )
 * ~QToolBox ()
 */
HB_FUNC( QT_QTOOLBOX )
{
   hb_retptr( ( QToolBox* ) new QToolBox( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) );
}

/*
 * DESTRUCTOR
 */
HB_FUNC( QT_QTOOLBOX_DESTROY )
{
   hbqt_par_QToolBox( 1 )->~QToolBox();
}

/*
 * int addItem ( QWidget * widget, const QIcon & iconSet, const QString & text )
 */
HB_FUNC( QT_QTOOLBOX_ADDITEM )
{
   hb_retni( hbqt_par_QToolBox( 1 )->addItem( hbqt_par_QWidget( 2 ), QIcon( hbqt_par_QString( 3 ) ), hbqt_par_QString( 4 ) ) );
}

/*
 * int addItem ( QWidget * w, const QString & text )
 */
HB_FUNC( QT_QTOOLBOX_ADDITEM_1 )
{
   hb_retni( hbqt_par_QToolBox( 1 )->addItem( hbqt_par_QWidget( 2 ), hbqt_par_QString( 3 ) ) );
}

/*
 * int count () const
 */
HB_FUNC( QT_QTOOLBOX_COUNT )
{
   hb_retni( hbqt_par_QToolBox( 1 )->count() );
}

/*
 * int currentIndex () const
 */
HB_FUNC( QT_QTOOLBOX_CURRENTINDEX )
{
   hb_retni( hbqt_par_QToolBox( 1 )->currentIndex() );
}

/*
 * QWidget * currentWidget () const
 */
HB_FUNC( QT_QTOOLBOX_CURRENTWIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QToolBox( 1 )->currentWidget() );
}

/*
 * int indexOf ( QWidget * widget ) const
 */
HB_FUNC( QT_QTOOLBOX_INDEXOF )
{
   hb_retni( hbqt_par_QToolBox( 1 )->indexOf( hbqt_par_QWidget( 2 ) ) );
}

/*
 * int insertItem ( int index, QWidget * widget, const QIcon & icon, const QString & text )
 */
HB_FUNC( QT_QTOOLBOX_INSERTITEM )
{
   hb_retni( hbqt_par_QToolBox( 1 )->insertItem( hb_parni( 2 ), hbqt_par_QWidget( 3 ), QIcon( hbqt_par_QString( 4 ) ), hbqt_par_QString( 5 ) ) );
}

/*
 * int insertItem ( int index, QWidget * widget, const QString & text )
 */
HB_FUNC( QT_QTOOLBOX_INSERTITEM_1 )
{
   hb_retni( hbqt_par_QToolBox( 1 )->insertItem( hb_parni( 2 ), hbqt_par_QWidget( 3 ), hbqt_par_QString( 4 ) ) );
}

/*
 * bool isItemEnabled ( int index ) const
 */
HB_FUNC( QT_QTOOLBOX_ISITEMENABLED )
{
   hb_retl( hbqt_par_QToolBox( 1 )->isItemEnabled( hb_parni( 2 ) ) );
}

/*
 * QIcon itemIcon ( int index ) const
 */
HB_FUNC( QT_QTOOLBOX_ITEMICON )
{
   hb_retptr( new QIcon( hbqt_par_QToolBox( 1 )->itemIcon( hb_parni( 2 ) ) ) );
}

/*
 * QString itemText ( int index ) const
 */
HB_FUNC( QT_QTOOLBOX_ITEMTEXT )
{
   hb_retc( hbqt_par_QToolBox( 1 )->itemText( hb_parni( 2 ) ).toAscii().data() );
}

/*
 * QString itemToolTip ( int index ) const
 */
HB_FUNC( QT_QTOOLBOX_ITEMTOOLTIP )
{
   hb_retc( hbqt_par_QToolBox( 1 )->itemToolTip( hb_parni( 2 ) ).toAscii().data() );
}

/*
 * void removeItem ( int index )
 */
HB_FUNC( QT_QTOOLBOX_REMOVEITEM )
{
   hbqt_par_QToolBox( 1 )->removeItem( hb_parni( 2 ) );
}

/*
 * void setItemEnabled ( int index, bool enabled )
 */
HB_FUNC( QT_QTOOLBOX_SETITEMENABLED )
{
   hbqt_par_QToolBox( 1 )->setItemEnabled( hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setItemIcon ( int index, const QIcon & icon )
 */
HB_FUNC( QT_QTOOLBOX_SETITEMICON )
{
   hbqt_par_QToolBox( 1 )->setItemIcon( hb_parni( 2 ), QIcon( hbqt_par_QString( 3 ) ) );
}

/*
 * void setItemText ( int index, const QString & text )
 */
HB_FUNC( QT_QTOOLBOX_SETITEMTEXT )
{
   hbqt_par_QToolBox( 1 )->setItemText( hb_parni( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * void setItemToolTip ( int index, const QString & toolTip )
 */
HB_FUNC( QT_QTOOLBOX_SETITEMTOOLTIP )
{
   hbqt_par_QToolBox( 1 )->setItemToolTip( hb_parni( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * QWidget * widget ( int index ) const
 */
HB_FUNC( QT_QTOOLBOX_WIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QToolBox( 1 )->widget( hb_parni( 2 ) ) );
}

/*
 * void setCurrentIndex ( int index )
 */
HB_FUNC( QT_QTOOLBOX_SETCURRENTINDEX )
{
   hbqt_par_QToolBox( 1 )->setCurrentIndex( hb_parni( 2 ) );
}

/*
 * void setCurrentWidget ( QWidget * widget )
 */
HB_FUNC( QT_QTOOLBOX_SETCURRENTWIDGET )
{
   hbqt_par_QToolBox( 1 )->setCurrentWidget( hbqt_par_QWidget( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
