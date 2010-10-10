/*
 * $Id$
 */

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

#include "hbqt.h"

#if QT_VERSION >= 0x040500

#include "hbqt_hbqtableview.h"

#include <QtCore/QPointer>

HBQTableView::HBQTableView( QWidget * parent ) : QTableView( parent )
{
}

HBQTableView::~HBQTableView()
{
}

void HBQTableView::keyPressEvent( QKeyEvent * event )
{
   emit sg_keyPressEvent( event );
}

void HBQTableView::mouseDoubleClickEvent( QMouseEvent * event )
{
   emit sg_mouseDoubleClickEvent( event );
}

void HBQTableView::mouseMoveEvent( QMouseEvent * event )
{
   emit sg_mouseMoveEvent( event );
}

void HBQTableView::mousePressEvent( QMouseEvent * event )
{
   emit sg_mousePressEvent( event );
}

void HBQTableView::mouseReleaseEvent( QMouseEvent * event )
{
   emit sg_mouseReleaseEvent( event );
}

void HBQTableView::wheelEvent( QWheelEvent * event )
{
   emit sg_wheelEvent( event );
}

void HBQTableView::resizeEvent( QResizeEvent * event )
{
   emit sg_resizeEvent( event );
}

QModelIndex HBQTableView::moveCursor( HBQTableView::CursorAction cursorAction, Qt::KeyboardModifiers modifiers )
{
// HB_TRACE( HB_TR_DEBUG, ( "HBQTableView::moveCursor( action=%i %i )", cursorAction, QAbstractItemView::MoveDown ) );
   return QTableView::moveCursor( cursorAction, modifiers );
}

QModelIndex HBQTableView::navigate( int cursorAction )
{
   return moveCursor( ( HBQTableView::CursorAction ) cursorAction, ( Qt::KeyboardModifiers ) 0 );
}

void HBQTableView::scrollContentsBy( int x, int y )
{
   emit sg_scrollContentsBy( x, y );
}

void HBQTableView::scrollTo( const QModelIndex & index, QAbstractItemView::ScrollHint hint )
{
// HB_TRACE( HB_TR_DEBUG, ( "HBQTableView::scrollTo( row = %i col = %i )", index.row(), index.column() ) );

   QTableView::scrollTo( index, hint );
}

#endif
