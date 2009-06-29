/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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


#include <QtGui/QCursor>


/*
 * QCursor ()
 * QCursor ( Qt::CursorShape shape )
 * QCursor ( const QBitmap & bitmap, const QBitmap & mask, int hotX = -1, int hotY = -1 )
 * QCursor ( const QPixmap & pixmap, int hotX = -1, int hotY = -1 )
 * QCursor ( const QCursor & c )
 * QCursor ( HCURSOR cursor )
 * QCursor ( Qt::HANDLE handle )
 * ~QCursor ()
 */
HB_FUNC( QT_QCURSOR )
{
   hb_retptr( ( QCursor* ) new QCursor() );
}

/*
 * const QBitmap * bitmap () const
 */
HB_FUNC( QT_QCURSOR_BITMAP )
{
   hb_retptr( ( QBitmap* ) hbqt_par_QCursor( 1 )->bitmap() );
}

/*
 * QPoint hotSpot () const
 */
HB_FUNC( QT_QCURSOR_HOTSPOT )
{
   hb_retptr( new QPoint( hbqt_par_QCursor( 1 )->hotSpot() ) );
}

/*
 * const QBitmap * mask () const
 */
HB_FUNC( QT_QCURSOR_MASK )
{
   hb_retptr( ( QBitmap* ) hbqt_par_QCursor( 1 )->mask() );
}

/*
 * QPixmap pixmap () const
 */
HB_FUNC( QT_QCURSOR_PIXMAP )
{
   hb_retptr( new QPixmap( hbqt_par_QCursor( 1 )->pixmap() ) );
}

/*
 * void setShape ( Qt::CursorShape shape )
 */
HB_FUNC( QT_QCURSOR_SETSHAPE )
{
   hbqt_par_QCursor( 1 )->setShape( ( Qt::CursorShape ) hb_parni( 2 ) );
}

/*
 * Qt::CursorShape shape () const
 */
HB_FUNC( QT_QCURSOR_SHAPE )
{
   hb_retni( ( Qt::CursorShape ) hbqt_par_QCursor( 1 )->shape() );
}

/*
 * QPoint pos ()
 */
HB_FUNC( QT_QCURSOR_POS )
{
   hb_retptr( new QPoint( hbqt_par_QCursor( 1 )->pos() ) );
}

/*
 * void setPos ( int x, int y )
 */
HB_FUNC( QT_QCURSOR_SETPOS )
{
   hbqt_par_QCursor( 1 )->setPos( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setPos ( const QPoint & p )
 */
HB_FUNC( QT_QCURSOR_SETPOS_1 )
{
   hbqt_par_QCursor( 1 )->setPos( *hbqt_par_QPoint( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

