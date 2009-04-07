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

/*
 *  Constructed[ 10/11 [ 90.91% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QSize> availableSizes ( Mode mode = Normal, State state = Off ) const
 */


#include <QtGui/QIcon>


/*
 * QIcon ()
 * QIcon ( const QPixmap & pixmap )
 * QIcon ( const QIcon & other )
 * QIcon ( const QString & fileName )
 * QIcon ( QIconEngine * engine )
 * QIcon ( QIconEngineV2 * engine )
 * ~QIcon ()
 */
HB_FUNC( QT_QICON )
{
   hb_retptr( ( QIcon* ) new QIcon() );
}

/*
 * QSize actualSize ( const QSize & size, Mode mode = Normal, State state = Off ) const
 */
HB_FUNC( QT_QICON_ACTUALSIZE )
{
   hb_retptr( new QSize( hbqt_par_QIcon( 1 )->actualSize( *hbqt_par_QSize( 2 ), ( HB_ISNIL( 3 ) ? ( QIcon::Mode ) QIcon::Normal : ( QIcon::Mode ) hb_parni( 3 ) ), ( HB_ISNIL( 4 ) ? ( QIcon::State ) QIcon::Off : ( QIcon::State ) hb_parni( 4 ) ) ) ) );
}

/*
 * void addFile ( const QString & fileName, const QSize & size = QSize(), Mode mode = Normal, State state = Off )
 */
HB_FUNC( QT_QICON_ADDFILE )
{
   hbqt_par_QIcon( 1 )->addFile( hbqt_par_QString( 2 ), *hbqt_par_QSize( 3 ) );
}

/*
 * void addPixmap ( const QPixmap & pixmap, Mode mode = Normal, State state = Off )
 */
HB_FUNC( QT_QICON_ADDPIXMAP )
{
   hbqt_par_QIcon( 1 )->addPixmap( *hbqt_par_QPixmap( 2 ), ( HB_ISNIL( 3 ) ? ( QIcon::Mode ) QIcon::Normal : ( QIcon::Mode ) hb_parni( 3 ) ), ( HB_ISNIL( 4 ) ? ( QIcon::State ) QIcon::Off : ( QIcon::State ) hb_parni( 4 ) ) );
}

/*
 * qint64 cacheKey () const
 */
HB_FUNC( QT_QICON_CACHEKEY )
{
   hb_retni( hbqt_par_QIcon( 1 )->cacheKey() );
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QICON_ISNULL )
{
   hb_retl( hbqt_par_QIcon( 1 )->isNull() );
}

/*
 * void paint ( QPainter * painter, const QRect & rect, Qt::Alignment alignment = Qt::AlignCenter, Mode mode = Normal, State state = Off ) const
 */
HB_FUNC( QT_QICON_PAINT )
{
   hbqt_par_QIcon( 1 )->paint( hbqt_par_QPainter( 2 ), *hbqt_par_QRect( 3 ), ( HB_ISNIL( 4 ) ? ( Qt::Alignment ) Qt::AlignCenter : ( Qt::Alignment ) hb_parni( 4 ) ), ( HB_ISNIL( 5 ) ? ( QIcon::Mode ) QIcon::Normal : ( QIcon::Mode ) hb_parni( 5 ) ), ( HB_ISNIL( 6 ) ? ( QIcon::State ) QIcon::Off : ( QIcon::State ) hb_parni( 6 ) ) );
}

/*
 * void paint ( QPainter * painter, int x, int y, int w, int h, Qt::Alignment alignment = Qt::AlignCenter, Mode mode = Normal, State state = Off ) const
 */
HB_FUNC( QT_QICON_PAINT_1 )
{
   hbqt_par_QIcon( 1 )->paint( hbqt_par_QPainter( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), ( HB_ISNIL( 7 ) ? ( Qt::Alignment ) Qt::AlignCenter : ( Qt::Alignment ) hb_parni( 7 ) ), ( HB_ISNIL( 8 ) ? ( QIcon::Mode ) QIcon::Normal : ( QIcon::Mode ) hb_parni( 8 ) ), ( HB_ISNIL( 9 ) ? ( QIcon::State ) QIcon::Off : ( QIcon::State ) hb_parni( 9 ) ) );
}

/*
 * QPixmap pixmap ( const QSize & size, Mode mode = Normal, State state = Off ) const
 */
HB_FUNC( QT_QICON_PIXMAP )
{
   hb_retptr( new QPixmap( hbqt_par_QIcon( 1 )->pixmap( *hbqt_par_QSize( 2 ), ( HB_ISNIL( 3 ) ? ( QIcon::Mode ) QIcon::Normal : ( QIcon::Mode ) hb_parni( 3 ) ), ( HB_ISNIL( 4 ) ? ( QIcon::State ) QIcon::Off : ( QIcon::State ) hb_parni( 4 ) ) ) ) );
}

/*
 * QPixmap pixmap ( int w, int h, Mode mode = Normal, State state = Off ) const
 */
HB_FUNC( QT_QICON_PIXMAP_1 )
{
   hb_retptr( new QPixmap( hbqt_par_QIcon( 1 )->pixmap( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISNIL( 4 ) ? ( QIcon::Mode ) QIcon::Normal : ( QIcon::Mode ) hb_parni( 4 ) ), ( HB_ISNIL( 5 ) ? ( QIcon::State ) QIcon::Off : ( QIcon::State ) hb_parni( 5 ) ) ) ) );
}

/*
 * QPixmap pixmap ( int extent, Mode mode = Normal, State state = Off ) const
 */
HB_FUNC( QT_QICON_PIXMAP_2 )
{
   hb_retptr( new QPixmap( hbqt_par_QIcon( 1 )->pixmap( hb_parni( 2 ), ( HB_ISNIL( 3 ) ? ( QIcon::Mode ) QIcon::Normal : ( QIcon::Mode ) hb_parni( 3 ) ), ( HB_ISNIL( 4 ) ? ( QIcon::State ) QIcon::Off : ( QIcon::State ) hb_parni( 4 ) ) ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

