/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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

#include "hbapiitm.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtGui/QWidget>

/*----------------------------------------------------------------------*/

void hbqt_ret_QRect( QRect qrc )
{
   PHB_ITEM info = hb_itemArrayNew( 4 );

   hb_arraySetNI( info, 1, qrc.x() );
   hb_arraySetNI( info, 2, qrc.y() );
   hb_arraySetNI( info, 3, qrc.x() + qrc.width() );
   hb_arraySetNI( info, 4, qrc.y() + qrc.height() );

   hb_itemReturnRelease( info );
}

/*----------------------------------------------------------------------*/

QRect hbqt_const_QRect( int i )
{
   QRect qrc;

   qrc.setX( hb_parvni( i, 1 ) );
   qrc.setY( hb_parvni( i, 2 ) );
   qrc.setWidth( hb_parvni( i, 3 ) - hb_parvni( i, 1 ) + 1 );
   qrc.setHeight( hb_parvni( i, 4 ) - hb_parvni( i, 2 ) + 1 );

   return qrc;
}

/*----------------------------------------------------------------------*/

void hbqt_ret_QSize( QSize qsz )
{
   PHB_ITEM info = hb_itemArrayNew( 2 );

   hb_arraySetNI( info, 1, qsz.width() );
   hb_arraySetNI( info, 2, qsz.height() );

   hb_itemReturnRelease( info );
}

/*----------------------------------------------------------------------*/

QSize hbqt_const_QSize( int i )
{
   QSize qsz;

   qsz.setWidth( hb_parvni( i, 1 ) );
   qsz.setHeight( hb_parvni( i, 2 ) );

   return qsz;
}

/*----------------------------------------------------------------------*/

void hbqt_ret_QPoint( QPoint qpt )
{
   PHB_ITEM info = hb_itemArrayNew( 2 );

   hb_arraySetNI( info, 1, qpt.x() );
   hb_arraySetNI( info, 2, qpt.y() );

   hb_itemReturnRelease( info );
}

/*----------------------------------------------------------------------*/

QPoint hbqt_const_QPoint( int i )
{
   QPoint qpt;

   qpt.setX( hb_parvni( i, 1 ) );
   qpt.setY( hb_parvni( i, 2 ) );

   return qpt;
}

/*----------------------------------------------------------------------*/

void hbqt_ret_QRectF( QRectF qrc )
{
   PHB_ITEM info = hb_itemArrayNew( 4 );

   hb_arraySetND( info, 1, qrc.x() );
   hb_arraySetND( info, 2, qrc.y() );
   hb_arraySetND( info, 3, qrc.x() + qrc.width() );
   hb_arraySetND( info, 4, qrc.y() + qrc.height() );

   hb_itemReturnRelease( info );
}

/*----------------------------------------------------------------------*/

QRectF hbqt_const_QRectF( int i )
{
   QRectF qrc;

   qrc.setX( hb_parvnd( i, 1 ) );
   qrc.setY( hb_parvnd( i, 2 ) );
   qrc.setWidth( hb_parvnd( i, 3 ) - hb_parvnd( i, 1 ) + 1 );
   qrc.setHeight( hb_parvnd( i, 4 ) - hb_parvnd( i, 2 ) + 1 );

   return qrc;
}

/*----------------------------------------------------------------------*/

void hbqt_ret_QSizeF( QSizeF qsz )
{
   PHB_ITEM info = hb_itemArrayNew( 2 );

   hb_arraySetND( info, 1, qsz.width() );
   hb_arraySetND( info, 2, qsz.height() );

   hb_itemReturnRelease( info );
}

/*----------------------------------------------------------------------*/

QSizeF hbqt_const_QSizeF( int i )
{
   QSizeF qsz;

   qsz.setWidth( hb_parvnd( i, 1 ) );
   qsz.setHeight( hb_parvnd( i, 2 ) );

   return qsz;
}

/*----------------------------------------------------------------------*/

void hbqt_ret_QPointF( QPointF qpt )
{
   PHB_ITEM info = hb_itemArrayNew( 2 );

   hb_arraySetND( info, 1, qpt.x() );
   hb_arraySetND( info, 2, qpt.y() );

   hb_itemReturnRelease( info );
}

/*----------------------------------------------------------------------*/

QPointF hbqt_const_QPointF( int i )
{
   QPointF qpt;

   qpt.setX( hb_parvnd( i, 1 ) );
   qpt.setY( hb_parvnd( i, 2 ) );

   return qpt;
}

/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
