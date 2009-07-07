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
 *  Constructed[ 21/23 [ 91.30% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QVector<qreal> dashPattern () const
 *  void setDashPattern ( const QVector<qreal> & pattern )
 */


#include <QtGui/QPen>


/*
 * QPen ()
 * QPen ( Qt::PenStyle style )
 * QPen ( const QColor & color )
 * QPen ( const QBrush & brush, qreal width, Qt::PenStyle style = Qt::SolidLine, Qt::PenCapStyle cap = Qt::SquareCap, Qt::PenJoinStyle join = Qt::BevelJoin )
 * QPen ( const QPen & pen )
 * ~QPen ()
 */
HB_FUNC( QT_QPEN )
{
   if( hb_pcount() == 1 && HB_ISNUM( 1 ) )
   {
      hb_retptr( ( QPen* ) new QPen( ( Qt::PenStyle ) hb_parni( 1 ) ) );
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      hb_retptr( ( QPen* ) new QPen( *hbqt_par_QColor( 1 ) ) );

      #if 0       /* TODO */
      QString objName = hbqt_par_QObject( 1 )->objectName();

      if( objName == ( QString ) "QColor" )
      {
         hb_retptr( ( QPen* ) new QPen( *hbqt_par_QColor( 1 ) ) );
      }
      else if( objName == ( QString ) "QPen" )
      {
         hb_retptr( ( QPen* ) new QPen( *hbqt_par_QPen( 1 ) ) );
      }
      else
      {
         hb_retptr( ( QPen* ) new QPen() );
      }
      #endif
   }
   else if( hb_pcount() >= 2 && HB_ISPOINTER( 1 ) && HB_ISNUM( 2 ) )
   {
      hb_retptr( ( QPen* ) new QPen( *hbqt_par_QBrush( 1 ), hb_parnd( 2 ),
                                     HB_ISNUM( 3 ) ? ( Qt::PenStyle ) hb_parni( 3 ) : Qt::SolidLine,
                                     HB_ISNUM( 4 ) ? ( Qt::PenCapStyle ) hb_parni( 4 ) : Qt::SquareCap,
                                     HB_ISNUM( 5 ) ? ( Qt::PenJoinStyle ) hb_parni( 5 ) : Qt::BevelJoin
                                   ) );
   }
   else
   {
      hb_retptr( ( QPen* ) new QPen() );
   }
}

/*
 * QBrush brush () const
 */
HB_FUNC( QT_QPEN_BRUSH )
{
   hb_retptr( new QBrush( hbqt_par_QPen( 1 )->brush() ) );
}

/*
 * Qt::PenCapStyle capStyle () const
 */
HB_FUNC( QT_QPEN_CAPSTYLE )
{
   hb_retni( ( Qt::PenCapStyle ) hbqt_par_QPen( 1 )->capStyle() );
}

/*
 * QColor color () const
 */
HB_FUNC( QT_QPEN_COLOR )
{
   hb_retptr( new QColor( hbqt_par_QPen( 1 )->color() ) );
}

/*
 * qreal dashOffset () const
 */
HB_FUNC( QT_QPEN_DASHOFFSET )
{
   hb_retnd( hbqt_par_QPen( 1 )->dashOffset() );
}

/*
 * bool isCosmetic () const
 */
HB_FUNC( QT_QPEN_ISCOSMETIC )
{
   hb_retl( hbqt_par_QPen( 1 )->isCosmetic() );
}

/*
 * bool isSolid () const
 */
HB_FUNC( QT_QPEN_ISSOLID )
{
   hb_retl( hbqt_par_QPen( 1 )->isSolid() );
}

/*
 * Qt::PenJoinStyle joinStyle () const
 */
HB_FUNC( QT_QPEN_JOINSTYLE )
{
   hb_retni( ( Qt::PenJoinStyle ) hbqt_par_QPen( 1 )->joinStyle() );
}

/*
 * qreal miterLimit () const
 */
HB_FUNC( QT_QPEN_MITERLIMIT )
{
   hb_retnd( hbqt_par_QPen( 1 )->miterLimit() );
}

/*
 * void setBrush ( const QBrush & brush )
 */
HB_FUNC( QT_QPEN_SETBRUSH )
{
   hbqt_par_QPen( 1 )->setBrush( *hbqt_par_QBrush( 2 ) );
}

/*
 * void setCapStyle ( Qt::PenCapStyle style )
 */
HB_FUNC( QT_QPEN_SETCAPSTYLE )
{
   hbqt_par_QPen( 1 )->setCapStyle( ( Qt::PenCapStyle ) hb_parni( 2 ) );
}

/*
 * void setColor ( const QColor & color )
 */
HB_FUNC( QT_QPEN_SETCOLOR )
{
   hbqt_par_QPen( 1 )->setColor( *hbqt_par_QColor( 2 ) );
}

/*
 * void setCosmetic ( bool cosmetic )
 */
HB_FUNC( QT_QPEN_SETCOSMETIC )
{
   hbqt_par_QPen( 1 )->setCosmetic( hb_parl( 2 ) );
}

/*
 * void setDashOffset ( qreal offset )
 */
HB_FUNC( QT_QPEN_SETDASHOFFSET )
{
   hbqt_par_QPen( 1 )->setDashOffset( hb_parnd( 2 ) );
}

/*
 * void setJoinStyle ( Qt::PenJoinStyle style )
 */
HB_FUNC( QT_QPEN_SETJOINSTYLE )
{
   hbqt_par_QPen( 1 )->setJoinStyle( ( Qt::PenJoinStyle ) hb_parni( 2 ) );
}

/*
 * void setMiterLimit ( qreal limit )
 */
HB_FUNC( QT_QPEN_SETMITERLIMIT )
{
   hbqt_par_QPen( 1 )->setMiterLimit( hb_parnd( 2 ) );
}

/*
 * void setStyle ( Qt::PenStyle style )
 */
HB_FUNC( QT_QPEN_SETSTYLE )
{
   hbqt_par_QPen( 1 )->setStyle( ( Qt::PenStyle ) hb_parni( 2 ) );
}

/*
 * void setWidth ( int width )
 */
HB_FUNC( QT_QPEN_SETWIDTH )
{
   hbqt_par_QPen( 1 )->setWidth( hb_parni( 2 ) );
}

/*
 * void setWidthF ( qreal width )
 */
HB_FUNC( QT_QPEN_SETWIDTHF )
{
   hbqt_par_QPen( 1 )->setWidthF( hb_parnd( 2 ) );
}

/*
 * Qt::PenStyle style () const
 */
HB_FUNC( QT_QPEN_STYLE )
{
   hb_retni( ( Qt::PenStyle ) hbqt_par_QPen( 1 )->style() );
}

/*
 * int width () const
 */
HB_FUNC( QT_QPEN_WIDTH )
{
   hb_retni( hbqt_par_QPen( 1 )->width() );
}

/*
 * qreal widthF () const
 */
HB_FUNC( QT_QPEN_WIDTHF )
{
   hb_retnd( hbqt_par_QPen( 1 )->widthF() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

