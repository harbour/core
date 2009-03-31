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
 *  Constructed[ 4/14 [ 28.57% ] ]
 *  
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *  
 *  const QColor & color () const
 *  const QMatrix & matrix () const
 *  void setColor ( const QColor & color )
 *  void setMatrix ( const QMatrix & matrix )
 *  void setTexture ( const QPixmap & pixmap )
 *  void setTextureImage ( const QImage & image )
 *  void setTransform ( const QTransform & matrix )
 *  QPixmap texture () const
 *  QImage textureImage () const
 *  QTransform transform () const
 *  
 *  *** Commented out protos which construct fine but do not compile ***
 *  
 *  //const QGradient * gradient () const
 */ 


#include <QtGui/QBrush>


/*
 * QBrush ()
 * QBrush ( Qt::BrushStyle style )
 * QBrush ( const QColor & color, Qt::BrushStyle style = Qt::SolidPattern )
 * QBrush ( Qt::GlobalColor color, Qt::BrushStyle style = Qt::SolidPattern )
 * QBrush ( const QColor & color, const QPixmap & pixmap )
 * QBrush ( Qt::GlobalColor color, const QPixmap & pixmap )
 * QBrush ( const QPixmap & pixmap )
 * QBrush ( const QImage & image )
 * QBrush ( const QBrush & other )
 * QBrush ( const QGradient & gradient )
 * ~QBrush ()
 */
HB_FUNC( QT_QBRUSH )
{
   hb_retptr( ( QBrush* ) new QBrush() );
}

/*
 * bool isOpaque () const
 */
HB_FUNC( QT_QBRUSH_ISOPAQUE )
{
   hb_retl( hbqt_par_QBrush( 1 )->isOpaque(  ) );
}

/*
 * void setColor ( Qt::GlobalColor color )
 */
HB_FUNC( QT_QBRUSH_SETCOLOR )
{
   hbqt_par_QBrush( 1 )->setColor( ( Qt::GlobalColor ) hb_parni( 2 ) );
}

/*
 * void setStyle ( Qt::BrushStyle style )
 */
HB_FUNC( QT_QBRUSH_SETSTYLE )
{
   hbqt_par_QBrush( 1 )->setStyle( ( Qt::BrushStyle ) hb_parni( 2 ) );
}

/*
 * Qt::BrushStyle style () const
 */
HB_FUNC( QT_QBRUSH_STYLE )
{
   hb_retni( hbqt_par_QBrush( 1 )->style(  ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

