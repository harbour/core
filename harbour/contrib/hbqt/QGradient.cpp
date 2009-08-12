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

/*
 *  enum CoordinateMode { LogicalMode, StretchToDeviceMode, ObjectBoundingMode }
 *  enum Spread { PadSpread, RepeatSpread, ReflectSpread }
 *  enum Type { LinearGradient, RadialGradient, ConicalGradient, NoGradient }
 */


#include <QtGui/QGradient>


/*
 *
 */
HB_FUNC( QT_QGRADIENT )
{

}

/*
 * DESTRUCTOR
 */
HB_FUNC( QT_QGRADIENT_DESTROY )
{

}

/*
 * CoordinateMode coordinateMode () const
 */
HB_FUNC( QT_QGRADIENT_COORDINATEMODE )
{
   hb_retni( ( QGradient::CoordinateMode ) hbqt_par_QGradient( 1 )->coordinateMode() );
}

/*
 * void setColorAt ( qreal position, const QColor & color )
 */
HB_FUNC( QT_QGRADIENT_SETCOLORAT )
{
   hbqt_par_QGradient( 1 )->setColorAt( hb_parnd( 2 ), *hbqt_par_QColor( 3 ) );
}

/*
 * void setCoordinateMode ( CoordinateMode mode )
 */
HB_FUNC( QT_QGRADIENT_SETCOORDINATEMODE )
{
   hbqt_par_QGradient( 1 )->setCoordinateMode( ( QGradient::CoordinateMode ) hb_parni( 2 ) );
}

/*
 * void setSpread ( Spread method )
 */
HB_FUNC( QT_QGRADIENT_SETSPREAD )
{
   hbqt_par_QGradient( 1 )->setSpread( ( QGradient::Spread ) hb_parni( 2 ) );
}

/*
 * void setStops ( const QGradientStops & stopPoints )
 */
HB_FUNC( QT_QGRADIENT_SETSTOPS )
{
   hbqt_par_QGradient( 1 )->setStops( *hbqt_par_QGradientStops( 2 ) );
}

/*
 * Spread spread () const
 */
HB_FUNC( QT_QGRADIENT_SPREAD )
{
   hb_retni( ( QGradient::Spread ) hbqt_par_QGradient( 1 )->spread() );
}

/*
 * QGradientStops stops () const
 */
HB_FUNC( QT_QGRADIENT_STOPS )
{
   hb_retptr( new QGradientStops( hbqt_par_QGradient( 1 )->stops() ) );
}

/*
 * Type type () const
 */
HB_FUNC( QT_QGRADIENT_TYPE )
{
   hb_retni( ( QGradient::Type ) hbqt_par_QGradient( 1 )->type() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
