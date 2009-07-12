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
 *  enum Mode { Hex, Dec, Oct, Bin }
 *  enum SegmentStyle { Outline, Filled, Flat }
 */


#include <QtGui/QLCDNumber>


/*
 * QLCDNumber ( QWidget * parent = 0 )
 * QLCDNumber ( uint numDigits, QWidget * parent = 0 )
 * ~QLCDNumber ()
 */
HB_FUNC( QT_QLCDNUMBER )
{
   hb_retptr( ( QLCDNumber * ) new QLCDNumber( hbqt_par_QWidget( 1 ) ) );
}

/*
 * DESTRUCTOR
 */
HB_FUNC( QT_QLCDNUMBER_DESTROY )
{
   hbqt_par_QLCDNumber( 1 )->~QLCDNumber();
}

/*
 * bool checkOverflow ( double num ) const
 */
HB_FUNC( QT_QLCDNUMBER_CHECKOVERFLOW )
{
   hb_retl( hbqt_par_QLCDNumber( 1 )->checkOverflow( hb_parnd( 2 ) ) );
}

/*
 * bool checkOverflow ( int num ) const
 */
HB_FUNC( QT_QLCDNUMBER_CHECKOVERFLOW_1 )
{
   hb_retl( hbqt_par_QLCDNumber( 1 )->checkOverflow( hb_parni( 2 ) ) );
}

/*
 * int intValue () const
 */
HB_FUNC( QT_QLCDNUMBER_INTVALUE )
{
   hb_retni( hbqt_par_QLCDNumber( 1 )->intValue() );
}

/*
 * Mode mode () const
 */
HB_FUNC( QT_QLCDNUMBER_MODE )
{
   hb_retni( ( QLCDNumber::Mode ) hbqt_par_QLCDNumber( 1 )->mode() );
}

/*
 * int numDigits () const
 */
HB_FUNC( QT_QLCDNUMBER_NUMDIGITS )
{
   hb_retni( hbqt_par_QLCDNumber( 1 )->numDigits() );
}

/*
 * SegmentStyle segmentStyle () const
 */
HB_FUNC( QT_QLCDNUMBER_SEGMENTSTYLE )
{
   hb_retni( ( QLCDNumber::SegmentStyle ) hbqt_par_QLCDNumber( 1 )->segmentStyle() );
}

/*
 * void setMode ( Mode )
 */
HB_FUNC( QT_QLCDNUMBER_SETMODE )
{
   hbqt_par_QLCDNumber( 1 )->setMode( ( QLCDNumber::Mode ) hb_parni( 2 ) );
}

/*
 * void setNumDigits ( int nDigits )
 */
HB_FUNC( QT_QLCDNUMBER_SETNUMDIGITS )
{
   hbqt_par_QLCDNumber( 1 )->setNumDigits( hb_parni( 2 ) );
}

/*
 * void setSegmentStyle ( SegmentStyle )
 */
HB_FUNC( QT_QLCDNUMBER_SETSEGMENTSTYLE )
{
   hbqt_par_QLCDNumber( 1 )->setSegmentStyle( ( QLCDNumber::SegmentStyle ) hb_parni( 2 ) );
}

/*
 * bool smallDecimalPoint () const
 */
HB_FUNC( QT_QLCDNUMBER_SMALLDECIMALPOINT )
{
   hb_retl( hbqt_par_QLCDNumber( 1 )->smallDecimalPoint() );
}

/*
 * double value () const
 */
HB_FUNC( QT_QLCDNUMBER_VALUE )
{
   hb_retnd( hbqt_par_QLCDNumber( 1 )->value() );
}

/*
 * void display ( const QString & s )
 */
HB_FUNC( QT_QLCDNUMBER_DISPLAY )
{
   hbqt_par_QLCDNumber( 1 )->display( hbqt_par_QString( 2 ) );
}

/*
 * void display ( double num )
 */
HB_FUNC( QT_QLCDNUMBER_DISPLAY_1 )
{
   hbqt_par_QLCDNumber( 1 )->display( hb_parnd( 2 ) );
}

/*
 * void display ( int num )
 */
HB_FUNC( QT_QLCDNUMBER_DISPLAY_2 )
{
   hbqt_par_QLCDNumber( 1 )->display( hb_parni( 2 ) );
}

/*
 * void setBinMode ()
 */
HB_FUNC( QT_QLCDNUMBER_SETBINMODE )
{
   hbqt_par_QLCDNumber( 1 )->setBinMode();
}

/*
 * void setDecMode ()
 */
HB_FUNC( QT_QLCDNUMBER_SETDECMODE )
{
   hbqt_par_QLCDNumber( 1 )->setDecMode();
}

/*
 * void setHexMode ()
 */
HB_FUNC( QT_QLCDNUMBER_SETHEXMODE )
{
   hbqt_par_QLCDNumber( 1 )->setHexMode();
}

/*
 * void setOctMode ()
 */
HB_FUNC( QT_QLCDNUMBER_SETOCTMODE )
{
   hbqt_par_QLCDNumber( 1 )->setOctMode();
}

/*
 * void setSmallDecimalPoint ( bool )
 */
HB_FUNC( QT_QLCDNUMBER_SETSMALLDECIMALPOINT )
{
   hbqt_par_QLCDNumber( 1 )->setSmallDecimalPoint( hb_parl( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

