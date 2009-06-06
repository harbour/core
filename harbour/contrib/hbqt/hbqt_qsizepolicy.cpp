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


#include <QtGui/QSizePolicy>


/*
 * QSizePolicy ()
 * QSizePolicy ( Policy horizontal, Policy vertical )
 * QSizePolicy ( Policy horizontal, Policy vertical, ControlType type )
 */
HB_FUNC( QT_QSIZEPOLICY )
{
   hb_retptr( ( QSizePolicy* ) new QSizePolicy() );
}

/*
 * ControlType controlType () const
 */
HB_FUNC( QT_QSIZEPOLICY_CONTROLTYPE )
{
   hb_retni( ( QSizePolicy::ControlType ) hbqt_par_QSizePolicy( 1 )->controlType() );
}

/*
 * Qt::Orientations expandingDirections () const
 */
HB_FUNC( QT_QSIZEPOLICY_EXPANDINGDIRECTIONS )
{
   hb_retni( ( Qt::Orientations ) hbqt_par_QSizePolicy( 1 )->expandingDirections() );
}

/*
 * bool hasHeightForWidth () const
 */
HB_FUNC( QT_QSIZEPOLICY_HASHEIGHTFORWIDTH )
{
   hb_retl( hbqt_par_QSizePolicy( 1 )->hasHeightForWidth() );
}

/*
 * Policy horizontalPolicy () const
 */
HB_FUNC( QT_QSIZEPOLICY_HORIZONTALPOLICY )
{
   hb_retni( ( QSizePolicy::Policy ) hbqt_par_QSizePolicy( 1 )->horizontalPolicy() );
}

/*
 * int horizontalStretch () const
 */
HB_FUNC( QT_QSIZEPOLICY_HORIZONTALSTRETCH )
{
   hb_retni( hbqt_par_QSizePolicy( 1 )->horizontalStretch() );
}

/*
 * void setControlType ( ControlType type )
 */
HB_FUNC( QT_QSIZEPOLICY_SETCONTROLTYPE )
{
   hbqt_par_QSizePolicy( 1 )->setControlType( ( QSizePolicy::ControlType ) hb_parni( 2 ) );
}

/*
 * void setHeightForWidth ( bool dependent )
 */
HB_FUNC( QT_QSIZEPOLICY_SETHEIGHTFORWIDTH )
{
   hbqt_par_QSizePolicy( 1 )->setHeightForWidth( hb_parl( 2 ) );
}

/*
 * void setHorizontalPolicy ( Policy policy )
 */
HB_FUNC( QT_QSIZEPOLICY_SETHORIZONTALPOLICY )
{
   hbqt_par_QSizePolicy( 1 )->setHorizontalPolicy( ( QSizePolicy::Policy ) hb_parni( 2 ) );
}

/*
 * void setHorizontalStretch ( uchar stretchFactor )
 */
HB_FUNC( QT_QSIZEPOLICY_SETHORIZONTALSTRETCH )
{
   hbqt_par_QSizePolicy( 1 )->setHorizontalStretch( ( char ) hb_parni( 2 ) );
}

/*
 * void setVerticalPolicy ( Policy policy )
 */
HB_FUNC( QT_QSIZEPOLICY_SETVERTICALPOLICY )
{
   hbqt_par_QSizePolicy( 1 )->setVerticalPolicy( ( QSizePolicy::Policy ) hb_parni( 2 ) );
}

/*
 * void setVerticalStretch ( uchar stretchFactor )
 */
HB_FUNC( QT_QSIZEPOLICY_SETVERTICALSTRETCH )
{
   hbqt_par_QSizePolicy( 1 )->setVerticalStretch( ( char ) hb_parni( 2 ) );
}

/*
 * void transpose ()
 */
HB_FUNC( QT_QSIZEPOLICY_TRANSPOSE )
{
   hbqt_par_QSizePolicy( 1 )->transpose();
}

/*
 * Policy verticalPolicy () const
 */
HB_FUNC( QT_QSIZEPOLICY_VERTICALPOLICY )
{
   hb_retni( ( QSizePolicy::Policy ) hbqt_par_QSizePolicy( 1 )->verticalPolicy() );
}

/*
 * int verticalStretch () const
 */
HB_FUNC( QT_QSIZEPOLICY_VERTICALSTRETCH )
{
   hb_retni( hbqt_par_QSizePolicy( 1 )->verticalStretch() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

