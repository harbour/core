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


#include <QtGui/QSpinBox>


/*
 * QSpinBox ( QWidget * parent = 0 )
 */
HB_FUNC( QT_QSPINBOX )
{
   hb_retptr( ( QSpinBox* ) new QSpinBox( hbqt_par_QWidget( 1 ) ) );
}

/*
 * QString cleanText () const
 */
HB_FUNC( QT_QSPINBOX_CLEANTEXT )
{
   hb_retc( hbqt_par_QSpinBox( 1 )->cleanText().toLatin1().data() );
}

/*
 * int maximum () const
 */
HB_FUNC( QT_QSPINBOX_MAXIMUM )
{
   hb_retni( hbqt_par_QSpinBox( 1 )->maximum() );
}

/*
 * int minimum () const
 */
HB_FUNC( QT_QSPINBOX_MINIMUM )
{
   hb_retni( hbqt_par_QSpinBox( 1 )->minimum() );
}

/*
 * QString prefix () const
 */
HB_FUNC( QT_QSPINBOX_PREFIX )
{
   hb_retc( hbqt_par_QSpinBox( 1 )->prefix().toLatin1().data() );
}

/*
 * void setMaximum ( int max )
 */
HB_FUNC( QT_QSPINBOX_SETMAXIMUM )
{
   hbqt_par_QSpinBox( 1 )->setMaximum( hb_parni( 2 ) );
}

/*
 * void setMinimum ( int min )
 */
HB_FUNC( QT_QSPINBOX_SETMINIMUM )
{
   hbqt_par_QSpinBox( 1 )->setMinimum( hb_parni( 2 ) );
}

/*
 * void setPrefix ( const QString & prefix )
 */
HB_FUNC( QT_QSPINBOX_SETPREFIX )
{
   hbqt_par_QSpinBox( 1 )->setPrefix( hbqt_par_QString( 2 ) );
}

/*
 * void setRange ( int minimum, int maximum )
 */
HB_FUNC( QT_QSPINBOX_SETRANGE )
{
   hbqt_par_QSpinBox( 1 )->setRange( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setSingleStep ( int val )
 */
HB_FUNC( QT_QSPINBOX_SETSINGLESTEP )
{
   hbqt_par_QSpinBox( 1 )->setSingleStep( hb_parni( 2 ) );
}

/*
 * void setSuffix ( const QString & suffix )
 */
HB_FUNC( QT_QSPINBOX_SETSUFFIX )
{
   hbqt_par_QSpinBox( 1 )->setSuffix( hbqt_par_QString( 2 ) );
}

/*
 * int singleStep () const
 */
HB_FUNC( QT_QSPINBOX_SINGLESTEP )
{
   hb_retni( hbqt_par_QSpinBox( 1 )->singleStep() );
}

/*
 * QString suffix () const
 */
HB_FUNC( QT_QSPINBOX_SUFFIX )
{
   hb_retc( hbqt_par_QSpinBox( 1 )->suffix().toLatin1().data() );
}

/*
 * int value () const
 */
HB_FUNC( QT_QSPINBOX_VALUE )
{
   hb_retni( hbqt_par_QSpinBox( 1 )->value() );
}

/*
 * void setValue ( int val )
 */
HB_FUNC( QT_QSPINBOX_SETVALUE )
{
   hbqt_par_QSpinBox( 1 )->setValue( hb_parni( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

