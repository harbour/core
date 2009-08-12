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
 *  enum OptionType { SO_Button, SO_ComboBox, SO_Complex, SO_Default, ..., SO_Q3ListViewItem }
 *  enum StyleOptionType { Type }
 *  enum StyleOptionVersion { Version }
 */


#include <QtGui/QStyleOption>


/*
 * QStyleOption ( int version = QStyleOption::Version, int type = SO_Default )
 * QStyleOption ( const QStyleOption & other )
 * ~QStyleOption ()
 */
HB_FUNC( QT_QSTYLEOPTION )
{
   hb_retptr( ( QStyleOption* ) new QStyleOption() );
}

/*
 * DESTRUCTOR
 */
HB_FUNC( QT_QSTYLEOPTION_DESTROY )
{
   hbqt_par_QStyleOption( 1 )->~QStyleOption();
}

/*
 * void initFrom ( const QWidget * widget )
 */
HB_FUNC( QT_QSTYLEOPTION_INITFROM )
{
   hbqt_par_QStyleOption( 1 )->initFrom( hbqt_par_QWidget( 2 ) );
}

/*
 * Qt::LayoutDirection direction
 */
HB_FUNC( QT_QSTYLEOPTION_DIRECTION )
{
   hb_retni( ( Qt::LayoutDirection ) hbqt_par_QStyleOption( 1 )->direction );
}

/*
 * QFontMetrics fontMetrics
 */
HB_FUNC( QT_QSTYLEOPTION_FONTMETRICS )
{
   hb_retptr( new QFontMetrics( hbqt_par_QStyleOption( 1 )->fontMetrics ) );
}

/*
 * QPalette palette
 */
HB_FUNC( QT_QSTYLEOPTION_PALETTE )
{
   hb_retptr( new QPalette( hbqt_par_QStyleOption( 1 )->palette ) );
}

/*
 * QRect rect
 */
HB_FUNC( QT_QSTYLEOPTION_RECT )
{
   hb_retptr( new QRect( hbqt_par_QStyleOption( 1 )->rect ) );
}

/*
 * QStyle::State state
 */
HB_FUNC( QT_QSTYLEOPTION_STATE )
{
   hb_retni( ( QStyle::State ) hbqt_par_QStyleOption( 1 )->state );
}

/*
 * int type
 */
HB_FUNC( QT_QSTYLEOPTION_TYPE )
{
   hb_retni( hbqt_par_QStyleOption( 1 )->type );
}

/*
 * int version
 */
HB_FUNC( QT_QSTYLEOPTION_VERSION )
{
   hb_retni( hbqt_par_QStyleOption( 1 )->version );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
