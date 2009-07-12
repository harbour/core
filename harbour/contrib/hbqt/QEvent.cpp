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
 *  enum Type { None, AccessibilityDescription, AccessibilityHelp, AccessibilityPrepare, ..., MaxUser }
 */


#include <QtCore/QEvent>


/*
 * QEvent ( Type type )
 * virtual ~QEvent ()
 */
HB_FUNC( QT_QEVENT )
{
   hb_retptr( ( QEvent* ) new QEvent( ( QEvent::Type ) hb_parni( 1 ) ) );
}

/*
 * DESTRUCTOR
 */
HB_FUNC( QT_QEVENT_DESTROY )
{
   hbqt_par_QEvent( 1 )->~QEvent();
}

/*
 * void accept ()
 */
HB_FUNC( QT_QEVENT_ACCEPT )
{
   hbqt_par_QEvent( 1 )->accept();
}

/*
 * void ignore ()
 */
HB_FUNC( QT_QEVENT_IGNORE )
{
   hbqt_par_QEvent( 1 )->ignore();
}

/*
 * bool isAccepted () const
 */
HB_FUNC( QT_QEVENT_ISACCEPTED )
{
   hb_retl( hbqt_par_QEvent( 1 )->isAccepted() );
}

/*
 * void setAccepted ( bool accepted )
 */
HB_FUNC( QT_QEVENT_SETACCEPTED )
{
   hbqt_par_QEvent( 1 )->setAccepted( hb_parl( 2 ) );
}

/*
 * bool spontaneous () const
 */
HB_FUNC( QT_QEVENT_SPONTANEOUS )
{
   hb_retl( hbqt_par_QEvent( 1 )->spontaneous() );
}

/*
 * Type type () const
 */
HB_FUNC( QT_QEVENT_TYPE )
{
   hb_retni( ( QEvent::Type ) hbqt_par_QEvent( 1 )->type() );
}

/*
 * int registerEventType ( int hint = -1 )
 */
HB_FUNC( QT_QEVENT_REGISTEREVENTTYPE )
{
   hb_retni( hbqt_par_QEvent( 1 )->registerEventType( ( HB_ISNUM( 2 ) ? hb_parni( 2 ) : -1 ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

