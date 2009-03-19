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

#if QT_VERSION >= 0x040500

#include <QtGui/QAbstractButton>

/*----------------------------------------------------------------------*/
/*
bool autoExclusive () const
*/
HB_FUNC( QT_QABSTRACTBUTTON_AUTOEXCLUSIVE )
{
   hb_retl( hbqt_par_QAbstractButton( 1 )->autoExclusive() );
}

/*
bool autoRepeat () const
*/
HB_FUNC( QT_QABSTRACTBUTTON_AUTOREPEAT )
{
   hb_retl( hbqt_par_QAbstractButton( 1 )->autoRepeat() );
}

/*
int autoRepeatDelay () const
*/
HB_FUNC( QT_QABSTRACTBUTTON_AUTOREPEATDELAY )
{
   hb_retni( hbqt_par_QAbstractButton( 1 )->autoRepeatDelay() );
}

/*
int autoRepeatInterval () const
*/
HB_FUNC( QT_QABSTRACTBUTTON_AUTOREPEATINTERVAL )
{
   hb_retni( hbqt_par_QAbstractButton( 1 )->autoRepeatInterval() );
}

/*
QButtonGroup * group () const
*/
HB_FUNC( QT_QABSTRACTBUTTON_GROUP )
{
   hb_retptr( ( QButtonGroup* ) hbqt_par_QAbstractButton( 1 )->group() );
}

/*
bool isCheckable () const
*/
HB_FUNC( QT_QABSTRACTBUTTON_ISCHECKABLE )
{
   hb_retl( hbqt_par_QAbstractButton( 1 )->isCheckable() );
}

/*
bool isChecked () const
*/
HB_FUNC( QT_QABSTRACTBUTTON_ISCHECKED )
{
   hb_retl( hbqt_par_QAbstractButton( 1 )->isChecked() );
}

/*
bool isDown () const
*/
HB_FUNC( QT_QABSTRACTBUTTON_ISDOWN )
{
   hb_retl( hbqt_par_QAbstractButton( 1 )->isDown() );
}

/*
void setAutoExclusive ( bool )
*/
HB_FUNC( QT_QABSTRACTBUTTON_SETAUTOEXCLUSIVE )
{
  hbqt_par_QAbstractButton( 1 )->setAutoExclusive( hb_parl( 2 ) );
}

/*
void setAutoRepeat ( bool )
*/
HB_FUNC( QT_QABSTRACTBUTTON_SETAUTOREPEAT )
{
  hbqt_par_QAbstractButton( 1 )->setAutoRepeat( hb_parl( 2 ) );
}

/*
void setAutoRepeatDelay ( int )
*/
HB_FUNC( QT_QABSTRACTBUTTON_SETAUTOREPEATDELAY )
{
  hbqt_par_QAbstractButton( 1 )->setAutoRepeatDelay( hb_parni( 2 ) );
}

/*
void setAutoRepeatInterval ( int )
*/
HB_FUNC( QT_QABSTRACTBUTTON_SETAUTOREPEATINTERVAL )
{
  hbqt_par_QAbstractButton( 1 )->setAutoRepeatInterval( hb_parni( 2 ) );
}

/*
void setCheckable ( bool )
*/
HB_FUNC( QT_QABSTRACTBUTTON_SETCHECKABLE )
{
  hbqt_par_QAbstractButton( 1 )->setCheckable( hb_parl( 2 ) );
}

/*
void setDown ( bool )
*/
HB_FUNC( QT_QABSTRACTBUTTON_SETDOWN )
{
  hbqt_par_QAbstractButton( 1 )->setDown( hb_parl( 2 ) );
}

/*
void setIcon ( const QIcon & icon )
*/
HB_FUNC( QT_QABSTRACTBUTTON_SETICON )
{
  hbqt_par_QAbstractButton( 1 )->setIcon( QIcon( hbqt_par_QString( 2 ) ) );
}

/*
void setText ( const QString & text )
*/
HB_FUNC( QT_QABSTRACTBUTTON_SETTEXT )
{
  hbqt_par_QAbstractButton( 1 )->setText( hbqt_par_QString( 2 ) );
}

/*
QString text () const
*/
HB_FUNC( QT_QABSTRACTBUTTON_TEXT )
{
   QString      str1   = hbqt_par_QAbstractButton( 1 )->text();
   QByteArray   ba     = str1.toLatin1();
   const char * c_str2 = ba.data();
   hb_retc( c_str2 );
}

/*
void animateClick ( int msec = 100 )
*/
HB_FUNC( QT_QABSTRACTBUTTON_ANIMATECLICK )
{
   hbqt_par_QAbstractButton( 1 )->animateClick( hb_parni( 2 ) );
}

/*
void click ()
*/
HB_FUNC( QT_QABSTRACTBUTTON_CLICK )
{
   hbqt_par_QAbstractButton( 1 )->click();
}

/*
void setChecked ( bool )
*/
HB_FUNC( QT_QABSTRACTBUTTON_SETCHECKED )
{
   hbqt_par_QAbstractButton( 1 )->setChecked( hb_parl( 2 ) );
}

/*
void toggle ()
*/
HB_FUNC( QT_QABSTRACTBUTTON_TOGGLE )
{
   hbqt_par_QAbstractButton( 1 )->toggle();
}

/*----------------------------------------------------------------------*/
#endif
