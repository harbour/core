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

#include <QtGui/QAbstractSpinBox>

/*----------------------------------------------------------------------*/
/*
Qt::Alignment alignment () const
*/
HB_FUNC( QT_QABSTRACTSPINBOX_ALIGNMENT )
{
  hb_retni( hbqt_par_QAbstractSpinBox( 1 )->alignment() );
}

/*
ButtonSymbols buttonSymbols () const
*/
HB_FUNC( QT_QABSTRACTSPINBOX_BUTTONSYMBOLS )
{
  hb_retni( hbqt_par_QAbstractSpinBox( 1 )->buttonSymbols() );
}

/*
CorrectionMode correctionMode () const
*/
HB_FUNC( QT_QABSTRACTSPINBOX_CORRECTIONMODE )
{
  hb_retni( hbqt_par_QAbstractSpinBox( 1 )->correctionMode() );
}

/*
virtual void fixup ( QString & input ) const
*/
HB_FUNC( QT_QABSTRACTSPINBOX_FIXUP )
{
  QString par2 = hb_parc( 2 );
  hbqt_par_QAbstractSpinBox( 1 )->fixup( par2 );
}

/*
bool hasAcceptableInput () const
*/
HB_FUNC( QT_QABSTRACTSPINBOX_HASACCEPTABLEINPUT )
{
  hb_retl( hbqt_par_QAbstractSpinBox( 1 )->hasAcceptableInput() );
}

/*
bool hasFrame () const
*/
HB_FUNC( QT_QABSTRACTSPINBOX_HASFRAME )
{
  hb_retl( hbqt_par_QAbstractSpinBox( 1 )->hasFrame() );
}

/*
void interpretText ()
*/
HB_FUNC( QT_QABSTRACTSPINBOX_INTERPRETTEXT )
{
  hbqt_par_QAbstractSpinBox( 1 )->interpretText();
}

/*
bool isAccelerated () const
*/
HB_FUNC( QT_QABSTRACTSPINBOX_ISACCELERATED )
{
  hb_retl( hbqt_par_QAbstractSpinBox( 1 )->isAccelerated() );
}

/*
bool isReadOnly () const
*/
HB_FUNC( QT_QABSTRACTSPINBOX_ISREADONLY )
{
  hb_retl( hbqt_par_QAbstractSpinBox( 1 )->isReadOnly() );
}

/*
bool keyboardTracking () const
*/
HB_FUNC( QT_QABSTRACTSPINBOX_KEYBOARDTRACKING )
{
  hb_retl( hbqt_par_QAbstractSpinBox( 1 )->keyboardTracking() );
}

/*
void setAccelerated ( bool on )
*/
HB_FUNC( QT_QABSTRACTSPINBOX_SETACCELERATED )
{
  hbqt_par_QAbstractSpinBox( 1 )->setAccelerated( hb_parl( 2 ) );
}

/*
void setAlignment ( Qt::Alignment flag )
*/
HB_FUNC( QT_QABSTRACTSPINBOX_SETALIGNMENT )
{
  hbqt_par_QAbstractSpinBox( 1 )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/*
void setButtonSymbols ( ButtonSymbols bs )
*/
HB_FUNC( QT_QABSTRACTSPINBOX_SETBUTTONSYMBOLS )
{
  hbqt_par_QAbstractSpinBox( 1 )->setButtonSymbols( ( QAbstractSpinBox::ButtonSymbols ) hb_parni( 2 ) );
}

/*
void setCorrectionMode ( CorrectionMode cm )
*/
HB_FUNC( QT_QABSTRACTSPINBOX_SETCORRECTIONMODE )
{
  hbqt_par_QAbstractSpinBox( 1 )->setCorrectionMode( ( QAbstractSpinBox::CorrectionMode ) hb_parni( 2 ) );
}

/*
void setFrame ( bool )
*/
HB_FUNC( QT_QABSTRACTSPINBOX_SETFRAME )
{
  hbqt_par_QAbstractSpinBox( 1 )->setFrame( hb_parl( 2 ) );
}

/*
void setKeyboardTracking ( bool kt )
*/
HB_FUNC( QT_QABSTRACTSPINBOX_SETKEYBOARDTRACKING )
{
  hbqt_par_QAbstractSpinBox( 1 )->setKeyboardTracking( hb_parl( 2 ) );
}

/*
void setReadOnly ( bool r )
*/
HB_FUNC( QT_QABSTRACTSPINBOX_SETREADONLY )
{
  hbqt_par_QAbstractSpinBox( 1 )->setReadOnly( hb_parl( 2 ) );
}

/*
void setSpecialValueText ( const QString & txt )
*/
HB_FUNC( QT_QABSTRACTSPINBOX_SETSPECIALVALUETEXT )
{
  QString par2 = hb_parc(2);
  hbqt_par_QAbstractSpinBox( 1 )->setSpecialValueText( par2 );
}

/*
void setWrapping ( bool w )
*/
HB_FUNC( QT_QABSTRACTSPINBOX_SETWRAPPING )
{
  hbqt_par_QAbstractSpinBox( 1 )->setWrapping( hb_parl( 2 ) );
}

/*
QString specialValueText () const
*/
HB_FUNC( QT_QABSTRACTSPINBOX_SPECIALVALUETEXT )
{
  hb_retc( hbqt_par_QAbstractSpinBox( 1 )->specialValueText().toLatin1().data() );
}

/*
virtual void stepBy ( int steps )
*/
HB_FUNC( QT_QABSTRACTSPINBOX_STEPBY )
{
  hbqt_par_QAbstractSpinBox( 1 )->stepBy( hb_parni(2) );
}

/*
QString text () const
*/
HB_FUNC( QT_QABSTRACTSPINBOX_TEXT )
{
  hb_retc( hbqt_par_QAbstractSpinBox( 1 )->text().toLatin1().data() );
}

/*
virtual QValidator::State validate ( QString & input, int & pos ) const
*/
HB_FUNC( QT_QABSTRACTSPINBOX_VALIDATE )
{
  //hb_retni( hbqt_par_QAbstractSpinBox( 1 )->validate( hbqt_par_QString( 2 ), hb_parni( 3 ) ) );
  QAbstractSpinBox * par1 = (QAbstractSpinBox *) hb_parptr(1);
  QString par2 = hb_parc(2);
  int par3 = hb_parni(3);
  int i = par1->validate( par2, par3 );
  hb_retni( i );
}

/*
bool wrapping () const
*/
HB_FUNC( QT_QABSTRACTSPINBOX_WRAPPING )
{
  hb_retl( hbqt_par_QAbstractSpinBox( 1 )->wrapping() );
}

/*
virtual void clear ()
*/
HB_FUNC( QT_QABSTRACTSPINBOX_CLEAR )
{
  hbqt_par_QAbstractSpinBox( 1 )->clear();
}

/*
void selectAll ()
*/
HB_FUNC( QT_QABSTRACTSPINBOX_SELECTALL )
{
  hbqt_par_QAbstractSpinBox( 1 )->selectAll();
}

/*
void stepDown ()
*/
HB_FUNC( QT_QABSTRACTSPINBOX_STEPDOWN )
{
  hbqt_par_QAbstractSpinBox( 1 )->stepDown();
}

/*
void stepUp ()
*/
HB_FUNC( QT_QABSTRACTSPINBOX_STEPUP )
{
  hbqt_par_QAbstractSpinBox( 1 )->stepUp();
}

/*----------------------------------------------------------------------*/
#endif
