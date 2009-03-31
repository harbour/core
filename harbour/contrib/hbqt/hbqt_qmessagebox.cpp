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
 *  Constructed[ 29/32 [ 90.63% ] ]
 *  
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *  
 *  QList<QAbstractButton *> buttons () const
 *  QPixmap iconPixmap () const
 *  void setIconPixmap ( const QPixmap & pixmap )
 */ 


#include <QtGui/QMessageBox>


/*
 * QMessageBox ( QWidget * parent = 0 )
 * QMessageBox ( Icon icon, const QString & title, const QString & text, StandardButtons buttons = NoButton, QWidget * parent = 0, Qt::WindowFlags f = Qt::Dialog | Qt::MSWindowsFixedSizeDialogHint )
 * ~QMessageBox ()
 */
HB_FUNC( QT_QMESSAGEBOX )
{
   hb_retptr( ( QMessageBox* ) new QMessageBox() );
}

/*
 * void addButton ( QAbstractButton * button, ButtonRole role )
 */
HB_FUNC( QT_QMESSAGEBOX_ADDBUTTON )
{
   hbqt_par_QMessageBox( 1 )->addButton( hbqt_par_QAbstractButton( 2 ), ( QMessageBox::ButtonRole ) hb_parni( 3 ) );
}

/*
 * QPushButton * addButton ( const QString & text, ButtonRole role )
 */
HB_FUNC( QT_QMESSAGEBOX_ADDBUTTON_1 )
{
   hb_retptr( ( QPushButton* ) hbqt_par_QMessageBox( 1 )->addButton( hbqt_par_QString( 2 ), ( QMessageBox::ButtonRole ) hb_parni( 3 ) ) );
}

/*
 * QPushButton * addButton ( StandardButton button )
 */
HB_FUNC( QT_QMESSAGEBOX_ADDBUTTON_2 )
{
   hb_retptr( ( QPushButton* ) hbqt_par_QMessageBox( 1 )->addButton( ( QMessageBox::StandardButton ) hb_parni( 2 ) ) );
}

/*
 * QAbstractButton * button ( StandardButton which ) const
 */
HB_FUNC( QT_QMESSAGEBOX_BUTTON )
{
   hb_retptr( ( QAbstractButton* ) hbqt_par_QMessageBox( 1 )->button( ( QMessageBox::StandardButton ) hb_parni( 2 ) ) );
}

/*
 * ButtonRole buttonRole ( QAbstractButton * button ) const
 */
HB_FUNC( QT_QMESSAGEBOX_BUTTONROLE )
{
   hb_retni( hbqt_par_QMessageBox( 1 )->buttonRole( hbqt_par_QAbstractButton( 2 ) ) );
}

/*
 * QAbstractButton * clickedButton () const
 */
HB_FUNC( QT_QMESSAGEBOX_CLICKEDBUTTON )
{
   hb_retptr( ( QAbstractButton* ) hbqt_par_QMessageBox( 1 )->clickedButton(  ) );
}

/*
 * QPushButton * defaultButton () const
 */
HB_FUNC( QT_QMESSAGEBOX_DEFAULTBUTTON )
{
   hb_retptr( ( QPushButton* ) hbqt_par_QMessageBox( 1 )->defaultButton(  ) );
}

/*
 * QString detailedText () const
 */
HB_FUNC( QT_QMESSAGEBOX_DETAILEDTEXT )
{
   hb_retc( hbqt_par_QMessageBox( 1 )->detailedText( ).toLatin1().data() );
}

/*
 * QAbstractButton * escapeButton () const
 */
HB_FUNC( QT_QMESSAGEBOX_ESCAPEBUTTON )
{
   hb_retptr( ( QAbstractButton* ) hbqt_par_QMessageBox( 1 )->escapeButton(  ) );
}

/*
 * Icon icon () const
 */
HB_FUNC( QT_QMESSAGEBOX_ICON )
{
   hb_retni( hbqt_par_QMessageBox( 1 )->icon(  ) );
}

/*
 * QString informativeText () const
 */
HB_FUNC( QT_QMESSAGEBOX_INFORMATIVETEXT )
{
   hb_retc( hbqt_par_QMessageBox( 1 )->informativeText( ).toLatin1().data() );
}

/*
 * void open ( QObject * receiver, const char * member )
 */
HB_FUNC( QT_QMESSAGEBOX_OPEN )
{
   hbqt_par_QMessageBox( 1 )->open( hbqt_par_QObject( 2 ), hbqt_par_char( 3 ) );
}

/*
 * void removeButton ( QAbstractButton * button )
 */
HB_FUNC( QT_QMESSAGEBOX_REMOVEBUTTON )
{
   hbqt_par_QMessageBox( 1 )->removeButton( hbqt_par_QAbstractButton( 2 ) );
}

/*
 * void setDefaultButton ( QPushButton * button )
 */
HB_FUNC( QT_QMESSAGEBOX_SETDEFAULTBUTTON )
{
   hbqt_par_QMessageBox( 1 )->setDefaultButton( hbqt_par_QPushButton( 2 ) );
}

/*
 * void setDefaultButton ( StandardButton button )
 */
HB_FUNC( QT_QMESSAGEBOX_SETDEFAULTBUTTON_1 )
{
   hbqt_par_QMessageBox( 1 )->setDefaultButton( ( QMessageBox::StandardButton ) hb_parni( 2 ) );
}

/*
 * void setDetailedText ( const QString & text )
 */
HB_FUNC( QT_QMESSAGEBOX_SETDETAILEDTEXT )
{
   hbqt_par_QMessageBox( 1 )->setDetailedText( hbqt_par_QString( 2 ) );
}

/*
 * void setEscapeButton ( QAbstractButton * button )
 */
HB_FUNC( QT_QMESSAGEBOX_SETESCAPEBUTTON )
{
   hbqt_par_QMessageBox( 1 )->setEscapeButton( hbqt_par_QAbstractButton( 2 ) );
}

/*
 * void setEscapeButton ( StandardButton button )
 */
HB_FUNC( QT_QMESSAGEBOX_SETESCAPEBUTTON_1 )
{
   hbqt_par_QMessageBox( 1 )->setEscapeButton( ( QMessageBox::StandardButton ) hb_parni( 2 ) );
}

/*
 * void setIcon ( Icon )
 */
HB_FUNC( QT_QMESSAGEBOX_SETICON )
{
   hbqt_par_QMessageBox( 1 )->setIcon( ( QMessageBox::Icon ) hb_parni( 2 ) );
}

/*
 * void setInformativeText ( const QString & text )
 */
HB_FUNC( QT_QMESSAGEBOX_SETINFORMATIVETEXT )
{
   hbqt_par_QMessageBox( 1 )->setInformativeText( hbqt_par_QString( 2 ) );
}

/*
 * void setStandardButtons ( StandardButtons buttons )
 */
HB_FUNC( QT_QMESSAGEBOX_SETSTANDARDBUTTONS )
{
   hbqt_par_QMessageBox( 1 )->setStandardButtons( ( QMessageBox::StandardButtons ) hb_parni( 2 ) );
}

/*
 * void setText ( const QString & text )
 */
HB_FUNC( QT_QMESSAGEBOX_SETTEXT )
{
   hbqt_par_QMessageBox( 1 )->setText( hbqt_par_QString( 2 ) );
}

/*
 * void setTextFormat ( Qt::TextFormat format )
 */
HB_FUNC( QT_QMESSAGEBOX_SETTEXTFORMAT )
{
   hbqt_par_QMessageBox( 1 )->setTextFormat( ( Qt::TextFormat ) hb_parni( 2 ) );
}

/*
 * void setWindowModality ( Qt::WindowModality windowModality )
 */
HB_FUNC( QT_QMESSAGEBOX_SETWINDOWMODALITY )
{
   hbqt_par_QMessageBox( 1 )->setWindowModality( ( Qt::WindowModality ) hb_parni( 2 ) );
}

/*
 * void setWindowTitle ( const QString & title )
 */
HB_FUNC( QT_QMESSAGEBOX_SETWINDOWTITLE )
{
   hbqt_par_QMessageBox( 1 )->setWindowTitle( hbqt_par_QString( 2 ) );
}

/*
 * StandardButton standardButton ( QAbstractButton * button ) const
 */
HB_FUNC( QT_QMESSAGEBOX_STANDARDBUTTON )
{
   hb_retni( hbqt_par_QMessageBox( 1 )->standardButton( hbqt_par_QAbstractButton( 2 ) ) );
}

/*
 * StandardButtons standardButtons () const
 */
HB_FUNC( QT_QMESSAGEBOX_STANDARDBUTTONS )
{
   hb_retni( hbqt_par_QMessageBox( 1 )->standardButtons(  ) );
}

/*
 * QString text () const
 */
HB_FUNC( QT_QMESSAGEBOX_TEXT )
{
   hb_retc( hbqt_par_QMessageBox( 1 )->text( ).toLatin1().data() );
}

/*
 * Qt::TextFormat textFormat () const
 */
HB_FUNC( QT_QMESSAGEBOX_TEXTFORMAT )
{
   hb_retni( hbqt_par_QMessageBox( 1 )->textFormat(  ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

