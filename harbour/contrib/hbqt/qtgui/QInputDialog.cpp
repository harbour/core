/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

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
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum InputDialogOption { NoButtons, UseListViewForComboBoxItems }
 *  enum InputMode { TextInput, IntInput, DoubleInput }
 *  flags InputDialogOptions
 */

#include <QtCore/QPointer>

#include <QtGui/QInputDialog>


/*
 * QInputDialog ( QWidget * parent = 0, Qt::WindowFlags flags = 0 )
 * ~QInputDialog ()
 */

QT_G_FUNC( release_QInputDialog )
{
#if defined(__debug__)
   hb_snprintf( str, sizeof(str), "release_QInputDialog" );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      const QMetaObject * m = ( ( QObject * ) ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         delete ( ( QInputDialog * ) ph );
         ph = NULL;
      }
      else
      {
#if defined(__debug__)
   hb_snprintf( str, sizeof(str), "  Object Name Missing: QInputDialog" );  OutputDebugString( str );
#endif
      }
   }
}

HB_FUNC( QT_QINPUTDIALOG )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAlloc( sizeof( QGC_POINTER ), Q_release );
   QPointer< QInputDialog > pObj = NULL;

   pObj = ( QInputDialog * ) new QInputDialog( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) ;

   p->ph = pObj;
   p->func = release_QInputDialog;

   hb_retptrGC( p );
}
/*
 * QString cancelButtonText () const
 */
HB_FUNC( QT_QINPUTDIALOG_CANCELBUTTONTEXT )
{
   hb_retc( hbqt_par_QInputDialog( 1 )->cancelButtonText().toAscii().data() );
}

/*
 * QStringList comboBoxItems () const
 */
HB_FUNC( QT_QINPUTDIALOG_COMBOBOXITEMS )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QStringList( hbqt_par_QInputDialog( 1 )->comboBoxItems() ), release_QStringList ) );
}

/*
 * virtual void done ( int result )
 */
HB_FUNC( QT_QINPUTDIALOG_DONE )
{
   hbqt_par_QInputDialog( 1 )->done( hb_parni( 2 ) );
}

/*
 * int doubleDecimals () const
 */
HB_FUNC( QT_QINPUTDIALOG_DOUBLEDECIMALS )
{
   hb_retni( hbqt_par_QInputDialog( 1 )->doubleDecimals() );
}

/*
 * double doubleMaximum () const
 */
HB_FUNC( QT_QINPUTDIALOG_DOUBLEMAXIMUM )
{
   hb_retnd( hbqt_par_QInputDialog( 1 )->doubleMaximum() );
}

/*
 * double doubleMinimum () const
 */
HB_FUNC( QT_QINPUTDIALOG_DOUBLEMINIMUM )
{
   hb_retnd( hbqt_par_QInputDialog( 1 )->doubleMinimum() );
}

/*
 * double doubleValue () const
 */
HB_FUNC( QT_QINPUTDIALOG_DOUBLEVALUE )
{
   hb_retnd( hbqt_par_QInputDialog( 1 )->doubleValue() );
}

/*
 * InputMode inputMode () const
 */
HB_FUNC( QT_QINPUTDIALOG_INPUTMODE )
{
   hb_retni( ( QInputDialog::InputMode ) hbqt_par_QInputDialog( 1 )->inputMode() );
}

/*
 * int intMaximum () const
 */
HB_FUNC( QT_QINPUTDIALOG_INTMAXIMUM )
{
   hb_retni( hbqt_par_QInputDialog( 1 )->intMaximum() );
}

/*
 * int intMinimum () const
 */
HB_FUNC( QT_QINPUTDIALOG_INTMINIMUM )
{
   hb_retni( hbqt_par_QInputDialog( 1 )->intMinimum() );
}

/*
 * int intStep () const
 */
HB_FUNC( QT_QINPUTDIALOG_INTSTEP )
{
   hb_retni( hbqt_par_QInputDialog( 1 )->intStep() );
}

/*
 * int intValue () const
 */
HB_FUNC( QT_QINPUTDIALOG_INTVALUE )
{
   hb_retni( hbqt_par_QInputDialog( 1 )->intValue() );
}

/*
 * bool isComboBoxEditable () const
 */
HB_FUNC( QT_QINPUTDIALOG_ISCOMBOBOXEDITABLE )
{
   hb_retl( hbqt_par_QInputDialog( 1 )->isComboBoxEditable() );
}

/*
 * QString labelText () const
 */
HB_FUNC( QT_QINPUTDIALOG_LABELTEXT )
{
   hb_retc( hbqt_par_QInputDialog( 1 )->labelText().toAscii().data() );
}

/*
 * QString okButtonText () const
 */
HB_FUNC( QT_QINPUTDIALOG_OKBUTTONTEXT )
{
   hb_retc( hbqt_par_QInputDialog( 1 )->okButtonText().toAscii().data() );
}

/*
 * void open ( QObject * receiver, const char * member )
 */
HB_FUNC( QT_QINPUTDIALOG_OPEN )
{
   hbqt_par_QInputDialog( 1 )->open( hbqt_par_QObject( 2 ), hbqt_par_char( 3 ) );
}

/*
 * InputDialogOptions options () const
 */
HB_FUNC( QT_QINPUTDIALOG_OPTIONS )
{
   hb_retni( ( QInputDialog::InputDialogOptions ) hbqt_par_QInputDialog( 1 )->options() );
}

/*
 * void setCancelButtonText ( const QString & text )
 */
HB_FUNC( QT_QINPUTDIALOG_SETCANCELBUTTONTEXT )
{
   hbqt_par_QInputDialog( 1 )->setCancelButtonText( hbqt_par_QString( 2 ) );
}

/*
 * void setComboBoxEditable ( bool editable )
 */
HB_FUNC( QT_QINPUTDIALOG_SETCOMBOBOXEDITABLE )
{
   hbqt_par_QInputDialog( 1 )->setComboBoxEditable( hb_parl( 2 ) );
}

/*
 * void setComboBoxItems ( const QStringList & items )
 */
HB_FUNC( QT_QINPUTDIALOG_SETCOMBOBOXITEMS )
{
   hbqt_par_QInputDialog( 1 )->setComboBoxItems( *hbqt_par_QStringList( 2 ) );
}

/*
 * void setDoubleDecimals ( int decimals )
 */
HB_FUNC( QT_QINPUTDIALOG_SETDOUBLEDECIMALS )
{
   hbqt_par_QInputDialog( 1 )->setDoubleDecimals( hb_parni( 2 ) );
}

/*
 * void setDoubleMaximum ( double max )
 */
HB_FUNC( QT_QINPUTDIALOG_SETDOUBLEMAXIMUM )
{
   hbqt_par_QInputDialog( 1 )->setDoubleMaximum( hb_parnd( 2 ) );
}

/*
 * void setDoubleMinimum ( double min )
 */
HB_FUNC( QT_QINPUTDIALOG_SETDOUBLEMINIMUM )
{
   hbqt_par_QInputDialog( 1 )->setDoubleMinimum( hb_parnd( 2 ) );
}

/*
 * void setDoubleRange ( double min, double max )
 */
HB_FUNC( QT_QINPUTDIALOG_SETDOUBLERANGE )
{
   hbqt_par_QInputDialog( 1 )->setDoubleRange( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/*
 * void setDoubleValue ( double value )
 */
HB_FUNC( QT_QINPUTDIALOG_SETDOUBLEVALUE )
{
   hbqt_par_QInputDialog( 1 )->setDoubleValue( hb_parnd( 2 ) );
}

/*
 * void setInputMode ( InputMode mode )
 */
HB_FUNC( QT_QINPUTDIALOG_SETINPUTMODE )
{
   hbqt_par_QInputDialog( 1 )->setInputMode( ( QInputDialog::InputMode ) hb_parni( 2 ) );
}

/*
 * void setIntMaximum ( int max )
 */
HB_FUNC( QT_QINPUTDIALOG_SETINTMAXIMUM )
{
   hbqt_par_QInputDialog( 1 )->setIntMaximum( hb_parni( 2 ) );
}

/*
 * void setIntMinimum ( int min )
 */
HB_FUNC( QT_QINPUTDIALOG_SETINTMINIMUM )
{
   hbqt_par_QInputDialog( 1 )->setIntMinimum( hb_parni( 2 ) );
}

/*
 * void setIntRange ( int min, int max )
 */
HB_FUNC( QT_QINPUTDIALOG_SETINTRANGE )
{
   hbqt_par_QInputDialog( 1 )->setIntRange( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setIntStep ( int step )
 */
HB_FUNC( QT_QINPUTDIALOG_SETINTSTEP )
{
   hbqt_par_QInputDialog( 1 )->setIntStep( hb_parni( 2 ) );
}

/*
 * void setIntValue ( int value )
 */
HB_FUNC( QT_QINPUTDIALOG_SETINTVALUE )
{
   hbqt_par_QInputDialog( 1 )->setIntValue( hb_parni( 2 ) );
}

/*
 * void setLabelText ( const QString & text )
 */
HB_FUNC( QT_QINPUTDIALOG_SETLABELTEXT )
{
   hbqt_par_QInputDialog( 1 )->setLabelText( hbqt_par_QString( 2 ) );
}

/*
 * void setOkButtonText ( const QString & text )
 */
HB_FUNC( QT_QINPUTDIALOG_SETOKBUTTONTEXT )
{
   hbqt_par_QInputDialog( 1 )->setOkButtonText( hbqt_par_QString( 2 ) );
}

/*
 * void setOption ( InputDialogOption option, bool on = true )
 */
HB_FUNC( QT_QINPUTDIALOG_SETOPTION )
{
   hbqt_par_QInputDialog( 1 )->setOption( ( QInputDialog::InputDialogOption ) hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setOptions ( InputDialogOptions options )
 */
HB_FUNC( QT_QINPUTDIALOG_SETOPTIONS )
{
   hbqt_par_QInputDialog( 1 )->setOptions( ( QInputDialog::InputDialogOptions ) hb_parni( 2 ) );
}

/*
 * void setTextEchoMode ( QLineEdit::EchoMode mode )
 */
HB_FUNC( QT_QINPUTDIALOG_SETTEXTECHOMODE )
{
   hbqt_par_QInputDialog( 1 )->setTextEchoMode( ( QLineEdit::EchoMode ) hb_parni( 2 ) );
}

/*
 * void setTextValue ( const QString & text )
 */
HB_FUNC( QT_QINPUTDIALOG_SETTEXTVALUE )
{
   hbqt_par_QInputDialog( 1 )->setTextValue( hbqt_par_QString( 2 ) );
}

/*
 * bool testOption ( InputDialogOption option ) const
 */
HB_FUNC( QT_QINPUTDIALOG_TESTOPTION )
{
   hb_retl( hbqt_par_QInputDialog( 1 )->testOption( ( QInputDialog::InputDialogOption ) hb_parni( 2 ) ) );
}

/*
 * QLineEdit::EchoMode textEchoMode () const
 */
HB_FUNC( QT_QINPUTDIALOG_TEXTECHOMODE )
{
   hb_retni( ( QLineEdit::EchoMode ) hbqt_par_QInputDialog( 1 )->textEchoMode() );
}

/*
 * QString textValue () const
 */
HB_FUNC( QT_QINPUTDIALOG_TEXTVALUE )
{
   hb_retc( hbqt_par_QInputDialog( 1 )->textValue().toAscii().data() );
}

/*
 * double getDouble ( QWidget * parent, const QString & title, const QString & label, double value = 0, double min = -2147483647, double max = 2147483647, int decimals = 1, bool * ok = 0, Qt::WindowFlags flags = 0 )
 */
HB_FUNC( QT_QINPUTDIALOG_GETDOUBLE )
{
   bool iOk = 0;

   hb_retnd( hbqt_par_QInputDialog( 1 )->getDouble( hbqt_par_QWidget( 2 ), hbqt_par_QString( 3 ), hbqt_par_QString( 4 ), hb_parnd( 5 ), hb_parnd( 6 ), hb_parnd( 7 ), ( HB_ISNUM( 8 ) ? hb_parni( 8 ) : 1 ), &iOk, ( Qt::WindowFlags ) hb_parni( 10 ) ) );

   hb_stornl( iOk, 9 );
}

/*
 * int getInt ( QWidget * parent, const QString & title, const QString & label, int value = 0, int min = -2147483647, int max = 2147483647, int step = 1, bool * ok = 0, Qt::WindowFlags flags = 0 )
 */
HB_FUNC( QT_QINPUTDIALOG_GETINT )
{
   bool iOk = 0;

   hb_retni( hbqt_par_QInputDialog( 1 )->getInt( hbqt_par_QWidget( 2 ), hbqt_par_QString( 3 ), hbqt_par_QString( 4 ), hb_parni( 5 ), ( HB_ISNUM( 6 ) ? hb_parni( 6 ) : -2147483647 ), ( HB_ISNUM( 7 ) ? hb_parni( 7 ) : 2147483647 ), ( HB_ISNUM( 8 ) ? hb_parni( 8 ) : 1 ), &iOk, ( Qt::WindowFlags ) hb_parni( 10 ) ) );

   hb_stornl( iOk, 9 );
}

/*
 * QString getItem ( QWidget * parent, const QString & title, const QString & label, const QStringList & items, int current = 0, bool editable = true, bool * ok = 0, Qt::WindowFlags flags = 0 )
 */
HB_FUNC( QT_QINPUTDIALOG_GETITEM )
{
   bool iOk = 0;

   hb_retc( hbqt_par_QInputDialog( 1 )->getItem( hbqt_par_QWidget( 2 ), hbqt_par_QString( 3 ), hbqt_par_QString( 4 ), *hbqt_par_QStringList( 5 ), hb_parni( 6 ), hb_parl( 7 ), &iOk, ( Qt::WindowFlags ) hb_parni( 9 ) ).toAscii().data() );

   hb_stornl( iOk, 8 );
}

/*
 * QString getText ( QWidget * parent, const QString & title, const QString & label, QLineEdit::EchoMode mode = QLineEdit::Normal, const QString & text = QString(), bool * ok = 0, Qt::WindowFlags flags = 0 )
 */
HB_FUNC( QT_QINPUTDIALOG_GETTEXT )
{
   bool iOk = 0;

   hb_retc( hbqt_par_QInputDialog( 1 )->getText( hbqt_par_QWidget( 2 ), hbqt_par_QString( 3 ), hbqt_par_QString( 4 ), ( HB_ISNUM( 5 ) ? ( QLineEdit::EchoMode ) hb_parni( 5 ) : ( QLineEdit::EchoMode ) QLineEdit::Normal ), hbqt_par_QString( 6 ), &iOk, ( Qt::WindowFlags ) hb_parni( 8 ) ).toAscii().data() );

   hb_stornl( iOk, 7 );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
