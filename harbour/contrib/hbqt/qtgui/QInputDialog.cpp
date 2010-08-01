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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://harbour-project.org
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

#include "../hbqt.h"
#include "hbqtgui_garbage.h"
#include "hbqtcore_garbage.h"

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

typedef struct
{
   QPointer< QInputDialog > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QInputDialog;

QT_G_FUNC( hbqt_gcRelease_QInputDialog )
{
   QInputDialog  * ph = NULL ;
   QGC_POINTER_QInputDialog * p = ( QGC_POINTER_QInputDialog * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QInputDialog   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QInputDialog   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QInputDialog          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QInputDialog    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QInputDialog    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QInputDialog( void * pObj, bool bNew )
{
   QGC_POINTER_QInputDialog * p = ( QGC_POINTER_QInputDialog * ) hb_gcAllocate( sizeof( QGC_POINTER_QInputDialog ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QInputDialog >( ( QInputDialog * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QInputDialog;
   p->type = HBQT_TYPE_QInputDialog;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QInputDialog  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QInputDialog", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QINPUTDIALOG )
{
   QInputDialog * pObj = NULL;

   pObj = ( QInputDialog * ) new QInputDialog( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QInputDialog( ( void * ) pObj, true ) );
}

/*
 * QString cancelButtonText () const
 */
HB_FUNC( QT_QINPUTDIALOG_CANCELBUTTONTEXT )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retc( ( p )->cancelButtonText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_CANCELBUTTONTEXT FP=hb_retc( ( p )->cancelButtonText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QStringList comboBoxItems () const
 */
HB_FUNC( QT_QINPUTDIALOG_COMBOBOXITEMS )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->comboBoxItems() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_COMBOBOXITEMS FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->comboBoxItems() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual void done ( int result )
 */
HB_FUNC( QT_QINPUTDIALOG_DONE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->done( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_DONE FP=( p )->done( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * int doubleDecimals () const
 */
HB_FUNC( QT_QINPUTDIALOG_DOUBLEDECIMALS )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retni( ( p )->doubleDecimals() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_DOUBLEDECIMALS FP=hb_retni( ( p )->doubleDecimals() ); p is NULL" ) );
   }
}

/*
 * double doubleMaximum () const
 */
HB_FUNC( QT_QINPUTDIALOG_DOUBLEMAXIMUM )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retnd( ( p )->doubleMaximum() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_DOUBLEMAXIMUM FP=hb_retnd( ( p )->doubleMaximum() ); p is NULL" ) );
   }
}

/*
 * double doubleMinimum () const
 */
HB_FUNC( QT_QINPUTDIALOG_DOUBLEMINIMUM )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retnd( ( p )->doubleMinimum() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_DOUBLEMINIMUM FP=hb_retnd( ( p )->doubleMinimum() ); p is NULL" ) );
   }
}

/*
 * double doubleValue () const
 */
HB_FUNC( QT_QINPUTDIALOG_DOUBLEVALUE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retnd( ( p )->doubleValue() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_DOUBLEVALUE FP=hb_retnd( ( p )->doubleValue() ); p is NULL" ) );
   }
}

/*
 * InputMode inputMode () const
 */
HB_FUNC( QT_QINPUTDIALOG_INPUTMODE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retni( ( QInputDialog::InputMode ) ( p )->inputMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_INPUTMODE FP=hb_retni( ( QInputDialog::InputMode ) ( p )->inputMode() ); p is NULL" ) );
   }
}

/*
 * int intMaximum () const
 */
HB_FUNC( QT_QINPUTDIALOG_INTMAXIMUM )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retni( ( p )->intMaximum() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_INTMAXIMUM FP=hb_retni( ( p )->intMaximum() ); p is NULL" ) );
   }
}

/*
 * int intMinimum () const
 */
HB_FUNC( QT_QINPUTDIALOG_INTMINIMUM )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retni( ( p )->intMinimum() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_INTMINIMUM FP=hb_retni( ( p )->intMinimum() ); p is NULL" ) );
   }
}

/*
 * int intStep () const
 */
HB_FUNC( QT_QINPUTDIALOG_INTSTEP )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retni( ( p )->intStep() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_INTSTEP FP=hb_retni( ( p )->intStep() ); p is NULL" ) );
   }
}

/*
 * int intValue () const
 */
HB_FUNC( QT_QINPUTDIALOG_INTVALUE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retni( ( p )->intValue() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_INTVALUE FP=hb_retni( ( p )->intValue() ); p is NULL" ) );
   }
}

/*
 * bool isComboBoxEditable () const
 */
HB_FUNC( QT_QINPUTDIALOG_ISCOMBOBOXEDITABLE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retl( ( p )->isComboBoxEditable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_ISCOMBOBOXEDITABLE FP=hb_retl( ( p )->isComboBoxEditable() ); p is NULL" ) );
   }
}

/*
 * QString labelText () const
 */
HB_FUNC( QT_QINPUTDIALOG_LABELTEXT )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retc( ( p )->labelText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_LABELTEXT FP=hb_retc( ( p )->labelText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString okButtonText () const
 */
HB_FUNC( QT_QINPUTDIALOG_OKBUTTONTEXT )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retc( ( p )->okButtonText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_OKBUTTONTEXT FP=hb_retc( ( p )->okButtonText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void open ( QObject * receiver, const char * member )
 */
HB_FUNC( QT_QINPUTDIALOG_OPEN )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->open( hbqt_par_QObject( 2 ), hbqt_par_char( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_OPEN FP=( p )->open( hbqt_par_QObject( 2 ), hbqt_par_char( 3 ) ); p is NULL" ) );
   }
}

/*
 * InputDialogOptions options () const
 */
HB_FUNC( QT_QINPUTDIALOG_OPTIONS )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retni( ( QInputDialog::InputDialogOptions ) ( p )->options() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_OPTIONS FP=hb_retni( ( QInputDialog::InputDialogOptions ) ( p )->options() ); p is NULL" ) );
   }
}

/*
 * void setCancelButtonText ( const QString & text )
 */
HB_FUNC( QT_QINPUTDIALOG_SETCANCELBUTTONTEXT )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setCancelButtonText( QInputDialog::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_SETCANCELBUTTONTEXT FP=( p )->setCancelButtonText( QInputDialog::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setComboBoxEditable ( bool editable )
 */
HB_FUNC( QT_QINPUTDIALOG_SETCOMBOBOXEDITABLE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setComboBoxEditable( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_SETCOMBOBOXEDITABLE FP=( p )->setComboBoxEditable( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setComboBoxItems ( const QStringList & items )
 */
HB_FUNC( QT_QINPUTDIALOG_SETCOMBOBOXITEMS )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setComboBoxItems( *hbqt_par_QStringList( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_SETCOMBOBOXITEMS FP=( p )->setComboBoxItems( *hbqt_par_QStringList( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDoubleDecimals ( int decimals )
 */
HB_FUNC( QT_QINPUTDIALOG_SETDOUBLEDECIMALS )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setDoubleDecimals( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_SETDOUBLEDECIMALS FP=( p )->setDoubleDecimals( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDoubleMaximum ( double max )
 */
HB_FUNC( QT_QINPUTDIALOG_SETDOUBLEMAXIMUM )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setDoubleMaximum( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_SETDOUBLEMAXIMUM FP=( p )->setDoubleMaximum( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDoubleMinimum ( double min )
 */
HB_FUNC( QT_QINPUTDIALOG_SETDOUBLEMINIMUM )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setDoubleMinimum( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_SETDOUBLEMINIMUM FP=( p )->setDoubleMinimum( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDoubleRange ( double min, double max )
 */
HB_FUNC( QT_QINPUTDIALOG_SETDOUBLERANGE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setDoubleRange( hb_parnd( 2 ), hb_parnd( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_SETDOUBLERANGE FP=( p )->setDoubleRange( hb_parnd( 2 ), hb_parnd( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setDoubleValue ( double value )
 */
HB_FUNC( QT_QINPUTDIALOG_SETDOUBLEVALUE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setDoubleValue( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_SETDOUBLEVALUE FP=( p )->setDoubleValue( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setInputMode ( InputMode mode )
 */
HB_FUNC( QT_QINPUTDIALOG_SETINPUTMODE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setInputMode( ( QInputDialog::InputMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_SETINPUTMODE FP=( p )->setInputMode( ( QInputDialog::InputMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setIntMaximum ( int max )
 */
HB_FUNC( QT_QINPUTDIALOG_SETINTMAXIMUM )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setIntMaximum( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_SETINTMAXIMUM FP=( p )->setIntMaximum( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setIntMinimum ( int min )
 */
HB_FUNC( QT_QINPUTDIALOG_SETINTMINIMUM )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setIntMinimum( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_SETINTMINIMUM FP=( p )->setIntMinimum( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setIntRange ( int min, int max )
 */
HB_FUNC( QT_QINPUTDIALOG_SETINTRANGE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setIntRange( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_SETINTRANGE FP=( p )->setIntRange( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setIntStep ( int step )
 */
HB_FUNC( QT_QINPUTDIALOG_SETINTSTEP )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setIntStep( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_SETINTSTEP FP=( p )->setIntStep( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setIntValue ( int value )
 */
HB_FUNC( QT_QINPUTDIALOG_SETINTVALUE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setIntValue( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_SETINTVALUE FP=( p )->setIntValue( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setLabelText ( const QString & text )
 */
HB_FUNC( QT_QINPUTDIALOG_SETLABELTEXT )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setLabelText( QInputDialog::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_SETLABELTEXT FP=( p )->setLabelText( QInputDialog::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setOkButtonText ( const QString & text )
 */
HB_FUNC( QT_QINPUTDIALOG_SETOKBUTTONTEXT )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setOkButtonText( QInputDialog::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_SETOKBUTTONTEXT FP=( p )->setOkButtonText( QInputDialog::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setOption ( InputDialogOption option, bool on = true )
 */
HB_FUNC( QT_QINPUTDIALOG_SETOPTION )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setOption( ( QInputDialog::InputDialogOption ) hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_SETOPTION FP=( p )->setOption( ( QInputDialog::InputDialogOption ) hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setOptions ( InputDialogOptions options )
 */
HB_FUNC( QT_QINPUTDIALOG_SETOPTIONS )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setOptions( ( QInputDialog::InputDialogOptions ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_SETOPTIONS FP=( p )->setOptions( ( QInputDialog::InputDialogOptions ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTextEchoMode ( QLineEdit::EchoMode mode )
 */
HB_FUNC( QT_QINPUTDIALOG_SETTEXTECHOMODE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setTextEchoMode( ( QLineEdit::EchoMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_SETTEXTECHOMODE FP=( p )->setTextEchoMode( ( QLineEdit::EchoMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTextValue ( const QString & text )
 */
HB_FUNC( QT_QINPUTDIALOG_SETTEXTVALUE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      ( p )->setTextValue( QInputDialog::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_SETTEXTVALUE FP=( p )->setTextValue( QInputDialog::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool testOption ( InputDialogOption option ) const
 */
HB_FUNC( QT_QINPUTDIALOG_TESTOPTION )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retl( ( p )->testOption( ( QInputDialog::InputDialogOption ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_TESTOPTION FP=hb_retl( ( p )->testOption( ( QInputDialog::InputDialogOption ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QLineEdit::EchoMode textEchoMode () const
 */
HB_FUNC( QT_QINPUTDIALOG_TEXTECHOMODE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retni( ( QLineEdit::EchoMode ) ( p )->textEchoMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_TEXTECHOMODE FP=hb_retni( ( QLineEdit::EchoMode ) ( p )->textEchoMode() ); p is NULL" ) );
   }
}

/*
 * QString textValue () const
 */
HB_FUNC( QT_QINPUTDIALOG_TEXTVALUE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   if( p )
      hb_retc( ( p )->textValue().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_TEXTVALUE FP=hb_retc( ( p )->textValue().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * double getDouble ( QWidget * parent, const QString & title, const QString & label, double value = 0, double min = -2147483647, double max = 2147483647, int decimals = 1, bool * ok = 0, Qt::WindowFlags flags = 0 )
 */
HB_FUNC( QT_QINPUTDIALOG_GETDOUBLE )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   bool iOk = 0;

   if( p )
      hb_retnd( ( p )->getDouble( hbqt_par_QWidget( 2 ), QInputDialog::tr( hb_parc( 3 ) ), QInputDialog::tr( hb_parc( 4 ) ), hb_parnd( 5 ), hb_parnd( 6 ), hb_parnd( 7 ), hb_parnidef( 8, 1 ), &iOk, ( Qt::WindowFlags ) hb_parni( 10 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_GETDOUBLE FP=hb_retnd( ( p )->getDouble( hbqt_par_QWidget( 2 ), QInputDialog::tr( hb_parc( 3 ) ), QInputDialog::tr( hb_parc( 4 ) ), hb_parnd( 5 ), hb_parnd( 6 ), hb_parnd( 7 ), hb_parnidef( 8, 1 ), &iOk, ( Qt::WindowFlags ) hb_parni( 10 ) ) ); p is NULL" ) );
   }

   hb_stornl( iOk, 9 );
}

/*
 * int getInt ( QWidget * parent, const QString & title, const QString & label, int value = 0, int min = -2147483647, int max = 2147483647, int step = 1, bool * ok = 0, Qt::WindowFlags flags = 0 )
 */
HB_FUNC( QT_QINPUTDIALOG_GETINT )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   bool iOk = 0;

   if( p )
      hb_retni( ( p )->getInt( hbqt_par_QWidget( 2 ), QInputDialog::tr( hb_parc( 3 ) ), QInputDialog::tr( hb_parc( 4 ) ), hb_parni( 5 ), hb_parnidef( 6, -2147483647 ), hb_parnidef( 7, 2147483647 ), hb_parnidef( 8, 1 ), &iOk, ( Qt::WindowFlags ) hb_parni( 10 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_GETINT FP=hb_retni( ( p )->getInt( hbqt_par_QWidget( 2 ), QInputDialog::tr( hb_parc( 3 ) ), QInputDialog::tr( hb_parc( 4 ) ), hb_parni( 5 ), hb_parnidef( 6, -2147483647 ), hb_parnidef( 7, 2147483647 ), hb_parnidef( 8, 1 ), &iOk, ( Qt::WindowFlags ) hb_parni( 10 ) ) ); p is NULL" ) );
   }

   hb_stornl( iOk, 9 );
}

/*
 * QString getItem ( QWidget * parent, const QString & title, const QString & label, const QStringList & items, int current = 0, bool editable = true, bool * ok = 0, Qt::WindowFlags flags = 0 )
 */
HB_FUNC( QT_QINPUTDIALOG_GETITEM )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   bool iOk = 0;

   if( p )
      hb_retc( ( p )->getItem( hbqt_par_QWidget( 2 ), QInputDialog::tr( hb_parc( 3 ) ), QInputDialog::tr( hb_parc( 4 ) ), *hbqt_par_QStringList( 5 ), hb_parni( 6 ), hb_parl( 7 ), &iOk, ( Qt::WindowFlags ) hb_parni( 9 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_GETITEM FP=hb_retc( ( p )->getItem( hbqt_par_QWidget( 2 ), QInputDialog::tr( hb_parc( 3 ) ), QInputDialog::tr( hb_parc( 4 ) ), *hbqt_par_QStringList( 5 ), hb_parni( 6 ), hb_parl( 7 ), &iOk, ( Qt::WindowFlags ) hb_parni( 9 ) ).toAscii().data() ); p is NULL" ) );
   }

   hb_stornl( iOk, 8 );
}

/*
 * QString getText ( QWidget * parent, const QString & title, const QString & label, QLineEdit::EchoMode mode = QLineEdit::Normal, const QString & text = QString(), bool * ok = 0, Qt::WindowFlags flags = 0 )
 */
HB_FUNC( QT_QINPUTDIALOG_GETTEXT )
{
   QInputDialog * p = hbqt_par_QInputDialog( 1 );
   bool iOk = 0;

   if( p )
      hb_retc( ( p )->getText( hbqt_par_QWidget( 2 ), QInputDialog::tr( hb_parc( 3 ) ), QInputDialog::tr( hb_parc( 4 ) ), ( HB_ISNUM( 5 ) ? ( QLineEdit::EchoMode ) hb_parni( 5 ) : ( QLineEdit::EchoMode ) QLineEdit::Normal ), QInputDialog::tr( hb_parc( 6 ) ), &iOk, ( Qt::WindowFlags ) hb_parni( 8 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QINPUTDIALOG_GETTEXT FP=hb_retc( ( p )->getText( hbqt_par_QWidget( 2 ), QInputDialog::tr( hb_parc( 3 ) ), QInputDialog::tr( hb_parc( 4 ) ), ( HB_ISNUM( 5 ) ? ( QLineEdit::EchoMode ) hb_parni( 5 ) : ( QLineEdit::EchoMode ) QLineEdit::Normal ), QInputDialog::tr( hb_parc( 6 ) ), &iOk, ( Qt::WindowFlags ) hb_parni( 8 ) ).toAscii().data() ); p is NULL" ) );
   }

   hb_stornl( iOk, 7 );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
