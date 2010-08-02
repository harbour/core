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

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum PrintDialogOption { None, PrintToFile, PrintSelection, PrintPageRange, ..., PrintShowPageSize }
 *  enum PrintRange { AllPages, Selection, PageRange }
 *  flags PrintDialogOptions
 */

/*
 *  Constructed[ 10/11 [ 90.91% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  void setOptionTabs ( const QList<QWidget *> & tabs )
 */

#include <QtCore/QPointer>

#include <QtGui/QAbstractPrintDialog>


/*
 * QAbstractPrintDialog ( QPrinter * printer, QWidget * parent = 0 )
 */

typedef struct
{
   QPointer< QAbstractPrintDialog > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QAbstractPrintDialog;

QT_G_FUNC( hbqt_gcRelease_QAbstractPrintDialog )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QAbstractPrintDialog( void * pObj, bool bNew )
{
   QGC_POINTER_QAbstractPrintDialog * p = ( QGC_POINTER_QAbstractPrintDialog * ) hb_gcAllocate( sizeof( QGC_POINTER_QAbstractPrintDialog ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QAbstractPrintDialog >( ( QAbstractPrintDialog * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QAbstractPrintDialog;
   p->type = HBQT_TYPE_QAbstractPrintDialog;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QAbstractPrintDialog  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QAbstractPrintDialog", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QABSTRACTPRINTDIALOG )
{

}

/*
 * virtual int exec () = 0
 */
HB_FUNC( QT_QABSTRACTPRINTDIALOG_EXEC )
{
   QAbstractPrintDialog * p = hbqt_par_QAbstractPrintDialog( 1 );
   if( p )
      hb_retni( ( p )->exec() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTPRINTDIALOG_EXEC FP=hb_retni( ( p )->exec() ); p is NULL" ) );
   }
}

/*
 * int fromPage () const
 */
HB_FUNC( QT_QABSTRACTPRINTDIALOG_FROMPAGE )
{
   QAbstractPrintDialog * p = hbqt_par_QAbstractPrintDialog( 1 );
   if( p )
      hb_retni( ( p )->fromPage() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTPRINTDIALOG_FROMPAGE FP=hb_retni( ( p )->fromPage() ); p is NULL" ) );
   }
}

/*
 * int maxPage () const
 */
HB_FUNC( QT_QABSTRACTPRINTDIALOG_MAXPAGE )
{
   QAbstractPrintDialog * p = hbqt_par_QAbstractPrintDialog( 1 );
   if( p )
      hb_retni( ( p )->maxPage() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTPRINTDIALOG_MAXPAGE FP=hb_retni( ( p )->maxPage() ); p is NULL" ) );
   }
}

/*
 * int minPage () const
 */
HB_FUNC( QT_QABSTRACTPRINTDIALOG_MINPAGE )
{
   QAbstractPrintDialog * p = hbqt_par_QAbstractPrintDialog( 1 );
   if( p )
      hb_retni( ( p )->minPage() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTPRINTDIALOG_MINPAGE FP=hb_retni( ( p )->minPage() ); p is NULL" ) );
   }
}

/*
 * PrintRange printRange () const
 */
HB_FUNC( QT_QABSTRACTPRINTDIALOG_PRINTRANGE )
{
   QAbstractPrintDialog * p = hbqt_par_QAbstractPrintDialog( 1 );
   if( p )
      hb_retni( ( QAbstractPrintDialog::PrintRange ) ( p )->printRange() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTPRINTDIALOG_PRINTRANGE FP=hb_retni( ( QAbstractPrintDialog::PrintRange ) ( p )->printRange() ); p is NULL" ) );
   }
}

/*
 * QPrinter * printer () const
 */
HB_FUNC( QT_QABSTRACTPRINTDIALOG_PRINTER )
{
   QAbstractPrintDialog * p = hbqt_par_QAbstractPrintDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPrinter( ( p )->printer(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTPRINTDIALOG_PRINTER FP=hb_retptrGC( hbqt_gcAllocate_QPrinter( ( p )->printer(), false ) ); p is NULL" ) );
   }
}

/*
 * void setFromTo ( int from, int to )
 */
HB_FUNC( QT_QABSTRACTPRINTDIALOG_SETFROMTO )
{
   QAbstractPrintDialog * p = hbqt_par_QAbstractPrintDialog( 1 );
   if( p )
      ( p )->setFromTo( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTPRINTDIALOG_SETFROMTO FP=( p )->setFromTo( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setMinMax ( int min, int max )
 */
HB_FUNC( QT_QABSTRACTPRINTDIALOG_SETMINMAX )
{
   QAbstractPrintDialog * p = hbqt_par_QAbstractPrintDialog( 1 );
   if( p )
      ( p )->setMinMax( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTPRINTDIALOG_SETMINMAX FP=( p )->setMinMax( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setPrintRange ( PrintRange range )
 */
HB_FUNC( QT_QABSTRACTPRINTDIALOG_SETPRINTRANGE )
{
   QAbstractPrintDialog * p = hbqt_par_QAbstractPrintDialog( 1 );
   if( p )
      ( p )->setPrintRange( ( QAbstractPrintDialog::PrintRange ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTPRINTDIALOG_SETPRINTRANGE FP=( p )->setPrintRange( ( QAbstractPrintDialog::PrintRange ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * int toPage () const
 */
HB_FUNC( QT_QABSTRACTPRINTDIALOG_TOPAGE )
{
   QAbstractPrintDialog * p = hbqt_par_QAbstractPrintDialog( 1 );
   if( p )
      hb_retni( ( p )->toPage() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTPRINTDIALOG_TOPAGE FP=hb_retni( ( p )->toPage() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
