/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */

#include "hbqtcore.h"
#include "hbqtgui.h"

#if QT_VERSION >= 0x040500

/*
 *  enum PrintDialogOption { None, PrintToFile, PrintSelection, PrintPageRange, ..., PrintShowPageSize }
 *  enum PrintRange { AllPages, Selection, PageRange }
 *  flags PrintDialogOptions
 */

/*
 *  Constructed[ 10/11 [ 90.91% ] ]
 *
 *  *** Unconvered Prototypes ***
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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QAbstractPrintDialog;

HBQT_GC_FUNC( hbqt_gcRelease_QAbstractPrintDialog )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QAbstractPrintDialog( void * pObj, bool bNew )
{
   HBQT_GC_T_QAbstractPrintDialog * p = ( HBQT_GC_T_QAbstractPrintDialog * ) hb_gcAllocate( sizeof( HBQT_GC_T_QAbstractPrintDialog ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QAbstractPrintDialog >( ( QAbstractPrintDialog * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QAbstractPrintDialog;
   p->type = HBQT_TYPE_QAbstractPrintDialog;

   return p;
}

HB_FUNC( QT_QABSTRACTPRINTDIALOG )
{

}

/* virtual int exec () = 0 */
HB_FUNC( QT_QABSTRACTPRINTDIALOG_EXEC )
{
   QAbstractPrintDialog * p = hbqt_par_QAbstractPrintDialog( 1 );
   if( p )
      hb_retni( ( p )->exec() );
}

/* int fromPage () const */
HB_FUNC( QT_QABSTRACTPRINTDIALOG_FROMPAGE )
{
   QAbstractPrintDialog * p = hbqt_par_QAbstractPrintDialog( 1 );
   if( p )
      hb_retni( ( p )->fromPage() );
}

/* int maxPage () const */
HB_FUNC( QT_QABSTRACTPRINTDIALOG_MAXPAGE )
{
   QAbstractPrintDialog * p = hbqt_par_QAbstractPrintDialog( 1 );
   if( p )
      hb_retni( ( p )->maxPage() );
}

/* int minPage () const */
HB_FUNC( QT_QABSTRACTPRINTDIALOG_MINPAGE )
{
   QAbstractPrintDialog * p = hbqt_par_QAbstractPrintDialog( 1 );
   if( p )
      hb_retni( ( p )->minPage() );
}

/* PrintRange printRange () const */
HB_FUNC( QT_QABSTRACTPRINTDIALOG_PRINTRANGE )
{
   QAbstractPrintDialog * p = hbqt_par_QAbstractPrintDialog( 1 );
   if( p )
      hb_retni( ( QAbstractPrintDialog::PrintRange ) ( p )->printRange() );
}

/* QPrinter * printer () const */
HB_FUNC( QT_QABSTRACTPRINTDIALOG_PRINTER )
{
   QAbstractPrintDialog * p = hbqt_par_QAbstractPrintDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPrinter( ( p )->printer(), false ) );
}

/* void setFromTo ( int from, int to ) */
HB_FUNC( QT_QABSTRACTPRINTDIALOG_SETFROMTO )
{
   QAbstractPrintDialog * p = hbqt_par_QAbstractPrintDialog( 1 );
   if( p )
      ( p )->setFromTo( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setMinMax ( int min, int max ) */
HB_FUNC( QT_QABSTRACTPRINTDIALOG_SETMINMAX )
{
   QAbstractPrintDialog * p = hbqt_par_QAbstractPrintDialog( 1 );
   if( p )
      ( p )->setMinMax( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setPrintRange ( PrintRange range ) */
HB_FUNC( QT_QABSTRACTPRINTDIALOG_SETPRINTRANGE )
{
   QAbstractPrintDialog * p = hbqt_par_QAbstractPrintDialog( 1 );
   if( p )
      ( p )->setPrintRange( ( QAbstractPrintDialog::PrintRange ) hb_parni( 2 ) );
}

/* int toPage () const */
HB_FUNC( QT_QABSTRACTPRINTDIALOG_TOPAGE )
{
   QAbstractPrintDialog * p = hbqt_par_QAbstractPrintDialog( 1 );
   if( p )
      hb_retni( ( p )->toPage() );
}


#endif /* #if QT_VERSION >= 0x040500 */
