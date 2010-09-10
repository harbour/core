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
 *  enum ColorMode { Color, GrayScale }
 *  enum DuplexMode { DuplexNone, DuplexAuto, DuplexLongSide, DuplexShortSide }
 *  enum Orientation { Portrait, Landscape }
 *  enum OutputFormat { NativeFormat, PdfFormat, PostScriptFormat }
 *  enum PageOrder { FirstPageFirst, LastPageFirst }
 *  enum PaperSize { A0, A1, A2, A3, ..., Custom }
 *  enum PaperSource { Auto, Cassette, Envelope, EnvelopeManual, ..., SmallFormat }
 *  enum PrintRange { AllPages, Selection, PageRange }
 *  enum PrinterMode { ScreenResolution, PrinterResolution, HighResolution }
 *  enum PrinterState { Idle, Active, Aborted, Error }
 *  enum Unit { Millimeter, Point, Inch, Pica, ..., DevicePixel }
 */

#include <QtCore/QPointer>

#include <QtGui/QPrinter>


/*
 * QPrinter ( PrinterMode mode = ScreenResolution )
 * QPrinter ( const QPrinterInfo & printer, PrinterMode mode = ScreenResolution )
 * ~QPrinter ()
 */

typedef struct
{
   QPrinter * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPrinter;

HBQT_GC_FUNC( hbqt_gcRelease_QPrinter )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QPrinter   /.\\", p->ph ) );
         delete ( ( QPrinter * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QPrinter   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QPrinter    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QPrinter    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPrinter( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QPrinter * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPrinter;
   p->type = HBQT_TYPE_QPrinter;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QPrinter", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QPrinter", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QPRINTER )
{
   QPrinter * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QPrinter( *hbqt_par_QPrinterInfo( 1 ), ( QPrinter::PrinterMode ) ( HB_ISNUM( 2 ) ? hb_parni( 2 ) : QPrinter::ScreenResolution ) ) ;
   }
   else
   {
      pObj = new QPrinter() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QPrinter( ( void * ) pObj, true ) );
}

/*
 * bool abort ()
 */
HB_FUNC( QT_QPRINTER_ABORT )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retl( ( p )->abort() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_ABORT FP=hb_retl( ( p )->abort() ); p is NULL" ) );
   }
}

/*
 * bool collateCopies () const
 */
HB_FUNC( QT_QPRINTER_COLLATECOPIES )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retl( ( p )->collateCopies() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_COLLATECOPIES FP=hb_retl( ( p )->collateCopies() ); p is NULL" ) );
   }
}

/*
 * ColorMode colorMode () const
 */
HB_FUNC( QT_QPRINTER_COLORMODE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( QPrinter::ColorMode ) ( p )->colorMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_COLORMODE FP=hb_retni( ( QPrinter::ColorMode ) ( p )->colorMode() ); p is NULL" ) );
   }
}

/*
 * QString creator () const
 */
HB_FUNC( QT_QPRINTER_CREATOR )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retc( ( p )->creator().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_CREATOR FP=hb_retc( ( p )->creator().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString docName () const
 */
HB_FUNC( QT_QPRINTER_DOCNAME )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retc( ( p )->docName().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_DOCNAME FP=hb_retc( ( p )->docName().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool doubleSidedPrinting () const
 */
HB_FUNC( QT_QPRINTER_DOUBLESIDEDPRINTING )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retl( ( p )->doubleSidedPrinting() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_DOUBLESIDEDPRINTING FP=hb_retl( ( p )->doubleSidedPrinting() ); p is NULL" ) );
   }
}

/*
 * DuplexMode duplex () const
 */
HB_FUNC( QT_QPRINTER_DUPLEX )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( QPrinter::DuplexMode ) ( p )->duplex() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_DUPLEX FP=hb_retni( ( QPrinter::DuplexMode ) ( p )->duplex() ); p is NULL" ) );
   }
}

/*
 * bool fontEmbeddingEnabled () const
 */
HB_FUNC( QT_QPRINTER_FONTEMBEDDINGENABLED )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retl( ( p )->fontEmbeddingEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_FONTEMBEDDINGENABLED FP=hb_retl( ( p )->fontEmbeddingEnabled() ); p is NULL" ) );
   }
}

/*
 * int fromPage () const
 */
HB_FUNC( QT_QPRINTER_FROMPAGE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( p )->fromPage() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_FROMPAGE FP=hb_retni( ( p )->fromPage() ); p is NULL" ) );
   }
}

/*
 * bool fullPage () const
 */
HB_FUNC( QT_QPRINTER_FULLPAGE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retl( ( p )->fullPage() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_FULLPAGE FP=hb_retl( ( p )->fullPage() ); p is NULL" ) );
   }
}

/*
 * void getPageMargins ( qreal * left, qreal * top, qreal * right, qreal * bottom, Unit unit ) const
 */
HB_FUNC( QT_QPRINTER_GETPAGEMARGINS )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   qreal qrLeft = 0;
   qreal qrTop = 0;
   qreal qrRight = 0;
   qreal qrBottom = 0;

   if( p )
      ( p )->getPageMargins( &qrLeft, &qrTop, &qrRight, &qrBottom, ( QPrinter::Unit ) hb_parni( 6 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_GETPAGEMARGINS FP=( p )->getPageMargins( &qrLeft, &qrTop, &qrRight, &qrBottom, ( QPrinter::Unit ) hb_parni( 6 ) ); p is NULL" ) );
   }

   hb_stornd( qrLeft, 2 );
   hb_stornd( qrTop, 3 );
   hb_stornd( qrRight, 4 );
   hb_stornd( qrBottom, 5 );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QPRINTER_ISVALID )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_ISVALID FP=hb_retl( ( p )->isValid() ); p is NULL" ) );
   }
}

/*
 * bool newPage ()
 */
HB_FUNC( QT_QPRINTER_NEWPAGE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retl( ( p )->newPage() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_NEWPAGE FP=hb_retl( ( p )->newPage() ); p is NULL" ) );
   }
}

/*
 * int numCopies () const
 */
HB_FUNC( QT_QPRINTER_NUMCOPIES )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( p )->numCopies() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_NUMCOPIES FP=hb_retni( ( p )->numCopies() ); p is NULL" ) );
   }
}

/*
 * Orientation orientation () const
 */
HB_FUNC( QT_QPRINTER_ORIENTATION )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( QPrinter::Orientation ) ( p )->orientation() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_ORIENTATION FP=hb_retni( ( QPrinter::Orientation ) ( p )->orientation() ); p is NULL" ) );
   }
}

/*
 * QString outputFileName () const
 */
HB_FUNC( QT_QPRINTER_OUTPUTFILENAME )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retc( ( p )->outputFileName().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_OUTPUTFILENAME FP=hb_retc( ( p )->outputFileName().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * OutputFormat outputFormat () const
 */
HB_FUNC( QT_QPRINTER_OUTPUTFORMAT )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( QPrinter::OutputFormat ) ( p )->outputFormat() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_OUTPUTFORMAT FP=hb_retni( ( QPrinter::OutputFormat ) ( p )->outputFormat() ); p is NULL" ) );
   }
}

/*
 * PageOrder pageOrder () const
 */
HB_FUNC( QT_QPRINTER_PAGEORDER )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( QPrinter::PageOrder ) ( p )->pageOrder() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_PAGEORDER FP=hb_retni( ( QPrinter::PageOrder ) ( p )->pageOrder() ); p is NULL" ) );
   }
}

/*
 * QRect pageRect () const
 */
HB_FUNC( QT_QPRINTER_PAGERECT )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->pageRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_PAGERECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->pageRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF pageRect ( Unit unit ) const
 */
HB_FUNC( QT_QPRINTER_PAGERECT_1 )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->pageRect( ( QPrinter::Unit ) hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_PAGERECT_1 FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->pageRect( ( QPrinter::Unit ) hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual QPaintEngine * paintEngine () const
 */
HB_FUNC( QT_QPRINTER_PAINTENGINE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPaintEngine( ( p )->paintEngine(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_PAINTENGINE FP=hb_retptrGC( hbqt_gcAllocate_QPaintEngine( ( p )->paintEngine(), false ) ); p is NULL" ) );
   }
}

/*
 * QRect paperRect () const
 */
HB_FUNC( QT_QPRINTER_PAPERRECT )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->paperRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_PAPERRECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->paperRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF paperRect ( Unit unit ) const
 */
HB_FUNC( QT_QPRINTER_PAPERRECT_1 )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->paperRect( ( QPrinter::Unit ) hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_PAPERRECT_1 FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->paperRect( ( QPrinter::Unit ) hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * PaperSize paperSize () const
 */
HB_FUNC( QT_QPRINTER_PAPERSIZE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( QPrinter::PaperSize ) ( p )->paperSize() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_PAPERSIZE FP=hb_retni( ( QPrinter::PaperSize ) ( p )->paperSize() ); p is NULL" ) );
   }
}

/*
 * QSizeF paperSize ( Unit unit ) const
 */
HB_FUNC( QT_QPRINTER_PAPERSIZE_1 )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->paperSize( ( QPrinter::Unit ) hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_PAPERSIZE_1 FP=hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->paperSize( ( QPrinter::Unit ) hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * PaperSource paperSource () const
 */
HB_FUNC( QT_QPRINTER_PAPERSOURCE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( QPrinter::PaperSource ) ( p )->paperSource() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_PAPERSOURCE FP=hb_retni( ( QPrinter::PaperSource ) ( p )->paperSource() ); p is NULL" ) );
   }
}

/*
 * QPrintEngine * printEngine () const
 */
HB_FUNC( QT_QPRINTER_PRINTENGINE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPrintEngine( ( p )->printEngine(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_PRINTENGINE FP=hb_retptrGC( hbqt_gcAllocate_QPrintEngine( ( p )->printEngine(), false ) ); p is NULL" ) );
   }
}

/*
 * QString printProgram () const
 */
HB_FUNC( QT_QPRINTER_PRINTPROGRAM )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retc( ( p )->printProgram().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_PRINTPROGRAM FP=hb_retc( ( p )->printProgram().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * PrintRange printRange () const
 */
HB_FUNC( QT_QPRINTER_PRINTRANGE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( QPrinter::PrintRange ) ( p )->printRange() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_PRINTRANGE FP=hb_retni( ( QPrinter::PrintRange ) ( p )->printRange() ); p is NULL" ) );
   }
}

/*
 * QString printerName () const
 */
HB_FUNC( QT_QPRINTER_PRINTERNAME )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retc( ( p )->printerName().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_PRINTERNAME FP=hb_retc( ( p )->printerName().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * PrinterState printerState () const
 */
HB_FUNC( QT_QPRINTER_PRINTERSTATE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( QPrinter::PrinterState ) ( p )->printerState() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_PRINTERSTATE FP=hb_retni( ( QPrinter::PrinterState ) ( p )->printerState() ); p is NULL" ) );
   }
}

/*
 * int resolution () const
 */
HB_FUNC( QT_QPRINTER_RESOLUTION )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( p )->resolution() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_RESOLUTION FP=hb_retni( ( p )->resolution() ); p is NULL" ) );
   }
}

/*
 * void setCollateCopies ( bool collate )
 */
HB_FUNC( QT_QPRINTER_SETCOLLATECOPIES )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setCollateCopies( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETCOLLATECOPIES FP=( p )->setCollateCopies( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setColorMode ( ColorMode newColorMode )
 */
HB_FUNC( QT_QPRINTER_SETCOLORMODE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setColorMode( ( QPrinter::ColorMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETCOLORMODE FP=( p )->setColorMode( ( QPrinter::ColorMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCreator ( const QString & creator )
 */
HB_FUNC( QT_QPRINTER_SETCREATOR )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setCreator( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETCREATOR FP=( p )->setCreator( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDocName ( const QString & name )
 */
HB_FUNC( QT_QPRINTER_SETDOCNAME )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setDocName( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETDOCNAME FP=( p )->setDocName( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDoubleSidedPrinting ( bool doubleSided )
 */
HB_FUNC( QT_QPRINTER_SETDOUBLESIDEDPRINTING )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setDoubleSidedPrinting( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETDOUBLESIDEDPRINTING FP=( p )->setDoubleSidedPrinting( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDuplex ( DuplexMode duplex )
 */
HB_FUNC( QT_QPRINTER_SETDUPLEX )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setDuplex( ( QPrinter::DuplexMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETDUPLEX FP=( p )->setDuplex( ( QPrinter::DuplexMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFontEmbeddingEnabled ( bool enable )
 */
HB_FUNC( QT_QPRINTER_SETFONTEMBEDDINGENABLED )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setFontEmbeddingEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETFONTEMBEDDINGENABLED FP=( p )->setFontEmbeddingEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFromTo ( int from, int to )
 */
HB_FUNC( QT_QPRINTER_SETFROMTO )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setFromTo( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETFROMTO FP=( p )->setFromTo( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setFullPage ( bool fp )
 */
HB_FUNC( QT_QPRINTER_SETFULLPAGE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setFullPage( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETFULLPAGE FP=( p )->setFullPage( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setNumCopies ( int numCopies )
 */
HB_FUNC( QT_QPRINTER_SETNUMCOPIES )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setNumCopies( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETNUMCOPIES FP=( p )->setNumCopies( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setOrientation ( Orientation orientation )
 */
HB_FUNC( QT_QPRINTER_SETORIENTATION )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setOrientation( ( QPrinter::Orientation ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETORIENTATION FP=( p )->setOrientation( ( QPrinter::Orientation ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setOutputFileName ( const QString & fileName )
 */
HB_FUNC( QT_QPRINTER_SETOUTPUTFILENAME )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setOutputFileName( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETOUTPUTFILENAME FP=( p )->setOutputFileName( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setOutputFormat ( OutputFormat format )
 */
HB_FUNC( QT_QPRINTER_SETOUTPUTFORMAT )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setOutputFormat( ( QPrinter::OutputFormat ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETOUTPUTFORMAT FP=( p )->setOutputFormat( ( QPrinter::OutputFormat ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPageMargins ( qreal left, qreal top, qreal right, qreal bottom, Unit unit )
 */
HB_FUNC( QT_QPRINTER_SETPAGEMARGINS )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setPageMargins( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), ( QPrinter::Unit ) hb_parni( 6 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETPAGEMARGINS FP=( p )->setPageMargins( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), ( QPrinter::Unit ) hb_parni( 6 ) ); p is NULL" ) );
   }
}

/*
 * void setPageOrder ( PageOrder pageOrder )
 */
HB_FUNC( QT_QPRINTER_SETPAGEORDER )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setPageOrder( ( QPrinter::PageOrder ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETPAGEORDER FP=( p )->setPageOrder( ( QPrinter::PageOrder ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPaperSize ( PaperSize newPaperSize )
 */
HB_FUNC( QT_QPRINTER_SETPAPERSIZE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setPaperSize( ( QPrinter::PaperSize ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETPAPERSIZE FP=( p )->setPaperSize( ( QPrinter::PaperSize ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPaperSize ( const QSizeF & paperSize, Unit unit )
 */
HB_FUNC( QT_QPRINTER_SETPAPERSIZE_1 )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setPaperSize( *hbqt_par_QSizeF( 2 ), ( QPrinter::Unit ) hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETPAPERSIZE_1 FP=( p )->setPaperSize( *hbqt_par_QSizeF( 2 ), ( QPrinter::Unit ) hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setPaperSource ( PaperSource source )
 */
HB_FUNC( QT_QPRINTER_SETPAPERSOURCE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setPaperSource( ( QPrinter::PaperSource ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETPAPERSOURCE FP=( p )->setPaperSource( ( QPrinter::PaperSource ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPrintProgram ( const QString & printProg )
 */
HB_FUNC( QT_QPRINTER_SETPRINTPROGRAM )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setPrintProgram( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETPRINTPROGRAM FP=( p )->setPrintProgram( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPrintRange ( PrintRange range )
 */
HB_FUNC( QT_QPRINTER_SETPRINTRANGE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setPrintRange( ( QPrinter::PrintRange ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETPRINTRANGE FP=( p )->setPrintRange( ( QPrinter::PrintRange ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPrinterName ( const QString & name )
 */
HB_FUNC( QT_QPRINTER_SETPRINTERNAME )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setPrinterName( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETPRINTERNAME FP=( p )->setPrinterName( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setResolution ( int dpi )
 */
HB_FUNC( QT_QPRINTER_SETRESOLUTION )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setResolution( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SETRESOLUTION FP=( p )->setResolution( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * QList<int> supportedResolutions () const
 */
HB_FUNC( QT_QPRINTER_SUPPORTEDRESOLUTIONS )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<int>( ( p )->supportedResolutions() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_SUPPORTEDRESOLUTIONS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<int>( ( p )->supportedResolutions() ), true ) ); p is NULL" ) );
   }
}

/*
 * int toPage () const
 */
HB_FUNC( QT_QPRINTER_TOPAGE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( p )->toPage() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPRINTER_TOPAGE FP=hb_retni( ( p )->toPage() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
