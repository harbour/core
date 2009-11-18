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

/*
 *  Constructed[ 55/61 [ 90.16% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<PaperSource> supportedPaperSources () const
 *  QList<int> supportedResolutions () const
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  // QString printerSelectionOption () const
 *  // void setPrinterSelectionOption ( const QString & option )
 *  // void setWinPageSize ( int pageSize )
 *  // int winPageSize () const
 */

#include <QtCore/QPointer>

#include <QtGui/QPrinter>


/*
 * QPrinter ( PrinterMode mode = ScreenResolution )
 * QPrinter ( const QPrinterInfo & printer, PrinterMode mode = ScreenResolution )
 * ~QPrinter ()
 */

QT_G_FUNC( release_QPrinter )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QPrinter                     p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "release_QPrinter                    ph=%p", p->ph ) );

   if( p && p->ph )
   {
      delete ( ( QPrinter * ) p->ph );
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "release_QPrinter                    Object deleted!" ) );
      #if defined( __HB_DEBUG__ )
         hbqt_debug( "  YES release_QPrinter                    %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() );
      #endif
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "release_QPrinter                    Object Allready deleted!" ) );
      #if defined( __HB_DEBUG__ )
         hbqt_debug( "  DEL release_QPrinter" );
      #endif
   }
}

void * gcAllocate_QPrinter( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QPrinter;
   #if defined( __HB_DEBUG__ )
      hbqt_debug( "          new_QPrinter                    %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() );
   #endif
   return( p );
}

HB_FUNC( QT_QPRINTER )
{
   void * pObj = NULL;

   pObj = ( QPrinter* ) new QPrinter() ;

   hb_retptrGC( gcAllocate_QPrinter( pObj ) );
}
/*
 * bool abort ()
 */
HB_FUNC( QT_QPRINTER_ABORT )
{
   hb_retl( hbqt_par_QPrinter( 1 )->abort() );
}

/*
 * bool collateCopies () const
 */
HB_FUNC( QT_QPRINTER_COLLATECOPIES )
{
   hb_retl( hbqt_par_QPrinter( 1 )->collateCopies() );
}

/*
 * ColorMode colorMode () const
 */
HB_FUNC( QT_QPRINTER_COLORMODE )
{
   hb_retni( ( QPrinter::ColorMode ) hbqt_par_QPrinter( 1 )->colorMode() );
}

/*
 * QString creator () const
 */
HB_FUNC( QT_QPRINTER_CREATOR )
{
   hb_retc( hbqt_par_QPrinter( 1 )->creator().toAscii().data() );
}

/*
 * QString docName () const
 */
HB_FUNC( QT_QPRINTER_DOCNAME )
{
   hb_retc( hbqt_par_QPrinter( 1 )->docName().toAscii().data() );
}

/*
 * bool doubleSidedPrinting () const
 */
HB_FUNC( QT_QPRINTER_DOUBLESIDEDPRINTING )
{
   hb_retl( hbqt_par_QPrinter( 1 )->doubleSidedPrinting() );
}

/*
 * DuplexMode duplex () const
 */
HB_FUNC( QT_QPRINTER_DUPLEX )
{
   hb_retni( ( QPrinter::DuplexMode ) hbqt_par_QPrinter( 1 )->duplex() );
}

/*
 * bool fontEmbeddingEnabled () const
 */
HB_FUNC( QT_QPRINTER_FONTEMBEDDINGENABLED )
{
   hb_retl( hbqt_par_QPrinter( 1 )->fontEmbeddingEnabled() );
}

/*
 * int fromPage () const
 */
HB_FUNC( QT_QPRINTER_FROMPAGE )
{
   hb_retni( hbqt_par_QPrinter( 1 )->fromPage() );
}

/*
 * bool fullPage () const
 */
HB_FUNC( QT_QPRINTER_FULLPAGE )
{
   hb_retl( hbqt_par_QPrinter( 1 )->fullPage() );
}

/*
 * void getPageMargins ( qreal * left, qreal * top, qreal * right, qreal * bottom, Unit unit ) const
 */
HB_FUNC( QT_QPRINTER_GETPAGEMARGINS )
{
   qreal qrLeft = 0;
   qreal qrTop = 0;
   qreal qrRight = 0;
   qreal qrBottom = 0;

   hbqt_par_QPrinter( 1 )->getPageMargins( &qrLeft, &qrTop, &qrRight, &qrBottom, ( QPrinter::Unit ) hb_parni( 6 ) );

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
   hb_retl( hbqt_par_QPrinter( 1 )->isValid() );
}

/*
 * bool newPage ()
 */
HB_FUNC( QT_QPRINTER_NEWPAGE )
{
   hb_retl( hbqt_par_QPrinter( 1 )->newPage() );
}

/*
 * int numCopies () const
 */
HB_FUNC( QT_QPRINTER_NUMCOPIES )
{
   hb_retni( hbqt_par_QPrinter( 1 )->numCopies() );
}

/*
 * Orientation orientation () const
 */
HB_FUNC( QT_QPRINTER_ORIENTATION )
{
   hb_retni( ( QPrinter::Orientation ) hbqt_par_QPrinter( 1 )->orientation() );
}

/*
 * QString outputFileName () const
 */
HB_FUNC( QT_QPRINTER_OUTPUTFILENAME )
{
   hb_retc( hbqt_par_QPrinter( 1 )->outputFileName().toAscii().data() );
}

/*
 * OutputFormat outputFormat () const
 */
HB_FUNC( QT_QPRINTER_OUTPUTFORMAT )
{
   hb_retni( ( QPrinter::OutputFormat ) hbqt_par_QPrinter( 1 )->outputFormat() );
}

/*
 * PageOrder pageOrder () const
 */
HB_FUNC( QT_QPRINTER_PAGEORDER )
{
   hb_retni( ( QPrinter::PageOrder ) hbqt_par_QPrinter( 1 )->pageOrder() );
}

/*
 * QRect pageRect () const
 */
HB_FUNC( QT_QPRINTER_PAGERECT )
{
   hb_retptrGC( gcAllocate_QRect( new QRect( hbqt_par_QPrinter( 1 )->pageRect() ) ) );
}

/*
 * QRectF pageRect ( Unit unit ) const
 */
HB_FUNC( QT_QPRINTER_PAGERECT_1 )
{
   hb_retptrGC( gcAllocate_QRectF( new QRectF( hbqt_par_QPrinter( 1 )->pageRect( ( QPrinter::Unit ) hb_parni( 2 ) ) ) ) );
}

/*
 * virtual QPaintEngine * paintEngine () const
 */
HB_FUNC( QT_QPRINTER_PAINTENGINE )
{
   hb_retptr( ( QPaintEngine* ) hbqt_par_QPrinter( 1 )->paintEngine() );
}

/*
 * QRect paperRect () const
 */
HB_FUNC( QT_QPRINTER_PAPERRECT )
{
   hb_retptrGC( gcAllocate_QRect( new QRect( hbqt_par_QPrinter( 1 )->paperRect() ) ) );
}

/*
 * QRectF paperRect ( Unit unit ) const
 */
HB_FUNC( QT_QPRINTER_PAPERRECT_1 )
{
   hb_retptrGC( gcAllocate_QRectF( new QRectF( hbqt_par_QPrinter( 1 )->paperRect( ( QPrinter::Unit ) hb_parni( 2 ) ) ) ) );
}

/*
 * PaperSize paperSize () const
 */
HB_FUNC( QT_QPRINTER_PAPERSIZE )
{
   hb_retni( ( QPrinter::PaperSize ) hbqt_par_QPrinter( 1 )->paperSize() );
}

/*
 * QSizeF paperSize ( Unit unit ) const
 */
HB_FUNC( QT_QPRINTER_PAPERSIZE_1 )
{
   hb_retptrGC( gcAllocate_QSizeF( new QSizeF( hbqt_par_QPrinter( 1 )->paperSize( ( QPrinter::Unit ) hb_parni( 2 ) ) ) ) );
}

/*
 * PaperSource paperSource () const
 */
HB_FUNC( QT_QPRINTER_PAPERSOURCE )
{
   hb_retni( ( QPrinter::PaperSource ) hbqt_par_QPrinter( 1 )->paperSource() );
}

/*
 * QPrintEngine * printEngine () const
 */
HB_FUNC( QT_QPRINTER_PRINTENGINE )
{
   hb_retptr( ( QPrintEngine* ) hbqt_par_QPrinter( 1 )->printEngine() );
}

/*
 * QString printProgram () const
 */
HB_FUNC( QT_QPRINTER_PRINTPROGRAM )
{
   hb_retc( hbqt_par_QPrinter( 1 )->printProgram().toAscii().data() );
}

/*
 * PrintRange printRange () const
 */
HB_FUNC( QT_QPRINTER_PRINTRANGE )
{
   hb_retni( ( QPrinter::PrintRange ) hbqt_par_QPrinter( 1 )->printRange() );
}

/*
 * QString printerName () const
 */
HB_FUNC( QT_QPRINTER_PRINTERNAME )
{
   hb_retc( hbqt_par_QPrinter( 1 )->printerName().toAscii().data() );
}

/*
 * PrinterState printerState () const
 */
HB_FUNC( QT_QPRINTER_PRINTERSTATE )
{
   hb_retni( ( QPrinter::PrinterState ) hbqt_par_QPrinter( 1 )->printerState() );
}

/*
 * int resolution () const
 */
HB_FUNC( QT_QPRINTER_RESOLUTION )
{
   hb_retni( hbqt_par_QPrinter( 1 )->resolution() );
}

/*
 * void setCollateCopies ( bool collate )
 */
HB_FUNC( QT_QPRINTER_SETCOLLATECOPIES )
{
   hbqt_par_QPrinter( 1 )->setCollateCopies( hb_parl( 2 ) );
}

/*
 * void setColorMode ( ColorMode newColorMode )
 */
HB_FUNC( QT_QPRINTER_SETCOLORMODE )
{
   hbqt_par_QPrinter( 1 )->setColorMode( ( QPrinter::ColorMode ) hb_parni( 2 ) );
}

/*
 * void setCreator ( const QString & creator )
 */
HB_FUNC( QT_QPRINTER_SETCREATOR )
{
   hbqt_par_QPrinter( 1 )->setCreator( hbqt_par_QString( 2 ) );
}

/*
 * void setDocName ( const QString & name )
 */
HB_FUNC( QT_QPRINTER_SETDOCNAME )
{
   hbqt_par_QPrinter( 1 )->setDocName( hbqt_par_QString( 2 ) );
}

/*
 * void setDoubleSidedPrinting ( bool doubleSided )
 */
HB_FUNC( QT_QPRINTER_SETDOUBLESIDEDPRINTING )
{
   hbqt_par_QPrinter( 1 )->setDoubleSidedPrinting( hb_parl( 2 ) );
}

/*
 * void setDuplex ( DuplexMode duplex )
 */
HB_FUNC( QT_QPRINTER_SETDUPLEX )
{
   hbqt_par_QPrinter( 1 )->setDuplex( ( QPrinter::DuplexMode ) hb_parni( 2 ) );
}

/*
 * void setFontEmbeddingEnabled ( bool enable )
 */
HB_FUNC( QT_QPRINTER_SETFONTEMBEDDINGENABLED )
{
   hbqt_par_QPrinter( 1 )->setFontEmbeddingEnabled( hb_parl( 2 ) );
}

/*
 * void setFromTo ( int from, int to )
 */
HB_FUNC( QT_QPRINTER_SETFROMTO )
{
   hbqt_par_QPrinter( 1 )->setFromTo( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setFullPage ( bool fp )
 */
HB_FUNC( QT_QPRINTER_SETFULLPAGE )
{
   hbqt_par_QPrinter( 1 )->setFullPage( hb_parl( 2 ) );
}

/*
 * void setNumCopies ( int numCopies )
 */
HB_FUNC( QT_QPRINTER_SETNUMCOPIES )
{
   hbqt_par_QPrinter( 1 )->setNumCopies( hb_parni( 2 ) );
}

/*
 * void setOrientation ( Orientation orientation )
 */
HB_FUNC( QT_QPRINTER_SETORIENTATION )
{
   hbqt_par_QPrinter( 1 )->setOrientation( ( QPrinter::Orientation ) hb_parni( 2 ) );
}

/*
 * void setOutputFileName ( const QString & fileName )
 */
HB_FUNC( QT_QPRINTER_SETOUTPUTFILENAME )
{
   hbqt_par_QPrinter( 1 )->setOutputFileName( hbqt_par_QString( 2 ) );
}

/*
 * void setOutputFormat ( OutputFormat format )
 */
HB_FUNC( QT_QPRINTER_SETOUTPUTFORMAT )
{
   hbqt_par_QPrinter( 1 )->setOutputFormat( ( QPrinter::OutputFormat ) hb_parni( 2 ) );
}

/*
 * void setPageMargins ( qreal left, qreal top, qreal right, qreal bottom, Unit unit )
 */
HB_FUNC( QT_QPRINTER_SETPAGEMARGINS )
{
   hbqt_par_QPrinter( 1 )->setPageMargins( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), ( QPrinter::Unit ) hb_parni( 6 ) );
}

/*
 * void setPageOrder ( PageOrder pageOrder )
 */
HB_FUNC( QT_QPRINTER_SETPAGEORDER )
{
   hbqt_par_QPrinter( 1 )->setPageOrder( ( QPrinter::PageOrder ) hb_parni( 2 ) );
}

/*
 * void setPaperSize ( PaperSize newPaperSize )
 */
HB_FUNC( QT_QPRINTER_SETPAPERSIZE )
{
   hbqt_par_QPrinter( 1 )->setPaperSize( ( QPrinter::PaperSize ) hb_parni( 2 ) );
}

/*
 * void setPaperSize ( const QSizeF & paperSize, Unit unit )
 */
HB_FUNC( QT_QPRINTER_SETPAPERSIZE_1 )
{
   hbqt_par_QPrinter( 1 )->setPaperSize( *hbqt_par_QSizeF( 2 ), ( QPrinter::Unit ) hb_parni( 3 ) );
}

/*
 * void setPaperSource ( PaperSource source )
 */
HB_FUNC( QT_QPRINTER_SETPAPERSOURCE )
{
   hbqt_par_QPrinter( 1 )->setPaperSource( ( QPrinter::PaperSource ) hb_parni( 2 ) );
}

/*
 * void setPrintProgram ( const QString & printProg )
 */
HB_FUNC( QT_QPRINTER_SETPRINTPROGRAM )
{
   hbqt_par_QPrinter( 1 )->setPrintProgram( hbqt_par_QString( 2 ) );
}

/*
 * void setPrintRange ( PrintRange range )
 */
HB_FUNC( QT_QPRINTER_SETPRINTRANGE )
{
   hbqt_par_QPrinter( 1 )->setPrintRange( ( QPrinter::PrintRange ) hb_parni( 2 ) );
}

/*
 * void setPrinterName ( const QString & name )
 */
HB_FUNC( QT_QPRINTER_SETPRINTERNAME )
{
   hbqt_par_QPrinter( 1 )->setPrinterName( hbqt_par_QString( 2 ) );
}

/*
 * void setResolution ( int dpi )
 */
HB_FUNC( QT_QPRINTER_SETRESOLUTION )
{
   hbqt_par_QPrinter( 1 )->setResolution( hb_parni( 2 ) );
}

/*
 * int toPage () const
 */
HB_FUNC( QT_QPRINTER_TOPAGE )
{
   hb_retni( hbqt_par_QPrinter( 1 )->toPage() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
