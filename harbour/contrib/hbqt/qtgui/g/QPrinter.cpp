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
 *  Constructed[ 56/56 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // QString printerSelectionOption () const
 *  // void setPrinterSelectionOption ( const QString & option )
 *  // void setWinPageSize ( int pageSize )
 *  // QList<PaperSource> supportedPaperSources () const
 *  // int winPageSize () const
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
         delete ( ( QPrinter * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QPrinter( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QPrinter * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPrinter;
   p->type = HBQT_TYPE_QPrinter;

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

/* bool abort () */
HB_FUNC( QT_QPRINTER_ABORT )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retl( ( p )->abort() );
}

/* bool collateCopies () const */
HB_FUNC( QT_QPRINTER_COLLATECOPIES )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retl( ( p )->collateCopies() );
}

/* ColorMode colorMode () const */
HB_FUNC( QT_QPRINTER_COLORMODE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( QPrinter::ColorMode ) ( p )->colorMode() );
}

/* QString creator () const */
HB_FUNC( QT_QPRINTER_CREATOR )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retstr_utf8( ( p )->creator().toUtf8().data() );
}

/* QString docName () const */
HB_FUNC( QT_QPRINTER_DOCNAME )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retstr_utf8( ( p )->docName().toUtf8().data() );
}

/* bool doubleSidedPrinting () const */
HB_FUNC( QT_QPRINTER_DOUBLESIDEDPRINTING )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retl( ( p )->doubleSidedPrinting() );
}

/* DuplexMode duplex () const */
HB_FUNC( QT_QPRINTER_DUPLEX )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( QPrinter::DuplexMode ) ( p )->duplex() );
}

/* bool fontEmbeddingEnabled () const */
HB_FUNC( QT_QPRINTER_FONTEMBEDDINGENABLED )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retl( ( p )->fontEmbeddingEnabled() );
}

/* int fromPage () const */
HB_FUNC( QT_QPRINTER_FROMPAGE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( p )->fromPage() );
}

/* bool fullPage () const */
HB_FUNC( QT_QPRINTER_FULLPAGE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retl( ( p )->fullPage() );
}

/* void getPageMargins ( qreal * left, qreal * top, qreal * right, qreal * bottom, Unit unit ) const */
HB_FUNC( QT_QPRINTER_GETPAGEMARGINS )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   qreal qrLeft = 0;
   qreal qrTop = 0;
   qreal qrRight = 0;
   qreal qrBottom = 0;

   if( p )
      ( p )->getPageMargins( &qrLeft, &qrTop, &qrRight, &qrBottom, ( QPrinter::Unit ) hb_parni( 6 ) );

   hb_stornd( qrLeft, 2 );
   hb_stornd( qrTop, 3 );
   hb_stornd( qrRight, 4 );
   hb_stornd( qrBottom, 5 );
}

/* bool isValid () const */
HB_FUNC( QT_QPRINTER_ISVALID )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* bool newPage () */
HB_FUNC( QT_QPRINTER_NEWPAGE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retl( ( p )->newPage() );
}

/* int numCopies () const */
HB_FUNC( QT_QPRINTER_NUMCOPIES )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( p )->numCopies() );
}

/* Orientation orientation () const */
HB_FUNC( QT_QPRINTER_ORIENTATION )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( QPrinter::Orientation ) ( p )->orientation() );
}

/* QString outputFileName () const */
HB_FUNC( QT_QPRINTER_OUTPUTFILENAME )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retstr_utf8( ( p )->outputFileName().toUtf8().data() );
}

/* OutputFormat outputFormat () const */
HB_FUNC( QT_QPRINTER_OUTPUTFORMAT )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( QPrinter::OutputFormat ) ( p )->outputFormat() );
}

/* PageOrder pageOrder () const */
HB_FUNC( QT_QPRINTER_PAGEORDER )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( QPrinter::PageOrder ) ( p )->pageOrder() );
}

/* QRect pageRect () const */
HB_FUNC( QT_QPRINTER_PAGERECT )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->pageRect() ), true ) );
}

/* QRectF pageRect ( Unit unit ) const */
HB_FUNC( QT_QPRINTER_PAGERECT_1 )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->pageRect( ( QPrinter::Unit ) hb_parni( 2 ) ) ), true ) );
}

/* virtual QPaintEngine * paintEngine () const */
HB_FUNC( QT_QPRINTER_PAINTENGINE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPaintEngine( ( p )->paintEngine(), false ) );
}

/* QRect paperRect () const */
HB_FUNC( QT_QPRINTER_PAPERRECT )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->paperRect() ), true ) );
}

/* QRectF paperRect ( Unit unit ) const */
HB_FUNC( QT_QPRINTER_PAPERRECT_1 )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->paperRect( ( QPrinter::Unit ) hb_parni( 2 ) ) ), true ) );
}

/* PaperSize paperSize () const */
HB_FUNC( QT_QPRINTER_PAPERSIZE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( QPrinter::PaperSize ) ( p )->paperSize() );
}

/* QSizeF paperSize ( Unit unit ) const */
HB_FUNC( QT_QPRINTER_PAPERSIZE_1 )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->paperSize( ( QPrinter::Unit ) hb_parni( 2 ) ) ), true ) );
}

/* PaperSource paperSource () const */
HB_FUNC( QT_QPRINTER_PAPERSOURCE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( QPrinter::PaperSource ) ( p )->paperSource() );
}

/* QPrintEngine * printEngine () const */
HB_FUNC( QT_QPRINTER_PRINTENGINE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPrintEngine( ( p )->printEngine(), false ) );
}

/* QString printProgram () const */
HB_FUNC( QT_QPRINTER_PRINTPROGRAM )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retstr_utf8( ( p )->printProgram().toUtf8().data() );
}

/* PrintRange printRange () const */
HB_FUNC( QT_QPRINTER_PRINTRANGE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( QPrinter::PrintRange ) ( p )->printRange() );
}

/* QString printerName () const */
HB_FUNC( QT_QPRINTER_PRINTERNAME )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retstr_utf8( ( p )->printerName().toUtf8().data() );
}

/* PrinterState printerState () const */
HB_FUNC( QT_QPRINTER_PRINTERSTATE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( QPrinter::PrinterState ) ( p )->printerState() );
}

/* int resolution () const */
HB_FUNC( QT_QPRINTER_RESOLUTION )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( p )->resolution() );
}

/* void setCollateCopies ( bool collate ) */
HB_FUNC( QT_QPRINTER_SETCOLLATECOPIES )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setCollateCopies( hb_parl( 2 ) );
}

/* void setColorMode ( ColorMode newColorMode ) */
HB_FUNC( QT_QPRINTER_SETCOLORMODE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setColorMode( ( QPrinter::ColorMode ) hb_parni( 2 ) );
}

/* void setCreator ( const QString & creator ) */
HB_FUNC( QT_QPRINTER_SETCREATOR )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
   {
      void * pText;
      ( p )->setCreator( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setDocName ( const QString & name ) */
HB_FUNC( QT_QPRINTER_SETDOCNAME )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
   {
      void * pText;
      ( p )->setDocName( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setDoubleSidedPrinting ( bool doubleSided ) */
HB_FUNC( QT_QPRINTER_SETDOUBLESIDEDPRINTING )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setDoubleSidedPrinting( hb_parl( 2 ) );
}

/* void setDuplex ( DuplexMode duplex ) */
HB_FUNC( QT_QPRINTER_SETDUPLEX )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setDuplex( ( QPrinter::DuplexMode ) hb_parni( 2 ) );
}

/* void setFontEmbeddingEnabled ( bool enable ) */
HB_FUNC( QT_QPRINTER_SETFONTEMBEDDINGENABLED )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setFontEmbeddingEnabled( hb_parl( 2 ) );
}

/* void setFromTo ( int from, int to ) */
HB_FUNC( QT_QPRINTER_SETFROMTO )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setFromTo( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setFullPage ( bool fp ) */
HB_FUNC( QT_QPRINTER_SETFULLPAGE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setFullPage( hb_parl( 2 ) );
}

/* void setNumCopies ( int numCopies ) */
HB_FUNC( QT_QPRINTER_SETNUMCOPIES )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setNumCopies( hb_parni( 2 ) );
}

/* void setOrientation ( Orientation orientation ) */
HB_FUNC( QT_QPRINTER_SETORIENTATION )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setOrientation( ( QPrinter::Orientation ) hb_parni( 2 ) );
}

/* void setOutputFileName ( const QString & fileName ) */
HB_FUNC( QT_QPRINTER_SETOUTPUTFILENAME )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
   {
      void * pText;
      ( p )->setOutputFileName( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setOutputFormat ( OutputFormat format ) */
HB_FUNC( QT_QPRINTER_SETOUTPUTFORMAT )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setOutputFormat( ( QPrinter::OutputFormat ) hb_parni( 2 ) );
}

/* void setPageMargins ( qreal left, qreal top, qreal right, qreal bottom, Unit unit ) */
HB_FUNC( QT_QPRINTER_SETPAGEMARGINS )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setPageMargins( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), ( QPrinter::Unit ) hb_parni( 6 ) );
}

/* void setPageOrder ( PageOrder pageOrder ) */
HB_FUNC( QT_QPRINTER_SETPAGEORDER )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setPageOrder( ( QPrinter::PageOrder ) hb_parni( 2 ) );
}

/* void setPaperSize ( PaperSize newPaperSize ) */
HB_FUNC( QT_QPRINTER_SETPAPERSIZE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setPaperSize( ( QPrinter::PaperSize ) hb_parni( 2 ) );
}

/* void setPaperSize ( const QSizeF & paperSize, Unit unit ) */
HB_FUNC( QT_QPRINTER_SETPAPERSIZE_1 )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setPaperSize( *hbqt_par_QSizeF( 2 ), ( QPrinter::Unit ) hb_parni( 3 ) );
}

/* void setPaperSource ( PaperSource source ) */
HB_FUNC( QT_QPRINTER_SETPAPERSOURCE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setPaperSource( ( QPrinter::PaperSource ) hb_parni( 2 ) );
}

/* void setPrintProgram ( const QString & printProg ) */
HB_FUNC( QT_QPRINTER_SETPRINTPROGRAM )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
   {
      void * pText;
      ( p )->setPrintProgram( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setPrintRange ( PrintRange range ) */
HB_FUNC( QT_QPRINTER_SETPRINTRANGE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setPrintRange( ( QPrinter::PrintRange ) hb_parni( 2 ) );
}

/* void setPrinterName ( const QString & name ) */
HB_FUNC( QT_QPRINTER_SETPRINTERNAME )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
   {
      void * pText;
      ( p )->setPrinterName( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setResolution ( int dpi ) */
HB_FUNC( QT_QPRINTER_SETRESOLUTION )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      ( p )->setResolution( hb_parni( 2 ) );
}

/* QList<int> supportedResolutions () const */
HB_FUNC( QT_QPRINTER_SUPPORTEDRESOLUTIONS )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<int>( ( p )->supportedResolutions() ), true ) );
}

/* int toPage () const */
HB_FUNC( QT_QPRINTER_TOPAGE )
{
   QPrinter * p = hbqt_par_QPrinter( 1 );
   if( p )
      hb_retni( ( p )->toPage() );
}


#endif /* #if QT_VERSION >= 0x040500 */
