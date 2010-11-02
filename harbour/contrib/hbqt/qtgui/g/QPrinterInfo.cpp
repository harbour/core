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
 *  Constructed[ 5/6 [ 83.33% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  QList<QPrinter::PaperSize> supportedPaperSizes () const
 */

#include <QtCore/QPointer>

#include <QtGui/QPrinterInfo>


/*
 * QPrinterInfo ()
 * QPrinterInfo ( const QPrinterInfo & src )
 * QPrinterInfo ( const QPrinter & printer )
 * ~QPrinterInfo ()
 */

typedef struct
{
   QPrinterInfo * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPrinterInfo;

HBQT_GC_FUNC( hbqt_gcRelease_QPrinterInfo )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QPrinterInfo * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPrinterInfo( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QPrinterInfo * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPrinterInfo;
   p->type = HBQT_TYPE_QPrinterInfo;

   return p;
}

HB_FUNC( QT_QPRINTERINFO )
{
   QPrinterInfo * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QPrinterInfo( *hbqt_par_QPrinterInfo( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new QPrinterInfo( *hbqt_par_QPrinter( 2 ) ) ;
   }
   else
   {
      pObj = new QPrinterInfo() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QPrinterInfo( ( void * ) pObj, true ) );
}

/* bool isDefault () const */
HB_FUNC( QT_QPRINTERINFO_ISDEFAULT )
{
   QPrinterInfo * p = hbqt_par_QPrinterInfo( 1 );
   if( p )
      hb_retl( ( p )->isDefault() );
}

/* bool isNull () const */
HB_FUNC( QT_QPRINTERINFO_ISNULL )
{
   QPrinterInfo * p = hbqt_par_QPrinterInfo( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
}

/* QString printerName () const */
HB_FUNC( QT_QPRINTERINFO_PRINTERNAME )
{
   QPrinterInfo * p = hbqt_par_QPrinterInfo( 1 );
   if( p )
      hb_retstr_utf8( ( p )->printerName().toUtf8().data() );
}

/* QList<QPrinterInfo> availablePrinters () */
HB_FUNC( QT_QPRINTERINFO_AVAILABLEPRINTERS )
{
   QPrinterInfo * p = hbqt_par_QPrinterInfo( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QPrinterInfo>( ( p )->availablePrinters() ), true ) );
}

/* QPrinterInfo defaultPrinter () */
HB_FUNC( QT_QPRINTERINFO_DEFAULTPRINTER )
{
   QPrinterInfo * p = hbqt_par_QPrinterInfo( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPrinterInfo( new QPrinterInfo( ( p )->defaultPrinter() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
