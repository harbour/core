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
 *  enum PrintEnginePropertyKey { PPK_CollateCopies, PPK_ColorMode, PPK_Creator, PPK_Duplex, ..., PPK_CustomBase }
 */

/*
 *  Constructed[ 6/6 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QPrintEngine>


/*
 * QPrintEngine ()
 * virtual ~QPrintEngine ()
 */

typedef struct
{
   QPrintEngine * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPrintEngine;

HBQT_GC_FUNC( hbqt_gcRelease_QPrintEngine )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QPrintEngine( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QPrintEngine * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPrintEngine;
   p->type = HBQT_TYPE_QPrintEngine;

   return p;
}

HB_FUNC( QT_QPRINTENGINE )
{

}

/* virtual bool abort () */
HB_FUNC( QT_QPRINTENGINE_ABORT )
{
   QPrintEngine * p = hbqt_par_QPrintEngine( 1 );
   if( p )
      hb_retl( ( p )->abort() );
}

/* virtual int metric ( QPaintDevice::PaintDeviceMetric id ) const */
HB_FUNC( QT_QPRINTENGINE_METRIC )
{
   QPrintEngine * p = hbqt_par_QPrintEngine( 1 );
   if( p )
      hb_retni( ( p )->metric( ( QPaintDevice::PaintDeviceMetric ) hb_parni( 2 ) ) );
}

/* virtual bool newPage () */
HB_FUNC( QT_QPRINTENGINE_NEWPAGE )
{
   QPrintEngine * p = hbqt_par_QPrintEngine( 1 );
   if( p )
      hb_retl( ( p )->newPage() );
}

/* virtual QPrinter::PrinterState printerState () const */
HB_FUNC( QT_QPRINTENGINE_PRINTERSTATE )
{
   QPrintEngine * p = hbqt_par_QPrintEngine( 1 );
   if( p )
      hb_retni( ( QPrinter::PrinterState ) ( p )->printerState() );
}

/* virtual QVariant property ( PrintEnginePropertyKey key ) const */
HB_FUNC( QT_QPRINTENGINE_PROPERTY )
{
   QPrintEngine * p = hbqt_par_QPrintEngine( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->property( ( QPrintEngine::PrintEnginePropertyKey ) hb_parni( 2 ) ) ), true ) );
}

/* virtual void setProperty ( PrintEnginePropertyKey key, const QVariant & value ) */
HB_FUNC( QT_QPRINTENGINE_SETPROPERTY )
{
   QPrintEngine * p = hbqt_par_QPrintEngine( 1 );
   if( p )
      ( p )->setProperty( ( QPrintEngine::PrintEnginePropertyKey ) hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
