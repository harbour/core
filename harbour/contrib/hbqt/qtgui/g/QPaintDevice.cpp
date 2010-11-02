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
 *  enum PaintDeviceMetric { PdmWidth, PdmHeight, PdmWidthMM, PdmHeightMM, ..., PdmPhysicalDpiY }
 */

/*
 *  Constructed[ 12/12 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QPaintDevice>

/*
 * QPaintDevice ()
 * virtual ~QPaintDevice ()
 */

typedef struct
{
   QPaintDevice * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPaintDevice;

HBQT_GC_FUNC( hbqt_gcRelease_QPaintDevice )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QPaintDevice( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QPaintDevice * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPaintDevice;
   p->type = HBQT_TYPE_QPaintDevice;

   return p;
}

HB_FUNC( QT_QPAINTDEVICE )
{

}

/* int depth () const */
HB_FUNC( QT_QPAINTDEVICE_DEPTH )
{
   QPaintDevice * p = hbqt_par_QPaintDevice( 1 );
   if( p )
      hb_retni( ( p )->depth() );
}

/* int height () const */
HB_FUNC( QT_QPAINTDEVICE_HEIGHT )
{
   QPaintDevice * p = hbqt_par_QPaintDevice( 1 );
   if( p )
      hb_retni( ( p )->height() );
}

/* int heightMM () const */
HB_FUNC( QT_QPAINTDEVICE_HEIGHTMM )
{
   QPaintDevice * p = hbqt_par_QPaintDevice( 1 );
   if( p )
      hb_retni( ( p )->heightMM() );
}

/* int logicalDpiX () const */
HB_FUNC( QT_QPAINTDEVICE_LOGICALDPIX )
{
   QPaintDevice * p = hbqt_par_QPaintDevice( 1 );
   if( p )
      hb_retni( ( p )->logicalDpiX() );
}

/* int logicalDpiY () const */
HB_FUNC( QT_QPAINTDEVICE_LOGICALDPIY )
{
   QPaintDevice * p = hbqt_par_QPaintDevice( 1 );
   if( p )
      hb_retni( ( p )->logicalDpiY() );
}

/* int numColors () const */
HB_FUNC( QT_QPAINTDEVICE_NUMCOLORS )
{
   QPaintDevice * p = hbqt_par_QPaintDevice( 1 );
   if( p )
      hb_retni( ( p )->numColors() );
}

/* virtual QPaintEngine * paintEngine () const = 0 */
HB_FUNC( QT_QPAINTDEVICE_PAINTENGINE )
{
   QPaintDevice * p = hbqt_par_QPaintDevice( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPaintEngine( ( p )->paintEngine(), false ) );
}

/* bool paintingActive () const */
HB_FUNC( QT_QPAINTDEVICE_PAINTINGACTIVE )
{
   QPaintDevice * p = hbqt_par_QPaintDevice( 1 );
   if( p )
      hb_retl( ( p )->paintingActive() );
}

/* int physicalDpiX () const */
HB_FUNC( QT_QPAINTDEVICE_PHYSICALDPIX )
{
   QPaintDevice * p = hbqt_par_QPaintDevice( 1 );
   if( p )
      hb_retni( ( p )->physicalDpiX() );
}

/* int physicalDpiY () const */
HB_FUNC( QT_QPAINTDEVICE_PHYSICALDPIY )
{
   QPaintDevice * p = hbqt_par_QPaintDevice( 1 );
   if( p )
      hb_retni( ( p )->physicalDpiY() );
}

/* int width () const */
HB_FUNC( QT_QPAINTDEVICE_WIDTH )
{
   QPaintDevice * p = hbqt_par_QPaintDevice( 1 );
   if( p )
      hb_retni( ( p )->width() );
}

/* int widthMM () const */
HB_FUNC( QT_QPAINTDEVICE_WIDTHMM )
{
   QPaintDevice * p = hbqt_par_QPaintDevice( 1 );
   if( p )
      hb_retni( ( p )->widthMM() );
}


#endif /* #if QT_VERSION >= 0x040500 */
