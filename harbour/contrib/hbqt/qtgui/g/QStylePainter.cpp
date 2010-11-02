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
 *  Constructed[ 8/8 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStylePainter>


/*
 * QStylePainter ()
 * QStylePainter ( QWidget * widget )
 * QStylePainter ( QPaintDevice * pd, QWidget * widget )
 */

typedef struct
{
   QStylePainter * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStylePainter;

HBQT_GC_FUNC( hbqt_gcRelease_QStylePainter )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QStylePainter * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QStylePainter( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStylePainter * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStylePainter;
   p->type = HBQT_TYPE_QStylePainter;

   return p;
}

HB_FUNC( QT_QSTYLEPAINTER )
{
   QStylePainter * pObj = NULL;

   pObj = new QStylePainter() ;

   hb_retptrGC( hbqt_gcAllocate_QStylePainter( ( void * ) pObj, true ) );
}

/* bool begin ( QWidget * widget ) */
HB_FUNC( QT_QSTYLEPAINTER_BEGIN )
{
   QStylePainter * p = hbqt_par_QStylePainter( 1 );
   if( p )
      hb_retl( ( p )->begin( hbqt_par_QWidget( 2 ) ) );
}

/* bool begin ( QPaintDevice * pd, QWidget * widget ) */
HB_FUNC( QT_QSTYLEPAINTER_BEGIN_1 )
{
   QStylePainter * p = hbqt_par_QStylePainter( 1 );
   if( p )
      hb_retl( ( p )->begin( hbqt_par_QPaintDevice( 2 ), hbqt_par_QWidget( 3 ) ) );
}

/* void drawComplexControl ( QStyle::ComplexControl cc, const QStyleOptionComplex & option ) */
HB_FUNC( QT_QSTYLEPAINTER_DRAWCOMPLEXCONTROL )
{
   QStylePainter * p = hbqt_par_QStylePainter( 1 );
   if( p )
      ( p )->drawComplexControl( ( QStyle::ComplexControl ) hb_parni( 2 ), *hbqt_par_QStyleOptionComplex( 3 ) );
}

/* void drawControl ( QStyle::ControlElement ce, const QStyleOption & option ) */
HB_FUNC( QT_QSTYLEPAINTER_DRAWCONTROL )
{
   QStylePainter * p = hbqt_par_QStylePainter( 1 );
   if( p )
      ( p )->drawControl( ( QStyle::ControlElement ) hb_parni( 2 ), *hbqt_par_QStyleOption( 3 ) );
}

/* void drawItemPixmap ( const QRect & rect, int flags, const QPixmap & pixmap ) */
HB_FUNC( QT_QSTYLEPAINTER_DRAWITEMPIXMAP )
{
   QStylePainter * p = hbqt_par_QStylePainter( 1 );
   if( p )
      ( p )->drawItemPixmap( *hbqt_par_QRect( 2 ), hb_parni( 3 ), *hbqt_par_QPixmap( 4 ) );
}

/* void drawItemText ( const QRect & rect, int flags, const QPalette & pal, bool enabled, const QString & text, QPalette::ColorRole textRole = QPalette::NoRole ) */
HB_FUNC( QT_QSTYLEPAINTER_DRAWITEMTEXT )
{
   QStylePainter * p = hbqt_par_QStylePainter( 1 );
   if( p )
   {
      void * pText;
      ( p )->drawItemText( *hbqt_par_QRect( 2 ), hb_parni( 3 ), *hbqt_par_QPalette( 4 ), hb_parl( 5 ), hb_parstr_utf8( 6, &pText, NULL ), ( HB_ISNUM( 7 ) ? ( QPalette::ColorRole ) hb_parni( 7 ) : ( QPalette::ColorRole ) QPalette::NoRole ) );
      hb_strfree( pText );
   }
}

/* void drawPrimitive ( QStyle::PrimitiveElement pe, const QStyleOption & option ) */
HB_FUNC( QT_QSTYLEPAINTER_DRAWPRIMITIVE )
{
   QStylePainter * p = hbqt_par_QStylePainter( 1 );
   if( p )
      ( p )->drawPrimitive( ( QStyle::PrimitiveElement ) hb_parni( 2 ), *hbqt_par_QStyleOption( 3 ) );
}

/* QStyle * style () const */
HB_FUNC( QT_QSTYLEPAINTER_STYLE )
{
   QStylePainter * p = hbqt_par_QStylePainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStyle( ( p )->style(), false ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
