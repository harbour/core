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
 *  enum CoordinateMode { LogicalMode, StretchToDeviceMode, ObjectBoundingMode }
 *  enum Spread { PadSpread, RepeatSpread, ReflectSpread }
 *  enum Type { LinearGradient, RadialGradient, ConicalGradient, NoGradient }
 */

/*
 *  Constructed[ 6/6 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // void setStops ( const QGradientStops & stopPoints )
 *  // QGradientStops stops () const
 */

#include <QtCore/QPointer>

#include <QtGui/QGradient>


/*
 * QGradient ()
 */

typedef struct
{
   QGradient * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGradient;

HBQT_GC_FUNC( hbqt_gcRelease_QGradient )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QGradient( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGradient * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGradient;
   p->type = HBQT_TYPE_QGradient;

   return p;
}

HB_FUNC( QT_QGRADIENT )
{

}

/* CoordinateMode coordinateMode () const */
HB_FUNC( QT_QGRADIENT_COORDINATEMODE )
{
   QGradient * p = hbqt_par_QGradient( 1 );
   if( p )
      hb_retni( ( QGradient::CoordinateMode ) ( p )->coordinateMode() );
}

/* void setColorAt ( qreal position, const QColor & color ) */
HB_FUNC( QT_QGRADIENT_SETCOLORAT )
{
   QGradient * p = hbqt_par_QGradient( 1 );
   if( p )
      ( p )->setColorAt( hb_parnd( 2 ), *hbqt_par_QColor( 3 ) );
}

/* void setCoordinateMode ( CoordinateMode mode ) */
HB_FUNC( QT_QGRADIENT_SETCOORDINATEMODE )
{
   QGradient * p = hbqt_par_QGradient( 1 );
   if( p )
      ( p )->setCoordinateMode( ( QGradient::CoordinateMode ) hb_parni( 2 ) );
}

/* void setSpread ( Spread method ) */
HB_FUNC( QT_QGRADIENT_SETSPREAD )
{
   QGradient * p = hbqt_par_QGradient( 1 );
   if( p )
      ( p )->setSpread( ( QGradient::Spread ) hb_parni( 2 ) );
}

/* Spread spread () const */
HB_FUNC( QT_QGRADIENT_SPREAD )
{
   QGradient * p = hbqt_par_QGradient( 1 );
   if( p )
      hb_retni( ( QGradient::Spread ) ( p )->spread() );
}

/* Type type () const */
HB_FUNC( QT_QGRADIENT_TYPE )
{
   QGradient * p = hbqt_par_QGradient( 1 );
   if( p )
      hb_retni( ( QGradient::Type ) ( p )->type() );
}


#endif /* #if QT_VERSION >= 0x040500 */
