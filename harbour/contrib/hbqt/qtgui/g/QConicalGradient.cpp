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
 *  Constructed[ 5/5 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QConicalGradient>


/*
 * QConicalGradient ()
 * QConicalGradient ( const QPointF & center, qreal angle )
 * QConicalGradient ( qreal cx, qreal cy, qreal angle )
 */

typedef struct
{
   QConicalGradient * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QConicalGradient;

HBQT_GC_FUNC( hbqt_gcRelease_QConicalGradient )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QConicalGradient * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QConicalGradient( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QConicalGradient * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QConicalGradient;
   p->type = HBQT_TYPE_QConicalGradient;

   return p;
}

HB_FUNC( QT_QCONICALGRADIENT )
{
   QConicalGradient * pObj = NULL;

   pObj = new QConicalGradient() ;

   hb_retptrGC( hbqt_gcAllocate_QConicalGradient( ( void * ) pObj, true ) );
}

/* qreal angle () const */
HB_FUNC( QT_QCONICALGRADIENT_ANGLE )
{
   QConicalGradient * p = hbqt_par_QConicalGradient( 1 );
   if( p )
      hb_retnd( ( p )->angle() );
}

/* QPointF center () const */
HB_FUNC( QT_QCONICALGRADIENT_CENTER )
{
   QConicalGradient * p = hbqt_par_QConicalGradient( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->center() ), true ) );
}

/* void setAngle ( qreal angle ) */
HB_FUNC( QT_QCONICALGRADIENT_SETANGLE )
{
   QConicalGradient * p = hbqt_par_QConicalGradient( 1 );
   if( p )
      ( p )->setAngle( hb_parnd( 2 ) );
}

/* void setCenter ( const QPointF & center ) */
HB_FUNC( QT_QCONICALGRADIENT_SETCENTER )
{
   QConicalGradient * p = hbqt_par_QConicalGradient( 1 );
   if( p )
      ( p )->setCenter( *hbqt_par_QPointF( 2 ) );
}

/* void setCenter ( qreal x, qreal y ) */
HB_FUNC( QT_QCONICALGRADIENT_SETCENTER_1 )
{
   QConicalGradient * p = hbqt_par_QConicalGradient( 1 );
   if( p )
      ( p )->setCenter( hb_parnd( 2 ), hb_parnd( 3 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
