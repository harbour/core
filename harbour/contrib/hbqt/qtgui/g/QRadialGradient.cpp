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
 *  Constructed[ 8/8 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QRadialGradient>


/*
 * QRadialGradient ()
 * QRadialGradient ( const QPointF & center, qreal radius, const QPointF & focalPoint )
 * QRadialGradient ( qreal cx, qreal cy, qreal radius, qreal fx, qreal fy )
 * QRadialGradient ( const QPointF & center, qreal radius )
 * QRadialGradient ( qreal cx, qreal cy, qreal radius )
 */

typedef struct
{
   QRadialGradient * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QRadialGradient;

HBQT_GC_FUNC( hbqt_gcRelease_QRadialGradient )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QRadialGradient * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QRadialGradient( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QRadialGradient * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QRadialGradient;
   p->type = HBQT_TYPE_QRadialGradient;

   return p;
}

HB_FUNC( QT_QRADIALGRADIENT )
{
   QRadialGradient * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QRadialGradient( *hbqt_par_QRadialGradient( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = new QRadialGradient( *hbqt_par_QPointF( 1 ), hb_parnd( 2 ) ) ;
   }
   else if( hb_pcount() == 3 && HB_ISPOINTER( 1 ) && HB_ISNUM( 2 ) && HB_ISPOINTER( 3 ) )
   {
      pObj = new QRadialGradient( *hbqt_par_QPointF( 1 ), hb_parnd( 2 ), *hbqt_par_QPointF( 3 ) ) ;
   }
   else if( hb_pcount() == 5 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) && HB_ISNUM( 5 ) )
   {
      pObj = new QRadialGradient( hb_parnd( 1 ), hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ;
   }
   else
   {
      pObj = new QRadialGradient() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QRadialGradient( ( void * ) pObj, true ) );
}

/* QPointF center () const */
HB_FUNC( QT_QRADIALGRADIENT_CENTER )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->center() ), true ) );
}

/* QPointF focalPoint () const */
HB_FUNC( QT_QRADIALGRADIENT_FOCALPOINT )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->focalPoint() ), true ) );
}

/* qreal radius () const */
HB_FUNC( QT_QRADIALGRADIENT_RADIUS )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
      hb_retnd( ( p )->radius() );
}

/* void setCenter ( const QPointF & center ) */
HB_FUNC( QT_QRADIALGRADIENT_SETCENTER )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
      ( p )->setCenter( *hbqt_par_QPointF( 2 ) );
}

/* void setCenter ( qreal x, qreal y ) */
HB_FUNC( QT_QRADIALGRADIENT_SETCENTER_1 )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
      ( p )->setCenter( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/* void setFocalPoint ( const QPointF & focalPoint ) */
HB_FUNC( QT_QRADIALGRADIENT_SETFOCALPOINT )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
      ( p )->setFocalPoint( *hbqt_par_QPointF( 2 ) );
}

/* void setFocalPoint ( qreal x, qreal y ) */
HB_FUNC( QT_QRADIALGRADIENT_SETFOCALPOINT_1 )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
      ( p )->setFocalPoint( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/* void setRadius ( qreal radius ) */
HB_FUNC( QT_QRADIALGRADIENT_SETRADIUS )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
      ( p )->setRadius( hb_parnd( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
