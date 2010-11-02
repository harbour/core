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
 */

#include <QtCore/QPointer>

#include <QtGui/QLinearGradient>


/*
 * QLinearGradient ()
 * QLinearGradient ( const QPointF & start, const QPointF & finalStop )
 * QLinearGradient ( qreal x1, qreal y1, qreal x2, qreal y2 )
 */

typedef struct
{
   QLinearGradient * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QLinearGradient;

HBQT_GC_FUNC( hbqt_gcRelease_QLinearGradient )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QLinearGradient * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QLinearGradient( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QLinearGradient * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QLinearGradient;
   p->type = HBQT_TYPE_QLinearGradient;

   return p;
}

HB_FUNC( QT_QLINEARGRADIENT )
{
   QLinearGradient * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QLinearGradient( *hbqt_par_QLinearGradient( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new QLinearGradient( *hbqt_par_QPointF( 1 ), *hbqt_par_QPointF( 2 ) ) ;
   }
   else if( hb_pcount() == 4 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) )
   {
      pObj = new QLinearGradient( hb_parnd( 1 ), hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) ) ;
   }
   else
   {
      pObj = new QLinearGradient() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QLinearGradient( ( void * ) pObj, true ) );
}

/* QPointF finalStop () const */
HB_FUNC( QT_QLINEARGRADIENT_FINALSTOP )
{
   QLinearGradient * p = hbqt_par_QLinearGradient( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->finalStop() ), true ) );
}

/* void setFinalStop ( const QPointF & stop ) */
HB_FUNC( QT_QLINEARGRADIENT_SETFINALSTOP )
{
   QLinearGradient * p = hbqt_par_QLinearGradient( 1 );
   if( p )
      ( p )->setFinalStop( *hbqt_par_QPointF( 2 ) );
}

/* void setFinalStop ( qreal x, qreal y ) */
HB_FUNC( QT_QLINEARGRADIENT_SETFINALSTOP_1 )
{
   QLinearGradient * p = hbqt_par_QLinearGradient( 1 );
   if( p )
      ( p )->setFinalStop( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/* void setStart ( const QPointF & start ) */
HB_FUNC( QT_QLINEARGRADIENT_SETSTART )
{
   QLinearGradient * p = hbqt_par_QLinearGradient( 1 );
   if( p )
      ( p )->setStart( *hbqt_par_QPointF( 2 ) );
}

/* void setStart ( qreal x, qreal y ) */
HB_FUNC( QT_QLINEARGRADIENT_SETSTART_1 )
{
   QLinearGradient * p = hbqt_par_QLinearGradient( 1 );
   if( p )
      ( p )->setStart( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/* QPointF start () const */
HB_FUNC( QT_QLINEARGRADIENT_START )
{
   QLinearGradient * p = hbqt_par_QLinearGradient( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->start() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
