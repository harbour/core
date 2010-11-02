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

#if QT_VERSION >= 0x040500

/*
 *  Constructed[ 8/8 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QPointF>


/*
 * QPointF ()
 * QPointF ( const QPoint & point )
 * QPointF ( qreal x, qreal y )
 * ~QPointF ()
 */

typedef struct
{
   QPointF * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPointF;

HBQT_GC_FUNC( hbqt_gcRelease_QPointF )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QPointF * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QPointF( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QPointF * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPointF;
   p->type = HBQT_TYPE_QPointF;

   return p;
}

HB_FUNC( QT_QPOINTF )
{
   QPointF * pObj = NULL;

   if( hb_pcount() == 2 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = new QPointF( ( qreal ) hb_parnd( 1 ), ( qreal ) hb_parnd( 2 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QPointF( *hbqt_par_QPoint( 1 ) ) ;
   }
   else
   {
      pObj = new QPointF() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QPointF( ( void * ) pObj, true ) );
}

/* bool isNull () const */
HB_FUNC( QT_QPOINTF_ISNULL )
{
   QPointF * p = hbqt_par_QPointF( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
}

/* qreal & rx () */
HB_FUNC( QT_QPOINTF_RX )
{
   QPointF * p = hbqt_par_QPointF( 1 );
   if( p )
      hb_retnd( ( p )->rx() );
}

/* qreal & ry () */
HB_FUNC( QT_QPOINTF_RY )
{
   QPointF * p = hbqt_par_QPointF( 1 );
   if( p )
      hb_retnd( ( p )->ry() );
}

/* void setX ( qreal x ) */
HB_FUNC( QT_QPOINTF_SETX )
{
   QPointF * p = hbqt_par_QPointF( 1 );
   if( p )
      ( p )->setX( hb_parnd( 2 ) );
}

/* void setY ( qreal y ) */
HB_FUNC( QT_QPOINTF_SETY )
{
   QPointF * p = hbqt_par_QPointF( 1 );
   if( p )
      ( p )->setY( hb_parnd( 2 ) );
}

/* QPoint toPoint () const */
HB_FUNC( QT_QPOINTF_TOPOINT )
{
   QPointF * p = hbqt_par_QPointF( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->toPoint() ), true ) );
}

/* qreal x () const */
HB_FUNC( QT_QPOINTF_X )
{
   QPointF * p = hbqt_par_QPointF( 1 );
   if( p )
      hb_retnd( ( p )->x() );
}

/* qreal y () const */
HB_FUNC( QT_QPOINTF_Y )
{
   QPointF * p = hbqt_par_QPointF( 1 );
   if( p )
      hb_retnd( ( p )->y() );
}


#endif /* #if QT_VERSION >= 0x040500 */
