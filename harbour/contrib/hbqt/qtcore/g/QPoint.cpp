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

#include <QtCore/QPoint>


/*
 * QPoint ()
 * QPoint ( const QPoint & point )
 * QPoint ( int x, int y )
 * ~QPoint ()
 */

typedef struct
{
   QPoint * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPoint;

HBQT_GC_FUNC( hbqt_gcRelease_QPoint )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QPoint * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPoint( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QPoint * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPoint;
   p->type = HBQT_TYPE_QPoint;

   return p;
}

HB_FUNC( QT_QPOINT )
{
   QPoint * pObj = NULL;

   if( hb_pcount() == 2 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = new QPoint( hb_parni( 1 ), hb_parni( 2 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QPoint( *hbqt_par_QPoint( 1 ) ) ;
   }
   else
   {
      pObj = new QPoint() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QPoint( ( void * ) pObj, true ) );
}

/* bool isNull () const */
HB_FUNC( QT_QPOINT_ISNULL )
{
   QPoint * p = hbqt_par_QPoint( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
}

/* int manhattanLength () const */
HB_FUNC( QT_QPOINT_MANHATTANLENGTH )
{
   QPoint * p = hbqt_par_QPoint( 1 );
   if( p )
      hb_retni( ( p )->manhattanLength() );
}

/* int & rx () */
HB_FUNC( QT_QPOINT_RX )
{
   QPoint * p = hbqt_par_QPoint( 1 );
   if( p )
      hb_retni( ( p )->rx() );
}

/* int & ry () */
HB_FUNC( QT_QPOINT_RY )
{
   QPoint * p = hbqt_par_QPoint( 1 );
   if( p )
      hb_retni( ( p )->ry() );
}

/* void setX ( int x ) */
HB_FUNC( QT_QPOINT_SETX )
{
   QPoint * p = hbqt_par_QPoint( 1 );
   if( p )
      ( p )->setX( hb_parni( 2 ) );
}

/* void setY ( int y ) */
HB_FUNC( QT_QPOINT_SETY )
{
   QPoint * p = hbqt_par_QPoint( 1 );
   if( p )
      ( p )->setY( hb_parni( 2 ) );
}

/* int x () const */
HB_FUNC( QT_QPOINT_X )
{
   QPoint * p = hbqt_par_QPoint( 1 );
   if( p )
      hb_retni( ( p )->x() );
}

/* int y () const */
HB_FUNC( QT_QPOINT_Y )
{
   QPoint * p = hbqt_par_QPoint( 1 );
   if( p )
      hb_retni( ( p )->y() );
}


#endif /* #if QT_VERSION >= 0x040500 */
