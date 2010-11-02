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
 *  Constructed[ 17/17 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QLine>


/*
 * QLine ()
 * QLine ( const QPoint & p1, const QPoint & p2 )
 * QLine ( int x1, int y1, int x2, int y2 )
 * QLine ( const QLine & other )
 */

typedef struct
{
   QLine * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QLine;

HBQT_GC_FUNC( hbqt_gcRelease_QLine )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QLine * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QLine( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QLine * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QLine;
   p->type = HBQT_TYPE_QLine;

   return p;
}

HB_FUNC( QT_QLINE )
{
   QLine * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QLine( *hbqt_par_QLine( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new QLine( *hbqt_par_QPoint( 1 ), *hbqt_par_QPoint( 2 ) ) ;
   }
   else if( hb_pcount() == 4 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) )
   {
      pObj = new QLine( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) ;
   }
   else
   {
      pObj = new QLine() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QLine( ( void * ) pObj, true ) );
}

/* QPoint p1 () const */
HB_FUNC( QT_QLINE_P1 )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->p1() ), true ) );
}

/* QPoint p2 () const */
HB_FUNC( QT_QLINE_P2 )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->p2() ), true ) );
}

/* int x1 () const */
HB_FUNC( QT_QLINE_X1 )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retni( ( p )->x1() );
}

/* int x2 () const */
HB_FUNC( QT_QLINE_X2 )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retni( ( p )->x2() );
}

/* int y1 () const */
HB_FUNC( QT_QLINE_Y1 )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retni( ( p )->y1() );
}

/* int y2 () const */
HB_FUNC( QT_QLINE_Y2 )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retni( ( p )->y2() );
}

/* int dx () const */
HB_FUNC( QT_QLINE_DX )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retni( ( p )->dx() );
}

/* int dy () const */
HB_FUNC( QT_QLINE_DY )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retni( ( p )->dy() );
}

/* bool isNull () const */
HB_FUNC( QT_QLINE_ISNULL )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
}

/* void setP1 ( const QPoint & p1 ) */
HB_FUNC( QT_QLINE_SETP1 )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      ( p )->setP1( *hbqt_par_QPoint( 2 ) );
}

/* void setP2 ( const QPoint & p2 ) */
HB_FUNC( QT_QLINE_SETP2 )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      ( p )->setP2( *hbqt_par_QPoint( 2 ) );
}

/* void setLine ( int x1, int y1, int x2, int y2 ) */
HB_FUNC( QT_QLINE_SETLINE )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      ( p )->setLine( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/* void setPoints ( const QPoint & p1, const QPoint & p2 ) */
HB_FUNC( QT_QLINE_SETPOINTS )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      ( p )->setPoints( *hbqt_par_QPoint( 2 ), *hbqt_par_QPoint( 3 ) );
}

/* void translate ( const QPoint & offset ) */
HB_FUNC( QT_QLINE_TRANSLATE )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      ( p )->translate( *hbqt_par_QPoint( 2 ) );
}

/* void translate ( int dx, int dy ) */
HB_FUNC( QT_QLINE_TRANSLATE_1 )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      ( p )->translate( hb_parni( 2 ), hb_parni( 3 ) );
}

/* QLine translated ( const QPoint & offset ) const */
HB_FUNC( QT_QLINE_TRANSLATED )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLine( new QLine( ( p )->translated( *hbqt_par_QPoint( 2 ) ) ), true ) );
}

/* QLine translated ( int dx, int dy ) const */
HB_FUNC( QT_QLINE_TRANSLATED_1 )
{
   QLine * p = hbqt_par_QLine( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLine( new QLine( ( p )->translated( hb_parni( 2 ), hb_parni( 3 ) ) ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
