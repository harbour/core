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
 *  enum RegionType { Rectangle, Ellipse }
 */

/*
 *  Constructed[ 18/19 [ 94.74% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  QVector<QRect> rects () const
 *
 *  *** Commented out protostypes ***
 *
 *  // Handle handle () const
 */

#include <QtCore/QPointer>

#include <QtGui/QRegion>


/*
 * QRegion ()
 * QRegion ( int x, int y, int w, int h, RegionType t = Rectangle )
 * QRegion ( const QPolygon & a, Qt::FillRule fillRule = Qt::OddEvenFill )
 * QRegion ( const QRegion & r )
 * QRegion ( const QBitmap & bm )
 * QRegion ( const QRect & r, RegionType t = Rectangle )
 * ~QRegion ()
 */

typedef struct
{
   QRegion * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QRegion;

HBQT_GC_FUNC( hbqt_gcRelease_QRegion )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QRegion * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QRegion( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QRegion * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QRegion;
   p->type = HBQT_TYPE_QRegion;

   return p;
}

HB_FUNC( QT_QREGION )
{
   QRegion * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QRegion( *hbqt_par_QRegion( 1 ) ) ;
   }
   else if( hb_pcount() >= 4 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) )
   {
      pObj = new QRegion( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), HB_ISNUM( 5 ) ? ( QRegion::RegionType ) hb_parni( 5 ) : QRegion::Rectangle ) ;
   }
   else if( hb_pcount() >= 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      if(      ( QString ) "QPolygon" == hbqt_par_QString( 1 ) )
      {
         pObj = new QRegion( *hbqt_par_QPolygon( 2 ), HB_ISNUM( 3 ) ? ( Qt::FillRule ) hb_parni( 3 ) : Qt::OddEvenFill ) ;
      }
      else if( ( QString ) "QBitmap"  == hbqt_par_QString( 1 ) )
      {
         pObj = new QRegion( *hbqt_par_QBitmap( 2 ) ) ;
      }
      else if( ( QString ) "QRect"    == hbqt_par_QString( 1 ) )
      {
         pObj = new QRegion( *hbqt_par_QRect( 2 ), HB_ISNUM( 3 ) ? ( QRegion::RegionType ) hb_parni( 3 ) : QRegion::Rectangle ) ;
      }
      else
      {
         pObj = new QRegion() ;
      }
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = new QRegion( *hbqt_par_QRect( 1 ), ( QRegion::RegionType ) hb_parni( 2 ) ) ;
   }
   else
   {
      pObj = new QRegion() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QRegion( ( void * ) pObj, true ) );
}

/* QRect boundingRect () const */
HB_FUNC( QT_QREGION_BOUNDINGRECT )
{
   QRegion * p = hbqt_par_QRegion( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect() ), true ) );
}

/* bool contains ( const QPoint & p ) const */
HB_FUNC( QT_QREGION_CONTAINS )
{
   QRegion * p = hbqt_par_QRegion( 1 );
   if( p )
      hb_retl( ( p )->contains( *hbqt_par_QPoint( 2 ) ) );
}

/* bool contains ( const QRect & r ) const */
HB_FUNC( QT_QREGION_CONTAINS_1 )
{
   QRegion * p = hbqt_par_QRegion( 1 );
   if( p )
      hb_retl( ( p )->contains( *hbqt_par_QRect( 2 ) ) );
}

/* QRegion intersected ( const QRegion & r ) const */
HB_FUNC( QT_QREGION_INTERSECTED )
{
   QRegion * p = hbqt_par_QRegion( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->intersected( *hbqt_par_QRegion( 2 ) ) ), true ) );
}

/* QRegion intersected ( const QRect & rect ) const */
HB_FUNC( QT_QREGION_INTERSECTED_1 )
{
   QRegion * p = hbqt_par_QRegion( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->intersected( *hbqt_par_QRect( 2 ) ) ), true ) );
}

/* bool intersects ( const QRegion & region ) const */
HB_FUNC( QT_QREGION_INTERSECTS )
{
   QRegion * p = hbqt_par_QRegion( 1 );
   if( p )
      hb_retl( ( p )->intersects( *hbqt_par_QRegion( 2 ) ) );
}

/* bool intersects ( const QRect & rect ) const */
HB_FUNC( QT_QREGION_INTERSECTS_1 )
{
   QRegion * p = hbqt_par_QRegion( 1 );
   if( p )
      hb_retl( ( p )->intersects( *hbqt_par_QRect( 2 ) ) );
}

/* bool isEmpty () const */
HB_FUNC( QT_QREGION_ISEMPTY )
{
   QRegion * p = hbqt_par_QRegion( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
}

/* int numRects () const */
HB_FUNC( QT_QREGION_NUMRECTS )
{
   QRegion * p = hbqt_par_QRegion( 1 );
   if( p )
      hb_retni( ( p )->numRects() );
}

/* void setRects ( const QRect * rects, int number ) */
HB_FUNC( QT_QREGION_SETRECTS )
{
   QRegion * p = hbqt_par_QRegion( 1 );
   if( p )
      ( p )->setRects( hbqt_par_QRect( 2 ), hb_parni( 3 ) );
}

/* QRegion subtracted ( const QRegion & r ) const */
HB_FUNC( QT_QREGION_SUBTRACTED )
{
   QRegion * p = hbqt_par_QRegion( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->subtracted( *hbqt_par_QRegion( 2 ) ) ), true ) );
}

/* void translate ( int dx, int dy ) */
HB_FUNC( QT_QREGION_TRANSLATE )
{
   QRegion * p = hbqt_par_QRegion( 1 );
   if( p )
      ( p )->translate( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void translate ( const QPoint & point ) */
HB_FUNC( QT_QREGION_TRANSLATE_1 )
{
   QRegion * p = hbqt_par_QRegion( 1 );
   if( p )
      ( p )->translate( *hbqt_par_QPoint( 2 ) );
}

/* QRegion translated ( int dx, int dy ) const */
HB_FUNC( QT_QREGION_TRANSLATED )
{
   QRegion * p = hbqt_par_QRegion( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->translated( hb_parni( 2 ), hb_parni( 3 ) ) ), true ) );
}

/* QRegion translated ( const QPoint & p ) const */
HB_FUNC( QT_QREGION_TRANSLATED_1 )
{
   QRegion * p = hbqt_par_QRegion( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->translated( *hbqt_par_QPoint( 2 ) ) ), true ) );
}

/* QRegion united ( const QRegion & r ) const */
HB_FUNC( QT_QREGION_UNITED )
{
   QRegion * p = hbqt_par_QRegion( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->united( *hbqt_par_QRegion( 2 ) ) ), true ) );
}

/* QRegion united ( const QRect & rect ) const */
HB_FUNC( QT_QREGION_UNITED_1 )
{
   QRegion * p = hbqt_par_QRegion( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->united( *hbqt_par_QRect( 2 ) ) ), true ) );
}

/* QRegion xored ( const QRegion & r ) const */
HB_FUNC( QT_QREGION_XORED )
{
   QRegion * p = hbqt_par_QRegion( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->xored( *hbqt_par_QRegion( 2 ) ) ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
