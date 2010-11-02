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
 *  Constructed[ 14/14 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QSize>


/*
 * QSize ()
 * QSize ( const QSize & size )
 * QSize ( int width, int height )
 * ~QSize ()
 */

typedef struct
{
   QSize * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSize;

HBQT_GC_FUNC( hbqt_gcRelease_QSize )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QSize * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QSize( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QSize * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSize;
   p->type = HBQT_TYPE_QSize;

   return p;
}

HB_FUNC( QT_QSIZE )
{
   QSize * pObj = NULL;

   if( hb_pcount() == 2 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = new QSize( hb_parni( 1 ), hb_parni( 2 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QSize( *hbqt_par_QSize( 1 ) ) ;
   }
   else
   {
      pObj = new QSize() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QSize( ( void * ) pObj, true ) );
}

/* int height () const */
HB_FUNC( QT_QSIZE_HEIGHT )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
      hb_retni( ( p )->height() );
}

/* bool isEmpty () const */
HB_FUNC( QT_QSIZE_ISEMPTY )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
}

/* bool isNull () const */
HB_FUNC( QT_QSIZE_ISNULL )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
}

/* bool isValid () const */
HB_FUNC( QT_QSIZE_ISVALID )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* int & rheight () */
HB_FUNC( QT_QSIZE_RHEIGHT )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
      hb_retni( ( p )->rheight() );
}

/* int & rwidth () */
HB_FUNC( QT_QSIZE_RWIDTH )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
      hb_retni( ( p )->rwidth() );
}

/* void scale ( int width, int height, Qt::AspectRatioMode mode ) */
HB_FUNC( QT_QSIZE_SCALE )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
      ( p )->scale( hb_parni( 2 ), hb_parni( 3 ), ( Qt::AspectRatioMode ) hb_parni( 4 ) );
}

/* void scale ( const QSize & size, Qt::AspectRatioMode mode ) */
HB_FUNC( QT_QSIZE_SCALE_1 )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
      ( p )->scale( *hbqt_par_QSize( 2 ), ( Qt::AspectRatioMode ) hb_parni( 3 ) );
}

/* void setHeight ( int height ) */
HB_FUNC( QT_QSIZE_SETHEIGHT )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
      ( p )->setHeight( hb_parni( 2 ) );
}

/* void setWidth ( int width ) */
HB_FUNC( QT_QSIZE_SETWIDTH )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
      ( p )->setWidth( hb_parni( 2 ) );
}

/* void transpose () */
HB_FUNC( QT_QSIZE_TRANSPOSE )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
      ( p )->transpose();
}

/* int width () const */
HB_FUNC( QT_QSIZE_WIDTH )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
      hb_retni( ( p )->width() );
}

/* QSize boundedTo ( const QSize & otherSize ) const */
HB_FUNC( QT_QSIZE_BOUNDEDTO )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->boundedTo( *hbqt_par_QSize( 2 ) ) ), true ) );
}

/* QSize expandedTo ( const QSize & otherSize ) const */
HB_FUNC( QT_QSIZE_EXPANDEDTO )
{
   QSize * p = hbqt_par_QSize( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->expandedTo( *hbqt_par_QSize( 2 ) ) ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
