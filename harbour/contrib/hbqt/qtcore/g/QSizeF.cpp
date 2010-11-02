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
 *  Constructed[ 15/15 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QSizeF>


/*
 * QSizeF ()
 * QSizeF ( const QSize & size )
 * QSizeF ( qreal width, qreal height )
 * ~QSizeF ()
 */

typedef struct
{
   QSizeF * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSizeF;

HBQT_GC_FUNC( hbqt_gcRelease_QSizeF )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QSizeF * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QSizeF( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QSizeF * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSizeF;
   p->type = HBQT_TYPE_QSizeF;

   return p;
}

HB_FUNC( QT_QSIZEF )
{
   QSizeF * pObj = NULL;

   if( hb_pcount() == 2 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = new QSizeF( hb_parnd( 1 ), hb_parnd( 2 ) ) ;
   }
   else
   {
      pObj = new QSizeF() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QSizeF( ( void * ) pObj, true ) );
}

/* QSizeF boundedTo ( const QSizeF & otherSize ) const */
HB_FUNC( QT_QSIZEF_BOUNDEDTO )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->boundedTo( *hbqt_par_QSizeF( 2 ) ) ), true ) );
}

/* QSizeF expandedTo ( const QSizeF & otherSize ) const */
HB_FUNC( QT_QSIZEF_EXPANDEDTO )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->expandedTo( *hbqt_par_QSizeF( 2 ) ) ), true ) );
}

/* qreal height () const */
HB_FUNC( QT_QSIZEF_HEIGHT )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      hb_retnd( ( p )->height() );
}

/* bool isEmpty () const */
HB_FUNC( QT_QSIZEF_ISEMPTY )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
}

/* bool isNull () const */
HB_FUNC( QT_QSIZEF_ISNULL )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
}

/* bool isValid () const */
HB_FUNC( QT_QSIZEF_ISVALID )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* qreal & rheight () */
HB_FUNC( QT_QSIZEF_RHEIGHT )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      hb_retnd( ( p )->rheight() );
}

/* qreal & rwidth () */
HB_FUNC( QT_QSIZEF_RWIDTH )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      hb_retnd( ( p )->rwidth() );
}

/* void scale ( qreal width, qreal height, Qt::AspectRatioMode mode ) */
HB_FUNC( QT_QSIZEF_SCALE )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      ( p )->scale( hb_parnd( 2 ), hb_parnd( 3 ), ( Qt::AspectRatioMode ) hb_parni( 4 ) );
}

/* void scale ( const QSizeF & size, Qt::AspectRatioMode mode ) */
HB_FUNC( QT_QSIZEF_SCALE_1 )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      ( p )->scale( *hbqt_par_QSizeF( 2 ), ( Qt::AspectRatioMode ) hb_parni( 3 ) );
}

/* void setHeight ( qreal height ) */
HB_FUNC( QT_QSIZEF_SETHEIGHT )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      ( p )->setHeight( hb_parnd( 2 ) );
}

/* void setWidth ( qreal width ) */
HB_FUNC( QT_QSIZEF_SETWIDTH )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      ( p )->setWidth( hb_parnd( 2 ) );
}

/* QSize toSize () const */
HB_FUNC( QT_QSIZEF_TOSIZE )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->toSize() ), true ) );
}

/* void transpose () */
HB_FUNC( QT_QSIZEF_TRANSPOSE )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      ( p )->transpose();
}

/* qreal width () const */
HB_FUNC( QT_QSIZEF_WIDTH )
{
   QSizeF * p = hbqt_par_QSizeF( 1 );
   if( p )
      hb_retnd( ( p )->width() );
}


#endif /* #if QT_VERSION >= 0x040500 */
