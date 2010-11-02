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
 *  Constructed[ 15/15 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QBrush>


/*
 * QBrush ()
 * QBrush ( Qt::BrushStyle style )
 * QBrush ( const QColor & color, Qt::BrushStyle style = Qt::SolidPattern )
 * QBrush ( Qt::GlobalColor color, Qt::BrushStyle style = Qt::SolidPattern )
 * QBrush ( const QColor & color, const QPixmap & pixmap )
 * QBrush ( Qt::GlobalColor color, const QPixmap & pixmap )
 * QBrush ( const QPixmap & pixmap )
 * QBrush ( const QImage & image )
 * QBrush ( const QBrush & other )
 * QBrush ( const QGradient & gradient )
 * ~QBrush ()
 */

typedef struct
{
   QBrush * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QBrush;

HBQT_GC_FUNC( hbqt_gcRelease_QBrush )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QBrush * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QBrush( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QBrush * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QBrush;
   p->type = HBQT_TYPE_QBrush;

   return p;
}

HB_FUNC( QT_QBRUSH )
{
   QBrush * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
      if( q )
      {
         if( q->type == HBQT_TYPE_QBrush )
         {
            pObj = new QBrush( *hbqt_par_QBrush( 1 ) ) ;
         }
         else if( q->type == HBQT_TYPE_QPixmap )
         {
            pObj = new QBrush( *hbqt_par_QPixmap( 1 ) ) ;
         }
         else if( q->type == HBQT_TYPE_QImage )
         {
            pObj = new QBrush( *hbqt_par_QImage( 1 ) ) ;
         }
         else if( q->type == HBQT_TYPE_QGradient )
         {
            pObj = new QBrush( *hbqt_par_QGradient( 1 ) ) ;
         }
         else if( q->type == HBQT_TYPE_QColor )
         {
            pObj = new QBrush( *hbqt_par_QColor( 1 ), Qt::SolidPattern ) ;
         }
      }
      else
      {
         pObj = new QBrush( *hbqt_par_QBrush( 1 ) ) ;
      }
   }
   else if( hb_pcount() == 1 && HB_ISNUM( 1 ) )
   {
      pObj = new QBrush( ( Qt::GlobalColor ) hb_parni( 1 ), Qt::SolidPattern ) ;
   }
   else if( hb_pcount() == 2 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = new QBrush( ( Qt::GlobalColor ) hb_parni( 1 ), ( Qt::BrushStyle ) hb_parni( 2 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = new QBrush( *hbqt_par_QColor( 1 ), ( Qt::BrushStyle ) hb_parni( 2 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new QBrush( *hbqt_par_QColor( 1 ), *hbqt_par_QPixmap( 2 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISNUM( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new QBrush( ( Qt::GlobalColor ) hb_parni( 1 ), *hbqt_par_QPixmap( 2 ) ) ;
   }
   else if( hb_pcount() >= 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      QString objName = hbqt_par_QString( 1 );

      if(      objName == ( QString ) "QPixmap" )
      {
         pObj = new QBrush( *hbqt_par_QPixmap( 2 ) ) ;
      }
      else if( objName == ( QString ) "QImage" )
      {
         pObj = new QBrush( *hbqt_par_QImage( 2 ) ) ;
      }
      else if( objName == ( QString ) "QGradient" )
      {
         pObj = new QBrush( *hbqt_par_QGradient( 2 ) ) ;
      }
      else if( objName == ( QString ) "QColor" )
      {
         pObj = new QBrush( *hbqt_par_QColor( 2 ), HB_ISNUM( 3 ) ? ( Qt::BrushStyle ) hb_parni( 3 ) : Qt::SolidPattern ) ;
      }
      else
      {
         pObj = new QBrush() ;
      }
   }
   else
   {
      pObj = new QBrush() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QBrush( ( void * ) pObj, true ) );
}

/* const QColor & color () const */
HB_FUNC( QT_QBRUSH_COLOR )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->color() ), true ) );
}

/* const QGradient * gradient () const */
HB_FUNC( QT_QBRUSH_GRADIENT )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGradient( new QGradient( *( ( p )->gradient() ) ), true ) );
}

/* bool isOpaque () const */
HB_FUNC( QT_QBRUSH_ISOPAQUE )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      hb_retl( ( p )->isOpaque() );
}

/* const QMatrix & matrix () const */
HB_FUNC( QT_QBRUSH_MATRIX )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->matrix() ), true ) );
}

/* void setColor ( const QColor & color ) */
HB_FUNC( QT_QBRUSH_SETCOLOR )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      ( p )->setColor( *hbqt_par_QColor( 2 ) );
}

/* void setColor ( Qt::GlobalColor color ) */
HB_FUNC( QT_QBRUSH_SETCOLOR_1 )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      ( p )->setColor( ( Qt::GlobalColor ) hb_parni( 2 ) );
}

/* void setMatrix ( const QMatrix & matrix ) */
HB_FUNC( QT_QBRUSH_SETMATRIX )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      ( p )->setMatrix( *hbqt_par_QMatrix( 2 ) );
}

/* void setStyle ( Qt::BrushStyle style ) */
HB_FUNC( QT_QBRUSH_SETSTYLE )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      ( p )->setStyle( ( Qt::BrushStyle ) hb_parni( 2 ) );
}

/* void setTexture ( const QPixmap & pixmap ) */
HB_FUNC( QT_QBRUSH_SETTEXTURE )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      ( p )->setTexture( *hbqt_par_QPixmap( 2 ) );
}

/* void setTextureImage ( const QImage & image ) */
HB_FUNC( QT_QBRUSH_SETTEXTUREIMAGE )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      ( p )->setTextureImage( *hbqt_par_QImage( 2 ) );
}

/* void setTransform ( const QTransform & matrix ) */
HB_FUNC( QT_QBRUSH_SETTRANSFORM )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      ( p )->setTransform( *hbqt_par_QTransform( 2 ) );
}

/* Qt::BrushStyle style () const */
HB_FUNC( QT_QBRUSH_STYLE )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      hb_retni( ( Qt::BrushStyle ) ( p )->style() );
}

/* QPixmap texture () const */
HB_FUNC( QT_QBRUSH_TEXTURE )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->texture() ), true ) );
}

/* QImage textureImage () const */
HB_FUNC( QT_QBRUSH_TEXTUREIMAGE )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->textureImage() ), true ) );
}

/* QTransform transform () const */
HB_FUNC( QT_QBRUSH_TRANSFORM )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->transform() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
