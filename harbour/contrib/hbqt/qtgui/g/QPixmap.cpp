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
 *  enum HBitmapFormat { NoAlpha, PremultipliedAlpha, Alpha }
 *  enum ShareMode { ImplicitlyShared, ExplicitlyShared }
 */

/*
 *  Constructed[ 40/40 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // QRgb * clut () const
 *  // Qt::HANDLE handle () const
 *  // bool loadFromData ( const uchar * data, uint len, const char * format = 0, Qt::ImageConversionFlags flags = Qt::AutoColor )
 *  // int numCols () const
 *  // const uchar * qwsBits () const
 *  // int qwsBytesPerLine () const
 *  // CGImageRef toMacCGImageRef () const
 *  //HBITMAP toWinHBITMAP ( HBitmapFormat format = NoAlpha ) const
 *  //QPixmap fromMacCGImageRef ( CGImageRef image )
 *  //QPixmap fromWinHBITMAP ( HBITMAP bitmap, HBitmapFormat format = NoAlpha )
 *  //QPixmap fromX11Pixmap ( Qt::HANDLE pixmap, ShareMode mode = ImplicitlyShared )
 *  //QPixmap grabWindow ( WId window, int x = 0, int y = 0, int width = -1, int height = -1 )
 */

#include <QtCore/QPointer>

#include <QtGui/QPixmap>
#include <QtGui/QBitmap>

/*
 * QPixmap ()
 * QPixmap ( int width, int height )
 * QPixmap ( const QString & fileName, const char * format = 0, Qt::ImageConversionFlags flags = Qt::AutoColor )
 * QPixmap ( const char * const[] xpm )
 * QPixmap ( const QPixmap & pixmap )
 * QPixmap ( const QSize & size )
 * ~QPixmap ()
 */

typedef struct
{
   QPixmap * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPixmap;

HBQT_GC_FUNC( hbqt_gcRelease_QPixmap )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QPixmap * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPixmap( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QPixmap * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPixmap;
   p->type = HBQT_TYPE_QPixmap;

   return p;
}

HB_FUNC( QT_QPIXMAP )
{
   QPixmap * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISCHAR( 1 ) )
   {
      pObj = new QPixmap( hbqt_par_QString( 1 ), ( const char * ) 0, Qt::AutoColor ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
      if( q )
      {
         if( q->type == HBQT_TYPE_QPixmap )
         {
            pObj = new QPixmap( *hbqt_par_QPixmap( 1 ) ) ;
         }
         if( q->type == HBQT_TYPE_QSize )
         {
            pObj = new QPixmap( *hbqt_par_QSize( 1 ) ) ;
         }
      }
      else
      {
         pObj = new QPixmap( *hbqt_par_QPixmap( 1 ) ) ;
      }
   }
   else if( hb_pcount() == 2 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = new QPixmap( hb_parni( 1 ), hb_parni( 2 ) ) ;
   }
   else
   {
      pObj = new QPixmap() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QPixmap( ( void * ) pObj, true ) );
}

/* QPixmap alphaChannel () const */
HB_FUNC( QT_QPIXMAP_ALPHACHANNEL )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->alphaChannel() ), true ) );
}

/* qint64 cacheKey () const */
HB_FUNC( QT_QPIXMAP_CACHEKEY )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retnint( ( p )->cacheKey() );
}

/* QPixmap copy ( const QRect & rectangle = QRect() ) const */
HB_FUNC( QT_QPIXMAP_COPY )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->copy( ( HB_ISOBJECT( 2 ) ? *hbqt_par_QRect( 2 ) : QRect() ) ) ), true ) );
}

/* QPixmap copy ( int x, int y, int width, int height ) const */
HB_FUNC( QT_QPIXMAP_COPY_1 )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->copy( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ) ), true ) );
}

/* QBitmap createHeuristicMask ( bool clipTight = true ) const */
HB_FUNC( QT_QPIXMAP_CREATEHEURISTICMASK )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( ( p )->createHeuristicMask( hb_parl( 2 ) ) ), true ) );
}

/* QBitmap createMaskFromColor ( const QColor & maskColor, Qt::MaskMode mode ) const */
HB_FUNC( QT_QPIXMAP_CREATEMASKFROMCOLOR )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( ( p )->createMaskFromColor( *hbqt_par_QColor( 2 ), ( Qt::MaskMode ) hb_parni( 3 ) ) ), true ) );
}

/* QBitmap createMaskFromColor ( const QColor & maskColor ) const */
HB_FUNC( QT_QPIXMAP_CREATEMASKFROMCOLOR_1 )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( ( p )->createMaskFromColor( *hbqt_par_QColor( 2 ) ) ), true ) );
}

/* int depth () const */
HB_FUNC( QT_QPIXMAP_DEPTH )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retni( ( p )->depth() );
}

/* void detach () */
HB_FUNC( QT_QPIXMAP_DETACH )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      ( p )->detach();
}

/* void fill ( const QColor & color = Qt::white ) */
HB_FUNC( QT_QPIXMAP_FILL )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      ( p )->fill( *hbqt_par_QColor( 2 ) );
}

/* void fill ( const QWidget * widget, const QPoint & offset ) */
HB_FUNC( QT_QPIXMAP_FILL_1 )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      ( p )->fill( hbqt_par_QWidget( 2 ), *hbqt_par_QPoint( 3 ) );
}

/* void fill ( const QWidget * widget, int x, int y ) */
HB_FUNC( QT_QPIXMAP_FILL_2 )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      ( p )->fill( hbqt_par_QWidget( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/* bool hasAlpha () const */
HB_FUNC( QT_QPIXMAP_HASALPHA )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retl( ( p )->hasAlpha() );
}

/* bool hasAlphaChannel () const */
HB_FUNC( QT_QPIXMAP_HASALPHACHANNEL )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retl( ( p )->hasAlphaChannel() );
}

/* int height () const */
HB_FUNC( QT_QPIXMAP_HEIGHT )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retni( ( p )->height() );
}

/* bool isNull () const */
HB_FUNC( QT_QPIXMAP_ISNULL )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
}

/* bool isQBitmap () const */
HB_FUNC( QT_QPIXMAP_ISQBITMAP )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retl( ( p )->isQBitmap() );
}

/* bool load ( const QString & fileName, const char * format = 0, Qt::ImageConversionFlags flags = Qt::AutoColor ) */
HB_FUNC( QT_QPIXMAP_LOAD )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->load( hb_parstr_utf8( 2, &pText, NULL ), ( const char * ) hb_parc( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::ImageConversionFlags ) hb_parni( 4 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ) );
      hb_strfree( pText );
   }
}

/* bool loadFromData ( const QByteArray & data, const char * format = 0, Qt::ImageConversionFlags flags = Qt::AutoColor ) */
HB_FUNC( QT_QPIXMAP_LOADFROMDATA )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retl( ( p )->loadFromData( *hbqt_par_QByteArray( 2 ), ( const char * ) hb_parc( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::ImageConversionFlags ) hb_parni( 4 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ) );
}

/* QBitmap mask () const */
HB_FUNC( QT_QPIXMAP_MASK )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( ( p )->mask() ), true ) );
}

/* QRect rect () const */
HB_FUNC( QT_QPIXMAP_RECT )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->rect() ), true ) );
}

/* bool save ( const QString & fileName, const char * format = 0, int quality = -1 ) const */
HB_FUNC( QT_QPIXMAP_SAVE )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->save( hb_parstr_utf8( 2, &pText, NULL ), ( const char * ) hb_parc( 3 ), hb_parnidef( 4, -1 ) ) );
      hb_strfree( pText );
   }
}

/* bool save ( QIODevice * device, const char * format = 0, int quality = -1 ) const */
HB_FUNC( QT_QPIXMAP_SAVE_1 )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retl( ( p )->save( hbqt_par_QIODevice( 2 ), ( const char * ) hb_parc( 3 ), hb_parnidef( 4, -1 ) ) );
}

/* QPixmap scaled ( int width, int height, Qt::AspectRatioMode aspectRatioMode = Qt::IgnoreAspectRatio, Qt::TransformationMode transformMode = Qt::FastTransformation ) const */
HB_FUNC( QT_QPIXMAP_SCALED )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->scaled( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::AspectRatioMode ) hb_parni( 4 ) : ( Qt::AspectRatioMode ) Qt::IgnoreAspectRatio ), ( HB_ISNUM( 5 ) ? ( Qt::TransformationMode ) hb_parni( 5 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
}

/* QPixmap scaled ( const QSize & size, Qt::AspectRatioMode aspectRatioMode = Qt::IgnoreAspectRatio, Qt::TransformationMode transformMode = Qt::FastTransformation ) const */
HB_FUNC( QT_QPIXMAP_SCALED_1 )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->scaled( *hbqt_par_QSize( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::AspectRatioMode ) hb_parni( 3 ) : ( Qt::AspectRatioMode ) Qt::IgnoreAspectRatio ), ( HB_ISNUM( 4 ) ? ( Qt::TransformationMode ) hb_parni( 4 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
}

/* QPixmap scaledToHeight ( int height, Qt::TransformationMode mode = Qt::FastTransformation ) const */
HB_FUNC( QT_QPIXMAP_SCALEDTOHEIGHT )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->scaledToHeight( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
}

/* QPixmap scaledToWidth ( int width, Qt::TransformationMode mode = Qt::FastTransformation ) const */
HB_FUNC( QT_QPIXMAP_SCALEDTOWIDTH )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->scaledToWidth( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
}

/* void setAlphaChannel ( const QPixmap & alphaChannel ) */
HB_FUNC( QT_QPIXMAP_SETALPHACHANNEL )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      ( p )->setAlphaChannel( *hbqt_par_QPixmap( 2 ) );
}

/* void setMask ( const QBitmap & mask ) */
HB_FUNC( QT_QPIXMAP_SETMASK )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      ( p )->setMask( *hbqt_par_QBitmap( 2 ) );
}

/* QSize size () const */
HB_FUNC( QT_QPIXMAP_SIZE )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->size() ), true ) );
}

/* QImage toImage () const */
HB_FUNC( QT_QPIXMAP_TOIMAGE )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->toImage() ), true ) );
}

/* QPixmap transformed ( const QTransform & transform, Qt::TransformationMode mode = Qt::FastTransformation ) const */
HB_FUNC( QT_QPIXMAP_TRANSFORMED )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->transformed( *hbqt_par_QTransform( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
}

/* QPixmap transformed ( const QMatrix & matrix, Qt::TransformationMode mode = Qt::FastTransformation ) const */
HB_FUNC( QT_QPIXMAP_TRANSFORMED_1 )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->transformed( *hbqt_par_QMatrix( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
}

/* int width () const */
HB_FUNC( QT_QPIXMAP_WIDTH )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retni( ( p )->width() );
}

/* int defaultDepth () */
HB_FUNC( QT_QPIXMAP_DEFAULTDEPTH )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retni( ( p )->defaultDepth() );
}

/* QPixmap fromImage ( const QImage & image, Qt::ImageConversionFlags flags = Qt::AutoColor ) */
HB_FUNC( QT_QPIXMAP_FROMIMAGE )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->fromImage( *hbqt_par_QImage( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ImageConversionFlags ) hb_parni( 3 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ) ), true ) );
}

/* QPixmap grabWidget ( QWidget * widget, const QRect & rectangle ) */
HB_FUNC( QT_QPIXMAP_GRABWIDGET )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->grabWidget( hbqt_par_QWidget( 2 ), *hbqt_par_QRect( 3 ) ) ), true ) );
}

/* QPixmap grabWidget ( QWidget * widget, int x = 0, int y = 0, int width = -1, int height = -1 ) */
HB_FUNC( QT_QPIXMAP_GRABWIDGET_1 )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->grabWidget( hbqt_par_QWidget( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parnidef( 5, -1 ), hb_parnidef( 6, -1 ) ) ), true ) );
}

/* QTransform trueMatrix ( const QTransform & matrix, int width, int height ) */
HB_FUNC( QT_QPIXMAP_TRUEMATRIX )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->trueMatrix( *hbqt_par_QTransform( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) ), true ) );
}

/* QMatrix trueMatrix ( const QMatrix & m, int w, int h ) */
HB_FUNC( QT_QPIXMAP_TRUEMATRIX_1 )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->trueMatrix( *hbqt_par_QMatrix( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
