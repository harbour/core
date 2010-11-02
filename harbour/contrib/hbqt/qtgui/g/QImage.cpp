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
 *  enum Format { Format_Invalid, Format_Mono, Format_MonoLSB, Format_Indexed8, ..., Format_ARGB4444_Premultiplied }
 *  enum InvertMode { InvertRgb, InvertRgba }
 */

/*
 *  Constructed[ 58/61 [ 95.08% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  QVector<QRgb> colorTable () const
 *  QImage convertToFormat ( Format format, const QVector<QRgb> & colorTable, Qt::ImageConversionFlags flags = Qt::AutoColor ) const
 *  void setColorTable ( const QVector<QRgb> colors )
 *
 *  *** Commented out protostypes ***
 *
 *  //uchar * bits ()
 *  // bool loadFromData ( const uchar * data, int len, const char * format = 0 )
 *  //uchar * scanLine ( int i )
 */

#include <QtCore/QPointer>

#include <QtCore/QStringList>
#include <QtGui/QImage>


/*
 * QImage ()
 * QImage ( const QSize & size, Format format )
 * QImage ( int width, int height, Format format )
 * QImage ( uchar * data, int width, int height, Format format )
 * QImage ( const uchar * data, int width, int height, Format format )
 * QImage ( uchar * data, int width, int height, int bytesPerLine, Format format )
 * QImage ( const uchar * data, int width, int height, int bytesPerLine, Format format )
 * QImage ( const char * const[] xpm )
 * QImage ( const QString & fileName, const char * format = 0 )
 * QImage ( const char * fileName, const char * format = 0 )
 * QImage ( const QImage & image )
 * ~QImage ()
 */

typedef struct
{
   QImage * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QImage;

HBQT_GC_FUNC( hbqt_gcRelease_QImage )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QImage * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QImage( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QImage * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QImage;
   p->type = HBQT_TYPE_QImage;

   return p;
}

HB_FUNC( QT_QIMAGE )
{
   QImage * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISCHAR( 1 ) )
   {
      pObj = new QImage( hbqt_par_QString( 1 ), ( const char * ) 0 ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QImage( *hbqt_par_QImage( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      pObj = new QImage( hbqt_par_QString( 1 ), ( const char * ) hb_parcx( 2 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = new QImage( *hbqt_par_QSize( 1 ), ( QImage::Format ) hb_parni( 2 ) ) ;
   }
   else if( hb_pcount() == 3 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      pObj = new QImage( hb_parni( 1 ), hb_parni( 2 ), ( QImage::Format ) hb_parni( 3 ) ) ;
   }
   else if( hb_pcount() == 4 && HB_ISCHAR( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) )
   {
      pObj = new QImage( ( const uchar * ) hb_parc( 1 ), hb_parni( 2 ), hb_parni( 3 ), ( QImage::Format ) hb_parni( 4 ) ) ;
   }
   else if( hb_pcount() == 5 && HB_ISCHAR( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) && HB_ISNUM( 5 ) )
   {
      pObj = new QImage( ( const uchar * ) hb_parc( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), ( QImage::Format ) hb_parni( 5 ) ) ;
   }
   else
   {
      pObj = new QImage() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QImage( ( void * ) pObj, true ) );
}

/* bool allGray () const */
HB_FUNC( QT_QIMAGE_ALLGRAY )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retl( ( p )->allGray() );
}

/* QImage alphaChannel () const */
HB_FUNC( QT_QIMAGE_ALPHACHANNEL )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->alphaChannel() ), true ) );
}

/* const uchar * bits () const */
HB_FUNC( QT_QIMAGE_BITS )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retc( ( const char * ) ( p )->bits() );
}

/* int bytesPerLine () const */
HB_FUNC( QT_QIMAGE_BYTESPERLINE )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retni( ( p )->bytesPerLine() );
}

/* qint64 cacheKey () const */
HB_FUNC( QT_QIMAGE_CACHEKEY )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retnint( ( p )->cacheKey() );
}

/* QRgb color ( int i ) const */
HB_FUNC( QT_QIMAGE_COLOR )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retnl( ( p )->color( hb_parni( 2 ) ) );
}

/* QImage convertToFormat ( Format format, Qt::ImageConversionFlags flags = Qt::AutoColor ) const */
HB_FUNC( QT_QIMAGE_CONVERTTOFORMAT )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->convertToFormat( ( QImage::Format ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ImageConversionFlags ) hb_parni( 3 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ) ), true ) );
}

/* QImage copy ( const QRect & rectangle = QRect() ) const */
HB_FUNC( QT_QIMAGE_COPY )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->copy( ( HB_ISOBJECT( 2 ) ? *hbqt_par_QRect( 2 ) : QRect() ) ) ), true ) );
}

/* QImage copy ( int x, int y, int width, int height ) const */
HB_FUNC( QT_QIMAGE_COPY_1 )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->copy( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ) ), true ) );
}

/* QImage createAlphaMask ( Qt::ImageConversionFlags flags = Qt::AutoColor ) const */
HB_FUNC( QT_QIMAGE_CREATEALPHAMASK )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->createAlphaMask( ( HB_ISNUM( 2 ) ? ( Qt::ImageConversionFlags ) hb_parni( 2 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ) ), true ) );
}

/* QImage createHeuristicMask ( bool clipTight = true ) const */
HB_FUNC( QT_QIMAGE_CREATEHEURISTICMASK )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->createHeuristicMask( hb_parl( 2 ) ) ), true ) );
}

/* QImage createMaskFromColor ( QRgb color, Qt::MaskMode mode = Qt::MaskInColor ) const */
HB_FUNC( QT_QIMAGE_CREATEMASKFROMCOLOR )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->createMaskFromColor( hb_parnl( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::MaskMode ) hb_parni( 3 ) : ( Qt::MaskMode ) Qt::MaskInColor ) ) ), true ) );
}

/* int depth () const */
HB_FUNC( QT_QIMAGE_DEPTH )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retni( ( p )->depth() );
}

/* int dotsPerMeterX () const */
HB_FUNC( QT_QIMAGE_DOTSPERMETERX )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retni( ( p )->dotsPerMeterX() );
}

/* int dotsPerMeterY () const */
HB_FUNC( QT_QIMAGE_DOTSPERMETERY )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retni( ( p )->dotsPerMeterY() );
}

/* void fill ( uint pixelValue ) */
HB_FUNC( QT_QIMAGE_FILL )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      ( p )->fill( hb_parni( 2 ) );
}

/* Format format () const */
HB_FUNC( QT_QIMAGE_FORMAT )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retni( ( QImage::Format ) ( p )->format() );
}

/* bool hasAlphaChannel () const */
HB_FUNC( QT_QIMAGE_HASALPHACHANNEL )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retl( ( p )->hasAlphaChannel() );
}

/* int height () const */
HB_FUNC( QT_QIMAGE_HEIGHT )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retni( ( p )->height() );
}

/* void invertPixels ( InvertMode mode = InvertRgb ) */
HB_FUNC( QT_QIMAGE_INVERTPIXELS )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      ( p )->invertPixels( ( HB_ISNUM( 2 ) ? ( QImage::InvertMode ) hb_parni( 2 ) : ( QImage::InvertMode ) QImage::InvertRgb ) );
}

/* bool isGrayscale () const */
HB_FUNC( QT_QIMAGE_ISGRAYSCALE )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retl( ( p )->isGrayscale() );
}

/* bool isNull () const */
HB_FUNC( QT_QIMAGE_ISNULL )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
}

/* bool load ( const QString & fileName, const char * format = 0 ) */
HB_FUNC( QT_QIMAGE_LOAD )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->load( hb_parstr_utf8( 2, &pText, NULL ), ( const char * ) hb_parc( 3 ) ) );
      hb_strfree( pText );
   }
}

/* bool load ( QIODevice * device, const char * format ) */
HB_FUNC( QT_QIMAGE_LOAD_1 )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retl( ( p )->load( hbqt_par_QIODevice( 2 ), ( const char * ) hb_parc( 3 ) ) );
}

/* bool loadFromData ( const QByteArray & data, const char * format = 0 ) */
HB_FUNC( QT_QIMAGE_LOADFROMDATA )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retl( ( p )->loadFromData( *hbqt_par_QByteArray( 2 ), ( const char * ) hb_parc( 3 ) ) );
}

/* QImage mirrored ( bool horizontal = false, bool vertical = true ) const */
HB_FUNC( QT_QIMAGE_MIRRORED )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->mirrored( hb_parl( 2 ), hb_parl( 3 ) ) ), true ) );
}

/* int numBytes () const */
HB_FUNC( QT_QIMAGE_NUMBYTES )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retni( ( p )->numBytes() );
}

/* int numColors () const */
HB_FUNC( QT_QIMAGE_NUMCOLORS )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retni( ( p )->numColors() );
}

/* QPoint offset () const */
HB_FUNC( QT_QIMAGE_OFFSET )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->offset() ), true ) );
}

/* QRgb pixel ( const QPoint & position ) const */
HB_FUNC( QT_QIMAGE_PIXEL )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retnl( ( p )->pixel( *hbqt_par_QPoint( 2 ) ) );
}

/* QRgb pixel ( int x, int y ) const */
HB_FUNC( QT_QIMAGE_PIXEL_1 )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retnl( ( p )->pixel( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/* int pixelIndex ( const QPoint & position ) const */
HB_FUNC( QT_QIMAGE_PIXELINDEX )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retni( ( p )->pixelIndex( *hbqt_par_QPoint( 2 ) ) );
}

/* int pixelIndex ( int x, int y ) const */
HB_FUNC( QT_QIMAGE_PIXELINDEX_1 )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retni( ( p )->pixelIndex( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/* QRect rect () const */
HB_FUNC( QT_QIMAGE_RECT )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->rect() ), true ) );
}

/* QImage rgbSwapped () const */
HB_FUNC( QT_QIMAGE_RGBSWAPPED )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->rgbSwapped() ), true ) );
}

/* bool save ( const QString & fileName, const char * format = 0, int quality = -1 ) const */
HB_FUNC( QT_QIMAGE_SAVE )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->save( hb_parstr_utf8( 2, &pText, NULL ), ( const char * ) hb_parc( 3 ), hb_parnidef( 4, -1 ) ) );
      hb_strfree( pText );
   }
}

/* bool save ( QIODevice * device, const char * format = 0, int quality = -1 ) const */
HB_FUNC( QT_QIMAGE_SAVE_1 )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retl( ( p )->save( hbqt_par_QIODevice( 2 ), ( const char * ) hb_parc( 3 ), hb_parnidef( 4, -1 ) ) );
}

/* QImage scaled ( const QSize & size, Qt::AspectRatioMode aspectRatioMode = Qt::IgnoreAspectRatio, Qt::TransformationMode transformMode = Qt::FastTransformation ) const */
HB_FUNC( QT_QIMAGE_SCALED )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->scaled( *hbqt_par_QSize( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::AspectRatioMode ) hb_parni( 3 ) : ( Qt::AspectRatioMode ) Qt::IgnoreAspectRatio ), ( HB_ISNUM( 4 ) ? ( Qt::TransformationMode ) hb_parni( 4 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
}

/* QImage scaled ( int width, int height, Qt::AspectRatioMode aspectRatioMode = Qt::IgnoreAspectRatio, Qt::TransformationMode transformMode = Qt::FastTransformation ) const */
HB_FUNC( QT_QIMAGE_SCALED_1 )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->scaled( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::AspectRatioMode ) hb_parni( 4 ) : ( Qt::AspectRatioMode ) Qt::IgnoreAspectRatio ), ( HB_ISNUM( 5 ) ? ( Qt::TransformationMode ) hb_parni( 5 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
}

/* QImage scaledToHeight ( int height, Qt::TransformationMode mode = Qt::FastTransformation ) const */
HB_FUNC( QT_QIMAGE_SCALEDTOHEIGHT )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->scaledToHeight( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
}

/* QImage scaledToWidth ( int width, Qt::TransformationMode mode = Qt::FastTransformation ) const */
HB_FUNC( QT_QIMAGE_SCALEDTOWIDTH )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->scaledToWidth( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
}

/* const uchar * scanLine ( int i ) const */
HB_FUNC( QT_QIMAGE_SCANLINE )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retc( ( const char * ) ( p )->scanLine( hb_parni( 2 ) ) );
}

/* void setColor ( int index, QRgb colorValue ) */
HB_FUNC( QT_QIMAGE_SETCOLOR )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      ( p )->setColor( hb_parni( 2 ), hb_parnl( 3 ) );
}

/* void setDotsPerMeterX ( int x ) */
HB_FUNC( QT_QIMAGE_SETDOTSPERMETERX )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      ( p )->setDotsPerMeterX( hb_parni( 2 ) );
}

/* void setDotsPerMeterY ( int y ) */
HB_FUNC( QT_QIMAGE_SETDOTSPERMETERY )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      ( p )->setDotsPerMeterY( hb_parni( 2 ) );
}

/* void setNumColors ( int numColors ) */
HB_FUNC( QT_QIMAGE_SETNUMCOLORS )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      ( p )->setNumColors( hb_parni( 2 ) );
}

/* void setOffset ( const QPoint & offset ) */
HB_FUNC( QT_QIMAGE_SETOFFSET )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      ( p )->setOffset( *hbqt_par_QPoint( 2 ) );
}

/* void setPixel ( const QPoint & position, uint index_or_rgb ) */
HB_FUNC( QT_QIMAGE_SETPIXEL )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      ( p )->setPixel( *hbqt_par_QPoint( 2 ), hb_parni( 3 ) );
}

/* void setPixel ( int x, int y, uint index_or_rgb ) */
HB_FUNC( QT_QIMAGE_SETPIXEL_1 )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      ( p )->setPixel( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/* void setText ( const QString & key, const QString & text ) */
HB_FUNC( QT_QIMAGE_SETTEXT )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
   {
      void * pText;
      ( p )->setText( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* QSize size () const */
HB_FUNC( QT_QIMAGE_SIZE )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->size() ), true ) );
}

/* QString text ( const QString & key = QString() ) const */
HB_FUNC( QT_QIMAGE_TEXT )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->text( hb_parstr_utf8( 2, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/* QStringList textKeys () const */
HB_FUNC( QT_QIMAGE_TEXTKEYS )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->textKeys() ), true ) );
}

/* QImage transformed ( const QMatrix & matrix, Qt::TransformationMode mode = Qt::FastTransformation ) const */
HB_FUNC( QT_QIMAGE_TRANSFORMED )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->transformed( *hbqt_par_QMatrix( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
}

/* QImage transformed ( const QTransform & matrix, Qt::TransformationMode mode = Qt::FastTransformation ) const */
HB_FUNC( QT_QIMAGE_TRANSFORMED_1 )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->transformed( *hbqt_par_QTransform( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
}

/* bool valid ( const QPoint & pos ) const */
HB_FUNC( QT_QIMAGE_VALID )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retl( ( p )->valid( *hbqt_par_QPoint( 2 ) ) );
}

/* bool valid ( int x, int y ) const */
HB_FUNC( QT_QIMAGE_VALID_1 )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retl( ( p )->valid( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/* int width () const */
HB_FUNC( QT_QIMAGE_WIDTH )
{
   QImage * p = hbqt_par_QImage( 1 );
   if( p )
      hb_retni( ( p )->width() );
}


#endif /* #if QT_VERSION >= 0x040500 */
