/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/

#include "hbapi.h"
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum Format { Format_Invalid, Format_Mono, Format_MonoLSB, Format_Indexed8, ..., Format_ARGB4444_Premultiplied }
 *  enum InvertMode { InvertRgb, InvertRgba }
 */

/*
 *  Constructed[ 60/64 [ 93.75% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QVector<QRgb> colorTable () const
 *  QImage convertToFormat ( Format format, const QVector<QRgb> & colorTable, Qt::ImageConversionFlags flags = Qt::AutoColor ) const
 *  void setColorTable ( const QVector<QRgb> colors )
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  // bool loadFromData ( const uchar * data, int len, const char * format = 0 )
 */

#include <QtCore/QPointer>

#include <QStringList>
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

QT_G_FUNC( release_QImage )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QImage                       p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "release_QImage                      ph=%p", p->ph ) );

   if( p && p->ph )
   {
      delete ( ( QImage * ) p->ph );
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "YES release_QImage                      Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_QImage                      Object Allready deleted!" ) );
   }
}

void * gcAllocate_QImage( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QImage;
   HB_TRACE( HB_TR_DEBUG, ( "          new_QImage                      %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QIMAGE )
{
   void * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = ( QImage* ) new QImage( *hbqt_par_QImage( 1 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISCHAR( 1 ) )
   {
      pObj = ( QImage* ) new QImage( hbqt_par_QString( 1 ), ( const char * ) 0 ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      pObj = ( QImage* ) new QImage( hbqt_par_QString( 1 ), ( const char * ) hb_parcx( 2 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = ( QImage* ) new QImage( *hbqt_par_QSize( 1 ), ( QImage::Format ) hb_parni( 2 ) ) ;
   }
   else if( hb_pcount() == 3 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      pObj = ( QImage* ) new QImage( hb_parni( 1 ), hb_parni( 2 ), ( QImage::Format ) hb_parni( 3 ) ) ;
   }
   else if( hb_pcount() == 4 && HB_ISCHAR( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) )
   {
      pObj = ( QImage* ) new QImage( ( const uchar * ) hb_parc( 1 ), hb_parni( 2 ), hb_parni( 3 ), ( QImage::Format ) hb_parni( 4 ) ) ;
   }
   else if( hb_pcount() == 5 && HB_ISCHAR( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) && HB_ISNUM( 5 ) )
   {
      pObj = ( QImage* ) new QImage( ( const uchar * ) hb_parc( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), ( QImage::Format ) hb_parni( 5 ) ) ;
   }
   else
   {
      pObj = ( QImage* ) new QImage() ;
   }

   hb_retptrGC( gcAllocate_QImage( pObj ) );
}
/*
 * bool allGray () const
 */
HB_FUNC( QT_QIMAGE_ALLGRAY )
{
   hb_retl( hbqt_par_QImage( 1 )->allGray() );
}

/*
 * QImage alphaChannel () const
 */
HB_FUNC( QT_QIMAGE_ALPHACHANNEL )
{
   hb_retptrGC( gcAllocate_QImage( new QImage( hbqt_par_QImage( 1 )->alphaChannel() ) ) );
}

/*
 * uchar * bits ()
 */
HB_FUNC( QT_QIMAGE_BITS )
{
   hb_retptr( ( uchar* ) hbqt_par_QImage( 1 )->bits() );
}

/*
 * const uchar * bits () const
 */
HB_FUNC( QT_QIMAGE_BITS_1 )
{
   hb_retptr( ( uchar* ) hbqt_par_QImage( 1 )->bits() );
}

/*
 * int bytesPerLine () const
 */
HB_FUNC( QT_QIMAGE_BYTESPERLINE )
{
   hb_retni( hbqt_par_QImage( 1 )->bytesPerLine() );
}

/*
 * qint64 cacheKey () const
 */
HB_FUNC( QT_QIMAGE_CACHEKEY )
{
   hb_retnint( hbqt_par_QImage( 1 )->cacheKey() );
}

/*
 * QRgb color ( int i ) const
 */
HB_FUNC( QT_QIMAGE_COLOR )
{
   hb_retnl( hbqt_par_QImage( 1 )->color( hb_parni( 2 ) ) );
}

/*
 * QImage convertToFormat ( Format format, Qt::ImageConversionFlags flags = Qt::AutoColor ) const
 */
HB_FUNC( QT_QIMAGE_CONVERTTOFORMAT )
{
   hb_retptrGC( gcAllocate_QImage( new QImage( hbqt_par_QImage( 1 )->convertToFormat( ( QImage::Format ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ImageConversionFlags ) hb_parni( 3 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ) ) ) );
}

/*
 * QImage copy ( const QRect & rectangle = QRect() ) const
 */
HB_FUNC( QT_QIMAGE_COPY )
{
   hb_retptrGC( gcAllocate_QImage( new QImage( hbqt_par_QImage( 1 )->copy( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QRect( 2 ) : QRect() ) ) ) ) );
}

/*
 * QImage copy ( int x, int y, int width, int height ) const
 */
HB_FUNC( QT_QIMAGE_COPY_1 )
{
   hb_retptrGC( gcAllocate_QImage( new QImage( hbqt_par_QImage( 1 )->copy( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ) ) ) );
}

/*
 * QImage createAlphaMask ( Qt::ImageConversionFlags flags = Qt::AutoColor ) const
 */
HB_FUNC( QT_QIMAGE_CREATEALPHAMASK )
{
   hb_retptrGC( gcAllocate_QImage( new QImage( hbqt_par_QImage( 1 )->createAlphaMask( ( HB_ISNUM( 2 ) ? ( Qt::ImageConversionFlags ) hb_parni( 2 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ) ) ) );
}

/*
 * QImage createHeuristicMask ( bool clipTight = true ) const
 */
HB_FUNC( QT_QIMAGE_CREATEHEURISTICMASK )
{
   hb_retptrGC( gcAllocate_QImage( new QImage( hbqt_par_QImage( 1 )->createHeuristicMask( hb_parl( 2 ) ) ) ) );
}

/*
 * QImage createMaskFromColor ( QRgb color, Qt::MaskMode mode = Qt::MaskInColor ) const
 */
HB_FUNC( QT_QIMAGE_CREATEMASKFROMCOLOR )
{
   hb_retptrGC( gcAllocate_QImage( new QImage( hbqt_par_QImage( 1 )->createMaskFromColor( hb_parnl( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::MaskMode ) hb_parni( 3 ) : ( Qt::MaskMode ) Qt::MaskInColor ) ) ) ) );
}

/*
 * int depth () const
 */
HB_FUNC( QT_QIMAGE_DEPTH )
{
   hb_retni( hbqt_par_QImage( 1 )->depth() );
}

/*
 * int dotsPerMeterX () const
 */
HB_FUNC( QT_QIMAGE_DOTSPERMETERX )
{
   hb_retni( hbqt_par_QImage( 1 )->dotsPerMeterX() );
}

/*
 * int dotsPerMeterY () const
 */
HB_FUNC( QT_QIMAGE_DOTSPERMETERY )
{
   hb_retni( hbqt_par_QImage( 1 )->dotsPerMeterY() );
}

/*
 * void fill ( uint pixelValue )
 */
HB_FUNC( QT_QIMAGE_FILL )
{
   hbqt_par_QImage( 1 )->fill( hb_parni( 2 ) );
}

/*
 * Format format () const
 */
HB_FUNC( QT_QIMAGE_FORMAT )
{
   hb_retni( ( QImage::Format ) hbqt_par_QImage( 1 )->format() );
}

/*
 * bool hasAlphaChannel () const
 */
HB_FUNC( QT_QIMAGE_HASALPHACHANNEL )
{
   hb_retl( hbqt_par_QImage( 1 )->hasAlphaChannel() );
}

/*
 * int height () const
 */
HB_FUNC( QT_QIMAGE_HEIGHT )
{
   hb_retni( hbqt_par_QImage( 1 )->height() );
}

/*
 * void invertPixels ( InvertMode mode = InvertRgb )
 */
HB_FUNC( QT_QIMAGE_INVERTPIXELS )
{
   hbqt_par_QImage( 1 )->invertPixels( ( HB_ISNUM( 2 ) ? ( QImage::InvertMode ) hb_parni( 2 ) : ( QImage::InvertMode ) QImage::InvertRgb ) );
}

/*
 * bool isGrayscale () const
 */
HB_FUNC( QT_QIMAGE_ISGRAYSCALE )
{
   hb_retl( hbqt_par_QImage( 1 )->isGrayscale() );
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QIMAGE_ISNULL )
{
   hb_retl( hbqt_par_QImage( 1 )->isNull() );
}

/*
 * bool load ( const QString & fileName, const char * format = 0 )
 */
HB_FUNC( QT_QIMAGE_LOAD )
{
   hb_retl( hbqt_par_QImage( 1 )->load( hbqt_par_QString( 2 ), hbqt_par_char( 3 ) ) );
}

/*
 * bool load ( QIODevice * device, const char * format )
 */
HB_FUNC( QT_QIMAGE_LOAD_1 )
{
   hb_retl( hbqt_par_QImage( 1 )->load( hbqt_par_QIODevice( 2 ), hbqt_par_char( 3 ) ) );
}

/*
 * bool loadFromData ( const QByteArray & data, const char * format = 0 )
 */
HB_FUNC( QT_QIMAGE_LOADFROMDATA )
{
   hb_retl( hbqt_par_QImage( 1 )->loadFromData( *hbqt_par_QByteArray( 2 ), hbqt_par_char( 3 ) ) );
}

/*
 * QImage mirrored ( bool horizontal = false, bool vertical = true ) const
 */
HB_FUNC( QT_QIMAGE_MIRRORED )
{
   hb_retptrGC( gcAllocate_QImage( new QImage( hbqt_par_QImage( 1 )->mirrored( hb_parl( 2 ), hb_parl( 3 ) ) ) ) );
}

/*
 * int numBytes () const
 */
HB_FUNC( QT_QIMAGE_NUMBYTES )
{
   hb_retni( hbqt_par_QImage( 1 )->numBytes() );
}

/*
 * int numColors () const
 */
HB_FUNC( QT_QIMAGE_NUMCOLORS )
{
   hb_retni( hbqt_par_QImage( 1 )->numColors() );
}

/*
 * QPoint offset () const
 */
HB_FUNC( QT_QIMAGE_OFFSET )
{
   hb_retptrGC( gcAllocate_QPoint( new QPoint( hbqt_par_QImage( 1 )->offset() ) ) );
}

/*
 * QRgb pixel ( const QPoint & position ) const
 */
HB_FUNC( QT_QIMAGE_PIXEL )
{
   hb_retnl( hbqt_par_QImage( 1 )->pixel( *hbqt_par_QPoint( 2 ) ) );
}

/*
 * QRgb pixel ( int x, int y ) const
 */
HB_FUNC( QT_QIMAGE_PIXEL_1 )
{
   hb_retnl( hbqt_par_QImage( 1 )->pixel( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * int pixelIndex ( const QPoint & position ) const
 */
HB_FUNC( QT_QIMAGE_PIXELINDEX )
{
   hb_retni( hbqt_par_QImage( 1 )->pixelIndex( *hbqt_par_QPoint( 2 ) ) );
}

/*
 * int pixelIndex ( int x, int y ) const
 */
HB_FUNC( QT_QIMAGE_PIXELINDEX_1 )
{
   hb_retni( hbqt_par_QImage( 1 )->pixelIndex( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * QRect rect () const
 */
HB_FUNC( QT_QIMAGE_RECT )
{
   hb_retptrGC( gcAllocate_QRect( new QRect( hbqt_par_QImage( 1 )->rect() ) ) );
}

/*
 * QImage rgbSwapped () const
 */
HB_FUNC( QT_QIMAGE_RGBSWAPPED )
{
   hb_retptrGC( gcAllocate_QImage( new QImage( hbqt_par_QImage( 1 )->rgbSwapped() ) ) );
}

/*
 * bool save ( const QString & fileName, const char * format = 0, int quality = -1 ) const
 */
HB_FUNC( QT_QIMAGE_SAVE )
{
   hb_retl( hbqt_par_QImage( 1 )->save( hbqt_par_QString( 2 ), hbqt_par_char( 3 ), ( HB_ISNUM( 4 ) ? hb_parni( 4 ) : -1 ) ) );
}

/*
 * bool save ( QIODevice * device, const char * format = 0, int quality = -1 ) const
 */
HB_FUNC( QT_QIMAGE_SAVE_1 )
{
   hb_retl( hbqt_par_QImage( 1 )->save( hbqt_par_QIODevice( 2 ), hbqt_par_char( 3 ), ( HB_ISNUM( 4 ) ? hb_parni( 4 ) : -1 ) ) );
}

/*
 * QImage scaled ( const QSize & size, Qt::AspectRatioMode aspectRatioMode = Qt::IgnoreAspectRatio, Qt::TransformationMode transformMode = Qt::FastTransformation ) const
 */
HB_FUNC( QT_QIMAGE_SCALED )
{
   hb_retptrGC( gcAllocate_QImage( new QImage( hbqt_par_QImage( 1 )->scaled( *hbqt_par_QSize( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::AspectRatioMode ) hb_parni( 3 ) : ( Qt::AspectRatioMode ) Qt::IgnoreAspectRatio ), ( HB_ISNUM( 4 ) ? ( Qt::TransformationMode ) hb_parni( 4 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ) ) );
}

/*
 * QImage scaled ( int width, int height, Qt::AspectRatioMode aspectRatioMode = Qt::IgnoreAspectRatio, Qt::TransformationMode transformMode = Qt::FastTransformation ) const
 */
HB_FUNC( QT_QIMAGE_SCALED_1 )
{
   hb_retptrGC( gcAllocate_QImage( new QImage( hbqt_par_QImage( 1 )->scaled( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::AspectRatioMode ) hb_parni( 4 ) : ( Qt::AspectRatioMode ) Qt::IgnoreAspectRatio ), ( HB_ISNUM( 5 ) ? ( Qt::TransformationMode ) hb_parni( 5 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ) ) );
}

/*
 * QImage scaledToHeight ( int height, Qt::TransformationMode mode = Qt::FastTransformation ) const
 */
HB_FUNC( QT_QIMAGE_SCALEDTOHEIGHT )
{
   hb_retptrGC( gcAllocate_QImage( new QImage( hbqt_par_QImage( 1 )->scaledToHeight( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ) ) );
}

/*
 * QImage scaledToWidth ( int width, Qt::TransformationMode mode = Qt::FastTransformation ) const
 */
HB_FUNC( QT_QIMAGE_SCALEDTOWIDTH )
{
   hb_retptrGC( gcAllocate_QImage( new QImage( hbqt_par_QImage( 1 )->scaledToWidth( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ) ) );
}

/*
 * uchar * scanLine ( int i )
 */
HB_FUNC( QT_QIMAGE_SCANLINE )
{
   hb_retptr( ( uchar* ) hbqt_par_QImage( 1 )->scanLine( hb_parni( 2 ) ) );
}

/*
 * const uchar * scanLine ( int i ) const
 */
HB_FUNC( QT_QIMAGE_SCANLINE_1 )
{
   hb_retptr( ( uchar* ) hbqt_par_QImage( 1 )->scanLine( hb_parni( 2 ) ) );
}

/*
 * void setColor ( int index, QRgb colorValue )
 */
HB_FUNC( QT_QIMAGE_SETCOLOR )
{
   hbqt_par_QImage( 1 )->setColor( hb_parni( 2 ), hb_parnl( 3 ) );
}

/*
 * void setDotsPerMeterX ( int x )
 */
HB_FUNC( QT_QIMAGE_SETDOTSPERMETERX )
{
   hbqt_par_QImage( 1 )->setDotsPerMeterX( hb_parni( 2 ) );
}

/*
 * void setDotsPerMeterY ( int y )
 */
HB_FUNC( QT_QIMAGE_SETDOTSPERMETERY )
{
   hbqt_par_QImage( 1 )->setDotsPerMeterY( hb_parni( 2 ) );
}

/*
 * void setNumColors ( int numColors )
 */
HB_FUNC( QT_QIMAGE_SETNUMCOLORS )
{
   hbqt_par_QImage( 1 )->setNumColors( hb_parni( 2 ) );
}

/*
 * void setOffset ( const QPoint & offset )
 */
HB_FUNC( QT_QIMAGE_SETOFFSET )
{
   hbqt_par_QImage( 1 )->setOffset( *hbqt_par_QPoint( 2 ) );
}

/*
 * void setPixel ( const QPoint & position, uint index_or_rgb )
 */
HB_FUNC( QT_QIMAGE_SETPIXEL )
{
   hbqt_par_QImage( 1 )->setPixel( *hbqt_par_QPoint( 2 ), hb_parni( 3 ) );
}

/*
 * void setPixel ( int x, int y, uint index_or_rgb )
 */
HB_FUNC( QT_QIMAGE_SETPIXEL_1 )
{
   hbqt_par_QImage( 1 )->setPixel( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/*
 * void setText ( const QString & key, const QString & text )
 */
HB_FUNC( QT_QIMAGE_SETTEXT )
{
   hbqt_par_QImage( 1 )->setText( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * QSize size () const
 */
HB_FUNC( QT_QIMAGE_SIZE )
{
   hb_retptrGC( gcAllocate_QSize( new QSize( hbqt_par_QImage( 1 )->size() ) ) );
}

/*
 * QString text ( const QString & key = QString() ) const
 */
HB_FUNC( QT_QIMAGE_TEXT )
{
   hb_retc( hbqt_par_QImage( 1 )->text( hbqt_par_QString( 2 ) ).toAscii().data() );
}

/*
 * QStringList textKeys () const
 */
HB_FUNC( QT_QIMAGE_TEXTKEYS )
{
   hb_retptrGC( gcAllocate_QStringList( new QStringList( hbqt_par_QImage( 1 )->textKeys() ) ) );
}

/*
 * QImage transformed ( const QMatrix & matrix, Qt::TransformationMode mode = Qt::FastTransformation ) const
 */
HB_FUNC( QT_QIMAGE_TRANSFORMED )
{
   hb_retptrGC( gcAllocate_QImage( new QImage( hbqt_par_QImage( 1 )->transformed( *hbqt_par_QMatrix( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ) ) );
}

/*
 * QImage transformed ( const QTransform & matrix, Qt::TransformationMode mode = Qt::FastTransformation ) const
 */
HB_FUNC( QT_QIMAGE_TRANSFORMED_1 )
{
   hb_retptrGC( gcAllocate_QImage( new QImage( hbqt_par_QImage( 1 )->transformed( *hbqt_par_QTransform( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ) ) );
}

/*
 * bool valid ( const QPoint & pos ) const
 */
HB_FUNC( QT_QIMAGE_VALID )
{
   hb_retl( hbqt_par_QImage( 1 )->valid( *hbqt_par_QPoint( 2 ) ) );
}

/*
 * bool valid ( int x, int y ) const
 */
HB_FUNC( QT_QIMAGE_VALID_1 )
{
   hb_retl( hbqt_par_QImage( 1 )->valid( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * int width () const
 */
HB_FUNC( QT_QIMAGE_WIDTH )
{
   hb_retni( hbqt_par_QImage( 1 )->width() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
