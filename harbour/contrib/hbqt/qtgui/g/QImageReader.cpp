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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://harbour-project.org
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

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum ImageReaderError { FileNotFoundError, DeviceError, UnsupportedFormatError, InvalidDataError, UnknownError }
 */

#include <QtCore/QPointer>

#include <QtGui/QColor>
#include <QtGui/QImageReader>


/*
 * QImageReader ()
 * QImageReader ( QIODevice * device, const QByteArray & format = QByteArray() )
 * QImageReader ( const QString & fileName, const QByteArray & format = QByteArray() )
 * ~QImageReader ()
 */

typedef struct
{
   QImageReader * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QImageReader;

HBQT_GC_FUNC( hbqt_gcRelease_QImageReader )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QImageReader   /.\\", p->ph ) );
         delete ( ( QImageReader * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QImageReader   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QImageReader    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QImageReader    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QImageReader( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QImageReader * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QImageReader;
   p->type = HBQT_TYPE_QImageReader;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QImageReader", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QImageReader", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QIMAGEREADER )
{
   QImageReader * pObj = NULL;

   pObj =  new QImageReader() ;

   hb_retptrGC( hbqt_gcAllocate_QImageReader( ( void * ) pObj, true ) );
}

/*
 * bool autoDetectImageFormat () const
 */
HB_FUNC( QT_QIMAGEREADER_AUTODETECTIMAGEFORMAT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retl( ( p )->autoDetectImageFormat() );
   }
}

/*
 * QColor backgroundColor () const
 */
HB_FUNC( QT_QIMAGEREADER_BACKGROUNDCOLOR )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->backgroundColor() ), true ) );
   }
}

/*
 * bool canRead () const
 */
HB_FUNC( QT_QIMAGEREADER_CANREAD )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retl( ( p )->canRead() );
   }
}

/*
 * QRect clipRect () const
 */
HB_FUNC( QT_QIMAGEREADER_CLIPRECT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->clipRect() ), true ) );
   }
}

/*
 * int currentImageNumber () const
 */
HB_FUNC( QT_QIMAGEREADER_CURRENTIMAGENUMBER )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retni( ( p )->currentImageNumber() );
   }
}

/*
 * QRect currentImageRect () const
 */
HB_FUNC( QT_QIMAGEREADER_CURRENTIMAGERECT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->currentImageRect() ), true ) );
   }
}

/*
 * QIODevice * device () const
 */
HB_FUNC( QT_QIMAGEREADER_DEVICE )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->device(), false ) );
   }
}

/*
 * ImageReaderError error () const
 */
HB_FUNC( QT_QIMAGEREADER_ERROR )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retni( ( QImageReader::ImageReaderError ) ( p )->error() );
   }
}

/*
 * QString errorString () const
 */
HB_FUNC( QT_QIMAGEREADER_ERRORSTRING )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->errorString().toUtf8().data() );
   }
}

/*
 * QString fileName () const
 */
HB_FUNC( QT_QIMAGEREADER_FILENAME )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->fileName().toUtf8().data() );
   }
}

/*
 * QByteArray format () const
 */
HB_FUNC( QT_QIMAGEREADER_FORMAT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->format() ), true ) );
   }
}

/*
 * int imageCount () const
 */
HB_FUNC( QT_QIMAGEREADER_IMAGECOUNT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retni( ( p )->imageCount() );
   }
}

/*
 * QImage::Format imageFormat () const
 */
HB_FUNC( QT_QIMAGEREADER_IMAGEFORMAT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retni( ( QImage::Format ) ( p )->imageFormat() );
   }
}

/*
 * bool jumpToImage ( int imageNumber )
 */
HB_FUNC( QT_QIMAGEREADER_JUMPTOIMAGE )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retl( ( p )->jumpToImage( hb_parni( 2 ) ) );
   }
}

/*
 * bool jumpToNextImage ()
 */
HB_FUNC( QT_QIMAGEREADER_JUMPTONEXTIMAGE )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retl( ( p )->jumpToNextImage() );
   }
}

/*
 * int loopCount () const
 */
HB_FUNC( QT_QIMAGEREADER_LOOPCOUNT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retni( ( p )->loopCount() );
   }
}

/*
 * int nextImageDelay () const
 */
HB_FUNC( QT_QIMAGEREADER_NEXTIMAGEDELAY )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retni( ( p )->nextImageDelay() );
   }
}

/*
 * int quality () const
 */
HB_FUNC( QT_QIMAGEREADER_QUALITY )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retni( ( p )->quality() );
   }
}

/*
 * QImage read ()
 */
HB_FUNC( QT_QIMAGEREADER_READ )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->read() ), true ) );
   }
}

/*
 * bool read ( QImage * image )
 */
HB_FUNC( QT_QIMAGEREADER_READ_1 )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retl( ( p )->read( hbqt_par_QImage( 2 ) ) );
   }
}

/*
 * QRect scaledClipRect () const
 */
HB_FUNC( QT_QIMAGEREADER_SCALEDCLIPRECT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->scaledClipRect() ), true ) );
   }
}

/*
 * QSize scaledSize () const
 */
HB_FUNC( QT_QIMAGEREADER_SCALEDSIZE )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->scaledSize() ), true ) );
   }
}

/*
 * void setAutoDetectImageFormat ( bool enabled )
 */
HB_FUNC( QT_QIMAGEREADER_SETAUTODETECTIMAGEFORMAT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      ( p )->setAutoDetectImageFormat( hb_parl( 2 ) );
   }
}

/*
 * void setBackgroundColor ( const QColor & color )
 */
HB_FUNC( QT_QIMAGEREADER_SETBACKGROUNDCOLOR )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      ( p )->setBackgroundColor( *hbqt_par_QColor( 2 ) );
   }
}

/*
 * void setClipRect ( const QRect & rect )
 */
HB_FUNC( QT_QIMAGEREADER_SETCLIPRECT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      ( p )->setClipRect( *hbqt_par_QRect( 2 ) );
   }
}

/*
 * void setDevice ( QIODevice * device )
 */
HB_FUNC( QT_QIMAGEREADER_SETDEVICE )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      ( p )->setDevice( hbqt_par_QIODevice( 2 ) );
   }
}

/*
 * void setFileName ( const QString & fileName )
 */
HB_FUNC( QT_QIMAGEREADER_SETFILENAME )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      void * pText;
      ( p )->setFileName( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setFormat ( const QByteArray & format )
 */
HB_FUNC( QT_QIMAGEREADER_SETFORMAT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      ( p )->setFormat( *hbqt_par_QByteArray( 2 ) );
   }
}

/*
 * void setQuality ( int quality )
 */
HB_FUNC( QT_QIMAGEREADER_SETQUALITY )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      ( p )->setQuality( hb_parni( 2 ) );
   }
}

/*
 * void setScaledClipRect ( const QRect & rect )
 */
HB_FUNC( QT_QIMAGEREADER_SETSCALEDCLIPRECT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      ( p )->setScaledClipRect( *hbqt_par_QRect( 2 ) );
   }
}

/*
 * void setScaledSize ( const QSize & size )
 */
HB_FUNC( QT_QIMAGEREADER_SETSCALEDSIZE )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      ( p )->setScaledSize( *hbqt_par_QSize( 2 ) );
   }
}

/*
 * QSize size () const
 */
HB_FUNC( QT_QIMAGEREADER_SIZE )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->size() ), true ) );
   }
}

/*
 * bool supportsAnimation () const
 */
HB_FUNC( QT_QIMAGEREADER_SUPPORTSANIMATION )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retl( ( p )->supportsAnimation() );
   }
}

/*
 * bool supportsOption ( QImageIOHandler::ImageOption option ) const
 */
HB_FUNC( QT_QIMAGEREADER_SUPPORTSOPTION )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retl( ( p )->supportsOption( ( QImageIOHandler::ImageOption ) hb_parni( 2 ) ) );
   }
}

/*
 * QString text ( const QString & key ) const
 */
HB_FUNC( QT_QIMAGEREADER_TEXT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->text( hb_parstr_utf8( 2, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QStringList textKeys () const
 */
HB_FUNC( QT_QIMAGEREADER_TEXTKEYS )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->textKeys() ), true ) );
   }
}

/*
 * QByteArray imageFormat ( const QString & fileName )
 */
HB_FUNC( QT_QIMAGEREADER_IMAGEFORMAT_1 )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->imageFormat( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QByteArray imageFormat ( QIODevice * device )
 */
HB_FUNC( QT_QIMAGEREADER_IMAGEFORMAT_2 )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->imageFormat( hbqt_par_QIODevice( 2 ) ) ), true ) );
   }
}

/*
 * QList<QByteArray> supportedImageFormats ()
 */
HB_FUNC( QT_QIMAGEREADER_SUPPORTEDIMAGEFORMATS )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->supportedImageFormats() ), true ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
