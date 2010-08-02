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

#include "hbqt.h"
#include "hbqtgui_garbage.h"
#include "hbqtcore_garbage.h"

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
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QImageReader;

QT_G_FUNC( hbqt_gcRelease_QImageReader )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

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
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

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
      hb_retl( ( p )->autoDetectImageFormat() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_AUTODETECTIMAGEFORMAT FP=hb_retl( ( p )->autoDetectImageFormat() ); p is NULL" ) );
   }
}

/*
 * QColor backgroundColor () const
 */
HB_FUNC( QT_QIMAGEREADER_BACKGROUNDCOLOR )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->backgroundColor() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_BACKGROUNDCOLOR FP=hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->backgroundColor() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool canRead () const
 */
HB_FUNC( QT_QIMAGEREADER_CANREAD )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retl( ( p )->canRead() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_CANREAD FP=hb_retl( ( p )->canRead() ); p is NULL" ) );
   }
}

/*
 * QRect clipRect () const
 */
HB_FUNC( QT_QIMAGEREADER_CLIPRECT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->clipRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_CLIPRECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->clipRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * int currentImageNumber () const
 */
HB_FUNC( QT_QIMAGEREADER_CURRENTIMAGENUMBER )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retni( ( p )->currentImageNumber() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_CURRENTIMAGENUMBER FP=hb_retni( ( p )->currentImageNumber() ); p is NULL" ) );
   }
}

/*
 * QRect currentImageRect () const
 */
HB_FUNC( QT_QIMAGEREADER_CURRENTIMAGERECT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->currentImageRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_CURRENTIMAGERECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->currentImageRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * QIODevice * device () const
 */
HB_FUNC( QT_QIMAGEREADER_DEVICE )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->device(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_DEVICE FP=hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->device(), false ) ); p is NULL" ) );
   }
}

/*
 * ImageReaderError error () const
 */
HB_FUNC( QT_QIMAGEREADER_ERROR )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retni( ( QImageReader::ImageReaderError ) ( p )->error() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_ERROR FP=hb_retni( ( QImageReader::ImageReaderError ) ( p )->error() ); p is NULL" ) );
   }
}

/*
 * QString errorString () const
 */
HB_FUNC( QT_QIMAGEREADER_ERRORSTRING )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retc( ( p )->errorString().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_ERRORSTRING FP=hb_retc( ( p )->errorString().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString fileName () const
 */
HB_FUNC( QT_QIMAGEREADER_FILENAME )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retc( ( p )->fileName().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_FILENAME FP=hb_retc( ( p )->fileName().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QByteArray format () const
 */
HB_FUNC( QT_QIMAGEREADER_FORMAT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->format() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_FORMAT FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->format() ), true ) ); p is NULL" ) );
   }
}

/*
 * int imageCount () const
 */
HB_FUNC( QT_QIMAGEREADER_IMAGECOUNT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retni( ( p )->imageCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_IMAGECOUNT FP=hb_retni( ( p )->imageCount() ); p is NULL" ) );
   }
}

/*
 * QImage::Format imageFormat () const
 */
HB_FUNC( QT_QIMAGEREADER_IMAGEFORMAT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retni( ( QImage::Format ) ( p )->imageFormat() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_IMAGEFORMAT FP=hb_retni( ( QImage::Format ) ( p )->imageFormat() ); p is NULL" ) );
   }
}

/*
 * bool jumpToImage ( int imageNumber )
 */
HB_FUNC( QT_QIMAGEREADER_JUMPTOIMAGE )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retl( ( p )->jumpToImage( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_JUMPTOIMAGE FP=hb_retl( ( p )->jumpToImage( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool jumpToNextImage ()
 */
HB_FUNC( QT_QIMAGEREADER_JUMPTONEXTIMAGE )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retl( ( p )->jumpToNextImage() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_JUMPTONEXTIMAGE FP=hb_retl( ( p )->jumpToNextImage() ); p is NULL" ) );
   }
}

/*
 * int loopCount () const
 */
HB_FUNC( QT_QIMAGEREADER_LOOPCOUNT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retni( ( p )->loopCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_LOOPCOUNT FP=hb_retni( ( p )->loopCount() ); p is NULL" ) );
   }
}

/*
 * int nextImageDelay () const
 */
HB_FUNC( QT_QIMAGEREADER_NEXTIMAGEDELAY )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retni( ( p )->nextImageDelay() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_NEXTIMAGEDELAY FP=hb_retni( ( p )->nextImageDelay() ); p is NULL" ) );
   }
}

/*
 * int quality () const
 */
HB_FUNC( QT_QIMAGEREADER_QUALITY )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retni( ( p )->quality() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_QUALITY FP=hb_retni( ( p )->quality() ); p is NULL" ) );
   }
}

/*
 * QImage read ()
 */
HB_FUNC( QT_QIMAGEREADER_READ )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->read() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_READ FP=hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->read() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool read ( QImage * image )
 */
HB_FUNC( QT_QIMAGEREADER_READ_1 )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retl( ( p )->read( hbqt_par_QImage( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_READ_1 FP=hb_retl( ( p )->read( hbqt_par_QImage( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QRect scaledClipRect () const
 */
HB_FUNC( QT_QIMAGEREADER_SCALEDCLIPRECT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->scaledClipRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_SCALEDCLIPRECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->scaledClipRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * QSize scaledSize () const
 */
HB_FUNC( QT_QIMAGEREADER_SCALEDSIZE )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->scaledSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_SCALEDSIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->scaledSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * void setAutoDetectImageFormat ( bool enabled )
 */
HB_FUNC( QT_QIMAGEREADER_SETAUTODETECTIMAGEFORMAT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      ( p )->setAutoDetectImageFormat( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_SETAUTODETECTIMAGEFORMAT FP=( p )->setAutoDetectImageFormat( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBackgroundColor ( const QColor & color )
 */
HB_FUNC( QT_QIMAGEREADER_SETBACKGROUNDCOLOR )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      ( p )->setBackgroundColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_SETBACKGROUNDCOLOR FP=( p )->setBackgroundColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setClipRect ( const QRect & rect )
 */
HB_FUNC( QT_QIMAGEREADER_SETCLIPRECT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      ( p )->setClipRect( *hbqt_par_QRect( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_SETCLIPRECT FP=( p )->setClipRect( *hbqt_par_QRect( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDevice ( QIODevice * device )
 */
HB_FUNC( QT_QIMAGEREADER_SETDEVICE )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      ( p )->setDevice( hbqt_par_QIODevice( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_SETDEVICE FP=( p )->setDevice( hbqt_par_QIODevice( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFileName ( const QString & fileName )
 */
HB_FUNC( QT_QIMAGEREADER_SETFILENAME )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      ( p )->setFileName( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_SETFILENAME FP=( p )->setFileName( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFormat ( const QByteArray & format )
 */
HB_FUNC( QT_QIMAGEREADER_SETFORMAT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      ( p )->setFormat( *hbqt_par_QByteArray( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_SETFORMAT FP=( p )->setFormat( *hbqt_par_QByteArray( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setQuality ( int quality )
 */
HB_FUNC( QT_QIMAGEREADER_SETQUALITY )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      ( p )->setQuality( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_SETQUALITY FP=( p )->setQuality( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setScaledClipRect ( const QRect & rect )
 */
HB_FUNC( QT_QIMAGEREADER_SETSCALEDCLIPRECT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      ( p )->setScaledClipRect( *hbqt_par_QRect( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_SETSCALEDCLIPRECT FP=( p )->setScaledClipRect( *hbqt_par_QRect( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setScaledSize ( const QSize & size )
 */
HB_FUNC( QT_QIMAGEREADER_SETSCALEDSIZE )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      ( p )->setScaledSize( *hbqt_par_QSize( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_SETSCALEDSIZE FP=( p )->setScaledSize( *hbqt_par_QSize( 2 ) ); p is NULL" ) );
   }
}

/*
 * QSize size () const
 */
HB_FUNC( QT_QIMAGEREADER_SIZE )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->size() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_SIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->size() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool supportsAnimation () const
 */
HB_FUNC( QT_QIMAGEREADER_SUPPORTSANIMATION )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retl( ( p )->supportsAnimation() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_SUPPORTSANIMATION FP=hb_retl( ( p )->supportsAnimation() ); p is NULL" ) );
   }
}

/*
 * bool supportsOption ( QImageIOHandler::ImageOption option ) const
 */
HB_FUNC( QT_QIMAGEREADER_SUPPORTSOPTION )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retl( ( p )->supportsOption( ( QImageIOHandler::ImageOption ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_SUPPORTSOPTION FP=hb_retl( ( p )->supportsOption( ( QImageIOHandler::ImageOption ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QString text ( const QString & key ) const
 */
HB_FUNC( QT_QIMAGEREADER_TEXT )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retc( ( p )->text( hbqt_par_QString( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_TEXT FP=hb_retc( ( p )->text( hbqt_par_QString( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QStringList textKeys () const
 */
HB_FUNC( QT_QIMAGEREADER_TEXTKEYS )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->textKeys() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_TEXTKEYS FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->textKeys() ), true ) ); p is NULL" ) );
   }
}

/*
 * QByteArray imageFormat ( const QString & fileName )
 */
HB_FUNC( QT_QIMAGEREADER_IMAGEFORMAT_1 )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->imageFormat( hbqt_par_QString( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_IMAGEFORMAT_1 FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->imageFormat( hbqt_par_QString( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QByteArray imageFormat ( QIODevice * device )
 */
HB_FUNC( QT_QIMAGEREADER_IMAGEFORMAT_2 )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->imageFormat( hbqt_par_QIODevice( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_IMAGEFORMAT_2 FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->imageFormat( hbqt_par_QIODevice( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QList<QByteArray> supportedImageFormats ()
 */
HB_FUNC( QT_QIMAGEREADER_SUPPORTEDIMAGEFORMATS )
{
   QImageReader * p = hbqt_par_QImageReader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->supportedImageFormats() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEREADER_SUPPORTEDIMAGEFORMATS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->supportedImageFormats() ), true ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
