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
 *  enum ImageWriterError { DeviceError, UnsupportedFormatError, UnknownError }
 */

/*
 *  Constructed[ 18/19 [ 94.74% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QByteArray> supportedImageFormats ()
 */

#include <QtCore/QPointer>

#include <QtGui/QImageWriter>


/*
 * QImageWriter ()
 * QImageWriter ( QIODevice * device, const QByteArray & format )
 * QImageWriter ( const QString & fileName, const QByteArray & format = QByteArray() )
 * ~QImageWriter ()
 */

QT_G_FUNC( release_QImageWriter )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QImageWriter                 p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "release_QImageWriter                ph=%p", p->ph ) );

   if( p && p->ph )
   {
      ( ( QImageWriter * ) p->ph )->~QImageWriter();
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "release_QImageWriter                Object deleted!" ) );
      #if defined(__debug__)
         just_debug( "  YES release_QImageWriter                %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
      #endif
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "release_QImageWriter                Object Allready deleted!" ) );
      #if defined(__debug__)
         just_debug( "  DEL release_QImageWriter" );
      #endif
   }
}

void * gcAllocate_QImageWriter( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QImageWriter;
   #if defined(__debug__)
      just_debug( "          new_QImageWriter                %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
   #endif
   return( p );
}

HB_FUNC( QT_QIMAGEWRITER )
{
   void * pObj = NULL;

   pObj = ( QImageWriter* ) new QImageWriter() ;

   hb_retptrGC( gcAllocate_QImageWriter( pObj ) );
}
/*
 * bool canWrite () const
 */
HB_FUNC( QT_QIMAGEWRITER_CANWRITE )
{
   hb_retl( hbqt_par_QImageWriter( 1 )->canWrite() );
}

/*
 * int compression () const
 */
HB_FUNC( QT_QIMAGEWRITER_COMPRESSION )
{
   hb_retni( hbqt_par_QImageWriter( 1 )->compression() );
}

/*
 * QIODevice * device () const
 */
HB_FUNC( QT_QIMAGEWRITER_DEVICE )
{
   hb_retptr( ( QIODevice* ) hbqt_par_QImageWriter( 1 )->device() );
}

/*
 * ImageWriterError error () const
 */
HB_FUNC( QT_QIMAGEWRITER_ERROR )
{
   hb_retni( ( QImageWriter::ImageWriterError ) hbqt_par_QImageWriter( 1 )->error() );
}

/*
 * QString errorString () const
 */
HB_FUNC( QT_QIMAGEWRITER_ERRORSTRING )
{
   hb_retc( hbqt_par_QImageWriter( 1 )->errorString().toAscii().data() );
}

/*
 * QString fileName () const
 */
HB_FUNC( QT_QIMAGEWRITER_FILENAME )
{
   hb_retc( hbqt_par_QImageWriter( 1 )->fileName().toAscii().data() );
}

/*
 * QByteArray format () const
 */
HB_FUNC( QT_QIMAGEWRITER_FORMAT )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QImageWriter( 1 )->format() ) ) );
}

/*
 * float gamma () const
 */
HB_FUNC( QT_QIMAGEWRITER_GAMMA )
{
   hb_retnd( hbqt_par_QImageWriter( 1 )->gamma() );
}

/*
 * int quality () const
 */
HB_FUNC( QT_QIMAGEWRITER_QUALITY )
{
   hb_retni( hbqt_par_QImageWriter( 1 )->quality() );
}

/*
 * void setCompression ( int compression )
 */
HB_FUNC( QT_QIMAGEWRITER_SETCOMPRESSION )
{
   hbqt_par_QImageWriter( 1 )->setCompression( hb_parni( 2 ) );
}

/*
 * void setDevice ( QIODevice * device )
 */
HB_FUNC( QT_QIMAGEWRITER_SETDEVICE )
{
   hbqt_par_QImageWriter( 1 )->setDevice( hbqt_par_QIODevice( 2 ) );
}

/*
 * void setFileName ( const QString & fileName )
 */
HB_FUNC( QT_QIMAGEWRITER_SETFILENAME )
{
   hbqt_par_QImageWriter( 1 )->setFileName( hbqt_par_QString( 2 ) );
}

/*
 * void setFormat ( const QByteArray & format )
 */
HB_FUNC( QT_QIMAGEWRITER_SETFORMAT )
{
   hbqt_par_QImageWriter( 1 )->setFormat( *hbqt_par_QByteArray( 2 ) );
}

/*
 * void setGamma ( float gamma )
 */
HB_FUNC( QT_QIMAGEWRITER_SETGAMMA )
{
   hbqt_par_QImageWriter( 1 )->setGamma( hb_parnd( 2 ) );
}

/*
 * void setQuality ( int quality )
 */
HB_FUNC( QT_QIMAGEWRITER_SETQUALITY )
{
   hbqt_par_QImageWriter( 1 )->setQuality( hb_parni( 2 ) );
}

/*
 * void setText ( const QString & key, const QString & text )
 */
HB_FUNC( QT_QIMAGEWRITER_SETTEXT )
{
   hbqt_par_QImageWriter( 1 )->setText( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * bool supportsOption ( QImageIOHandler::ImageOption option ) const
 */
HB_FUNC( QT_QIMAGEWRITER_SUPPORTSOPTION )
{
   hb_retl( hbqt_par_QImageWriter( 1 )->supportsOption( ( QImageIOHandler::ImageOption ) hb_parni( 2 ) ) );
}

/*
 * bool write ( const QImage & image )
 */
HB_FUNC( QT_QIMAGEWRITER_WRITE )
{
   hb_retl( hbqt_par_QImageWriter( 1 )->write( *hbqt_par_QImage( 2 ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
