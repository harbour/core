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
#include "hbqtgui.h"
#include "hbqtcore_garbage.h"
#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum ImageWriterError { DeviceError, UnsupportedFormatError, UnknownError }
 */

#include <QtCore/QPointer>

#include <QtGui/QImageWriter>


/*
 * QImageWriter ()
 * QImageWriter ( QIODevice * device, const QByteArray & format )
 * QImageWriter ( const QString & fileName, const QByteArray & format = QByteArray() )
 * ~QImageWriter ()
 */

typedef struct
{
   QImageWriter * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QImageWriter;

QT_G_FUNC( hbqt_gcRelease_QImageWriter )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QImageWriter   /.\\", p->ph ) );
         delete ( ( QImageWriter * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QImageWriter   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QImageWriter    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QImageWriter    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QImageWriter( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QImageWriter * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QImageWriter;
   p->type = HBQT_TYPE_QImageWriter;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QImageWriter", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QImageWriter", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QIMAGEWRITER )
{
   QImageWriter * pObj = NULL;

   pObj =  new QImageWriter() ;

   hb_retptrGC( hbqt_gcAllocate_QImageWriter( ( void * ) pObj, true ) );
}

/*
 * bool canWrite () const
 */
HB_FUNC( QT_QIMAGEWRITER_CANWRITE )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retl( ( p )->canWrite() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEWRITER_CANWRITE FP=hb_retl( ( p )->canWrite() ); p is NULL" ) );
   }
}

/*
 * int compression () const
 */
HB_FUNC( QT_QIMAGEWRITER_COMPRESSION )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retni( ( p )->compression() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEWRITER_COMPRESSION FP=hb_retni( ( p )->compression() ); p is NULL" ) );
   }
}

/*
 * QIODevice * device () const
 */
HB_FUNC( QT_QIMAGEWRITER_DEVICE )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->device(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEWRITER_DEVICE FP=hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->device(), false ) ); p is NULL" ) );
   }
}

/*
 * ImageWriterError error () const
 */
HB_FUNC( QT_QIMAGEWRITER_ERROR )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retni( ( QImageWriter::ImageWriterError ) ( p )->error() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEWRITER_ERROR FP=hb_retni( ( QImageWriter::ImageWriterError ) ( p )->error() ); p is NULL" ) );
   }
}

/*
 * QString errorString () const
 */
HB_FUNC( QT_QIMAGEWRITER_ERRORSTRING )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retc( ( p )->errorString().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEWRITER_ERRORSTRING FP=hb_retc( ( p )->errorString().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString fileName () const
 */
HB_FUNC( QT_QIMAGEWRITER_FILENAME )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retc( ( p )->fileName().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEWRITER_FILENAME FP=hb_retc( ( p )->fileName().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QByteArray format () const
 */
HB_FUNC( QT_QIMAGEWRITER_FORMAT )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->format() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEWRITER_FORMAT FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->format() ), true ) ); p is NULL" ) );
   }
}

/*
 * float gamma () const
 */
HB_FUNC( QT_QIMAGEWRITER_GAMMA )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retnd( ( p )->gamma() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEWRITER_GAMMA FP=hb_retnd( ( p )->gamma() ); p is NULL" ) );
   }
}

/*
 * int quality () const
 */
HB_FUNC( QT_QIMAGEWRITER_QUALITY )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retni( ( p )->quality() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEWRITER_QUALITY FP=hb_retni( ( p )->quality() ); p is NULL" ) );
   }
}

/*
 * void setCompression ( int compression )
 */
HB_FUNC( QT_QIMAGEWRITER_SETCOMPRESSION )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      ( p )->setCompression( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEWRITER_SETCOMPRESSION FP=( p )->setCompression( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDevice ( QIODevice * device )
 */
HB_FUNC( QT_QIMAGEWRITER_SETDEVICE )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      ( p )->setDevice( hbqt_par_QIODevice( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEWRITER_SETDEVICE FP=( p )->setDevice( hbqt_par_QIODevice( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFileName ( const QString & fileName )
 */
HB_FUNC( QT_QIMAGEWRITER_SETFILENAME )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      ( p )->setFileName( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEWRITER_SETFILENAME FP=( p )->setFileName( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFormat ( const QByteArray & format )
 */
HB_FUNC( QT_QIMAGEWRITER_SETFORMAT )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      ( p )->setFormat( *hbqt_par_QByteArray( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEWRITER_SETFORMAT FP=( p )->setFormat( *hbqt_par_QByteArray( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setGamma ( float gamma )
 */
HB_FUNC( QT_QIMAGEWRITER_SETGAMMA )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      ( p )->setGamma( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEWRITER_SETGAMMA FP=( p )->setGamma( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setQuality ( int quality )
 */
HB_FUNC( QT_QIMAGEWRITER_SETQUALITY )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      ( p )->setQuality( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEWRITER_SETQUALITY FP=( p )->setQuality( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setText ( const QString & key, const QString & text )
 */
HB_FUNC( QT_QIMAGEWRITER_SETTEXT )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      ( p )->setText( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEWRITER_SETTEXT FP=( p )->setText( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ); p is NULL" ) );
   }
}

/*
 * bool supportsOption ( QImageIOHandler::ImageOption option ) const
 */
HB_FUNC( QT_QIMAGEWRITER_SUPPORTSOPTION )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retl( ( p )->supportsOption( ( QImageIOHandler::ImageOption ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEWRITER_SUPPORTSOPTION FP=hb_retl( ( p )->supportsOption( ( QImageIOHandler::ImageOption ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool write ( const QImage & image )
 */
HB_FUNC( QT_QIMAGEWRITER_WRITE )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retl( ( p )->write( *hbqt_par_QImage( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEWRITER_WRITE FP=hb_retl( ( p )->write( *hbqt_par_QImage( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QList<QByteArray> supportedImageFormats ()
 */
HB_FUNC( QT_QIMAGEWRITER_SUPPORTEDIMAGEFORMATS )
{
   QImageWriter * p = hbqt_par_QImageWriter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->supportedImageFormats() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QIMAGEWRITER_SUPPORTEDIMAGEFORMATS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->supportedImageFormats() ), true ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
