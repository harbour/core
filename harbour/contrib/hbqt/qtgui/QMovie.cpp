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
 *  enum CacheMode { CacheNone, CacheAll }
 *  enum MovieState { NotRunning, Paused, Running }
 */

#include <QtCore/QPointer>

#include <QtGui/QMovie>
#include <QtGui/QColor>
#include <QtGui/QPixmap>


/* QMovie ( QObject * parent = 0 )
 * QMovie ( QIODevice * device, const QByteArray & format = QByteArray(), QObject * parent = 0 )
 * QMovie ( const QString & fileName, const QByteArray & format = QByteArray(), QObject * parent = 0 )
 * ~QMovie ()
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QMovie > pq;
} QGC_POINTER_QMovie;

QT_G_FUNC( release_QMovie )
{
   QGC_POINTER_QMovie * p = ( QGC_POINTER_QMovie * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QMovie                       p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QMovie                      ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QMovie * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QMovie * ) p->ph )->~QMovie();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QMovie * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QMovie                      Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO release_QMovie                      Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_QMovie                      Object Already deleted!" ) );
   }
}

void * hbqt_gcAllocate_QMovie( void * pObj )
{
   QGC_POINTER_QMovie * p = ( QGC_POINTER_QMovie * ) hb_gcAllocate( sizeof( QGC_POINTER_QMovie ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = release_QMovie;
   new( & p->pq ) QPointer< QMovie >( ( QMovie * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QMovie                      %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QMOVIE )
{
   void * pObj = NULL;

   pObj = new QMovie() ;

   hb_retptrGC( hbqt_gcAllocate_QMovie( pObj ) );
}
/*
 * QColor backgroundColor () const
 */
HB_FUNC( QT_QMOVIE_BACKGROUNDCOLOR )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( hbqt_par_QMovie( 1 )->backgroundColor() ) ) );
}

/*
 * CacheMode cacheMode () const
 */
HB_FUNC( QT_QMOVIE_CACHEMODE )
{
   hb_retni( ( QMovie::CacheMode ) hbqt_par_QMovie( 1 )->cacheMode() );
}

/*
 * int currentFrameNumber () const
 */
HB_FUNC( QT_QMOVIE_CURRENTFRAMENUMBER )
{
   hb_retni( hbqt_par_QMovie( 1 )->currentFrameNumber() );
}

/*
 * QImage currentImage () const
 */
HB_FUNC( QT_QMOVIE_CURRENTIMAGE )
{
   hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( hbqt_par_QMovie( 1 )->currentImage() ) ) );
}

/*
 * QPixmap currentPixmap () const
 */
HB_FUNC( QT_QMOVIE_CURRENTPIXMAP )
{
   hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( hbqt_par_QMovie( 1 )->currentPixmap() ) ) );
}

/*
 * QIODevice * device () const
 */
HB_FUNC( QT_QMOVIE_DEVICE )
{
   hb_retptr( ( QIODevice* ) hbqt_par_QMovie( 1 )->device() );
}

/*
 * QString fileName () const
 */
HB_FUNC( QT_QMOVIE_FILENAME )
{
   hb_retc( hbqt_par_QMovie( 1 )->fileName().toAscii().data() );
}

/*
 * QByteArray format () const
 */
HB_FUNC( QT_QMOVIE_FORMAT )
{
   hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( hbqt_par_QMovie( 1 )->format() ) ) );
}

/*
 * int frameCount () const
 */
HB_FUNC( QT_QMOVIE_FRAMECOUNT )
{
   hb_retni( hbqt_par_QMovie( 1 )->frameCount() );
}

/*
 * QRect frameRect () const
 */
HB_FUNC( QT_QMOVIE_FRAMERECT )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QMovie( 1 )->frameRect() ) ) );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QMOVIE_ISVALID )
{
   hb_retl( hbqt_par_QMovie( 1 )->isValid() );
}

/*
 * bool jumpToFrame ( int frameNumber )
 */
HB_FUNC( QT_QMOVIE_JUMPTOFRAME )
{
   hb_retl( hbqt_par_QMovie( 1 )->jumpToFrame( hb_parni( 2 ) ) );
}

/*
 * int loopCount () const
 */
HB_FUNC( QT_QMOVIE_LOOPCOUNT )
{
   hb_retni( hbqt_par_QMovie( 1 )->loopCount() );
}

/*
 * int nextFrameDelay () const
 */
HB_FUNC( QT_QMOVIE_NEXTFRAMEDELAY )
{
   hb_retni( hbqt_par_QMovie( 1 )->nextFrameDelay() );
}

/*
 * QSize scaledSize ()
 */
HB_FUNC( QT_QMOVIE_SCALEDSIZE )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QMovie( 1 )->scaledSize() ) ) );
}

/*
 * void setBackgroundColor ( const QColor & color )
 */
HB_FUNC( QT_QMOVIE_SETBACKGROUNDCOLOR )
{
   hbqt_par_QMovie( 1 )->setBackgroundColor( *hbqt_par_QColor( 2 ) );
}

/*
 * void setCacheMode ( CacheMode mode )
 */
HB_FUNC( QT_QMOVIE_SETCACHEMODE )
{
   hbqt_par_QMovie( 1 )->setCacheMode( ( QMovie::CacheMode ) hb_parni( 2 ) );
}

/*
 * void setDevice ( QIODevice * device )
 */
HB_FUNC( QT_QMOVIE_SETDEVICE )
{
   hbqt_par_QMovie( 1 )->setDevice( hbqt_par_QIODevice( 2 ) );
}

/*
 * void setFileName ( const QString & fileName )
 */
HB_FUNC( QT_QMOVIE_SETFILENAME )
{
   hbqt_par_QMovie( 1 )->setFileName( hbqt_par_QString( 2 ) );
}

/*
 * void setFormat ( const QByteArray & format )
 */
HB_FUNC( QT_QMOVIE_SETFORMAT )
{
   hbqt_par_QMovie( 1 )->setFormat( *hbqt_par_QByteArray( 2 ) );
}

/*
 * void setScaledSize ( const QSize & size )
 */
HB_FUNC( QT_QMOVIE_SETSCALEDSIZE )
{
   hbqt_par_QMovie( 1 )->setScaledSize( *hbqt_par_QSize( 2 ) );
}

/*
 * int speed () const
 */
HB_FUNC( QT_QMOVIE_SPEED )
{
   hb_retni( hbqt_par_QMovie( 1 )->speed() );
}

/*
 * MovieState state () const
 */
HB_FUNC( QT_QMOVIE_STATE )
{
   hb_retni( ( QMovie::MovieState ) hbqt_par_QMovie( 1 )->state() );
}

/*
 * bool jumpToNextFrame ()
 */
HB_FUNC( QT_QMOVIE_JUMPTONEXTFRAME )
{
   hb_retl( hbqt_par_QMovie( 1 )->jumpToNextFrame() );
}

/*
 * void setPaused ( bool paused )
 */
HB_FUNC( QT_QMOVIE_SETPAUSED )
{
   hbqt_par_QMovie( 1 )->setPaused( hb_parl( 2 ) );
}

/*
 * void setSpeed ( int percentSpeed )
 */
HB_FUNC( QT_QMOVIE_SETSPEED )
{
   hbqt_par_QMovie( 1 )->setSpeed( hb_parni( 2 ) );
}

/*
 * void start ()
 */
HB_FUNC( QT_QMOVIE_START )
{
   hbqt_par_QMovie( 1 )->start();
}

/*
 * void stop ()
 */
HB_FUNC( QT_QMOVIE_STOP )
{
   hbqt_par_QMovie( 1 )->stop();
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
