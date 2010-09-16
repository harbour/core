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
   QPointer< QMovie > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMovie;

HBQT_GC_FUNC( hbqt_gcRelease_QMovie )
{
   QMovie  * ph = NULL ;
   HBQT_GC_T_QMovie * p = ( HBQT_GC_T_QMovie * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QMovie   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QMovie   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QMovie          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QMovie    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QMovie    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QMovie( void * pObj, bool bNew )
{
   HBQT_GC_T_QMovie * p = ( HBQT_GC_T_QMovie * ) hb_gcAllocate( sizeof( HBQT_GC_T_QMovie ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QMovie >( ( QMovie * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMovie;
   p->type = HBQT_TYPE_QMovie;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QMovie  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QMovie", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QMOVIE )
{
   QMovie * pObj = NULL;

   pObj = new QMovie() ;

   hb_retptrGC( hbqt_gcAllocate_QMovie( ( void * ) pObj, true ) );
}

/*
 * QColor backgroundColor () const
 */
HB_FUNC( QT_QMOVIE_BACKGROUNDCOLOR )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->backgroundColor() ), true ) );
   }
}

/*
 * CacheMode cacheMode () const
 */
HB_FUNC( QT_QMOVIE_CACHEMODE )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      hb_retni( ( QMovie::CacheMode ) ( p )->cacheMode() );
   }
}

/*
 * int currentFrameNumber () const
 */
HB_FUNC( QT_QMOVIE_CURRENTFRAMENUMBER )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      hb_retni( ( p )->currentFrameNumber() );
   }
}

/*
 * QImage currentImage () const
 */
HB_FUNC( QT_QMOVIE_CURRENTIMAGE )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->currentImage() ), true ) );
   }
}

/*
 * QPixmap currentPixmap () const
 */
HB_FUNC( QT_QMOVIE_CURRENTPIXMAP )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->currentPixmap() ), true ) );
   }
}

/*
 * QIODevice * device () const
 */
HB_FUNC( QT_QMOVIE_DEVICE )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->device(), false ) );
   }
}

/*
 * QString fileName () const
 */
HB_FUNC( QT_QMOVIE_FILENAME )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->fileName().toUtf8().data() );
   }
}

/*
 * QByteArray format () const
 */
HB_FUNC( QT_QMOVIE_FORMAT )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->format() ), true ) );
   }
}

/*
 * int frameCount () const
 */
HB_FUNC( QT_QMOVIE_FRAMECOUNT )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      hb_retni( ( p )->frameCount() );
   }
}

/*
 * QRect frameRect () const
 */
HB_FUNC( QT_QMOVIE_FRAMERECT )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->frameRect() ), true ) );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QMOVIE_ISVALID )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      hb_retl( ( p )->isValid() );
   }
}

/*
 * bool jumpToFrame ( int frameNumber )
 */
HB_FUNC( QT_QMOVIE_JUMPTOFRAME )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      hb_retl( ( p )->jumpToFrame( hb_parni( 2 ) ) );
   }
}

/*
 * int loopCount () const
 */
HB_FUNC( QT_QMOVIE_LOOPCOUNT )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      hb_retni( ( p )->loopCount() );
   }
}

/*
 * int nextFrameDelay () const
 */
HB_FUNC( QT_QMOVIE_NEXTFRAMEDELAY )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      hb_retni( ( p )->nextFrameDelay() );
   }
}

/*
 * QSize scaledSize ()
 */
HB_FUNC( QT_QMOVIE_SCALEDSIZE )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->scaledSize() ), true ) );
   }
}

/*
 * void setBackgroundColor ( const QColor & color )
 */
HB_FUNC( QT_QMOVIE_SETBACKGROUNDCOLOR )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      ( p )->setBackgroundColor( *hbqt_par_QColor( 2 ) );
   }
}

/*
 * void setCacheMode ( CacheMode mode )
 */
HB_FUNC( QT_QMOVIE_SETCACHEMODE )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      ( p )->setCacheMode( ( QMovie::CacheMode ) hb_parni( 2 ) );
   }
}

/*
 * void setDevice ( QIODevice * device )
 */
HB_FUNC( QT_QMOVIE_SETDEVICE )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      ( p )->setDevice( hbqt_par_QIODevice( 2 ) );
   }
}

/*
 * void setFileName ( const QString & fileName )
 */
HB_FUNC( QT_QMOVIE_SETFILENAME )
{
   QMovie * p = hbqt_par_QMovie( 1 );
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
HB_FUNC( QT_QMOVIE_SETFORMAT )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      ( p )->setFormat( *hbqt_par_QByteArray( 2 ) );
   }
}

/*
 * void setScaledSize ( const QSize & size )
 */
HB_FUNC( QT_QMOVIE_SETSCALEDSIZE )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      ( p )->setScaledSize( *hbqt_par_QSize( 2 ) );
   }
}

/*
 * int speed () const
 */
HB_FUNC( QT_QMOVIE_SPEED )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      hb_retni( ( p )->speed() );
   }
}

/*
 * MovieState state () const
 */
HB_FUNC( QT_QMOVIE_STATE )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      hb_retni( ( QMovie::MovieState ) ( p )->state() );
   }
}

/*
 * bool jumpToNextFrame ()
 */
HB_FUNC( QT_QMOVIE_JUMPTONEXTFRAME )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      hb_retl( ( p )->jumpToNextFrame() );
   }
}

/*
 * void setPaused ( bool paused )
 */
HB_FUNC( QT_QMOVIE_SETPAUSED )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      ( p )->setPaused( hb_parl( 2 ) );
   }
}

/*
 * void setSpeed ( int percentSpeed )
 */
HB_FUNC( QT_QMOVIE_SETSPEED )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      ( p )->setSpeed( hb_parni( 2 ) );
   }
}

/*
 * void start ()
 */
HB_FUNC( QT_QMOVIE_START )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      ( p )->start();
   }
}

/*
 * void stop ()
 */
HB_FUNC( QT_QMOVIE_STOP )
{
   QMovie * p = hbqt_par_QMovie( 1 );
   if( p )
   {
      ( p )->stop();
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
