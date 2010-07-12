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

#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum HBitmapFormat { NoAlpha, PremultipliedAlpha, Alpha }
 *  enum ShareMode { ImplicitlyShared, ExplicitlyShared }
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
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QPixmap;

QT_G_FUNC( hbqt_gcRelease_QPixmap )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QPixmap   /.\\", p->ph ) );
         delete ( ( QPixmap * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QPixmap   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QPixmap    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QPixmap    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPixmap( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QPixmap * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPixmap;
   p->type = QT_TYPE_QPixmap;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QPixmap", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QPixmap", pObj ) );
   }
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
      pObj = new QPixmap( *hbqt_par_QPixmap( 1 ) ) ;
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

/*
 * QPixmap alphaChannel () const
 */
HB_FUNC( QT_QPIXMAP_ALPHACHANNEL )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->alphaChannel() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_ALPHACHANNEL FP=hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->alphaChannel() ), true ) ); p is NULL" ) );
   }
}

/*
 * qint64 cacheKey () const
 */
HB_FUNC( QT_QPIXMAP_CACHEKEY )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retnint( ( p )->cacheKey() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_CACHEKEY FP=hb_retnint( ( p )->cacheKey() ); p is NULL" ) );
   }
}

/*
 * QPixmap copy ( const QRect & rectangle = QRect() ) const
 */
HB_FUNC( QT_QPIXMAP_COPY )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->copy( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QRect( 2 ) : QRect() ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_COPY FP=hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->copy( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QRect( 2 ) : QRect() ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPixmap copy ( int x, int y, int width, int height ) const
 */
HB_FUNC( QT_QPIXMAP_COPY_1 )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->copy( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_COPY_1 FP=hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->copy( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QBitmap createHeuristicMask ( bool clipTight = true ) const
 */
HB_FUNC( QT_QPIXMAP_CREATEHEURISTICMASK )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( ( p )->createHeuristicMask( hb_parl( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_CREATEHEURISTICMASK FP=hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( ( p )->createHeuristicMask( hb_parl( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QBitmap createMaskFromColor ( const QColor & maskColor, Qt::MaskMode mode ) const
 */
HB_FUNC( QT_QPIXMAP_CREATEMASKFROMCOLOR )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( ( p )->createMaskFromColor( *hbqt_par_QColor( 2 ), ( Qt::MaskMode ) hb_parni( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_CREATEMASKFROMCOLOR FP=hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( ( p )->createMaskFromColor( *hbqt_par_QColor( 2 ), ( Qt::MaskMode ) hb_parni( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QBitmap createMaskFromColor ( const QColor & maskColor ) const
 */
HB_FUNC( QT_QPIXMAP_CREATEMASKFROMCOLOR_1 )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( ( p )->createMaskFromColor( *hbqt_par_QColor( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_CREATEMASKFROMCOLOR_1 FP=hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( ( p )->createMaskFromColor( *hbqt_par_QColor( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * int depth () const
 */
HB_FUNC( QT_QPIXMAP_DEPTH )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retni( ( p )->depth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_DEPTH FP=hb_retni( ( p )->depth() ); p is NULL" ) );
   }
}

/*
 * void detach ()
 */
HB_FUNC( QT_QPIXMAP_DETACH )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      ( p )->detach();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_DETACH FP=( p )->detach(); p is NULL" ) );
   }
}

/*
 * void fill ( const QColor & color = Qt::white )
 */
HB_FUNC( QT_QPIXMAP_FILL )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      ( p )->fill( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_FILL FP=( p )->fill( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void fill ( const QWidget * widget, const QPoint & offset )
 */
HB_FUNC( QT_QPIXMAP_FILL_1 )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      ( p )->fill( hbqt_par_QWidget( 2 ), *hbqt_par_QPoint( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_FILL_1 FP=( p )->fill( hbqt_par_QWidget( 2 ), *hbqt_par_QPoint( 3 ) ); p is NULL" ) );
   }
}

/*
 * void fill ( const QWidget * widget, int x, int y )
 */
HB_FUNC( QT_QPIXMAP_FILL_2 )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      ( p )->fill( hbqt_par_QWidget( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_FILL_2 FP=( p )->fill( hbqt_par_QWidget( 2 ), hb_parni( 3 ), hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * bool hasAlpha () const
 */
HB_FUNC( QT_QPIXMAP_HASALPHA )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retl( ( p )->hasAlpha() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_HASALPHA FP=hb_retl( ( p )->hasAlpha() ); p is NULL" ) );
   }
}

/*
 * bool hasAlphaChannel () const
 */
HB_FUNC( QT_QPIXMAP_HASALPHACHANNEL )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retl( ( p )->hasAlphaChannel() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_HASALPHACHANNEL FP=hb_retl( ( p )->hasAlphaChannel() ); p is NULL" ) );
   }
}

/*
 * int height () const
 */
HB_FUNC( QT_QPIXMAP_HEIGHT )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retni( ( p )->height() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_HEIGHT FP=hb_retni( ( p )->height() ); p is NULL" ) );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QPIXMAP_ISNULL )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_ISNULL FP=hb_retl( ( p )->isNull() ); p is NULL" ) );
   }
}

/*
 * bool isQBitmap () const
 */
HB_FUNC( QT_QPIXMAP_ISQBITMAP )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retl( ( p )->isQBitmap() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_ISQBITMAP FP=hb_retl( ( p )->isQBitmap() ); p is NULL" ) );
   }
}

/*
 * bool load ( const QString & fileName, const char * format = 0, Qt::ImageConversionFlags flags = Qt::AutoColor )
 */
HB_FUNC( QT_QPIXMAP_LOAD )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retl( ( p )->load( hbqt_par_QString( 2 ), hbqt_par_char( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::ImageConversionFlags ) hb_parni( 4 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_LOAD FP=hb_retl( ( p )->load( hbqt_par_QString( 2 ), hbqt_par_char( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::ImageConversionFlags ) hb_parni( 4 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ) ); p is NULL" ) );
   }
}

/*
 * bool loadFromData ( const QByteArray & data, const char * format = 0, Qt::ImageConversionFlags flags = Qt::AutoColor )
 */
HB_FUNC( QT_QPIXMAP_LOADFROMDATA )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retl( ( p )->loadFromData( *hbqt_par_QByteArray( 2 ), hbqt_par_char( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::ImageConversionFlags ) hb_parni( 4 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_LOADFROMDATA FP=hb_retl( ( p )->loadFromData( *hbqt_par_QByteArray( 2 ), hbqt_par_char( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::ImageConversionFlags ) hb_parni( 4 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ) ); p is NULL" ) );
   }
}

/*
 * QBitmap mask () const
 */
HB_FUNC( QT_QPIXMAP_MASK )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( ( p )->mask() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_MASK FP=hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( ( p )->mask() ), true ) ); p is NULL" ) );
   }
}

/*
 * QRect rect () const
 */
HB_FUNC( QT_QPIXMAP_RECT )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->rect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_RECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->rect() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool save ( const QString & fileName, const char * format = 0, int quality = -1 ) const
 */
HB_FUNC( QT_QPIXMAP_SAVE )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retl( ( p )->save( hbqt_par_QString( 2 ), hbqt_par_char( 3 ), hb_parnidef( 4, -1 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_SAVE FP=hb_retl( ( p )->save( hbqt_par_QString( 2 ), hbqt_par_char( 3 ), hb_parnidef( 4, -1 ) ) ); p is NULL" ) );
   }
}

/*
 * bool save ( QIODevice * device, const char * format = 0, int quality = -1 ) const
 */
HB_FUNC( QT_QPIXMAP_SAVE_1 )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retl( ( p )->save( hbqt_par_QIODevice( 2 ), hbqt_par_char( 3 ), hb_parnidef( 4, -1 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_SAVE_1 FP=hb_retl( ( p )->save( hbqt_par_QIODevice( 2 ), hbqt_par_char( 3 ), hb_parnidef( 4, -1 ) ) ); p is NULL" ) );
   }
}

/*
 * QPixmap scaled ( int width, int height, Qt::AspectRatioMode aspectRatioMode = Qt::IgnoreAspectRatio, Qt::TransformationMode transformMode = Qt::FastTransformation ) const
 */
HB_FUNC( QT_QPIXMAP_SCALED )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->scaled( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::AspectRatioMode ) hb_parni( 4 ) : ( Qt::AspectRatioMode ) Qt::IgnoreAspectRatio ), ( HB_ISNUM( 5 ) ? ( Qt::TransformationMode ) hb_parni( 5 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_SCALED FP=hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->scaled( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::AspectRatioMode ) hb_parni( 4 ) : ( Qt::AspectRatioMode ) Qt::IgnoreAspectRatio ), ( HB_ISNUM( 5 ) ? ( Qt::TransformationMode ) hb_parni( 5 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPixmap scaled ( const QSize & size, Qt::AspectRatioMode aspectRatioMode = Qt::IgnoreAspectRatio, Qt::TransformationMode transformMode = Qt::FastTransformation ) const
 */
HB_FUNC( QT_QPIXMAP_SCALED_1 )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->scaled( *hbqt_par_QSize( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::AspectRatioMode ) hb_parni( 3 ) : ( Qt::AspectRatioMode ) Qt::IgnoreAspectRatio ), ( HB_ISNUM( 4 ) ? ( Qt::TransformationMode ) hb_parni( 4 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_SCALED_1 FP=hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->scaled( *hbqt_par_QSize( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::AspectRatioMode ) hb_parni( 3 ) : ( Qt::AspectRatioMode ) Qt::IgnoreAspectRatio ), ( HB_ISNUM( 4 ) ? ( Qt::TransformationMode ) hb_parni( 4 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPixmap scaledToHeight ( int height, Qt::TransformationMode mode = Qt::FastTransformation ) const
 */
HB_FUNC( QT_QPIXMAP_SCALEDTOHEIGHT )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->scaledToHeight( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_SCALEDTOHEIGHT FP=hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->scaledToHeight( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPixmap scaledToWidth ( int width, Qt::TransformationMode mode = Qt::FastTransformation ) const
 */
HB_FUNC( QT_QPIXMAP_SCALEDTOWIDTH )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->scaledToWidth( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_SCALEDTOWIDTH FP=hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->scaledToWidth( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void setAlphaChannel ( const QPixmap & alphaChannel )
 */
HB_FUNC( QT_QPIXMAP_SETALPHACHANNEL )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      ( p )->setAlphaChannel( *hbqt_par_QPixmap( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_SETALPHACHANNEL FP=( p )->setAlphaChannel( *hbqt_par_QPixmap( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMask ( const QBitmap & mask )
 */
HB_FUNC( QT_QPIXMAP_SETMASK )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      ( p )->setMask( *hbqt_par_QBitmap( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_SETMASK FP=( p )->setMask( *hbqt_par_QBitmap( 2 ) ); p is NULL" ) );
   }
}

/*
 * QSize size () const
 */
HB_FUNC( QT_QPIXMAP_SIZE )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->size() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_SIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->size() ), true ) ); p is NULL" ) );
   }
}

/*
 * QImage toImage () const
 */
HB_FUNC( QT_QPIXMAP_TOIMAGE )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->toImage() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_TOIMAGE FP=hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->toImage() ), true ) ); p is NULL" ) );
   }
}

/*
 * QPixmap transformed ( const QTransform & transform, Qt::TransformationMode mode = Qt::FastTransformation ) const
 */
HB_FUNC( QT_QPIXMAP_TRANSFORMED )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->transformed( *hbqt_par_QTransform( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_TRANSFORMED FP=hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->transformed( *hbqt_par_QTransform( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPixmap transformed ( const QMatrix & matrix, Qt::TransformationMode mode = Qt::FastTransformation ) const
 */
HB_FUNC( QT_QPIXMAP_TRANSFORMED_1 )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->transformed( *hbqt_par_QMatrix( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_TRANSFORMED_1 FP=hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->transformed( *hbqt_par_QMatrix( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * int width () const
 */
HB_FUNC( QT_QPIXMAP_WIDTH )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retni( ( p )->width() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_WIDTH FP=hb_retni( ( p )->width() ); p is NULL" ) );
   }
}

/*
 * int defaultDepth ()
 */
HB_FUNC( QT_QPIXMAP_DEFAULTDEPTH )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retni( ( p )->defaultDepth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_DEFAULTDEPTH FP=hb_retni( ( p )->defaultDepth() ); p is NULL" ) );
   }
}

/*
 * QPixmap fromImage ( const QImage & image, Qt::ImageConversionFlags flags = Qt::AutoColor )
 */
HB_FUNC( QT_QPIXMAP_FROMIMAGE )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->fromImage( *hbqt_par_QImage( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ImageConversionFlags ) hb_parni( 3 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_FROMIMAGE FP=hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->fromImage( *hbqt_par_QImage( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ImageConversionFlags ) hb_parni( 3 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPixmap grabWidget ( QWidget * widget, const QRect & rectangle )
 */
HB_FUNC( QT_QPIXMAP_GRABWIDGET )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->grabWidget( hbqt_par_QWidget( 2 ), *hbqt_par_QRect( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_GRABWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->grabWidget( hbqt_par_QWidget( 2 ), *hbqt_par_QRect( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPixmap grabWidget ( QWidget * widget, int x = 0, int y = 0, int width = -1, int height = -1 )
 */
HB_FUNC( QT_QPIXMAP_GRABWIDGET_1 )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->grabWidget( hbqt_par_QWidget( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parnidef( 5, -1 ), hb_parnidef( 6, -1 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_GRABWIDGET_1 FP=hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->grabWidget( hbqt_par_QWidget( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parnidef( 5, -1 ), hb_parnidef( 6, -1 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QTransform trueMatrix ( const QTransform & matrix, int width, int height )
 */
HB_FUNC( QT_QPIXMAP_TRUEMATRIX )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->trueMatrix( *hbqt_par_QTransform( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_TRUEMATRIX FP=hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->trueMatrix( *hbqt_par_QTransform( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QMatrix trueMatrix ( const QMatrix & m, int w, int h )
 */
HB_FUNC( QT_QPIXMAP_TRUEMATRIX_1 )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->trueMatrix( *hbqt_par_QMatrix( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPIXMAP_TRUEMATRIX_1 FP=hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->trueMatrix( *hbqt_par_QMatrix( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) ), true ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
