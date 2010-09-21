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
 *  enum HBitmapFormat { NoAlpha, PremultipliedAlpha, Alpha }
 *  enum ShareMode { ImplicitlyShared, ExplicitlyShared }
 */

/*
 *  Constructed[ 32/66 [ 48.48% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  }
 *  }
 *  }
 *  }
 *  }
 *  }
 *  }
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  // QRgb * clut () const
 *  //QPixmap copy ( const QRect & rectangle = QRect() ) const
 *  //QPixmap copy ( int x, int y, int width, int height ) const
 *  //QBitmap createMaskFromColor ( const QColor & maskColor, Qt::MaskMode mode ) const
 *  //QBitmap createMaskFromColor ( const QColor & maskColor ) const
 *  //void fill ( const QColor & color = Qt::white )
 *  //void fill ( const QWidget * widget, const QPoint & offset )
 *  //void fill ( const QWidget * widget, int x, int y )
 *  // Qt::HANDLE handle () const
 *  // bool loadFromData ( const uchar * data, uint len, const char * format = 0, Qt::ImageConversionFlags flags = Qt::AutoColor )
 *  // int numCols () const
 *  // const uchar * qwsBits () const
 *  // int qwsBytesPerLine () const
 *  //bool save ( const QString & fileName, const char * format = 0, int quality = -1 ) const
 *  //bool save ( QIODevice * device, const char * format = 0, int quality = -1 ) const
 *  //QPixmap scaled ( int width, int height, Qt::AspectRatioMode aspectRatioMode = Qt::IgnoreAspectRatio, Qt::TransformationMode transformMode = Qt::FastTransformation ) const
 *  //QPixmap scaled ( const QSize & size, Qt::AspectRatioMode aspectRatioMode = Qt::IgnoreAspectRatio, Qt::TransformationMode transformMode = Qt::FastTransformation ) const
 *  // CGImageRef toMacCGImageRef () const
 *  //HBITMAP toWinHBITMAP ( HBitmapFormat format = NoAlpha ) const
 *  //QPixmap transformed ( const QTransform & transform, Qt::TransformationMode mode = Qt::FastTransformation ) const
 *  //QPixmap transformed ( const QMatrix & matrix, Qt::TransformationMode mode = Qt::FastTransformation ) const
 *  //QPixmap fromMacCGImageRef ( CGImageRef image )
 *  //QPixmap fromWinHBITMAP ( HBITMAP bitmap, HBitmapFormat format = NoAlpha )
 *  //QPixmap fromX11Pixmap ( Qt::HANDLE pixmap, ShareMode mode = ImplicitlyShared )
 *  //QPixmap grabWidget ( QWidget * widget, const QRect & rectangle )
 *  //QPixmap grabWidget ( QWidget * widget, int x = 0, int y = 0, int width = -1, int height = -1 )
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
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QPixmap * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPixmap;
   p->type = HBQT_TYPE_QPixmap;

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

/*
 * QPixmap alphaChannel () const
 */
HB_FUNC( QT_QPIXMAP_ALPHACHANNEL )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->alphaChannel() ), true ) );
   }
}

/*
 * qint64 cacheKey () const
 */
HB_FUNC( QT_QPIXMAP_CACHEKEY )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      hb_retnint( ( p )->cacheKey() );
   }
}

/*
 * QPixmap copy ( ... )
 */
HB_FUNC( QT_QPIXMAP_COPY )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      if( hb_pcount() == 2 && HB_ISPOINTER( 2 ) )
      {
         hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->copy( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QRect( 2 ) : QRect() ) ) ), true ) );
      }
      else if( hb_pcount() == 5 && HB_ISNUM( 2 ) )
      {
         hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->copy( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ) ), true ) );
      }
      else
      {
         hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->copy( QRect() ) ), true ) );
      }
   }
}

/*
 * QBitmap createHeuristicMask ( bool clipTight = true ) const
 */
HB_FUNC( QT_QPIXMAP_CREATEHEURISTICMASK )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( ( p )->createHeuristicMask( hb_parl( 2 ) ) ), true ) );
   }
}

/*
 * QBitmap createMaskFromColor ( ... )
 */
HB_FUNC( QT_QPIXMAP_CREATEMASKFROMCOLOR )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      if( hb_pcount() == 2 && HB_ISPOINTER( 2 ) )
      {
         hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( ( p )->createMaskFromColor( *hbqt_par_QColor( 2 ) ) ), true ) );
      }
      else if( hb_pcount() == 3 && HB_ISPOINTER( 2 ) && HB_ISNUM( 3 ) )
      {
         hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( ( p )->createMaskFromColor( *hbqt_par_QColor( 2 ), ( Qt::MaskMode ) hb_parni( 3 ) ) ), true ) );
      }
      else
      {
         ( p )->copy( QRect() );
      }
   }
}

/*
 * int depth () const
 */
HB_FUNC( QT_QPIXMAP_DEPTH )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      hb_retni( ( p )->depth() );
   }
}

/*
 * void detach ()
 */
HB_FUNC( QT_QPIXMAP_DETACH )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      ( p )->detach();
   }
}

/*
 * void fill ( ... )
 */
HB_FUNC( QT_QPIXMAP_FILL )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      if( hb_pcount() == 4 && HB_ISPOINTER( 2 ) && HB_ISNUM( 3 ) )
      {
         ( p )->fill( hbqt_par_QWidget( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
      }
      else if( hb_pcount() == 3 && HB_ISPOINTER( 2 ) && HB_ISPOINTER( 3 ) )
      {
         ( p )->fill( hbqt_par_QWidget( 2 ), *hbqt_par_QPoint( 3 ) );
      }
      else
      {
         ( p )->fill( *hbqt_par_QColor( 2 ) );
      }
   }
}

/*
 * bool hasAlpha () const
 */
HB_FUNC( QT_QPIXMAP_HASALPHA )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      hb_retl( ( p )->hasAlpha() );
   }
}

/*
 * bool hasAlphaChannel () const
 */
HB_FUNC( QT_QPIXMAP_HASALPHACHANNEL )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      hb_retl( ( p )->hasAlphaChannel() );
   }
}

/*
 * int height () const
 */
HB_FUNC( QT_QPIXMAP_HEIGHT )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      hb_retni( ( p )->height() );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QPIXMAP_ISNULL )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      hb_retl( ( p )->isNull() );
   }
}

/*
 * bool isQBitmap () const
 */
HB_FUNC( QT_QPIXMAP_ISQBITMAP )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      hb_retl( ( p )->isQBitmap() );
   }
}

/*
 * bool load ( const QString & fileName, const char * format = 0, Qt::ImageConversionFlags flags = Qt::AutoColor )
 */
HB_FUNC( QT_QPIXMAP_LOAD )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->load( hb_parstr_utf8( 2, &pText, NULL ), hbqt_par_char( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::ImageConversionFlags ) hb_parni( 4 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool loadFromData ( const QByteArray & data, const char * format = 0, Qt::ImageConversionFlags flags = Qt::AutoColor )
 */
HB_FUNC( QT_QPIXMAP_LOADFROMDATA )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      hb_retl( ( p )->loadFromData( *hbqt_par_QByteArray( 2 ), hbqt_par_char( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::ImageConversionFlags ) hb_parni( 4 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ) );
   }
}

/*
 * QBitmap mask () const
 */
HB_FUNC( QT_QPIXMAP_MASK )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( ( p )->mask() ), true ) );
   }
}

/*
 * QRect rect () const
 */
HB_FUNC( QT_QPIXMAP_RECT )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->rect() ), true ) );
   }
}

/*
 * bool save ( ... )
 */
HB_FUNC( QT_QPIXMAP_SAVE )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      if( hb_pcount() >= 2 && HB_ISCHAR( 2 ) )
      {
         void * pText;
         hb_retl( ( p )->save( hb_parstr_utf8( 2, &pText, NULL ), hbqt_par_char( 3 ), hb_parnidef( 4, -1 ) ) );
         hb_strfree( pText );
      }
      else if( hb_pcount() >= 2 && HB_ISPOINTER( 2 ) )
      {
         hb_retl( ( p )->save( hbqt_par_QIODevice( 2 ), hbqt_par_char( 3 ), hb_parnidef( 4, -1 ) ) );
      }
   }
}

/*
 * QPixmap scaled ( ... )
 */
HB_FUNC( QT_QPIXMAP_SCALED )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      if( hb_pcount() >= 2 && HB_ISPOINTER( 2 ) )
      {
         hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->scaled( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::AspectRatioMode ) hb_parni( 4 ) : ( Qt::AspectRatioMode ) Qt::IgnoreAspectRatio ), ( HB_ISNUM( 5 ) ? ( Qt::TransformationMode ) hb_parni( 5 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
      }
      else if( hb_pcount() >= 3 && HB_ISNUM( 2 )  && HB_ISNUM( 3 ) )
      {
         hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->scaled( *hbqt_par_QSize( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::AspectRatioMode ) hb_parni( 3 ) : ( Qt::AspectRatioMode ) Qt::IgnoreAspectRatio ), ( HB_ISNUM( 4 ) ? ( Qt::TransformationMode ) hb_parni( 4 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
      }
   }
}

/*
 * QPixmap scaledToHeight ( int height, Qt::TransformationMode mode = Qt::FastTransformation ) const
 */
HB_FUNC( QT_QPIXMAP_SCALEDTOHEIGHT )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->scaledToHeight( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
   }
}

/*
 * QPixmap scaledToWidth ( int width, Qt::TransformationMode mode = Qt::FastTransformation ) const
 */
HB_FUNC( QT_QPIXMAP_SCALEDTOWIDTH )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->scaledToWidth( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
   }
}

/*
 * void setAlphaChannel ( const QPixmap & alphaChannel )
 */
HB_FUNC( QT_QPIXMAP_SETALPHACHANNEL )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      ( p )->setAlphaChannel( *hbqt_par_QPixmap( 2 ) );
   }
}

/*
 * void setMask ( const QBitmap & mask )
 */
HB_FUNC( QT_QPIXMAP_SETMASK )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      ( p )->setMask( *hbqt_par_QBitmap( 2 ) );
   }
}

/*
 * QSize size () const
 */
HB_FUNC( QT_QPIXMAP_SIZE )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->size() ), true ) );
   }
}

/*
 * QImage toImage () const
 */
HB_FUNC( QT_QPIXMAP_TOIMAGE )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->toImage() ), true ) );
   }
}

/*
 * QPixmap transformed ( ... )
 */
HB_FUNC( QT_QPIXMAP_TRANSFORMED )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 2 );

      if( q->type == HBQT_TYPE_QTransform )
      {
         hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->transformed( *hbqt_par_QTransform( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
      }
      else if( q->type == HBQT_TYPE_QMatrix )
      {
         hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->transformed( *hbqt_par_QMatrix( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::TransformationMode ) hb_parni( 3 ) : ( Qt::TransformationMode ) Qt::FastTransformation ) ) ), true ) );
      }
   }
}

/*
 * int width () const
 */
HB_FUNC( QT_QPIXMAP_WIDTH )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      hb_retni( ( p )->width() );
   }
}

/*
 * int defaultDepth ()
 */
HB_FUNC( QT_QPIXMAP_DEFAULTDEPTH )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      hb_retni( ( p )->defaultDepth() );
   }
}

/*
 * QPixmap fromImage ( const QImage & image, Qt::ImageConversionFlags flags = Qt::AutoColor )
 */
HB_FUNC( QT_QPIXMAP_FROMIMAGE )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->fromImage( *hbqt_par_QImage( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ImageConversionFlags ) hb_parni( 3 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ) ), true ) );
   }
}

/*
 * QPixmap grabWidget ( ... )
 */
HB_FUNC( QT_QPIXMAP_GRABWIDGET )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      if( hb_pcount() == 3 && HB_ISPOINTER( 2 ) && HB_ISPOINTER( 3 ) )
      {
         hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->grabWidget( hbqt_par_QWidget( 2 ), *hbqt_par_QRect( 3 ) ) ), true ) );
      }
      else if( hb_pcount() >= 2 && HB_ISPOINTER( 2 ) )
      {
         hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->grabWidget( hbqt_par_QWidget( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parnidef( 5, -1 ), hb_parnidef( 6, -1 ) ) ), true ) );
      }
   }
}

/*
 * QTransform trueMatrix ( const QTransform & matrix, int width, int height )
 */
HB_FUNC( QT_QPIXMAP_TRUEMATRIX )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->trueMatrix( *hbqt_par_QTransform( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) ), true ) );
   }
}

/*
 * QMatrix trueMatrix ( const QMatrix & m, int w, int h )
 */
HB_FUNC( QT_QPIXMAP_TRUEMATRIX_1 )
{
   QPixmap * p = hbqt_par_QPixmap( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->trueMatrix( *hbqt_par_QMatrix( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) ), true ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
