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

#include <QtCore/QPointer>

#include <QtGui/QBrush>


/*
 * QBrush ()
 * QBrush ( Qt::BrushStyle style )
 * QBrush ( const QColor & color, Qt::BrushStyle style = Qt::SolidPattern )
 * QBrush ( Qt::GlobalColor color, Qt::BrushStyle style = Qt::SolidPattern )
 * QBrush ( const QColor & color, const QPixmap & pixmap )
 * QBrush ( Qt::GlobalColor color, const QPixmap & pixmap )
 * QBrush ( const QPixmap & pixmap )
 * QBrush ( const QImage & image )
 * QBrush ( const QBrush & other )
 * QBrush ( const QGradient & gradient )
 * ~QBrush ()
 */

typedef struct
{
   QBrush * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QBrush;

QT_G_FUNC( hbqt_gcRelease_QBrush )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QBrush   /.\\", p->ph ) );
         delete ( ( QBrush * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QBrush   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QBrush    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QBrush    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QBrush( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QBrush * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QBrush;
   p->type = HBQT_TYPE_QBrush;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QBrush", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QBrush", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QBRUSH )
{
   QBrush * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj =  new QBrush( *hbqt_par_QBrush( 1 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISNUM( 1 ) )
   {
      pObj =  new QBrush( ( Qt::GlobalColor ) hb_parni( 1 ), Qt::SolidPattern ) ;
   }
   else if( hb_pcount() == 2 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj =  new QBrush( ( Qt::GlobalColor ) hb_parni( 1 ), ( Qt::BrushStyle ) hb_parni( 2 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj =  new QBrush( *hbqt_par_QColor( 1 ), ( Qt::BrushStyle ) hb_parni( 2 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj =  new QBrush( *hbqt_par_QColor( 1 ), *hbqt_par_QPixmap( 2 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISNUM( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj =  new QBrush( ( Qt::GlobalColor ) hb_parni( 1 ), *hbqt_par_QPixmap( 2 ) ) ;
   }
   else if( hb_pcount() >= 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      QString objName = hbqt_par_QString( 1 );

      if(      objName == ( QString ) "QPixmap" )
      {
         pObj =  new QBrush( *hbqt_par_QPixmap( 2 ) ) ;
      }
      else if( objName == ( QString ) "QImage" )
      {
         pObj =  new QBrush( *hbqt_par_QImage( 2 ) ) ;
      }
      else if( objName == ( QString ) "QGradient" )
      {
         pObj =  new QBrush( *hbqt_par_QGradient( 2 ) ) ;
      }
      else if( objName == ( QString ) "QColor" )
      {
         pObj =  new QBrush( *hbqt_par_QColor( 2 ), HB_ISNUM( 3 ) ? ( Qt::BrushStyle ) hb_parni( 3 ) : Qt::SolidPattern ) ;
      }
      else
      {
         pObj =  new QBrush() ;
      }
   }
   else
   {
      pObj =  new QBrush() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QBrush( ( void * ) pObj, true ) );
}

/*
 * const QColor & color () const
 */
HB_FUNC( QT_QBRUSH_COLOR )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->color() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBRUSH_COLOR FP=hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->color() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool isOpaque () const
 */
HB_FUNC( QT_QBRUSH_ISOPAQUE )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      hb_retl( ( p )->isOpaque() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBRUSH_ISOPAQUE FP=hb_retl( ( p )->isOpaque() ); p is NULL" ) );
   }
}

/*
 * const QMatrix & matrix () const
 */
HB_FUNC( QT_QBRUSH_MATRIX )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->matrix() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBRUSH_MATRIX FP=hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->matrix() ), true ) ); p is NULL" ) );
   }
}

/*
 * void setColor ( const QColor & color )
 */
HB_FUNC( QT_QBRUSH_SETCOLOR )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      ( p )->setColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBRUSH_SETCOLOR FP=( p )->setColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setColor ( Qt::GlobalColor color )
 */
HB_FUNC( QT_QBRUSH_SETCOLOR_1 )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      ( p )->setColor( ( Qt::GlobalColor ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBRUSH_SETCOLOR_1 FP=( p )->setColor( ( Qt::GlobalColor ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMatrix ( const QMatrix & matrix )
 */
HB_FUNC( QT_QBRUSH_SETMATRIX )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      ( p )->setMatrix( *hbqt_par_QMatrix( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBRUSH_SETMATRIX FP=( p )->setMatrix( *hbqt_par_QMatrix( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStyle ( Qt::BrushStyle style )
 */
HB_FUNC( QT_QBRUSH_SETSTYLE )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      ( p )->setStyle( ( Qt::BrushStyle ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBRUSH_SETSTYLE FP=( p )->setStyle( ( Qt::BrushStyle ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTexture ( const QPixmap & pixmap )
 */
HB_FUNC( QT_QBRUSH_SETTEXTURE )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      ( p )->setTexture( *hbqt_par_QPixmap( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBRUSH_SETTEXTURE FP=( p )->setTexture( *hbqt_par_QPixmap( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTextureImage ( const QImage & image )
 */
HB_FUNC( QT_QBRUSH_SETTEXTUREIMAGE )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      ( p )->setTextureImage( *hbqt_par_QImage( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBRUSH_SETTEXTUREIMAGE FP=( p )->setTextureImage( *hbqt_par_QImage( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTransform ( const QTransform & matrix )
 */
HB_FUNC( QT_QBRUSH_SETTRANSFORM )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      ( p )->setTransform( *hbqt_par_QTransform( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBRUSH_SETTRANSFORM FP=( p )->setTransform( *hbqt_par_QTransform( 2 ) ); p is NULL" ) );
   }
}

/*
 * Qt::BrushStyle style () const
 */
HB_FUNC( QT_QBRUSH_STYLE )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      hb_retni( ( Qt::BrushStyle ) ( p )->style() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBRUSH_STYLE FP=hb_retni( ( Qt::BrushStyle ) ( p )->style() ); p is NULL" ) );
   }
}

/*
 * QPixmap texture () const
 */
HB_FUNC( QT_QBRUSH_TEXTURE )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->texture() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBRUSH_TEXTURE FP=hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->texture() ), true ) ); p is NULL" ) );
   }
}

/*
 * QImage textureImage () const
 */
HB_FUNC( QT_QBRUSH_TEXTUREIMAGE )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->textureImage() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBRUSH_TEXTUREIMAGE FP=hb_retptrGC( hbqt_gcAllocate_QImage( new QImage( ( p )->textureImage() ), true ) ); p is NULL" ) );
   }
}

/*
 * QTransform transform () const
 */
HB_FUNC( QT_QBRUSH_TRANSFORM )
{
   QBrush * p = hbqt_par_QBrush( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->transform() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBRUSH_TRANSFORM FP=hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->transform() ), true ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
