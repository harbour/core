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
 *  enum Mode { Normal, Disabled, Active, Selected }
 *  enum State { Off, On }
 */

#include <QtCore/QPointer>

#include <QtGui/QIcon>


/*
 * QIcon ()
 * QIcon ( const QPixmap & pixmap )
 * QIcon ( const QIcon & other )
 * QIcon ( const QString & fileName )
 * QIcon ( QIconEngine * engine )
 * QIcon ( QIconEngineV2 * engine )
 * ~QIcon ()
 */

typedef struct
{
   QIcon * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QIcon;

QT_G_FUNC( hbqt_gcRelease_QIcon )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QIcon   /.\\", p->ph ) );
         delete ( ( QIcon * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QIcon   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QIcon    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QIcon    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QIcon( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QIcon * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QIcon;
   p->type = HBQT_TYPE_QIcon;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QIcon", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QIcon", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QICON )
{
   QIcon * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISCHAR( 1 ) )
   {
      pObj =  new QIcon( hbqt_par_QString( 1 ) ) ;
   }
   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj =  new QIcon( *hbqt_par_QPixmap( 1 ) ) ;
   }
   else
   {
      pObj =  new QIcon() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QIcon( ( void * ) pObj, true ) );
}

/*
 * QSize actualSize ( const QSize & size, Mode mode = Normal, State state = Off ) const
 */
HB_FUNC( QT_QICON_ACTUALSIZE )
{
   QIcon * p = hbqt_par_QIcon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->actualSize( *hbqt_par_QSize( 2 ), ( HB_ISNUM( 3 ) ? ( QIcon::Mode ) hb_parni( 3 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 4 ) ? ( QIcon::State ) hb_parni( 4 ) : ( QIcon::State ) QIcon::Off ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QICON_ACTUALSIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->actualSize( *hbqt_par_QSize( 2 ), ( HB_ISNUM( 3 ) ? ( QIcon::Mode ) hb_parni( 3 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 4 ) ? ( QIcon::State ) hb_parni( 4 ) : ( QIcon::State ) QIcon::Off ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void addFile ( const QString & fileName, const QSize & size = QSize(), Mode mode = Normal, State state = Off )
 */
HB_FUNC( QT_QICON_ADDFILE )
{
   QIcon * p = hbqt_par_QIcon( 1 );
   if( p )
      ( p )->addFile( hbqt_par_QString( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QSize( 3 ) : QSize() ), ( HB_ISNUM( 4 ) ? ( QIcon::Mode ) hb_parni( 4 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 5 ) ? ( QIcon::State ) hb_parni( 5 ) : ( QIcon::State ) QIcon::Off ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QICON_ADDFILE FP=( p )->addFile( hbqt_par_QString( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QSize( 3 ) : QSize() ), ( HB_ISNUM( 4 ) ? ( QIcon::Mode ) hb_parni( 4 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 5 ) ? ( QIcon::State ) hb_parni( 5 ) : ( QIcon::State ) QIcon::Off ) ); p is NULL" ) );
   }
}

/*
 * void addPixmap ( const QPixmap & pixmap, Mode mode = Normal, State state = Off )
 */
HB_FUNC( QT_QICON_ADDPIXMAP )
{
   QIcon * p = hbqt_par_QIcon( 1 );
   if( p )
      ( p )->addPixmap( *hbqt_par_QPixmap( 2 ), ( HB_ISNUM( 3 ) ? ( QIcon::Mode ) hb_parni( 3 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 4 ) ? ( QIcon::State ) hb_parni( 4 ) : ( QIcon::State ) QIcon::Off ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QICON_ADDPIXMAP FP=( p )->addPixmap( *hbqt_par_QPixmap( 2 ), ( HB_ISNUM( 3 ) ? ( QIcon::Mode ) hb_parni( 3 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 4 ) ? ( QIcon::State ) hb_parni( 4 ) : ( QIcon::State ) QIcon::Off ) ); p is NULL" ) );
   }
}

/*
 * QList<QSize> availableSizes ( Mode mode = Normal, State state = Off ) const
 */
HB_FUNC( QT_QICON_AVAILABLESIZES )
{
   QIcon * p = hbqt_par_QIcon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QSize>( ( p )->availableSizes( ( HB_ISNUM( 2 ) ? ( QIcon::Mode ) hb_parni( 2 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 3 ) ? ( QIcon::State ) hb_parni( 3 ) : ( QIcon::State ) QIcon::Off ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QICON_AVAILABLESIZES FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QSize>( ( p )->availableSizes( ( HB_ISNUM( 2 ) ? ( QIcon::Mode ) hb_parni( 2 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 3 ) ? ( QIcon::State ) hb_parni( 3 ) : ( QIcon::State ) QIcon::Off ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * qint64 cacheKey () const
 */
HB_FUNC( QT_QICON_CACHEKEY )
{
   QIcon * p = hbqt_par_QIcon( 1 );
   if( p )
      hb_retnint( ( p )->cacheKey() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QICON_CACHEKEY FP=hb_retnint( ( p )->cacheKey() ); p is NULL" ) );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QICON_ISNULL )
{
   QIcon * p = hbqt_par_QIcon( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QICON_ISNULL FP=hb_retl( ( p )->isNull() ); p is NULL" ) );
   }
}

/*
 * void paint ( QPainter * painter, const QRect & rect, Qt::Alignment alignment = Qt::AlignCenter, Mode mode = Normal, State state = Off ) const
 */
HB_FUNC( QT_QICON_PAINT )
{
   QIcon * p = hbqt_par_QIcon( 1 );
   if( p )
      ( p )->paint( hbqt_par_QPainter( 2 ), *hbqt_par_QRect( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::Alignment ) hb_parni( 4 ) : ( Qt::Alignment ) Qt::AlignCenter ), ( HB_ISNUM( 5 ) ? ( QIcon::Mode ) hb_parni( 5 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 6 ) ? ( QIcon::State ) hb_parni( 6 ) : ( QIcon::State ) QIcon::Off ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QICON_PAINT FP=( p )->paint( hbqt_par_QPainter( 2 ), *hbqt_par_QRect( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::Alignment ) hb_parni( 4 ) : ( Qt::Alignment ) Qt::AlignCenter ), ( HB_ISNUM( 5 ) ? ( QIcon::Mode ) hb_parni( 5 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 6 ) ? ( QIcon::State ) hb_parni( 6 ) : ( QIcon::State ) QIcon::Off ) ); p is NULL" ) );
   }
}

/*
 * void paint ( QPainter * painter, int x, int y, int w, int h, Qt::Alignment alignment = Qt::AlignCenter, Mode mode = Normal, State state = Off ) const
 */
HB_FUNC( QT_QICON_PAINT_1 )
{
   QIcon * p = hbqt_par_QIcon( 1 );
   if( p )
      ( p )->paint( hbqt_par_QPainter( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), ( HB_ISNUM( 7 ) ? ( Qt::Alignment ) hb_parni( 7 ) : ( Qt::Alignment ) Qt::AlignCenter ), ( HB_ISNUM( 8 ) ? ( QIcon::Mode ) hb_parni( 8 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 9 ) ? ( QIcon::State ) hb_parni( 9 ) : ( QIcon::State ) QIcon::Off ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QICON_PAINT_1 FP=( p )->paint( hbqt_par_QPainter( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), ( HB_ISNUM( 7 ) ? ( Qt::Alignment ) hb_parni( 7 ) : ( Qt::Alignment ) Qt::AlignCenter ), ( HB_ISNUM( 8 ) ? ( QIcon::Mode ) hb_parni( 8 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 9 ) ? ( QIcon::State ) hb_parni( 9 ) : ( QIcon::State ) QIcon::Off ) ); p is NULL" ) );
   }
}

/*
 * QPixmap pixmap ( const QSize & size, Mode mode = Normal, State state = Off ) const
 */
HB_FUNC( QT_QICON_PIXMAP )
{
   QIcon * p = hbqt_par_QIcon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap( *hbqt_par_QSize( 2 ), ( HB_ISNUM( 3 ) ? ( QIcon::Mode ) hb_parni( 3 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 4 ) ? ( QIcon::State ) hb_parni( 4 ) : ( QIcon::State ) QIcon::Off ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QICON_PIXMAP FP=hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap( *hbqt_par_QSize( 2 ), ( HB_ISNUM( 3 ) ? ( QIcon::Mode ) hb_parni( 3 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 4 ) ? ( QIcon::State ) hb_parni( 4 ) : ( QIcon::State ) QIcon::Off ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPixmap pixmap ( int w, int h, Mode mode = Normal, State state = Off ) const
 */
HB_FUNC( QT_QICON_PIXMAP_1 )
{
   QIcon * p = hbqt_par_QIcon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( QIcon::Mode ) hb_parni( 4 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 5 ) ? ( QIcon::State ) hb_parni( 5 ) : ( QIcon::State ) QIcon::Off ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QICON_PIXMAP_1 FP=hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( QIcon::Mode ) hb_parni( 4 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 5 ) ? ( QIcon::State ) hb_parni( 5 ) : ( QIcon::State ) QIcon::Off ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPixmap pixmap ( int extent, Mode mode = Normal, State state = Off ) const
 */
HB_FUNC( QT_QICON_PIXMAP_2 )
{
   QIcon * p = hbqt_par_QIcon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QIcon::Mode ) hb_parni( 3 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 4 ) ? ( QIcon::State ) hb_parni( 4 ) : ( QIcon::State ) QIcon::Off ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QICON_PIXMAP_2 FP=hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QIcon::Mode ) hb_parni( 3 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 4 ) ? ( QIcon::State ) hb_parni( 4 ) : ( QIcon::State ) QIcon::Off ) ) ), true ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
