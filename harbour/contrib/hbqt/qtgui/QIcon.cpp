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
 *  enum Mode { Normal, Disabled, Active, Selected }
 *  enum State { Off, On }
 */

/*
 *  Constructed[ 10/11 [ 90.91% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QSize> availableSizes ( Mode mode = Normal, State state = Off ) const
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

QT_G_FUNC( hbqt_gcRelease_QIcon )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QIcon                        p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QIcon                       ph=%p", p->ph ) );

   if( p && p->ph )
   {
      delete ( ( QIcon * ) p->ph );
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "YES hbqt_gcRelease_QIcon                       Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL hbqt_gcRelease_QIcon                       Object Already deleted!" ) );
   }
}

void * hbqt_gcAllocate_QIcon( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = hbqt_gcRelease_QIcon;
   HB_TRACE( HB_TR_DEBUG, ( "          new_QIcon                       %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QICON )
{
   void * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISCHAR( 1 ) )
   {
      pObj = ( QIcon* ) new QIcon( hbqt_par_QString( 1 ) ) ;
   }
   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = ( QIcon* ) new QIcon( *hbqt_par_QPixmap( 1 ) ) ;
   }
   else
   {
      pObj = ( QIcon* ) new QIcon() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QIcon( pObj ) );
}
/*
 * QSize actualSize ( const QSize & size, Mode mode = Normal, State state = Off ) const
 */
HB_FUNC( QT_QICON_ACTUALSIZE )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QIcon( 1 )->actualSize( *hbqt_par_QSize( 2 ), ( HB_ISNUM( 3 ) ? ( QIcon::Mode ) hb_parni( 3 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 4 ) ? ( QIcon::State ) hb_parni( 4 ) : ( QIcon::State ) QIcon::Off ) ) ) ) );
}

/*
 * void addFile ( const QString & fileName, const QSize & size = QSize(), Mode mode = Normal, State state = Off )
 */
HB_FUNC( QT_QICON_ADDFILE )
{
   hbqt_par_QIcon( 1 )->addFile( hbqt_par_QString( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QSize( 3 ) : QSize() ), ( HB_ISNUM( 4 ) ? ( QIcon::Mode ) hb_parni( 4 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 5 ) ? ( QIcon::State ) hb_parni( 5 ) : ( QIcon::State ) QIcon::Off ) );
}

/*
 * void addPixmap ( const QPixmap & pixmap, Mode mode = Normal, State state = Off )
 */
HB_FUNC( QT_QICON_ADDPIXMAP )
{
   hbqt_par_QIcon( 1 )->addPixmap( *hbqt_par_QPixmap( 2 ), ( HB_ISNUM( 3 ) ? ( QIcon::Mode ) hb_parni( 3 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 4 ) ? ( QIcon::State ) hb_parni( 4 ) : ( QIcon::State ) QIcon::Off ) );
}

/*
 * qint64 cacheKey () const
 */
HB_FUNC( QT_QICON_CACHEKEY )
{
   hb_retnint( hbqt_par_QIcon( 1 )->cacheKey() );
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QICON_ISNULL )
{
   hb_retl( hbqt_par_QIcon( 1 )->isNull() );
}

/*
 * void paint ( QPainter * painter, const QRect & rect, Qt::Alignment alignment = Qt::AlignCenter, Mode mode = Normal, State state = Off ) const
 */
HB_FUNC( QT_QICON_PAINT )
{
   hbqt_par_QIcon( 1 )->paint( hbqt_par_QPainter( 2 ), *hbqt_par_QRect( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::Alignment ) hb_parni( 4 ) : ( Qt::Alignment ) Qt::AlignCenter ), ( HB_ISNUM( 5 ) ? ( QIcon::Mode ) hb_parni( 5 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 6 ) ? ( QIcon::State ) hb_parni( 6 ) : ( QIcon::State ) QIcon::Off ) );
}

/*
 * void paint ( QPainter * painter, int x, int y, int w, int h, Qt::Alignment alignment = Qt::AlignCenter, Mode mode = Normal, State state = Off ) const
 */
HB_FUNC( QT_QICON_PAINT_1 )
{
   hbqt_par_QIcon( 1 )->paint( hbqt_par_QPainter( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), ( HB_ISNUM( 7 ) ? ( Qt::Alignment ) hb_parni( 7 ) : ( Qt::Alignment ) Qt::AlignCenter ), ( HB_ISNUM( 8 ) ? ( QIcon::Mode ) hb_parni( 8 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 9 ) ? ( QIcon::State ) hb_parni( 9 ) : ( QIcon::State ) QIcon::Off ) );
}

/*
 * QPixmap pixmap ( const QSize & size, Mode mode = Normal, State state = Off ) const
 */
HB_FUNC( QT_QICON_PIXMAP )
{
   hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( hbqt_par_QIcon( 1 )->pixmap( *hbqt_par_QSize( 2 ), ( HB_ISNUM( 3 ) ? ( QIcon::Mode ) hb_parni( 3 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 4 ) ? ( QIcon::State ) hb_parni( 4 ) : ( QIcon::State ) QIcon::Off ) ) ) ) );
}

/*
 * QPixmap pixmap ( int w, int h, Mode mode = Normal, State state = Off ) const
 */
HB_FUNC( QT_QICON_PIXMAP_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( hbqt_par_QIcon( 1 )->pixmap( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( QIcon::Mode ) hb_parni( 4 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 5 ) ? ( QIcon::State ) hb_parni( 5 ) : ( QIcon::State ) QIcon::Off ) ) ) ) );
}

/*
 * QPixmap pixmap ( int extent, Mode mode = Normal, State state = Off ) const
 */
HB_FUNC( QT_QICON_PIXMAP_2 )
{
   hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( hbqt_par_QIcon( 1 )->pixmap( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QIcon::Mode ) hb_parni( 3 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 4 ) ? ( QIcon::State ) hb_parni( 4 ) : ( QIcon::State ) QIcon::Off ) ) ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
