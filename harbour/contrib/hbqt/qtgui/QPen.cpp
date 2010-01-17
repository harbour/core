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
 *  Constructed[ 21/23 [ 91.30% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QVector<qreal> dashPattern () const
 *  void setDashPattern ( const QVector<qreal> & pattern )
 */

#include <QtCore/QPointer>

#include <QtGui/QPen>


/*
 * QPen ()
 * QPen ( Qt::PenStyle style )
 * QPen ( const QColor & color )
 * QPen ( const QBrush & brush, qreal width, Qt::PenStyle style = Qt::SolidLine, Qt::PenCapStyle cap = Qt::SquareCap, Qt::PenJoinStyle join = Qt::BevelJoin )
 * QPen ( const QPen & pen )
 * ~QPen ()
 */

typedef struct
{
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
} QGC_POINTER_QPen;

QT_G_FUNC( hbqt_gcRelease_QPen )
{
      QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QPen * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QPen                       ph=%p %i B %i KB", p->ph, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QPen                        Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QPen                        Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPen( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPen;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QPen                       ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QPEN )
{
   void * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISNUM( 1 ) )
   {
      pObj = ( QPen* ) new QPen( ( Qt::PenStyle ) hb_parni( 1 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = ( QPen* ) new QPen( *hbqt_par_QPen( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      QString objName = ( QString ) hbqt_par_QString( 1 );

      if( objName == ( QString ) "QColor" )
      {
         pObj = ( QPen* ) new QPen( *hbqt_par_QColor( 2 ) ) ;
      }
      else
      {
         pObj = ( QPen* ) new QPen() ;
      }
   }
   else if( hb_pcount() >= 2 && HB_ISPOINTER( 1 ) && HB_ISNUM( 2 ) )
   {
      Qt::PenStyle iStyle = HB_ISNUM( 3 ) ? ( Qt::PenStyle ) hb_parni( 3 ) : Qt::SolidLine;
      Qt::PenCapStyle iCap = HB_ISNUM( 4 ) ? ( Qt::PenCapStyle ) hb_parni( 4 ) : Qt::SquareCap;
      Qt::PenJoinStyle iJoin = HB_ISNUM( 5 ) ? ( Qt::PenJoinStyle ) hb_parni( 5 ) : Qt::BevelJoin;

      pObj = ( QPen* ) new QPen( *hbqt_par_QBrush( 1 ), hb_parnd( 2 ), iStyle, iCap, iJoin ) ;
   }
   else
   {
      pObj = ( QPen* ) new QPen() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QPen( pObj, true ) );
}
/*
 * QBrush brush () const
 */
HB_FUNC( QT_QPEN_BRUSH )
{
   hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( hbqt_par_QPen( 1 )->brush() ), true ) );
}

/*
 * Qt::PenCapStyle capStyle () const
 */
HB_FUNC( QT_QPEN_CAPSTYLE )
{
   hb_retni( ( Qt::PenCapStyle ) hbqt_par_QPen( 1 )->capStyle() );
}

/*
 * QColor color () const
 */
HB_FUNC( QT_QPEN_COLOR )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( hbqt_par_QPen( 1 )->color() ), true ) );
}

/*
 * qreal dashOffset () const
 */
HB_FUNC( QT_QPEN_DASHOFFSET )
{
   hb_retnd( hbqt_par_QPen( 1 )->dashOffset() );
}

/*
 * bool isCosmetic () const
 */
HB_FUNC( QT_QPEN_ISCOSMETIC )
{
   hb_retl( hbqt_par_QPen( 1 )->isCosmetic() );
}

/*
 * bool isSolid () const
 */
HB_FUNC( QT_QPEN_ISSOLID )
{
   hb_retl( hbqt_par_QPen( 1 )->isSolid() );
}

/*
 * Qt::PenJoinStyle joinStyle () const
 */
HB_FUNC( QT_QPEN_JOINSTYLE )
{
   hb_retni( ( Qt::PenJoinStyle ) hbqt_par_QPen( 1 )->joinStyle() );
}

/*
 * qreal miterLimit () const
 */
HB_FUNC( QT_QPEN_MITERLIMIT )
{
   hb_retnd( hbqt_par_QPen( 1 )->miterLimit() );
}

/*
 * void setBrush ( const QBrush & brush )
 */
HB_FUNC( QT_QPEN_SETBRUSH )
{
   hbqt_par_QPen( 1 )->setBrush( *hbqt_par_QBrush( 2 ) );
}

/*
 * void setCapStyle ( Qt::PenCapStyle style )
 */
HB_FUNC( QT_QPEN_SETCAPSTYLE )
{
   hbqt_par_QPen( 1 )->setCapStyle( ( Qt::PenCapStyle ) hb_parni( 2 ) );
}

/*
 * void setColor ( const QColor & color )
 */
HB_FUNC( QT_QPEN_SETCOLOR )
{
   hbqt_par_QPen( 1 )->setColor( *hbqt_par_QColor( 2 ) );
}

/*
 * void setCosmetic ( bool cosmetic )
 */
HB_FUNC( QT_QPEN_SETCOSMETIC )
{
   hbqt_par_QPen( 1 )->setCosmetic( hb_parl( 2 ) );
}

/*
 * void setDashOffset ( qreal offset )
 */
HB_FUNC( QT_QPEN_SETDASHOFFSET )
{
   hbqt_par_QPen( 1 )->setDashOffset( hb_parnd( 2 ) );
}

/*
 * void setJoinStyle ( Qt::PenJoinStyle style )
 */
HB_FUNC( QT_QPEN_SETJOINSTYLE )
{
   hbqt_par_QPen( 1 )->setJoinStyle( ( Qt::PenJoinStyle ) hb_parni( 2 ) );
}

/*
 * void setMiterLimit ( qreal limit )
 */
HB_FUNC( QT_QPEN_SETMITERLIMIT )
{
   hbqt_par_QPen( 1 )->setMiterLimit( hb_parnd( 2 ) );
}

/*
 * void setStyle ( Qt::PenStyle style )
 */
HB_FUNC( QT_QPEN_SETSTYLE )
{
   hbqt_par_QPen( 1 )->setStyle( ( Qt::PenStyle ) hb_parni( 2 ) );
}

/*
 * void setWidth ( int width )
 */
HB_FUNC( QT_QPEN_SETWIDTH )
{
   hbqt_par_QPen( 1 )->setWidth( hb_parni( 2 ) );
}

/*
 * void setWidthF ( qreal width )
 */
HB_FUNC( QT_QPEN_SETWIDTHF )
{
   hbqt_par_QPen( 1 )->setWidthF( hb_parnd( 2 ) );
}

/*
 * Qt::PenStyle style () const
 */
HB_FUNC( QT_QPEN_STYLE )
{
   hb_retni( ( Qt::PenStyle ) hbqt_par_QPen( 1 )->style() );
}

/*
 * int width () const
 */
HB_FUNC( QT_QPEN_WIDTH )
{
   hb_retni( hbqt_par_QPen( 1 )->width() );
}

/*
 * qreal widthF () const
 */
HB_FUNC( QT_QPEN_WIDTHF )
{
   hb_retnd( hbqt_par_QPen( 1 )->widthF() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
