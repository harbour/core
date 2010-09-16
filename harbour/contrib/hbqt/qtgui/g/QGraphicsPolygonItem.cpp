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

#include <QtCore/QPointer>

#include <QtGui/QGraphicsPolygonItem>


/*
 * QGraphicsPolygonItem ( QGraphicsItem * parent = 0 )
 * QGraphicsPolygonItem ( const QPolygonF & polygon, QGraphicsItem * parent = 0 )
 * ~QGraphicsPolygonItem ()
 */

typedef struct
{
   QGraphicsPolygonItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsPolygonItem;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsPolygonItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QGraphicsPolygonItem   /.\\", p->ph ) );
         delete ( ( QGraphicsPolygonItem * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QGraphicsPolygonItem   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QGraphicsPolygonItem    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QGraphicsPolygonItem    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QGraphicsPolygonItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsPolygonItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsPolygonItem;
   p->type = HBQT_TYPE_QGraphicsPolygonItem;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QGraphicsPolygonItem", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QGraphicsPolygonItem", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QGRAPHICSPOLYGONITEM )
{
   QGraphicsPolygonItem * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISPOINTER( 1 ) )
   {
      HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
      if( p->type == HBQT_TYPE_QPolygonF )
      {
         pObj = new QGraphicsPolygonItem( *hbqt_par_QPolygonF( 1 ), ( HB_ISPOINTER( 2 ) ? hbqt_par_QGraphicsItem( 2 ) : 0 ) ) ;
      }
      else
      {
         pObj = new QGraphicsPolygonItem( hbqt_par_QGraphicsItem( 1 ) ) ;
      }
   }
   else
   {
      pObj = new QGraphicsPolygonItem() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QGraphicsPolygonItem( ( void * ) pObj, true ) );
}

/*
 * Qt::FillRule fillRule () const
 */
HB_FUNC( QT_QGRAPHICSPOLYGONITEM_FILLRULE )
{
   QGraphicsPolygonItem * p = hbqt_par_QGraphicsPolygonItem( 1 );
   if( p )
   {
      hb_retni( ( Qt::FillRule ) ( p )->fillRule() );
   }
}

/*
 * QPolygonF polygon () const
 */
HB_FUNC( QT_QGRAPHICSPOLYGONITEM_POLYGON )
{
   QGraphicsPolygonItem * p = hbqt_par_QGraphicsPolygonItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->polygon() ), true ) );
   }
}

/*
 * void setFillRule ( Qt::FillRule rule )
 */
HB_FUNC( QT_QGRAPHICSPOLYGONITEM_SETFILLRULE )
{
   QGraphicsPolygonItem * p = hbqt_par_QGraphicsPolygonItem( 1 );
   if( p )
   {
      ( p )->setFillRule( ( Qt::FillRule ) hb_parni( 2 ) );
   }
}

/*
 * void setPolygon ( const QPolygonF & polygon )
 */
HB_FUNC( QT_QGRAPHICSPOLYGONITEM_SETPOLYGON )
{
   QGraphicsPolygonItem * p = hbqt_par_QGraphicsPolygonItem( 1 );
   if( p )
   {
      ( p )->setPolygon( *hbqt_par_QPolygonF( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
