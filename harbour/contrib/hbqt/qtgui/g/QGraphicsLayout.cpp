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

#include <QtGui/QGraphicsLayout>


/*
 * QGraphicsLayout ( QGraphicsLayoutItem * parent = 0 )
 * ~QGraphicsLayout ()
 */

typedef struct
{
   QGraphicsLayout * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsLayout;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsLayout )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QGraphicsLayout( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsLayout * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsLayout;
   p->type = HBQT_TYPE_QGraphicsLayout;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QGraphicsLayout", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QGraphicsLayout", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QGRAPHICSLAYOUT )
{
   // hb_retptr( new QGraphicsLayout() );
}

/*
 * void activate ()
 */
HB_FUNC( QT_QGRAPHICSLAYOUT_ACTIVATE )
{
   QGraphicsLayout * p = hbqt_par_QGraphicsLayout( 1 );
   if( p )
   {
      ( p )->activate();
   }
}

/*
 * virtual int count () const = 0
 */
HB_FUNC( QT_QGRAPHICSLAYOUT_COUNT )
{
   QGraphicsLayout * p = hbqt_par_QGraphicsLayout( 1 );
   if( p )
   {
      hb_retni( ( p )->count() );
   }
}

/*
 * virtual void invalidate ()
 */
HB_FUNC( QT_QGRAPHICSLAYOUT_INVALIDATE )
{
   QGraphicsLayout * p = hbqt_par_QGraphicsLayout( 1 );
   if( p )
   {
      ( p )->invalidate();
   }
}

/*
 * bool isActivated () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUT_ISACTIVATED )
{
   QGraphicsLayout * p = hbqt_par_QGraphicsLayout( 1 );
   if( p )
   {
      hb_retl( ( p )->isActivated() );
   }
}

/*
 * virtual QGraphicsLayoutItem * itemAt ( int i ) const = 0
 */
HB_FUNC( QT_QGRAPHICSLAYOUT_ITEMAT )
{
   QGraphicsLayout * p = hbqt_par_QGraphicsLayout( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QGraphicsLayoutItem( ( p )->itemAt( hb_parni( 2 ) ), false ) );
   }
}

/*
 * virtual void removeAt ( int index ) = 0
 */
HB_FUNC( QT_QGRAPHICSLAYOUT_REMOVEAT )
{
   QGraphicsLayout * p = hbqt_par_QGraphicsLayout( 1 );
   if( p )
   {
      ( p )->removeAt( hb_parni( 2 ) );
   }
}

/*
 * void setContentsMargins ( qreal left, qreal top, qreal right, qreal bottom )
 */
HB_FUNC( QT_QGRAPHICSLAYOUT_SETCONTENTSMARGINS )
{
   QGraphicsLayout * p = hbqt_par_QGraphicsLayout( 1 );
   if( p )
   {
      ( p )->setContentsMargins( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
   }
}

/*
 * virtual void widgetEvent ( QEvent * e )
 */
HB_FUNC( QT_QGRAPHICSLAYOUT_WIDGETEVENT )
{
   QGraphicsLayout * p = hbqt_par_QGraphicsLayout( 1 );
   if( p )
   {
      ( p )->widgetEvent( hbqt_par_QEvent( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
