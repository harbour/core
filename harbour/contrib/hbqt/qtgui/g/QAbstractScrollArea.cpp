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
 *  Constructed[ 14/14 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // QWidgetList scrollBarWidgets ( Qt::Alignment alignment )
 */

#include <QtCore/QPointer>

#include <QtGui/QAbstractScrollArea>


/*
 * QAbstractScrollArea ( QWidget * parent = 0 )
 * ~QAbstractScrollArea ()
 */

typedef struct
{
   QPointer< QAbstractScrollArea > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QAbstractScrollArea;

HBQT_GC_FUNC( hbqt_gcRelease_QAbstractScrollArea )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QAbstractScrollArea( void * pObj, bool bNew )
{
   HBQT_GC_T_QAbstractScrollArea * p = ( HBQT_GC_T_QAbstractScrollArea * ) hb_gcAllocate( sizeof( HBQT_GC_T_QAbstractScrollArea ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QAbstractScrollArea >( ( QAbstractScrollArea * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QAbstractScrollArea;
   p->type = HBQT_TYPE_QAbstractScrollArea;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QAbstractScrollArea  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QAbstractScrollArea", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QABSTRACTSCROLLAREA )
{

}

/*
 * void addScrollBarWidget ( QWidget * widget, Qt::Alignment alignment )
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_ADDSCROLLBARWIDGET )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
   {
      ( p )->addScrollBarWidget( hbqt_par_QWidget( 2 ), ( Qt::Alignment ) hb_parni( 3 ) );
   }
}

/*
 * QWidget * cornerWidget () const
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_CORNERWIDGET )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->cornerWidget(), false ) );
   }
}

/*
 * QScrollBar * horizontalScrollBar () const
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_HORIZONTALSCROLLBAR )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QScrollBar( ( p )->horizontalScrollBar(), false ) );
   }
}

/*
 * Qt::ScrollBarPolicy horizontalScrollBarPolicy () const
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_HORIZONTALSCROLLBARPOLICY )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
   {
      hb_retni( ( Qt::ScrollBarPolicy ) ( p )->horizontalScrollBarPolicy() );
   }
}

/*
 * QSize maximumViewportSize () const
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_MAXIMUMVIEWPORTSIZE )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->maximumViewportSize() ), true ) );
   }
}

/*
 * void setCornerWidget ( QWidget * widget )
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_SETCORNERWIDGET )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
   {
      ( p )->setCornerWidget( hbqt_par_QWidget( 2 ) );
   }
}

/*
 * void setHorizontalScrollBar ( QScrollBar * scrollBar )
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_SETHORIZONTALSCROLLBAR )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
   {
      ( p )->setHorizontalScrollBar( hbqt_par_QScrollBar( 2 ) );
   }
}

/*
 * void setHorizontalScrollBarPolicy ( Qt::ScrollBarPolicy )
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_SETHORIZONTALSCROLLBARPOLICY )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
   {
      ( p )->setHorizontalScrollBarPolicy( ( Qt::ScrollBarPolicy ) hb_parni( 2 ) );
   }
}

/*
 * void setVerticalScrollBar ( QScrollBar * scrollBar )
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_SETVERTICALSCROLLBAR )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
   {
      ( p )->setVerticalScrollBar( hbqt_par_QScrollBar( 2 ) );
   }
}

/*
 * void setVerticalScrollBarPolicy ( Qt::ScrollBarPolicy )
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_SETVERTICALSCROLLBARPOLICY )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
   {
      ( p )->setVerticalScrollBarPolicy( ( Qt::ScrollBarPolicy ) hb_parni( 2 ) );
   }
}

/*
 * void setViewport ( QWidget * widget )
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_SETVIEWPORT )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
   {
      ( p )->setViewport( hbqt_par_QWidget( 2 ) );
   }
}

/*
 * QScrollBar * verticalScrollBar () const
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_VERTICALSCROLLBAR )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QScrollBar( ( p )->verticalScrollBar(), false ) );
   }
}

/*
 * Qt::ScrollBarPolicy verticalScrollBarPolicy () const
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_VERTICALSCROLLBARPOLICY )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
   {
      hb_retni( ( Qt::ScrollBarPolicy ) ( p )->verticalScrollBarPolicy() );
   }
}

/*
 * QWidget * viewport () const
 */
HB_FUNC( QT_QABSTRACTSCROLLAREA_VIEWPORT )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->viewport(), false ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
