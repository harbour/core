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

#include <QtCore/QPointer>

#include <QtGui/QWheelEvent>


/*
 * QWheelEvent ( const QPoint & pos, int delta, Qt::MouseButtons buttons, Qt::KeyboardModifiers modifiers, Qt::Orientation orient = Qt::Vertical )
 * QWheelEvent ( const QPoint & pos, const QPoint & globalPos, int delta, Qt::MouseButtons buttons, Qt::KeyboardModifiers modifiers, Qt::Orientation orient = Qt::Vertical )
 */

typedef struct
{
   QWheelEvent * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QWheelEvent;

QT_G_FUNC( hbqt_gcRelease_QWheelEvent )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QWheelEvent( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QWheelEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWheelEvent;
   p->type = HBQT_TYPE_QWheelEvent;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QWheelEvent", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QWheelEvent", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QWHEELEVENT )
{
   // hb_retptr( ( QWheelEvent* ) new QWheelEvent() );
}

/*
 * Qt::MouseButtons buttons () const
 */
HB_FUNC( QT_QWHEELEVENT_BUTTONS )
{
   QWheelEvent * p = hbqt_par_QWheelEvent( 1 );
   if( p )
      hb_retni( ( Qt::MouseButtons ) ( p )->buttons() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWHEELEVENT_BUTTONS FP=hb_retni( ( Qt::MouseButtons ) ( p )->buttons() ); p is NULL" ) );
   }
}

/*
 * int delta () const
 */
HB_FUNC( QT_QWHEELEVENT_DELTA )
{
   QWheelEvent * p = hbqt_par_QWheelEvent( 1 );
   if( p )
      hb_retni( ( p )->delta() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWHEELEVENT_DELTA FP=hb_retni( ( p )->delta() ); p is NULL" ) );
   }
}

/*
 * const QPoint & globalPos () const
 */
HB_FUNC( QT_QWHEELEVENT_GLOBALPOS )
{
   QWheelEvent * p = hbqt_par_QWheelEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->globalPos() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWHEELEVENT_GLOBALPOS FP=hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->globalPos() ), true ) ); p is NULL" ) );
   }
}

/*
 * int globalX () const
 */
HB_FUNC( QT_QWHEELEVENT_GLOBALX )
{
   QWheelEvent * p = hbqt_par_QWheelEvent( 1 );
   if( p )
      hb_retni( ( p )->globalX() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWHEELEVENT_GLOBALX FP=hb_retni( ( p )->globalX() ); p is NULL" ) );
   }
}

/*
 * int globalY () const
 */
HB_FUNC( QT_QWHEELEVENT_GLOBALY )
{
   QWheelEvent * p = hbqt_par_QWheelEvent( 1 );
   if( p )
      hb_retni( ( p )->globalY() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWHEELEVENT_GLOBALY FP=hb_retni( ( p )->globalY() ); p is NULL" ) );
   }
}

/*
 * Qt::Orientation orientation () const
 */
HB_FUNC( QT_QWHEELEVENT_ORIENTATION )
{
   QWheelEvent * p = hbqt_par_QWheelEvent( 1 );
   if( p )
      hb_retni( ( Qt::Orientation ) ( p )->orientation() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWHEELEVENT_ORIENTATION FP=hb_retni( ( Qt::Orientation ) ( p )->orientation() ); p is NULL" ) );
   }
}

/*
 * const QPoint & pos () const
 */
HB_FUNC( QT_QWHEELEVENT_POS )
{
   QWheelEvent * p = hbqt_par_QWheelEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWHEELEVENT_POS FP=hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos() ), true ) ); p is NULL" ) );
   }
}

/*
 * int x () const
 */
HB_FUNC( QT_QWHEELEVENT_X )
{
   QWheelEvent * p = hbqt_par_QWheelEvent( 1 );
   if( p )
      hb_retni( ( p )->x() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWHEELEVENT_X FP=hb_retni( ( p )->x() ); p is NULL" ) );
   }
}

/*
 * int y () const
 */
HB_FUNC( QT_QWHEELEVENT_Y )
{
   QWheelEvent * p = hbqt_par_QWheelEvent( 1 );
   if( p )
      hb_retni( ( p )->y() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWHEELEVENT_Y FP=hb_retni( ( p )->y() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
