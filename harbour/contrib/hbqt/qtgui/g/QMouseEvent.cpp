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

#include <QtGui/QMouseEvent>


/*
 * QMouseEvent ( Type type, const QPoint & position, Qt::MouseButton button, Qt::MouseButtons buttons, Qt::KeyboardModifiers modifiers )
 * QMouseEvent ( Type type, const QPoint & pos, const QPoint & globalPos, Qt::MouseButton button, Qt::MouseButtons buttons, Qt::KeyboardModifiers modifiers )
 * ~QMouseEvent ()
 */

typedef struct
{
   QMouseEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMouseEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QMouseEvent )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QMouseEvent   /.\\", p->ph ) );
         delete ( ( QMouseEvent * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QMouseEvent   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QMouseEvent    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QMouseEvent    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QMouseEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QMouseEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMouseEvent;
   p->type = HBQT_TYPE_QMouseEvent;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QMouseEvent", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QMouseEvent", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QMOUSEEVENT )
{
   QMouseEvent * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QMouseEvent( *hbqt_par_QMouseEvent( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QMouseEvent( ( void * ) pObj, true ) );
}

/*
 * Qt::MouseButton button () const
 */
HB_FUNC( QT_QMOUSEEVENT_BUTTON )
{
   QMouseEvent * p = hbqt_par_QMouseEvent( 1 );
   if( p )
      hb_retni( ( Qt::MouseButton ) ( p )->button() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMOUSEEVENT_BUTTON FP=hb_retni( ( Qt::MouseButton ) ( p )->button() ); p is NULL" ) );
   }
}

/*
 * Qt::MouseButtons buttons () const
 */
HB_FUNC( QT_QMOUSEEVENT_BUTTONS )
{
   QMouseEvent * p = hbqt_par_QMouseEvent( 1 );
   if( p )
      hb_retni( ( Qt::MouseButtons ) ( p )->buttons() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMOUSEEVENT_BUTTONS FP=hb_retni( ( Qt::MouseButtons ) ( p )->buttons() ); p is NULL" ) );
   }
}

/*
 * const QPoint & globalPos () const
 */
HB_FUNC( QT_QMOUSEEVENT_GLOBALPOS )
{
   QMouseEvent * p = hbqt_par_QMouseEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->globalPos() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMOUSEEVENT_GLOBALPOS FP=hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->globalPos() ), true ) ); p is NULL" ) );
   }
}

/*
 * int globalX () const
 */
HB_FUNC( QT_QMOUSEEVENT_GLOBALX )
{
   QMouseEvent * p = hbqt_par_QMouseEvent( 1 );
   if( p )
      hb_retni( ( p )->globalX() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMOUSEEVENT_GLOBALX FP=hb_retni( ( p )->globalX() ); p is NULL" ) );
   }
}

/*
 * int globalY () const
 */
HB_FUNC( QT_QMOUSEEVENT_GLOBALY )
{
   QMouseEvent * p = hbqt_par_QMouseEvent( 1 );
   if( p )
      hb_retni( ( p )->globalY() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMOUSEEVENT_GLOBALY FP=hb_retni( ( p )->globalY() ); p is NULL" ) );
   }
}

/*
 * const QPoint & pos () const
 */
HB_FUNC( QT_QMOUSEEVENT_POS )
{
   QMouseEvent * p = hbqt_par_QMouseEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMOUSEEVENT_POS FP=hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos() ), true ) ); p is NULL" ) );
   }
}

/*
 * QPointF posF () const
 */
HB_FUNC( QT_QMOUSEEVENT_POSF )
{
   QMouseEvent * p = hbqt_par_QMouseEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->posF() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMOUSEEVENT_POSF FP=hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->posF() ), true ) ); p is NULL" ) );
   }
}

/*
 * int x () const
 */
HB_FUNC( QT_QMOUSEEVENT_X )
{
   QMouseEvent * p = hbqt_par_QMouseEvent( 1 );
   if( p )
      hb_retni( ( p )->x() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMOUSEEVENT_X FP=hb_retni( ( p )->x() ); p is NULL" ) );
   }
}

/*
 * int y () const
 */
HB_FUNC( QT_QMOUSEEVENT_Y )
{
   QMouseEvent * p = hbqt_par_QMouseEvent( 1 );
   if( p )
      hb_retni( ( p )->y() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMOUSEEVENT_Y FP=hb_retni( ( p )->y() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
