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

#include <QtCore/QPointer>

#include <QtGui/QMouseEvent>


/*
 * QMouseEvent ( Type type, const QPoint & position, Qt::MouseButton button, Qt::MouseButtons buttons, Qt::KeyboardModifiers modifiers )
 * QMouseEvent ( Type type, const QPoint & pos, const QPoint & globalPos, Qt::MouseButton button, Qt::MouseButtons buttons, Qt::KeyboardModifiers modifiers )
 * ~QMouseEvent ()
 */

QT_G_FUNC( hbqt_gcRelease_QMouseEvent )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QMouseEvent                  p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QMouseEvent                 ph=%p", p->ph ) );

   if( p && p->ph )
   {
      delete ( ( QMouseEvent * ) p->ph );
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "YES hbqt_gcRelease_QMouseEvent                 Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL hbqt_gcRelease_QMouseEvent                 Object Already deleted!" ) );
   }
}

void * hbqt_gcAllocate_QMouseEvent( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = hbqt_gcRelease_QMouseEvent;
   HB_TRACE( HB_TR_DEBUG, ( "          new_QMouseEvent                 %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return p;
}

HB_FUNC( QT_QMOUSEEVENT )
{
   void * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QMouseEvent( *hbqt_par_QMouseEvent( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QMouseEvent( pObj ) );
}
/*
 * Qt::MouseButton button () const
 */
HB_FUNC( QT_QMOUSEEVENT_BUTTON )
{
   hb_retni( ( Qt::MouseButton ) hbqt_par_QMouseEvent( 1 )->button() );
}

/*
 * Qt::MouseButtons buttons () const
 */
HB_FUNC( QT_QMOUSEEVENT_BUTTONS )
{
   hb_retni( ( Qt::MouseButtons ) hbqt_par_QMouseEvent( 1 )->buttons() );
}

/*
 * const QPoint & globalPos () const
 */
HB_FUNC( QT_QMOUSEEVENT_GLOBALPOS )
{
   hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( hbqt_par_QMouseEvent( 1 )->globalPos() ) ) );
}

/*
 * int globalX () const
 */
HB_FUNC( QT_QMOUSEEVENT_GLOBALX )
{
   hb_retni( hbqt_par_QMouseEvent( 1 )->globalX() );
}

/*
 * int globalY () const
 */
HB_FUNC( QT_QMOUSEEVENT_GLOBALY )
{
   hb_retni( hbqt_par_QMouseEvent( 1 )->globalY() );
}

/*
 * const QPoint & pos () const
 */
HB_FUNC( QT_QMOUSEEVENT_POS )
{
   hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( hbqt_par_QMouseEvent( 1 )->pos() ) ) );
}

/*
 * QPointF posF () const
 */
HB_FUNC( QT_QMOUSEEVENT_POSF )
{
   hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( hbqt_par_QMouseEvent( 1 )->posF() ) ) );
}

/*
 * int x () const
 */
HB_FUNC( QT_QMOUSEEVENT_X )
{
   hb_retni( hbqt_par_QMouseEvent( 1 )->x() );
}

/*
 * int y () const
 */
HB_FUNC( QT_QMOUSEEVENT_Y )
{
   hb_retni( hbqt_par_QMouseEvent( 1 )->y() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
