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
 *  enum Reason { Mouse, Keyboard, Other }
 */

#include <QtCore/QPointer>

#include <QtGui/QContextMenuEvent>


/* QContextMenuEvent ( Reason reason, const QPoint & pos, const QPoint & globalPos, Qt::KeyboardModifiers modifiers )
 * QContextMenuEvent ( Reason reason, const QPoint & pos, const QPoint & globalPos )
 * QContextMenuEvent ( Reason reason, const QPoint & pos )
 */

QT_G_FUNC( release_QContextMenuEvent )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QContextMenuEvent            p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "release_QContextMenuEvent           ph=%p", p->ph ) );

   if( p && p->ph )
   {
      ( ( QContextMenuEvent * ) p->ph )->~QContextMenuEvent();
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "release_QContextMenuEvent           Object deleted!" ) );
      #if defined(__debug__)
         just_debug( "  YES release_QContextMenuEvent           %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
      #endif
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "release_QContextMenuEvent           Object Allready deleted!" ) );
      #if defined(__debug__)
         just_debug( "  DEL release_QContextMenuEvent" );
      #endif
   }
}

void * gcAllocate_QContextMenuEvent( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QContextMenuEvent;
   #if defined(__debug__)
      just_debug( "          new_QContextMenuEvent           %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
   #endif
   return( p );
}

HB_FUNC( QT_QCONTEXTMENUEVENT )
{
   void * pObj = NULL;

   pObj = new QContextMenuEvent( ( QContextMenuEvent::Reason ) hb_parni( 1 ), *hbqt_par_QPoint( 2 ) ) ;

   hb_retptrGC( gcAllocate_QContextMenuEvent( pObj ) );
}
/*
 * const QPoint & globalPos () const
 */
HB_FUNC( QT_QCONTEXTMENUEVENT_GLOBALPOS )
{
   hb_retptrGC( gcAllocate_QPoint( new QPoint( hbqt_par_QContextMenuEvent( 1 )->globalPos() ) ) );
}

/*
 * int globalX () const
 */
HB_FUNC( QT_QCONTEXTMENUEVENT_GLOBALX )
{
   hb_retni( hbqt_par_QContextMenuEvent( 1 )->globalX() );
}

/*
 * int globalY () const
 */
HB_FUNC( QT_QCONTEXTMENUEVENT_GLOBALY )
{
   hb_retni( hbqt_par_QContextMenuEvent( 1 )->globalY() );
}

/*
 * const QPoint & pos () const
 */
HB_FUNC( QT_QCONTEXTMENUEVENT_POS )
{
   hb_retptrGC( gcAllocate_QPoint( new QPoint( hbqt_par_QContextMenuEvent( 1 )->pos() ) ) );
}

/*
 * Reason reason () const
 */
HB_FUNC( QT_QCONTEXTMENUEVENT_REASON )
{
   hb_retni( ( QContextMenuEvent::Reason ) hbqt_par_QContextMenuEvent( 1 )->reason() );
}

/*
 * int x () const
 */
HB_FUNC( QT_QCONTEXTMENUEVENT_X )
{
   hb_retni( hbqt_par_QContextMenuEvent( 1 )->x() );
}

/*
 * int y () const
 */
HB_FUNC( QT_QCONTEXTMENUEVENT_Y )
{
   hb_retni( hbqt_par_QContextMenuEvent( 1 )->y() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
