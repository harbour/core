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
#include "hbqtcore_garbage.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 10/11 [ 90.91% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  }
 */

#include <QtCore/QPointer>

#include <QtGui/QDropEvent>


/*
 * QDropEvent ( const QPoint & pos, Qt::DropActions actions, const QMimeData * data, Qt::MouseButtons buttons, Qt::KeyboardModifiers modifiers, Type type = Drop )
 */

typedef struct
{
   QDropEvent * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QDropEvent;

QT_G_FUNC( hbqt_gcRelease_QDropEvent )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QDropEvent( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QDropEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDropEvent;
   p->type = HBQT_TYPE_QDropEvent;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QDropEvent", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QDropEvent", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QDROPEVENT )
{
   // hb_retptr( ( QDropEvent* ) new QDropEvent() );
}

/*
 * void acceptProposedAction ()
 */
HB_FUNC( QT_QDROPEVENT_ACCEPTPROPOSEDACTION )
{
   QDropEvent * p = hbqt_par_QDropEvent( 1 );
   if( p )
      ( p )->acceptProposedAction();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDROPEVENT_ACCEPTPROPOSEDACTION FP=( p )->acceptProposedAction(); p is NULL" ) );
   }
}

/*
 * Qt::DropAction dropAction () const
 */
HB_FUNC( QT_QDROPEVENT_DROPACTION )
{
   QDropEvent * p = hbqt_par_QDropEvent( 1 );
   if( p )
      hb_retni( ( Qt::DropAction ) ( p )->dropAction() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDROPEVENT_DROPACTION FP=hb_retni( ( Qt::DropAction ) ( p )->dropAction() ); p is NULL" ) );
   }
}

/*
 * Qt::KeyboardModifiers keyboardModifiers () const
 */
HB_FUNC( QT_QDROPEVENT_KEYBOARDMODIFIERS )
{
   QDropEvent * p = hbqt_par_QDropEvent( 1 );
   if( p )
      hb_retni( ( Qt::KeyboardModifiers ) ( p )->keyboardModifiers() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDROPEVENT_KEYBOARDMODIFIERS FP=hb_retni( ( Qt::KeyboardModifiers ) ( p )->keyboardModifiers() ); p is NULL" ) );
   }
}

/*
 * const QMimeData * mimeData () const
 */
HB_FUNC( QT_QDROPEVENT_MIMEDATA )
{
   hb_retptrGC( hbqt_gcAllocate_QMimeData( ( void* ) hbqt_par_QDropEvent( 1 )->mimeData(), false ) );
}

/*
 * Qt::MouseButtons mouseButtons () const
 */
HB_FUNC( QT_QDROPEVENT_MOUSEBUTTONS )
{
   QDropEvent * p = hbqt_par_QDropEvent( 1 );
   if( p )
      hb_retni( ( Qt::MouseButtons ) ( p )->mouseButtons() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDROPEVENT_MOUSEBUTTONS FP=hb_retni( ( Qt::MouseButtons ) ( p )->mouseButtons() ); p is NULL" ) );
   }
}

/*
 * const QPoint & pos () const
 */
HB_FUNC( QT_QDROPEVENT_POS )
{
   QDropEvent * p = hbqt_par_QDropEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDROPEVENT_POS FP=hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos() ), true ) ); p is NULL" ) );
   }
}

/*
 * Qt::DropActions possibleActions () const
 */
HB_FUNC( QT_QDROPEVENT_POSSIBLEACTIONS )
{
   QDropEvent * p = hbqt_par_QDropEvent( 1 );
   if( p )
      hb_retni( ( Qt::DropActions ) ( p )->possibleActions() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDROPEVENT_POSSIBLEACTIONS FP=hb_retni( ( Qt::DropActions ) ( p )->possibleActions() ); p is NULL" ) );
   }
}

/*
 * Qt::DropAction proposedAction () const
 */
HB_FUNC( QT_QDROPEVENT_PROPOSEDACTION )
{
   QDropEvent * p = hbqt_par_QDropEvent( 1 );
   if( p )
      hb_retni( ( Qt::DropAction ) ( p )->proposedAction() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDROPEVENT_PROPOSEDACTION FP=hb_retni( ( Qt::DropAction ) ( p )->proposedAction() ); p is NULL" ) );
   }
}

/*
 * void setDropAction ( Qt::DropAction action )
 */
HB_FUNC( QT_QDROPEVENT_SETDROPACTION )
{
   QDropEvent * p = hbqt_par_QDropEvent( 1 );
   if( p )
      ( p )->setDropAction( ( Qt::DropAction ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDROPEVENT_SETDROPACTION FP=( p )->setDropAction( ( Qt::DropAction ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * QWidget * source () const
 */
HB_FUNC( QT_QDROPEVENT_SOURCE )
{
   QDropEvent * p = hbqt_par_QDropEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->source(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDROPEVENT_SOURCE FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->source(), false ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
