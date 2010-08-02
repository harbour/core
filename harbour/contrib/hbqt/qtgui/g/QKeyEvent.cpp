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

#include <QtCore/QPointer>

#include <QtGui/QKeyEvent>


/*
 * QKeyEvent ( Type type, int key, Qt::KeyboardModifiers modifiers, const QString & text = QString(), bool autorep = false, ushort count = 1 )
 * ~QKeyEvent ()
 */

typedef struct
{
   QKeyEvent * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QKeyEvent;

QT_G_FUNC( hbqt_gcRelease_QKeyEvent )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QKeyEvent( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QKeyEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QKeyEvent;
   p->type = HBQT_TYPE_QKeyEvent;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QKeyEvent", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QKeyEvent", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QKEYEVENT )
{
   //hb_retptr( ( QKeyEvent * ) new QKeyEvent( *hbqt_par_QKeyEvent( 1 ) ) );
}

/*
 * int count () const
 */
HB_FUNC( QT_QKEYEVENT_COUNT )
{
   QKeyEvent * p = hbqt_par_QKeyEvent( 1 );
   if( p )
      hb_retni( ( p )->count() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QKEYEVENT_COUNT FP=hb_retni( ( p )->count() ); p is NULL" ) );
   }
}

/*
 * bool isAutoRepeat () const
 */
HB_FUNC( QT_QKEYEVENT_ISAUTOREPEAT )
{
   QKeyEvent * p = hbqt_par_QKeyEvent( 1 );
   if( p )
      hb_retl( ( p )->isAutoRepeat() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QKEYEVENT_ISAUTOREPEAT FP=hb_retl( ( p )->isAutoRepeat() ); p is NULL" ) );
   }
}

/*
 * int key () const
 */
HB_FUNC( QT_QKEYEVENT_KEY )
{
   QKeyEvent * p = hbqt_par_QKeyEvent( 1 );
   if( p )
      hb_retni( ( p )->key() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QKEYEVENT_KEY FP=hb_retni( ( p )->key() ); p is NULL" ) );
   }
}

/*
 * bool matches ( QKeySequence::StandardKey key ) const
 */
HB_FUNC( QT_QKEYEVENT_MATCHES )
{
   QKeyEvent * p = hbqt_par_QKeyEvent( 1 );
   if( p )
      hb_retl( ( p )->matches( ( QKeySequence::StandardKey ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QKEYEVENT_MATCHES FP=hb_retl( ( p )->matches( ( QKeySequence::StandardKey ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * Qt::KeyboardModifiers modifiers () const
 */
HB_FUNC( QT_QKEYEVENT_MODIFIERS )
{
   QKeyEvent * p = hbqt_par_QKeyEvent( 1 );
   if( p )
      hb_retni( ( Qt::KeyboardModifiers ) ( p )->modifiers() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QKEYEVENT_MODIFIERS FP=hb_retni( ( Qt::KeyboardModifiers ) ( p )->modifiers() ); p is NULL" ) );
   }
}

/*
 * quint32 nativeModifiers () const
 */
HB_FUNC( QT_QKEYEVENT_NATIVEMODIFIERS )
{
   QKeyEvent * p = hbqt_par_QKeyEvent( 1 );
   if( p )
      hb_retnl( ( p )->nativeModifiers() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QKEYEVENT_NATIVEMODIFIERS FP=hb_retnl( ( p )->nativeModifiers() ); p is NULL" ) );
   }
}

/*
 * quint32 nativeScanCode () const
 */
HB_FUNC( QT_QKEYEVENT_NATIVESCANCODE )
{
   QKeyEvent * p = hbqt_par_QKeyEvent( 1 );
   if( p )
      hb_retnl( ( p )->nativeScanCode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QKEYEVENT_NATIVESCANCODE FP=hb_retnl( ( p )->nativeScanCode() ); p is NULL" ) );
   }
}

/*
 * quint32 nativeVirtualKey () const
 */
HB_FUNC( QT_QKEYEVENT_NATIVEVIRTUALKEY )
{
   QKeyEvent * p = hbqt_par_QKeyEvent( 1 );
   if( p )
      hb_retnl( ( p )->nativeVirtualKey() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QKEYEVENT_NATIVEVIRTUALKEY FP=hb_retnl( ( p )->nativeVirtualKey() ); p is NULL" ) );
   }
}

/*
 * QString text () const
 */
HB_FUNC( QT_QKEYEVENT_TEXT )
{
   QKeyEvent * p = hbqt_par_QKeyEvent( 1 );
   if( p )
      hb_retc( ( p )->text().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QKEYEVENT_TEXT FP=hb_retc( ( p )->text().toAscii().data() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
