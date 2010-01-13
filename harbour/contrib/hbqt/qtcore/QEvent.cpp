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
 *  enum Type { None, AccessibilityDescription, AccessibilityHelp, AccessibilityPrepare, ..., MaxUser }
 */

#include <QtCore/QPointer>

#include <QtCore/QEvent>


/*
 * QEvent ( Type type )
 * virtual ~QEvent ()
 */

QT_G_FUNC( hbqt_gcRelease_QEvent )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QEvent                       p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QEvent                      ph=%p", p->ph ) );

   if( p && p->ph )
   {
      delete ( ( QEvent * ) p->ph );
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "YES hbqt_gcRelease_QEvent                      Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL hbqt_gcRelease_QEvent                      Object Already deleted!" ) );
   }
}

void * hbqt_gcAllocate_QEvent( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = hbqt_gcRelease_QEvent;
   HB_TRACE( HB_TR_DEBUG, ( "          new_QEvent                      %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return p;
}

HB_FUNC( QT_QEVENT )
{
   void * pObj = NULL;

   pObj = ( QEvent* ) new QEvent( ( QEvent::Type ) hb_parni( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QEvent( pObj ) );
}
/*
 * void accept ()
 */
HB_FUNC( QT_QEVENT_ACCEPT )
{
   hbqt_par_QEvent( 1 )->accept();
}

/*
 * void ignore ()
 */
HB_FUNC( QT_QEVENT_IGNORE )
{
   hbqt_par_QEvent( 1 )->ignore();
}

/*
 * bool isAccepted () const
 */
HB_FUNC( QT_QEVENT_ISACCEPTED )
{
   hb_retl( hbqt_par_QEvent( 1 )->isAccepted() );
}

/*
 * void setAccepted ( bool accepted )
 */
HB_FUNC( QT_QEVENT_SETACCEPTED )
{
   hbqt_par_QEvent( 1 )->setAccepted( hb_parl( 2 ) );
}

/*
 * bool spontaneous () const
 */
HB_FUNC( QT_QEVENT_SPONTANEOUS )
{
   hb_retl( hbqt_par_QEvent( 1 )->spontaneous() );
}

/*
 * Type type () const
 */
HB_FUNC( QT_QEVENT_TYPE )
{
   hb_retni( ( QEvent::Type ) hbqt_par_QEvent( 1 )->type() );
}

/*
 * int registerEventType ( int hint = -1 )
 */
HB_FUNC( QT_QEVENT_REGISTEREVENTTYPE )
{
   hb_retni( hbqt_par_QEvent( 1 )->registerEventType( ( HB_ISNUM( 2 ) ? hb_parni( 2 ) : -1 ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
