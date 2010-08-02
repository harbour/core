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

typedef struct
{
   QEvent * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QEvent;

QT_G_FUNC( hbqt_gcRelease_QEvent )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QEvent   /.\\", p->ph ) );
         delete ( ( QEvent * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QEvent   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QEvent    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QEvent    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QEvent( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QEvent;
   p->type = HBQT_TYPE_QEvent;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QEvent", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QEvent", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QEVENT )
{
   QEvent * pObj = NULL;

   pObj =  new QEvent( ( QEvent::Type ) hb_parni( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QEvent( ( void * ) pObj, true ) );
}

/*
 * void accept ()
 */
HB_FUNC( QT_QEVENT_ACCEPT )
{
   QEvent * p = hbqt_par_QEvent( 1 );
   if( p )
      ( p )->accept();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QEVENT_ACCEPT FP=( p )->accept(); p is NULL" ) );
   }
}

/*
 * void ignore ()
 */
HB_FUNC( QT_QEVENT_IGNORE )
{
   QEvent * p = hbqt_par_QEvent( 1 );
   if( p )
      ( p )->ignore();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QEVENT_IGNORE FP=( p )->ignore(); p is NULL" ) );
   }
}

/*
 * bool isAccepted () const
 */
HB_FUNC( QT_QEVENT_ISACCEPTED )
{
   QEvent * p = hbqt_par_QEvent( 1 );
   if( p )
      hb_retl( ( p )->isAccepted() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QEVENT_ISACCEPTED FP=hb_retl( ( p )->isAccepted() ); p is NULL" ) );
   }
}

/*
 * void setAccepted ( bool accepted )
 */
HB_FUNC( QT_QEVENT_SETACCEPTED )
{
   QEvent * p = hbqt_par_QEvent( 1 );
   if( p )
      ( p )->setAccepted( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QEVENT_SETACCEPTED FP=( p )->setAccepted( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool spontaneous () const
 */
HB_FUNC( QT_QEVENT_SPONTANEOUS )
{
   QEvent * p = hbqt_par_QEvent( 1 );
   if( p )
      hb_retl( ( p )->spontaneous() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QEVENT_SPONTANEOUS FP=hb_retl( ( p )->spontaneous() ); p is NULL" ) );
   }
}

/*
 * Type type () const
 */
HB_FUNC( QT_QEVENT_TYPE )
{
   QEvent * p = hbqt_par_QEvent( 1 );
   if( p )
      hb_retni( ( QEvent::Type ) ( p )->type() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QEVENT_TYPE FP=hb_retni( ( QEvent::Type ) ( p )->type() ); p is NULL" ) );
   }
}

/*
 * int registerEventType ( int hint = -1 )
 */
HB_FUNC( QT_QEVENT_REGISTEREVENTTYPE )
{
   QEvent * p = hbqt_par_QEvent( 1 );
   if( p )
      hb_retni( ( p )->registerEventType( hb_parnidef( 2, -1 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QEVENT_REGISTEREVENTTYPE FP=hb_retni( ( p )->registerEventType( hb_parnidef( 2, -1 ) ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
