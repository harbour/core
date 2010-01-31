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

#include <QtCore/QObject>
#include "../hbqt_hbslots.h"


/*
 * HBSlots()
 * ~HBSlots()
 */

typedef struct
{
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   QPointer< HBSlots > pq;
} QGC_POINTER_HBSlots;

QT_G_FUNC( hbqt_gcRelease_HBSlots )
{
   QGC_POINTER_HBSlots * p = ( QGC_POINTER_HBSlots * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_HBSlots   /.\\   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            delete ( ( HBSlots * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_HBSlots   \\./   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_HBSlotsph=%p pq=%p", p->ph, (void *)(p->pq) ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_HBSlots    :     Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_HBSlots    :    Object not created with new()" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_HBSlots( void * pObj, bool bNew )
{
   QGC_POINTER_HBSlots * p = ( QGC_POINTER_HBSlots * ) hb_gcAllocate( sizeof( QGC_POINTER_HBSlots ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_HBSlots;

   if( bNew )
   {
      new( & p->pq ) QPointer< HBSlots >( ( HBSlots * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_HBSlots                    ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_HBSLOTS )
{
   void * pObj = NULL;

   pObj = new HBSlots() ;

   hb_retptrGC( hbqt_gcAllocate_HBSlots( pObj, true ) );
}

/*
 * bool hbConnect( PHB_ITEM pObj, const char * slot, PHB_ITEM bBlock )
 */
HB_FUNC( QT_HBSLOTS_HBCONNECT )
{
   hb_retl( hbqt_par_HBSlots( 1 )->hbConnect( hb_param( 2, HB_IT_ANY ), hbqt_par_char( 3 ), hb_param( 4, HB_IT_ANY ) ) );
}

/*
 * bool hbDisconnect( PHB_ITEM obj, const char * slot )
 */
HB_FUNC( QT_HBSLOTS_HBDISCONNECT )
{
   hb_retl( hbqt_par_HBSlots( 1 )->hbDisconnect( hb_param( 2, HB_IT_ANY ), hbqt_par_char( 3 ) ) );
}

/*
 * bool hbIsConnected( PHB_ITEM obj, const char * slot )
 */
HB_FUNC( QT_HBSLOTS_HBISCONNECTED )
{
   hb_retl( hbqt_par_HBSlots( 1 )->hbIsConnected( hb_param( 2, HB_IT_ANY ), hbqt_par_char( 3 ) ) );
}

/*
 * bool hbClear()
 */
HB_FUNC( QT_HBSLOTS_HBCLEAR )
{
   hb_retl( hbqt_par_HBSlots( 1 )->hbClear() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
