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

#include <QtCore/QPointer>

#include <QtCore/QObject>
#include "hbqt_hbslots.h"


/*
 * HBSlots()
 * ~HBSlots()
 */

typedef struct
{
   QPointer< HBSlots > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_HBSlots;

HBQT_GC_FUNC( hbqt_gcRelease_HBSlots )
{
   HBSlots  * ph = NULL ;
   HBQT_GC_T_HBSlots * p = ( HBQT_GC_T_HBSlots * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_HBSlots   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_HBSlots   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_HBSlots          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_HBSlots    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_HBSlots    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_HBSlots( void * pObj, bool bNew )
{
   HBQT_GC_T_HBSlots * p = ( HBQT_GC_T_HBSlots * ) hb_gcAllocate( sizeof( HBQT_GC_T_HBSlots ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< HBSlots >( ( HBSlots * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_HBSlots;
   p->type = HBQT_TYPE_HBSlots;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_HBSlots  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_HBSlots", pObj ) );
   }
   return p;
}

HB_FUNC( QT_HBSLOTS )
{
   HBSlots * pObj = NULL;

   pObj = new HBSlots() ;

   hb_retptrGC( hbqt_gcAllocate_HBSlots( ( void * ) pObj, true ) );
}

/*
 * bool hbConnect( PHB_ITEM pObj, const char * slot, PHB_ITEM bBlock )
 */
HB_FUNC( QT_HBSLOTS_HBCONNECT )
{
   HBSlots * p = hbqt_par_HBSlots( 1 );
   if( p )
   {
      hb_retl( ( p )->hbConnect( hb_param( 2, HB_IT_ANY ), hbqt_par_char( 3 ), hb_param( 4, HB_IT_ANY ) ) );
   }
}

/*
 * bool hbDisconnect( PHB_ITEM obj, const char * slot )
 */
HB_FUNC( QT_HBSLOTS_HBDISCONNECT )
{
   HBSlots * p = hbqt_par_HBSlots( 1 );
   if( p )
   {
      hb_retl( ( p )->hbDisconnect( hb_param( 2, HB_IT_ANY ), hbqt_par_char( 3 ) ) );
   }
}

/*
 * bool hbIsConnected( PHB_ITEM obj, const char * slot )
 */
HB_FUNC( QT_HBSLOTS_HBISCONNECTED )
{
   HBSlots * p = hbqt_par_HBSlots( 1 );
   if( p )
   {
      hb_retl( ( p )->hbIsConnected( hb_param( 2, HB_IT_ANY ), hbqt_par_char( 3 ) ) );
   }
}

/*
 * bool hbClear()
 */
HB_FUNC( QT_HBSLOTS_HBCLEAR )
{
   HBSlots * p = hbqt_par_HBSlots( 1 );
   if( p )
   {
      hb_retl( ( p )->hbClear() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
