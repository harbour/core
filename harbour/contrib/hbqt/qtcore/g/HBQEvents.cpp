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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 4/4 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QObject>
#include "hbqt_hbqevents.h"


/*
 * HBQEvents()
 * ~HBQEvents()
 *
 */

typedef struct
{
   QPointer< HBQEvents > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_HBQEvents;

HBQT_GC_FUNC( hbqt_gcRelease_HBQEvents )
{
   HBQEvents  * ph = NULL ;
   HBQT_GC_T_HBQEvents * p = ( HBQT_GC_T_HBQEvents * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_HBQEvents   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_HBQEvents   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_HBQEvents          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_HBQEvents    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_HBQEvents    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_HBQEvents( void * pObj, bool bNew )
{
   HBQT_GC_T_HBQEvents * p = ( HBQT_GC_T_HBQEvents * ) hb_gcAllocate( sizeof( HBQT_GC_T_HBQEvents ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< HBQEvents >( ( HBQEvents * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_HBQEvents;
   p->type = HBQT_TYPE_HBQEvents;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_HBQEvents  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_HBQEvents", pObj ) );
   }
   return p;
}

HB_FUNC( QT_HBQEVENTS )
{
   HBQEvents * pObj = NULL;

   pObj = new HBQEvents() ;

   hb_retptrGC( hbqt_gcAllocate_HBQEvents( ( void * ) pObj, true ) );
}

/*
 * HBQEvents()
 */
HB_FUNC( QT_HBQEVENTS_HBQEVENTS )
{
   hb_retptrGC( hbqt_gcAllocate_HBQEvents( new HBQEvents(), true ) );
}

/*
 * bool hbConnect( PHB_ITEM obj, int event, PHB_ITEM block )
 */
HB_FUNC( QT_HBQEVENTS_HBCONNECT )
{
   HBQEvents * p = hbqt_par_HBQEvents( 1 );
   if( p )
   {
      hb_retl( ( p )->hbConnect( hb_param( 2, HB_IT_ANY ), hb_parni( 3 ), hb_param( 4, HB_IT_ANY ) ) );
   }
}

/*
 * bool hbDisconnect( PHB_ITEM obj, int event )
 */
HB_FUNC( QT_HBQEVENTS_HBDISCONNECT )
{
   HBQEvents * p = hbqt_par_HBQEvents( 1 );
   if( p )
   {
      hb_retl( ( p )->hbDisconnect( hb_param( 2, HB_IT_ANY ), hb_parni( 3 ) ) );
   }
}

/*
 * bool hbClear()
 */
HB_FUNC( QT_HBQEVENTS_HBCLEAR )
{
   HBQEvents * p = hbqt_par_HBQEvents( 1 );
   if( p )
   {
      hb_retl( ( p )->hbClear() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
