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
 *  Constructed[ 9/9 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // QObject * mapping ( QWidget * widget ) const
 *  // void setMapping ( QObject * sender, QWidget * widget )
 */

#include <QtCore/QPointer>

#include <QtCore/QSignalMapper>


/*
 * QSignalMapper ( QObject * parent = 0 )
 * ~QSignalMapper ()
 */

typedef struct
{
   QPointer< QSignalMapper > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSignalMapper;

HBQT_GC_FUNC( hbqt_gcRelease_QSignalMapper )
{
   QSignalMapper  * ph = NULL ;
   HBQT_GC_T_QSignalMapper * p = ( HBQT_GC_T_QSignalMapper * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QSignalMapper   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QSignalMapper   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QSignalMapper          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QSignalMapper    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QSignalMapper    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QSignalMapper( void * pObj, bool bNew )
{
   HBQT_GC_T_QSignalMapper * p = ( HBQT_GC_T_QSignalMapper * ) hb_gcAllocate( sizeof( HBQT_GC_T_QSignalMapper ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QSignalMapper >( ( QSignalMapper * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSignalMapper;
   p->type = HBQT_TYPE_QSignalMapper;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QSignalMapper  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QSignalMapper", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSIGNALMAPPER )
{
   QSignalMapper * pObj = NULL;

   pObj = new QSignalMapper( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QSignalMapper( ( void * ) pObj, true ) );
}

/*
 * QObject * mapping ( int id ) const
 */
HB_FUNC( QT_QSIGNALMAPPER_MAPPING )
{
   QSignalMapper * p = hbqt_par_QSignalMapper( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QObject( ( p )->mapping( hb_parni( 2 ) ), false ) );
   }
}

/*
 * QObject * mapping ( const QString & id ) const
 */
HB_FUNC( QT_QSIGNALMAPPER_MAPPING_1 )
{
   QSignalMapper * p = hbqt_par_QSignalMapper( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QObject( ( p )->mapping( hb_parstr_utf8( 2, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/*
 * QObject * mapping ( QObject * object ) const
 */
HB_FUNC( QT_QSIGNALMAPPER_MAPPING_2 )
{
   QSignalMapper * p = hbqt_par_QSignalMapper( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QObject( ( p )->mapping( hbqt_par_QObject( 2 ) ), false ) );
   }
}

/*
 * void removeMappings ( QObject * sender )
 */
HB_FUNC( QT_QSIGNALMAPPER_REMOVEMAPPINGS )
{
   QSignalMapper * p = hbqt_par_QSignalMapper( 1 );
   if( p )
   {
      ( p )->removeMappings( hbqt_par_QObject( 2 ) );
   }
}

/*
 * void setMapping ( QObject * sender, int id )
 */
HB_FUNC( QT_QSIGNALMAPPER_SETMAPPING )
{
   QSignalMapper * p = hbqt_par_QSignalMapper( 1 );
   if( p )
   {
      ( p )->setMapping( hbqt_par_QObject( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void setMapping ( QObject * sender, const QString & text )
 */
HB_FUNC( QT_QSIGNALMAPPER_SETMAPPING_1 )
{
   QSignalMapper * p = hbqt_par_QSignalMapper( 1 );
   if( p )
   {
      void * pText;
      ( p )->setMapping( hbqt_par_QObject( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setMapping ( QObject * sender, QObject * object )
 */
HB_FUNC( QT_QSIGNALMAPPER_SETMAPPING_2 )
{
   QSignalMapper * p = hbqt_par_QSignalMapper( 1 );
   if( p )
   {
      ( p )->setMapping( hbqt_par_QObject( 2 ), hbqt_par_QObject( 3 ) );
   }
}

/*
 * void map ()
 */
HB_FUNC( QT_QSIGNALMAPPER_MAP )
{
   QSignalMapper * p = hbqt_par_QSignalMapper( 1 );
   if( p )
   {
      ( p )->map();
   }
}

/*
 * void map ( QObject * sender )
 */
HB_FUNC( QT_QSIGNALMAPPER_MAP_1 )
{
   QSignalMapper * p = hbqt_par_QSignalMapper( 1 );
   if( p )
   {
      ( p )->map( hbqt_par_QObject( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
