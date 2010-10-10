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
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum RestartHint { RestartIfRunning, RestartAnyway, RestartImmediately, RestartNever }
 */

/*
 *  Constructed[ 16/16 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QSessionManager>


/*
 * QSessionManager ()
 */

typedef struct
{
   QPointer< QSessionManager > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSessionManager;

HBQT_GC_FUNC( hbqt_gcRelease_QSessionManager )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QSessionManager( void * pObj, bool bNew )
{
   HBQT_GC_T_QSessionManager * p = ( HBQT_GC_T_QSessionManager * ) hb_gcAllocate( sizeof( HBQT_GC_T_QSessionManager ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QSessionManager >( ( QSessionManager * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSessionManager;
   p->type = HBQT_TYPE_QSessionManager;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QSessionManager  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QSessionManager", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSESSIONMANAGER )
{
   // __HB_RETPTRGC__( new QSessionManager() );
}

/*
 * bool allowsErrorInteraction ()
 */
HB_FUNC( QT_QSESSIONMANAGER_ALLOWSERRORINTERACTION )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
   {
      hb_retl( ( p )->allowsErrorInteraction() );
   }
}

/*
 * bool allowsInteraction ()
 */
HB_FUNC( QT_QSESSIONMANAGER_ALLOWSINTERACTION )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
   {
      hb_retl( ( p )->allowsInteraction() );
   }
}

/*
 * void cancel ()
 */
HB_FUNC( QT_QSESSIONMANAGER_CANCEL )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
   {
      ( p )->cancel();
   }
}

/*
 * QStringList discardCommand () const
 */
HB_FUNC( QT_QSESSIONMANAGER_DISCARDCOMMAND )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->discardCommand() ), true ) );
   }
}

/*
 * bool isPhase2 () const
 */
HB_FUNC( QT_QSESSIONMANAGER_ISPHASE2 )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
   {
      hb_retl( ( p )->isPhase2() );
   }
}

/*
 * void release ()
 */
HB_FUNC( QT_QSESSIONMANAGER_RELEASE )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
   {
      ( p )->release();
   }
}

/*
 * void requestPhase2 ()
 */
HB_FUNC( QT_QSESSIONMANAGER_REQUESTPHASE2 )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
   {
      ( p )->requestPhase2();
   }
}

/*
 * QStringList restartCommand () const
 */
HB_FUNC( QT_QSESSIONMANAGER_RESTARTCOMMAND )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->restartCommand() ), true ) );
   }
}

/*
 * RestartHint restartHint () const
 */
HB_FUNC( QT_QSESSIONMANAGER_RESTARTHINT )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
   {
      hb_retni( ( QSessionManager::RestartHint ) ( p )->restartHint() );
   }
}

/*
 * QString sessionId () const
 */
HB_FUNC( QT_QSESSIONMANAGER_SESSIONID )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->sessionId().toUtf8().data() );
   }
}

/*
 * QString sessionKey () const
 */
HB_FUNC( QT_QSESSIONMANAGER_SESSIONKEY )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->sessionKey().toUtf8().data() );
   }
}

/*
 * void setDiscardCommand ( const QStringList & list )
 */
HB_FUNC( QT_QSESSIONMANAGER_SETDISCARDCOMMAND )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
   {
      ( p )->setDiscardCommand( *hbqt_par_QStringList( 2 ) );
   }
}

/*
 * void setManagerProperty ( const QString & name, const QStringList & value )
 */
HB_FUNC( QT_QSESSIONMANAGER_SETMANAGERPROPERTY )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
   {
      void * pText;
      ( p )->setManagerProperty( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QStringList( 3 ) );
      hb_strfree( pText );
   }
}

/*
 * void setManagerProperty ( const QString & name, const QString & value )
 */
HB_FUNC( QT_QSESSIONMANAGER_SETMANAGERPROPERTY_1 )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
   {
      void * pText;
      ( p )->setManagerProperty( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setRestartCommand ( const QStringList & command )
 */
HB_FUNC( QT_QSESSIONMANAGER_SETRESTARTCOMMAND )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
   {
      ( p )->setRestartCommand( *hbqt_par_QStringList( 2 ) );
   }
}

/*
 * void setRestartHint ( RestartHint hint )
 */
HB_FUNC( QT_QSESSIONMANAGER_SETRESTARTHINT )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
   {
      ( p )->setRestartHint( ( QSessionManager::RestartHint ) hb_parni( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
