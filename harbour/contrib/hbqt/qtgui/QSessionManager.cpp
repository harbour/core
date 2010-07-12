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

#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum RestartHint { RestartIfRunning, RestartAnyway, RestartImmediately, RestartNever }
 */

#include <QtCore/QPointer>

#include <QtGui/QSessionManager>


/*
 *
 */

typedef struct
{
   QPointer< QSessionManager > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QSessionManager;

QT_G_FUNC( hbqt_gcRelease_QSessionManager )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QSessionManager( void * pObj, bool bNew )
{
   QGC_POINTER_QSessionManager * p = ( QGC_POINTER_QSessionManager * ) hb_gcAllocate( sizeof( QGC_POINTER_QSessionManager ), hbqt_gcFuncs() );

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
   // hb_retptr( new () );
}

/*
 * bool allowsErrorInteraction ()
 */
HB_FUNC( QT_QSESSIONMANAGER_ALLOWSERRORINTERACTION )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      hb_retl( ( p )->allowsErrorInteraction() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSESSIONMANAGER_ALLOWSERRORINTERACTION FP=hb_retl( ( p )->allowsErrorInteraction() ); p is NULL" ) );
   }
}

/*
 * bool allowsInteraction ()
 */
HB_FUNC( QT_QSESSIONMANAGER_ALLOWSINTERACTION )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      hb_retl( ( p )->allowsInteraction() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSESSIONMANAGER_ALLOWSINTERACTION FP=hb_retl( ( p )->allowsInteraction() ); p is NULL" ) );
   }
}

/*
 * void cancel ()
 */
HB_FUNC( QT_QSESSIONMANAGER_CANCEL )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      ( p )->cancel();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSESSIONMANAGER_CANCEL FP=( p )->cancel(); p is NULL" ) );
   }
}

/*
 * QStringList discardCommand () const
 */
HB_FUNC( QT_QSESSIONMANAGER_DISCARDCOMMAND )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->discardCommand() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSESSIONMANAGER_DISCARDCOMMAND FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->discardCommand() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool isPhase2 () const
 */
HB_FUNC( QT_QSESSIONMANAGER_ISPHASE2 )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      hb_retl( ( p )->isPhase2() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSESSIONMANAGER_ISPHASE2 FP=hb_retl( ( p )->isPhase2() ); p is NULL" ) );
   }
}

/*
 * void release ()
 */
HB_FUNC( QT_QSESSIONMANAGER_RELEASE )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      ( p )->release();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSESSIONMANAGER_RELEASE FP=( p )->release(); p is NULL" ) );
   }
}

/*
 * void requestPhase2 ()
 */
HB_FUNC( QT_QSESSIONMANAGER_REQUESTPHASE2 )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      ( p )->requestPhase2();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSESSIONMANAGER_REQUESTPHASE2 FP=( p )->requestPhase2(); p is NULL" ) );
   }
}

/*
 * QStringList restartCommand () const
 */
HB_FUNC( QT_QSESSIONMANAGER_RESTARTCOMMAND )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->restartCommand() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSESSIONMANAGER_RESTARTCOMMAND FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->restartCommand() ), true ) ); p is NULL" ) );
   }
}

/*
 * RestartHint restartHint () const
 */
HB_FUNC( QT_QSESSIONMANAGER_RESTARTHINT )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      hb_retni( ( QSessionManager::RestartHint ) ( p )->restartHint() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSESSIONMANAGER_RESTARTHINT FP=hb_retni( ( QSessionManager::RestartHint ) ( p )->restartHint() ); p is NULL" ) );
   }
}

/*
 * QString sessionId () const
 */
HB_FUNC( QT_QSESSIONMANAGER_SESSIONID )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      hb_retc( ( p )->sessionId().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSESSIONMANAGER_SESSIONID FP=hb_retc( ( p )->sessionId().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString sessionKey () const
 */
HB_FUNC( QT_QSESSIONMANAGER_SESSIONKEY )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      hb_retc( ( p )->sessionKey().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSESSIONMANAGER_SESSIONKEY FP=hb_retc( ( p )->sessionKey().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void setDiscardCommand ( const QStringList & list )
 */
HB_FUNC( QT_QSESSIONMANAGER_SETDISCARDCOMMAND )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      ( p )->setDiscardCommand( *hbqt_par_QStringList( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSESSIONMANAGER_SETDISCARDCOMMAND FP=( p )->setDiscardCommand( *hbqt_par_QStringList( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setManagerProperty ( const QString & name, const QStringList & value )
 */
HB_FUNC( QT_QSESSIONMANAGER_SETMANAGERPROPERTY )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      ( p )->setManagerProperty( QSessionManager::tr( hb_parc( 2 ) ), *hbqt_par_QStringList( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSESSIONMANAGER_SETMANAGERPROPERTY FP=( p )->setManagerProperty( QSessionManager::tr( hb_parc( 2 ) ), *hbqt_par_QStringList( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setManagerProperty ( const QString & name, const QString & value )
 */
HB_FUNC( QT_QSESSIONMANAGER_SETMANAGERPROPERTY_1 )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      ( p )->setManagerProperty( QSessionManager::tr( hb_parc( 2 ) ), QSessionManager::tr( hb_parc( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSESSIONMANAGER_SETMANAGERPROPERTY_1 FP=( p )->setManagerProperty( QSessionManager::tr( hb_parc( 2 ) ), QSessionManager::tr( hb_parc( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * void setRestartCommand ( const QStringList & command )
 */
HB_FUNC( QT_QSESSIONMANAGER_SETRESTARTCOMMAND )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      ( p )->setRestartCommand( *hbqt_par_QStringList( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSESSIONMANAGER_SETRESTARTCOMMAND FP=( p )->setRestartCommand( *hbqt_par_QStringList( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setRestartHint ( RestartHint hint )
 */
HB_FUNC( QT_QSESSIONMANAGER_SETRESTARTHINT )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      ( p )->setRestartHint( ( QSessionManager::RestartHint ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSESSIONMANAGER_SETRESTARTHINT FP=( p )->setRestartHint( ( QSessionManager::RestartHint ) hb_parni( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
