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
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   QPointer< QSessionManager > pq;
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

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSessionManager;

   if( bNew )
   {
      new( & p->pq ) QPointer< QSessionManager >( ( QSessionManager * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QSessionManager            ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QSESSIONMANAGER )
{
}

/*
 * bool allowsErrorInteraction ()
 */
HB_FUNC( QT_QSESSIONMANAGER_ALLOWSERRORINTERACTION )
{
   hb_retl( hbqt_par_QSessionManager( 1 )->allowsErrorInteraction() );
}

/*
 * bool allowsInteraction ()
 */
HB_FUNC( QT_QSESSIONMANAGER_ALLOWSINTERACTION )
{
   hb_retl( hbqt_par_QSessionManager( 1 )->allowsInteraction() );
}

/*
 * void cancel ()
 */
HB_FUNC( QT_QSESSIONMANAGER_CANCEL )
{
   hbqt_par_QSessionManager( 1 )->cancel();
}

/*
 * QStringList discardCommand () const
 */
HB_FUNC( QT_QSESSIONMANAGER_DISCARDCOMMAND )
{
   hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( hbqt_par_QSessionManager( 1 )->discardCommand() ), true ) );
}

/*
 * bool isPhase2 () const
 */
HB_FUNC( QT_QSESSIONMANAGER_ISPHASE2 )
{
   hb_retl( hbqt_par_QSessionManager( 1 )->isPhase2() );
}

/*
 * void release ()
 */
HB_FUNC( QT_QSESSIONMANAGER_RELEASE )
{
   hbqt_par_QSessionManager( 1 )->release();
}

/*
 * void requestPhase2 ()
 */
HB_FUNC( QT_QSESSIONMANAGER_REQUESTPHASE2 )
{
   hbqt_par_QSessionManager( 1 )->requestPhase2();
}

/*
 * QStringList restartCommand () const
 */
HB_FUNC( QT_QSESSIONMANAGER_RESTARTCOMMAND )
{
   hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( hbqt_par_QSessionManager( 1 )->restartCommand() ), true ) );
}

/*
 * RestartHint restartHint () const
 */
HB_FUNC( QT_QSESSIONMANAGER_RESTARTHINT )
{
   hb_retni( ( QSessionManager::RestartHint ) hbqt_par_QSessionManager( 1 )->restartHint() );
}

/*
 * QString sessionId () const
 */
HB_FUNC( QT_QSESSIONMANAGER_SESSIONID )
{
   hb_retc( hbqt_par_QSessionManager( 1 )->sessionId().toAscii().data() );
}

/*
 * QString sessionKey () const
 */
HB_FUNC( QT_QSESSIONMANAGER_SESSIONKEY )
{
   hb_retc( hbqt_par_QSessionManager( 1 )->sessionKey().toAscii().data() );
}

/*
 * void setDiscardCommand ( const QStringList & list )
 */
HB_FUNC( QT_QSESSIONMANAGER_SETDISCARDCOMMAND )
{
   hbqt_par_QSessionManager( 1 )->setDiscardCommand( *hbqt_par_QStringList( 2 ) );
}

/*
 * void setManagerProperty ( const QString & name, const QStringList & value )
 */
HB_FUNC( QT_QSESSIONMANAGER_SETMANAGERPROPERTY )
{
   hbqt_par_QSessionManager( 1 )->setManagerProperty( QSessionManager::tr( hb_parc( 2 ) ), *hbqt_par_QStringList( 3 ) );
}

/*
 * void setManagerProperty ( const QString & name, const QString & value )
 */
HB_FUNC( QT_QSESSIONMANAGER_SETMANAGERPROPERTY_1 )
{
   hbqt_par_QSessionManager( 1 )->setManagerProperty( QSessionManager::tr( hb_parc( 2 ) ), QSessionManager::tr( hb_parc( 3 ) ) );
}

/*
 * void setRestartCommand ( const QStringList & command )
 */
HB_FUNC( QT_QSESSIONMANAGER_SETRESTARTCOMMAND )
{
   hbqt_par_QSessionManager( 1 )->setRestartCommand( *hbqt_par_QStringList( 2 ) );
}

/*
 * void setRestartHint ( RestartHint hint )
 */
HB_FUNC( QT_QSESSIONMANAGER_SETRESTARTHINT )
{
   hbqt_par_QSessionManager( 1 )->setRestartHint( ( QSessionManager::RestartHint ) hb_parni( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
