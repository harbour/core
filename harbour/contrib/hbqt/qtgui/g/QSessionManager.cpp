/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */

#include "hbqtcore.h"
#include "hbqtgui.h"

#if QT_VERSION >= 0x040500

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
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QSessionManager( void * pObj, bool bNew )
{
   HBQT_GC_T_QSessionManager * p = ( HBQT_GC_T_QSessionManager * ) hb_gcAllocate( sizeof( HBQT_GC_T_QSessionManager ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QSessionManager >( ( QSessionManager * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSessionManager;
   p->type = HBQT_TYPE_QSessionManager;

   return p;
}

HB_FUNC( QT_QSESSIONMANAGER )
{
   // __HB_RETPTRGC__( new QSessionManager() );
}

/* bool allowsErrorInteraction () */
HB_FUNC( QT_QSESSIONMANAGER_ALLOWSERRORINTERACTION )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      hb_retl( ( p )->allowsErrorInteraction() );
}

/* bool allowsInteraction () */
HB_FUNC( QT_QSESSIONMANAGER_ALLOWSINTERACTION )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      hb_retl( ( p )->allowsInteraction() );
}

/* void cancel () */
HB_FUNC( QT_QSESSIONMANAGER_CANCEL )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      ( p )->cancel();
}

/* QStringList discardCommand () const */
HB_FUNC( QT_QSESSIONMANAGER_DISCARDCOMMAND )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->discardCommand() ), true ) );
}

/* bool isPhase2 () const */
HB_FUNC( QT_QSESSIONMANAGER_ISPHASE2 )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      hb_retl( ( p )->isPhase2() );
}

/* void release () */
HB_FUNC( QT_QSESSIONMANAGER_RELEASE )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      ( p )->release();
}

/* void requestPhase2 () */
HB_FUNC( QT_QSESSIONMANAGER_REQUESTPHASE2 )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      ( p )->requestPhase2();
}

/* QStringList restartCommand () const */
HB_FUNC( QT_QSESSIONMANAGER_RESTARTCOMMAND )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->restartCommand() ), true ) );
}

/* RestartHint restartHint () const */
HB_FUNC( QT_QSESSIONMANAGER_RESTARTHINT )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      hb_retni( ( QSessionManager::RestartHint ) ( p )->restartHint() );
}

/* QString sessionId () const */
HB_FUNC( QT_QSESSIONMANAGER_SESSIONID )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      hb_retstr_utf8( ( p )->sessionId().toUtf8().data() );
}

/* QString sessionKey () const */
HB_FUNC( QT_QSESSIONMANAGER_SESSIONKEY )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      hb_retstr_utf8( ( p )->sessionKey().toUtf8().data() );
}

/* void setDiscardCommand ( const QStringList & list ) */
HB_FUNC( QT_QSESSIONMANAGER_SETDISCARDCOMMAND )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      ( p )->setDiscardCommand( *hbqt_par_QStringList( 2 ) );
}

/* void setManagerProperty ( const QString & name, const QStringList & value ) */
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

/* void setManagerProperty ( const QString & name, const QString & value ) */
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

/* void setRestartCommand ( const QStringList & command ) */
HB_FUNC( QT_QSESSIONMANAGER_SETRESTARTCOMMAND )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      ( p )->setRestartCommand( *hbqt_par_QStringList( 2 ) );
}

/* void setRestartHint ( RestartHint hint ) */
HB_FUNC( QT_QSESSIONMANAGER_SETRESTARTHINT )
{
   QSessionManager * p = hbqt_par_QSessionManager( 1 );
   if( p )
      ( p )->setRestartHint( ( QSessionManager::RestartHint ) hb_parni( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
