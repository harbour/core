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
#include "hbqtwebkit.h"

#if QT_VERSION >= 0x040500

/*
 *  Constructed[ 7/7 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // QList<QWebDatabase> databases () const
 */

#include <QtCore/QPointer>

#include <QtWebKit/QWebSecurityOrigin>


/*
 * QWebSecurityOrigin ( const QWebSecurityOrigin & other )
 * ~QWebSecurityOrigin ()
 */

typedef struct
{
   QWebSecurityOrigin * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QWebSecurityOrigin;

HBQT_GC_FUNC( hbqt_gcRelease_QWebSecurityOrigin )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QWebSecurityOrigin * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QWebSecurityOrigin( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QWebSecurityOrigin * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWebSecurityOrigin;
   p->type = HBQT_TYPE_QWebSecurityOrigin;

   return p;
}

HB_FUNC( QT_QWEBSECURITYORIGIN )
{
   QWebSecurityOrigin * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QWebSecurityOrigin( *hbqt_par_QWebSecurityOrigin( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QWebSecurityOrigin( ( void * ) pObj, true ) );
}

/* qint64 databaseQuota () const */
HB_FUNC( QT_QWEBSECURITYORIGIN_DATABASEQUOTA )
{
   QWebSecurityOrigin * p = hbqt_par_QWebSecurityOrigin( 1 );
   if( p )
      hb_retnint( ( p )->databaseQuota() );
}

/* qint64 databaseUsage () const */
HB_FUNC( QT_QWEBSECURITYORIGIN_DATABASEUSAGE )
{
   QWebSecurityOrigin * p = hbqt_par_QWebSecurityOrigin( 1 );
   if( p )
      hb_retnint( ( p )->databaseUsage() );
}

/* QString host () const */
HB_FUNC( QT_QWEBSECURITYORIGIN_HOST )
{
   QWebSecurityOrigin * p = hbqt_par_QWebSecurityOrigin( 1 );
   if( p )
      hb_retstr_utf8( ( p )->host().toUtf8().data() );
}

/* int port () const */
HB_FUNC( QT_QWEBSECURITYORIGIN_PORT )
{
   QWebSecurityOrigin * p = hbqt_par_QWebSecurityOrigin( 1 );
   if( p )
      hb_retni( ( p )->port() );
}

/* QString scheme () const */
HB_FUNC( QT_QWEBSECURITYORIGIN_SCHEME )
{
   QWebSecurityOrigin * p = hbqt_par_QWebSecurityOrigin( 1 );
   if( p )
      hb_retstr_utf8( ( p )->scheme().toUtf8().data() );
}

/* void setDatabaseQuota ( qint64 quota ) */
HB_FUNC( QT_QWEBSECURITYORIGIN_SETDATABASEQUOTA )
{
   QWebSecurityOrigin * p = hbqt_par_QWebSecurityOrigin( 1 );
   if( p )
      ( p )->setDatabaseQuota( hb_parnint( 2 ) );
}

/* QList<QWebSecurityOrigin> allOrigins () */
HB_FUNC( QT_QWEBSECURITYORIGIN_ALLORIGINS )
{
   QWebSecurityOrigin * p = hbqt_par_QWebSecurityOrigin( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QWebSecurityOrigin>( ( p )->allOrigins() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
