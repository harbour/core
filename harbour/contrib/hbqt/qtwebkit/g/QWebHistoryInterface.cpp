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
 *  Constructed[ 4/4 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtWebKit/QWebHistoryInterface>


/*
 * QWebHistoryInterface ( QObject * parent = 0 )
 * ~QWebHistoryInterface ()
 */

typedef struct
{
   QPointer< QWebHistoryInterface > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QWebHistoryInterface;

HBQT_GC_FUNC( hbqt_gcRelease_QWebHistoryInterface )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QWebHistoryInterface( void * pObj, bool bNew )
{
   HBQT_GC_T_QWebHistoryInterface * p = ( HBQT_GC_T_QWebHistoryInterface * ) hb_gcAllocate( sizeof( HBQT_GC_T_QWebHistoryInterface ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QWebHistoryInterface >( ( QWebHistoryInterface * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWebHistoryInterface;
   p->type = HBQT_TYPE_QWebHistoryInterface;

   return p;
}

HB_FUNC( QT_QWEBHISTORYINTERFACE )
{
   //__HB_RETPTRGC__( new QWebHistoryInterface( hbqt_par_QObject( 1 ) ) );
}

/* virtual void addHistoryEntry ( const QString & url ) = 0 */
HB_FUNC( QT_QWEBHISTORYINTERFACE_ADDHISTORYENTRY )
{
   QWebHistoryInterface * p = hbqt_par_QWebHistoryInterface( 1 );
   if( p )
   {
      void * pText;
      ( p )->addHistoryEntry( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* virtual bool historyContains ( const QString & url ) const = 0 */
HB_FUNC( QT_QWEBHISTORYINTERFACE_HISTORYCONTAINS )
{
   QWebHistoryInterface * p = hbqt_par_QWebHistoryInterface( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->historyContains( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* QWebHistoryInterface * defaultInterface () */
HB_FUNC( QT_QWEBHISTORYINTERFACE_DEFAULTINTERFACE )
{
   QWebHistoryInterface * p = hbqt_par_QWebHistoryInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebHistoryInterface( ( p )->defaultInterface(), false ) );
}

/* void setDefaultInterface ( QWebHistoryInterface * defaultInterface ) */
HB_FUNC( QT_QWEBHISTORYINTERFACE_SETDEFAULTINTERFACE )
{
   QWebHistoryInterface * p = hbqt_par_QWebHistoryInterface( 1 );
   if( p )
      ( p )->setDefaultInterface( hbqt_par_QWebHistoryInterface( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
