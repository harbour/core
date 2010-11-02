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
 *  Constructed[ 20/20 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QTextBrowser>


/*
 * QTextBrowser ( QWidget * parent = 0 )
 */

typedef struct
{
   QPointer< QTextBrowser > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextBrowser;

HBQT_GC_FUNC( hbqt_gcRelease_QTextBrowser )
{
   HBQT_GC_T_QTextBrowser * p = ( HBQT_GC_T_QTextBrowser * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QTextBrowser * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextBrowser( void * pObj, bool bNew )
{
   HBQT_GC_T_QTextBrowser * p = ( HBQT_GC_T_QTextBrowser * ) hb_gcAllocate( sizeof( HBQT_GC_T_QTextBrowser ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTextBrowser >( ( QTextBrowser * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextBrowser;
   p->type = HBQT_TYPE_QTextBrowser;

   return p;
}

HB_FUNC( QT_QTEXTBROWSER )
{
   QTextBrowser * pObj = NULL;

   pObj = new QTextBrowser( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTextBrowser( ( void * ) pObj, true ) );
}

/* int backwardHistoryCount () const */
HB_FUNC( QT_QTEXTBROWSER_BACKWARDHISTORYCOUNT )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retni( ( p )->backwardHistoryCount() );
}

/* void clearHistory () */
HB_FUNC( QT_QTEXTBROWSER_CLEARHISTORY )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      ( p )->clearHistory();
}

/* int forwardHistoryCount () const */
HB_FUNC( QT_QTEXTBROWSER_FORWARDHISTORYCOUNT )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retni( ( p )->forwardHistoryCount() );
}

/* QString historyTitle ( int i ) const */
HB_FUNC( QT_QTEXTBROWSER_HISTORYTITLE )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retstr_utf8( ( p )->historyTitle( hb_parni( 2 ) ).toUtf8().data() );
}

/* QUrl historyUrl ( int i ) const */
HB_FUNC( QT_QTEXTBROWSER_HISTORYURL )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->historyUrl( hb_parni( 2 ) ) ), true ) );
}

/* bool isBackwardAvailable () const */
HB_FUNC( QT_QTEXTBROWSER_ISBACKWARDAVAILABLE )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retl( ( p )->isBackwardAvailable() );
}

/* bool isForwardAvailable () const */
HB_FUNC( QT_QTEXTBROWSER_ISFORWARDAVAILABLE )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retl( ( p )->isForwardAvailable() );
}

/* virtual QVariant loadResource ( int type, const QUrl & name ) */
HB_FUNC( QT_QTEXTBROWSER_LOADRESOURCE )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->loadResource( hb_parni( 2 ), *hbqt_par_QUrl( 3 ) ) ), true ) );
}

/* bool openExternalLinks () const */
HB_FUNC( QT_QTEXTBROWSER_OPENEXTERNALLINKS )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retl( ( p )->openExternalLinks() );
}

/* bool openLinks () const */
HB_FUNC( QT_QTEXTBROWSER_OPENLINKS )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retl( ( p )->openLinks() );
}

/* QStringList searchPaths () const */
HB_FUNC( QT_QTEXTBROWSER_SEARCHPATHS )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->searchPaths() ), true ) );
}

/* void setOpenExternalLinks ( bool open ) */
HB_FUNC( QT_QTEXTBROWSER_SETOPENEXTERNALLINKS )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      ( p )->setOpenExternalLinks( hb_parl( 2 ) );
}

/* void setOpenLinks ( bool open ) */
HB_FUNC( QT_QTEXTBROWSER_SETOPENLINKS )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      ( p )->setOpenLinks( hb_parl( 2 ) );
}

/* void setSearchPaths ( const QStringList & paths ) */
HB_FUNC( QT_QTEXTBROWSER_SETSEARCHPATHS )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      ( p )->setSearchPaths( *hbqt_par_QStringList( 2 ) );
}

/* QUrl source () const */
HB_FUNC( QT_QTEXTBROWSER_SOURCE )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->source() ), true ) );
}

/* virtual void backward () */
HB_FUNC( QT_QTEXTBROWSER_BACKWARD )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      ( p )->backward();
}

/* virtual void forward () */
HB_FUNC( QT_QTEXTBROWSER_FORWARD )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      ( p )->forward();
}

/* virtual void home () */
HB_FUNC( QT_QTEXTBROWSER_HOME )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      ( p )->home();
}

/* virtual void reload () */
HB_FUNC( QT_QTEXTBROWSER_RELOAD )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      ( p )->reload();
}

/* virtual void setSource ( const QUrl & name ) */
HB_FUNC( QT_QTEXTBROWSER_SETSOURCE )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      ( p )->setSource( *hbqt_par_QUrl( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
