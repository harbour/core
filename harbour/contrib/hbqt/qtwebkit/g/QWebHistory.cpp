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
 *  Constructed[ 17/17 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtWebKit/QWebHistory>


/*
 * QWebHistory ()
 */

typedef struct
{
   QWebHistory * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QWebHistory;

HBQT_GC_FUNC( hbqt_gcRelease_QWebHistory )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QWebHistory( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QWebHistory * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWebHistory;
   p->type = HBQT_TYPE_QWebHistory;

   return p;
}

HB_FUNC( QT_QWEBHISTORY )
{
   //__HB_RETPTRGC__( new QWebHistory () );
}

/* void back () */
HB_FUNC( QT_QWEBHISTORY_BACK )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      ( p )->back();
}

/* QWebHistoryItem backItem () const */
HB_FUNC( QT_QWEBHISTORY_BACKITEM )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebHistoryItem( new QWebHistoryItem( ( p )->backItem() ), true ) );
}

/* QList<QWebHistoryItem> backItems ( int maxItems ) const */
HB_FUNC( QT_QWEBHISTORY_BACKITEMS )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QWebHistoryItem>( ( p )->backItems( hb_parni( 2 ) ) ), true ) );
}

/* bool canGoBack () const */
HB_FUNC( QT_QWEBHISTORY_CANGOBACK )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retl( ( p )->canGoBack() );
}

/* bool canGoForward () const */
HB_FUNC( QT_QWEBHISTORY_CANGOFORWARD )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retl( ( p )->canGoForward() );
}

/* void clear () */
HB_FUNC( QT_QWEBHISTORY_CLEAR )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      ( p )->clear();
}

/* int count () const */
HB_FUNC( QT_QWEBHISTORY_COUNT )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retni( ( p )->count() );
}

/* QWebHistoryItem currentItem () const */
HB_FUNC( QT_QWEBHISTORY_CURRENTITEM )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebHistoryItem( new QWebHistoryItem( ( p )->currentItem() ), true ) );
}

/* int currentItemIndex () const */
HB_FUNC( QT_QWEBHISTORY_CURRENTITEMINDEX )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retni( ( p )->currentItemIndex() );
}

/* void forward () */
HB_FUNC( QT_QWEBHISTORY_FORWARD )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      ( p )->forward();
}

/* QWebHistoryItem forwardItem () const */
HB_FUNC( QT_QWEBHISTORY_FORWARDITEM )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebHistoryItem( new QWebHistoryItem( ( p )->forwardItem() ), true ) );
}

/* QList<QWebHistoryItem> forwardItems ( int maxItems ) const */
HB_FUNC( QT_QWEBHISTORY_FORWARDITEMS )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QWebHistoryItem>( ( p )->forwardItems( hb_parni( 2 ) ) ), true ) );
}

/* void goToItem ( const QWebHistoryItem & item ) */
HB_FUNC( QT_QWEBHISTORY_GOTOITEM )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      ( p )->goToItem( *hbqt_par_QWebHistoryItem( 2 ) );
}

/* QWebHistoryItem itemAt ( int i ) const */
HB_FUNC( QT_QWEBHISTORY_ITEMAT )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebHistoryItem( new QWebHistoryItem( ( p )->itemAt( hb_parni( 2 ) ) ), true ) );
}

/* QList<QWebHistoryItem> items () const */
HB_FUNC( QT_QWEBHISTORY_ITEMS )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QWebHistoryItem>( ( p )->items() ), true ) );
}

/* int maximumItemCount () const */
HB_FUNC( QT_QWEBHISTORY_MAXIMUMITEMCOUNT )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retni( ( p )->maximumItemCount() );
}

/* void setMaximumItemCount ( int count ) */
HB_FUNC( QT_QWEBHISTORY_SETMAXIMUMITEMCOUNT )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      ( p )->setMaximumItemCount( hb_parni( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
