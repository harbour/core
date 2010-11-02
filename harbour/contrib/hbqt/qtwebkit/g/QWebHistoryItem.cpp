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
 *  Constructed[ 8/8 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include "hbqtgui.h"

#include <QtWebKit/QWebHistoryItem>
#include <QtCore/QVariant>


/*
 * QWebHistoryItem ( const QWebHistoryItem & other )
 * ~QWebHistoryItem ()
 */

typedef struct
{
   QWebHistoryItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QWebHistoryItem;

HBQT_GC_FUNC( hbqt_gcRelease_QWebHistoryItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QWebHistoryItem * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QWebHistoryItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QWebHistoryItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWebHistoryItem;
   p->type = HBQT_TYPE_QWebHistoryItem;

   return p;
}

HB_FUNC( QT_QWEBHISTORYITEM )
{
   QWebHistoryItem * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QWebHistoryItem( *hbqt_par_QWebHistoryItem( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QWebHistoryItem( ( void * ) pObj, true ) );
}

/* QIcon icon () const */
HB_FUNC( QT_QWEBHISTORYITEM_ICON )
{
   QWebHistoryItem * p = hbqt_par_QWebHistoryItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) );
}

/* bool isValid () const */
HB_FUNC( QT_QWEBHISTORYITEM_ISVALID )
{
   QWebHistoryItem * p = hbqt_par_QWebHistoryItem( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* QDateTime lastVisited () const */
HB_FUNC( QT_QWEBHISTORYITEM_LASTVISITED )
{
   QWebHistoryItem * p = hbqt_par_QWebHistoryItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->lastVisited() ), true ) );
}

/* QUrl originalUrl () const */
HB_FUNC( QT_QWEBHISTORYITEM_ORIGINALURL )
{
   QWebHistoryItem * p = hbqt_par_QWebHistoryItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->originalUrl() ), true ) );
}

/* void setUserData ( const QVariant & userData ) */
HB_FUNC( QT_QWEBHISTORYITEM_SETUSERDATA )
{
   QWebHistoryItem * p = hbqt_par_QWebHistoryItem( 1 );
   if( p )
      ( p )->setUserData( *hbqt_par_QVariant( 2 ) );
}

/* QString title () const */
HB_FUNC( QT_QWEBHISTORYITEM_TITLE )
{
   QWebHistoryItem * p = hbqt_par_QWebHistoryItem( 1 );
   if( p )
      hb_retstr_utf8( ( p )->title().toUtf8().data() );
}

/* QUrl url () const */
HB_FUNC( QT_QWEBHISTORYITEM_URL )
{
   QWebHistoryItem * p = hbqt_par_QWebHistoryItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->url() ), true ) );
}

/* QVariant userData () const */
HB_FUNC( QT_QWEBHISTORYITEM_USERDATA )
{
   QWebHistoryItem * p = hbqt_par_QWebHistoryItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->userData() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
