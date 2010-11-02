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
 *  Constructed[ 14/14 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include "hbqtgui.h"

#include <QtWebKit/QWebHitTestResult>


/*
 * QWebHitTestResult ()
 * QWebHitTestResult ( const QWebHitTestResult & other )
 * ~QWebHitTestResult ()
 */

typedef struct
{
   QWebHitTestResult * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QWebHitTestResult;

HBQT_GC_FUNC( hbqt_gcRelease_QWebHitTestResult )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QWebHitTestResult * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QWebHitTestResult( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QWebHitTestResult * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWebHitTestResult;
   p->type = HBQT_TYPE_QWebHitTestResult;

   return p;
}

HB_FUNC( QT_QWEBHITTESTRESULT )
{
   QWebHitTestResult * pObj = NULL;

   pObj = new QWebHitTestResult() ;

   hb_retptrGC( hbqt_gcAllocate_QWebHitTestResult( ( void * ) pObj, true ) );
}

/* QString alternateText () const */
HB_FUNC( QT_QWEBHITTESTRESULT_ALTERNATETEXT )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retstr_utf8( ( p )->alternateText().toUtf8().data() );
}

/* QRect boundingRect () const */
HB_FUNC( QT_QWEBHITTESTRESULT_BOUNDINGRECT )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect() ), true ) );
}

/* QWebFrame * frame () const */
HB_FUNC( QT_QWEBHITTESTRESULT_FRAME )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebFrame( ( p )->frame(), false ) );
}

/* QUrl imageUrl () const */
HB_FUNC( QT_QWEBHITTESTRESULT_IMAGEURL )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->imageUrl() ), true ) );
}

/* bool isContentEditable () const */
HB_FUNC( QT_QWEBHITTESTRESULT_ISCONTENTEDITABLE )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retl( ( p )->isContentEditable() );
}

/* bool isContentSelected () const */
HB_FUNC( QT_QWEBHITTESTRESULT_ISCONTENTSELECTED )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retl( ( p )->isContentSelected() );
}

/* bool isNull () const */
HB_FUNC( QT_QWEBHITTESTRESULT_ISNULL )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
}

/* QWebFrame * linkTargetFrame () const */
HB_FUNC( QT_QWEBHITTESTRESULT_LINKTARGETFRAME )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebFrame( ( p )->linkTargetFrame(), false ) );
}

/* QString linkText () const */
HB_FUNC( QT_QWEBHITTESTRESULT_LINKTEXT )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retstr_utf8( ( p )->linkText().toUtf8().data() );
}

/* QUrl linkTitle () const */
HB_FUNC( QT_QWEBHITTESTRESULT_LINKTITLE )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->linkTitle() ), true ) );
}

/* QUrl linkUrl () const */
HB_FUNC( QT_QWEBHITTESTRESULT_LINKURL )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->linkUrl() ), true ) );
}

/* QPixmap pixmap () const */
HB_FUNC( QT_QWEBHITTESTRESULT_PIXMAP )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap() ), true ) );
}

/* QPoint pos () const */
HB_FUNC( QT_QWEBHITTESTRESULT_POS )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos() ), true ) );
}

/* QString title () const */
HB_FUNC( QT_QWEBHITTESTRESULT_TITLE )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retstr_utf8( ( p )->title().toUtf8().data() );
}


#endif /* #if QT_VERSION >= 0x040500 */
