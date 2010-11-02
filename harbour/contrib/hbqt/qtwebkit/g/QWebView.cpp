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
 *  Constructed[ 26/26 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include "hbqtgui.h"
#include "hbqtnetwork.h"

#include <QtWebKit/QWebView>


/*
 * QWebView ( QWidget * parent = 0 )
 * virtual ~QWebView ()
 */

typedef struct
{
   QPointer< QWebView > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QWebView;

HBQT_GC_FUNC( hbqt_gcRelease_QWebView )
{
   QWebView  * ph = NULL;
   HBQT_GC_T_QWebView * p = ( HBQT_GC_T_QWebView * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( p->ph );
            p->ph = NULL;
         }
         else
            p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QWebView( void * pObj, bool bNew )
{
   HBQT_GC_T_QWebView * p = ( HBQT_GC_T_QWebView * ) hb_gcAllocate( sizeof( HBQT_GC_T_QWebView ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QWebView >( ( QWebView * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWebView;
   p->type = HBQT_TYPE_QWebView;

   return p;
}

HB_FUNC( QT_QWEBVIEW )
{
   QWebView * pObj = NULL;

   pObj = new QWebView( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QWebView( ( void * ) pObj, true ) );
}

/* bool findText ( const QString & subString, QWebPage::FindFlags options = 0 ) */
HB_FUNC( QT_QWEBVIEW_FINDTEXT )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->findText( hb_parstr_utf8( 2, &pText, NULL ), ( QWebPage::FindFlags ) hb_parni( 3 ) ) );
      hb_strfree( pText );
   }
}

/* QWebHistory * history () const */
HB_FUNC( QT_QWEBVIEW_HISTORY )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebHistory( ( p )->history(), false ) );
}

/* QIcon icon () const */
HB_FUNC( QT_QWEBVIEW_ICON )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) );
}

/* bool isModified () const */
HB_FUNC( QT_QWEBVIEW_ISMODIFIED )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      hb_retl( ( p )->isModified() );
}

/* void load ( const QUrl & url ) */
HB_FUNC( QT_QWEBVIEW_LOAD )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      ( p )->load( *hbqt_par_QUrl( 2 ) );
}

/* void load ( const QNetworkRequest & request, QNetworkAccessManager::Operation operation = QNetworkAccessManager::GetOperation, const QByteArray & body = QByteArray() ) */
HB_FUNC( QT_QWEBVIEW_LOAD_1 )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      ( p )->load( *hbqt_par_QNetworkRequest( 2 ), ( HB_ISNUM( 3 ) ? ( QNetworkAccessManager::Operation ) hb_parni( 3 ) : ( QNetworkAccessManager::Operation ) QNetworkAccessManager::GetOperation ), ( HB_ISOBJECT( 4 ) ? *hbqt_par_QByteArray( 4 ) : QByteArray() ) );
}

/* QWebPage * page () const */
HB_FUNC( QT_QWEBVIEW_PAGE )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebPage( ( p )->page(), false ) );
}

/* QAction * pageAction ( QWebPage::WebAction action ) const */
HB_FUNC( QT_QWEBVIEW_PAGEACTION )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->pageAction( ( QWebPage::WebAction ) hb_parni( 2 ) ), false ) );
}

/* QString selectedText () const */
HB_FUNC( QT_QWEBVIEW_SELECTEDTEXT )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      hb_retstr_utf8( ( p )->selectedText().toUtf8().data() );
}

/* void setContent ( const QByteArray & data, const QString & mimeType = QString(), const QUrl & baseUrl = QUrl() ) */
HB_FUNC( QT_QWEBVIEW_SETCONTENT )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
   {
      void * pText;
      ( p )->setContent( *hbqt_par_QByteArray( 2 ), hb_parstr_utf8( 3, &pText, NULL ), ( HB_ISOBJECT( 4 ) ? *hbqt_par_QUrl( 4 ) : QUrl() ) );
      hb_strfree( pText );
   }
}

/* void setHtml ( const QString & html, const QUrl & baseUrl = QUrl() ) */
HB_FUNC( QT_QWEBVIEW_SETHTML )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
   {
      void * pText;
      ( p )->setHtml( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISOBJECT( 3 ) ? *hbqt_par_QUrl( 3 ) : QUrl() ) );
      hb_strfree( pText );
   }
}

/* void setPage ( QWebPage * page ) */
HB_FUNC( QT_QWEBVIEW_SETPAGE )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      ( p )->setPage( hbqt_par_QWebPage( 2 ) );
}

/* void setTextSizeMultiplier ( qreal factor ) */
HB_FUNC( QT_QWEBVIEW_SETTEXTSIZEMULTIPLIER )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      ( p )->setTextSizeMultiplier( hb_parnd( 2 ) );
}

/* void setUrl ( const QUrl & url ) */
HB_FUNC( QT_QWEBVIEW_SETURL )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      ( p )->setUrl( *hbqt_par_QUrl( 2 ) );
}

/* void setZoomFactor ( qreal factor ) */
HB_FUNC( QT_QWEBVIEW_SETZOOMFACTOR )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      ( p )->setZoomFactor( hb_parnd( 2 ) );
}

/* QWebSettings * settings () const */
HB_FUNC( QT_QWEBVIEW_SETTINGS )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebSettings( ( p )->settings(), false ) );
}

/* qreal textSizeMultiplier () const */
HB_FUNC( QT_QWEBVIEW_TEXTSIZEMULTIPLIER )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      hb_retnd( ( p )->textSizeMultiplier() );
}

/* QString title () const */
HB_FUNC( QT_QWEBVIEW_TITLE )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      hb_retstr_utf8( ( p )->title().toUtf8().data() );
}

/* void triggerPageAction ( QWebPage::WebAction action, bool checked = false ) */
HB_FUNC( QT_QWEBVIEW_TRIGGERPAGEACTION )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      ( p )->triggerPageAction( ( QWebPage::WebAction ) hb_parni( 2 ), hb_parl( 3 ) );
}

/* QUrl url () const */
HB_FUNC( QT_QWEBVIEW_URL )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->url() ), true ) );
}

/* qreal zoomFactor () const */
HB_FUNC( QT_QWEBVIEW_ZOOMFACTOR )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      hb_retnd( ( p )->zoomFactor() );
}

/* void back () */
HB_FUNC( QT_QWEBVIEW_BACK )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      ( p )->back();
}

/* void forward () */
HB_FUNC( QT_QWEBVIEW_FORWARD )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      ( p )->forward();
}

/* void print ( QPrinter * printer ) const */
HB_FUNC( QT_QWEBVIEW_PRINT )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      ( p )->print( hbqt_par_QPrinter( 2 ) );
}

/* void reload () */
HB_FUNC( QT_QWEBVIEW_RELOAD )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      ( p )->reload();
}

/* void stop () */
HB_FUNC( QT_QWEBVIEW_STOP )
{
   QWebView * p = hbqt_par_QWebView( 1 );
   if( p )
      ( p )->stop();
}


#endif /* #if QT_VERSION >= 0x040500 */
