/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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

#include "hbapi.h"
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/


#include <QtWebKit/QWebView>



/*
 * QWebView ( QWidget * parent = 0 )
 * virtual ~QWebView ()
 */
HB_FUNC( QT_QWEBVIEW )
{
   hb_retptr( new QWebView( hbqt_par_QWidget( 1 ) ) );
}

/*
 * bool findText ( const QString & subString, QWebPage::FindFlags options = 0 )
 */
HB_FUNC( QT_QWEBVIEW_FINDTEXT )
{
   hb_retl( hbqt_par_QWebView( 1 )->findText( hbqt_par_QString( 2 ), ( QWebPage::FindFlags ) hb_parni( 3 ) ) );
}

/*
 * QWebHistory * history () const
 */
HB_FUNC( QT_QWEBVIEW_HISTORY )
{
   hb_retptr( ( QWebHistory* ) hbqt_par_QWebView( 1 )->history() );
}

/*
 * QIcon icon () const
 */
HB_FUNC( QT_QWEBVIEW_ICON )
{
   hb_retptr( new QIcon( hbqt_par_QWebView( 1 )->icon() ) );
}

/*
 * bool isModified () const
 */
HB_FUNC( QT_QWEBVIEW_ISMODIFIED )
{
   hb_retl( hbqt_par_QWebView( 1 )->isModified() );
}

/*
 * void load ( const QUrl & url )
 */
HB_FUNC( QT_QWEBVIEW_LOAD )
{
   hbqt_par_QWebView( 1 )->load( *hbqt_par_QUrl( 2 ) );
}

/*
 * void load ( const QNetworkRequest & request, QNetworkAccessManager::Operation operation = QNetworkAccessManager::GetOperation, const QByteArray & body = QByteArray() )
 */
HB_FUNC( QT_QWEBVIEW_LOAD_1 )
{
   hbqt_par_QWebView( 1 )->load( *hbqt_par_QNetworkRequest( 2 ), ( HB_ISNUM( 3 ) ? ( QNetworkAccessManager::Operation ) hb_parni( 3 ) : ( QNetworkAccessManager::Operation ) QNetworkAccessManager::GetOperation ), ( HB_ISNIL( 4 ) ? QByteArray() : *hbqt_par_QByteArray( 4 ) ) );
}

/*
 * QWebPage * page () const
 */
HB_FUNC( QT_QWEBVIEW_PAGE )
{
   hb_retptr( ( QWebPage* ) hbqt_par_QWebView( 1 )->page() );
}

/*
 * QAction * pageAction ( QWebPage::WebAction action ) const
 */
HB_FUNC( QT_QWEBVIEW_PAGEACTION )
{
   hb_retptr( ( QAction* ) hbqt_par_QWebView( 1 )->pageAction( ( QWebPage::WebAction ) hb_parni( 2 ) ) );
}

/*
 * QString selectedText () const
 */
HB_FUNC( QT_QWEBVIEW_SELECTEDTEXT )
{
   hb_retc( hbqt_par_QWebView( 1 )->selectedText().toLatin1().data() );
}

/*
 * void setContent ( const QByteArray & data, const QString & mimeType = QString(), const QUrl & baseUrl = QUrl() )
 */
HB_FUNC( QT_QWEBVIEW_SETCONTENT )
{
   hbqt_par_QWebView( 1 )->setContent( *hbqt_par_QByteArray( 2 ), hbqt_par_QString( 3 ), ( HB_ISNIL( 4 ) ? QUrl() : *hbqt_par_QUrl( 4 ) ) );
}

/*
 * void setHtml ( const QString & html, const QUrl & baseUrl = QUrl() )
 */
HB_FUNC( QT_QWEBVIEW_SETHTML )
{
   hbqt_par_QWebView( 1 )->setHtml( hbqt_par_QString( 2 ), ( HB_ISNIL( 3 ) ? QUrl() : *hbqt_par_QUrl( 3 ) ) );
}

/*
 * void setPage ( QWebPage * page )
 */
HB_FUNC( QT_QWEBVIEW_SETPAGE )
{
   hbqt_par_QWebView( 1 )->setPage( hbqt_par_QWebPage( 2 ) );
}

/*
 * void setTextSizeMultiplier ( qreal factor )
 */
HB_FUNC( QT_QWEBVIEW_SETTEXTSIZEMULTIPLIER )
{
   hbqt_par_QWebView( 1 )->setTextSizeMultiplier( hb_parnd( 2 ) );
}

/*
 * void setUrl ( const QUrl & url )
 */
HB_FUNC( QT_QWEBVIEW_SETURL )
{
   hbqt_par_QWebView( 1 )->setUrl( *hbqt_par_QUrl( 2 ) );
}

/*
 * void setZoomFactor ( qreal factor )
 */
HB_FUNC( QT_QWEBVIEW_SETZOOMFACTOR )
{
   hbqt_par_QWebView( 1 )->setZoomFactor( hb_parnd( 2 ) );
}

/*
 * QWebSettings * settings () const
 */
HB_FUNC( QT_QWEBVIEW_SETTINGS )
{
   hb_retptr( ( QWebSettings* ) hbqt_par_QWebView( 1 )->settings() );
}

/*
 * qreal textSizeMultiplier () const
 */
HB_FUNC( QT_QWEBVIEW_TEXTSIZEMULTIPLIER )
{
   hb_retnd( hbqt_par_QWebView( 1 )->textSizeMultiplier() );
}

/*
 * QString title () const
 */
HB_FUNC( QT_QWEBVIEW_TITLE )
{
   hb_retc( hbqt_par_QWebView( 1 )->title().toLatin1().data() );
}

/*
 * void triggerPageAction ( QWebPage::WebAction action, bool checked = false )
 */
HB_FUNC( QT_QWEBVIEW_TRIGGERPAGEACTION )
{
   hbqt_par_QWebView( 1 )->triggerPageAction( ( QWebPage::WebAction ) hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * QUrl url () const
 */
HB_FUNC( QT_QWEBVIEW_URL )
{
   hb_retptr( new QUrl( hbqt_par_QWebView( 1 )->url() ) );
}

/*
 * qreal zoomFactor () const
 */
HB_FUNC( QT_QWEBVIEW_ZOOMFACTOR )
{
   hb_retnd( hbqt_par_QWebView( 1 )->zoomFactor() );
}

/*
 * void back ()
 */
HB_FUNC( QT_QWEBVIEW_BACK )
{
   hbqt_par_QWebView( 1 )->back();
}

/*
 * void forward ()
 */
HB_FUNC( QT_QWEBVIEW_FORWARD )
{
   hbqt_par_QWebView( 1 )->forward();
}

/*
 * void print ( QPrinter * printer ) const
 */
HB_FUNC( QT_QWEBVIEW_PRINT )
{
   hbqt_par_QWebView( 1 )->print( hbqt_par_QPrinter( 2 ) );
}

/*
 * void reload ()
 */
HB_FUNC( QT_QWEBVIEW_RELOAD )
{
   hbqt_par_QWebView( 1 )->reload();
}

/*
 * void stop ()
 */
HB_FUNC( QT_QWEBVIEW_STOP )
{
   hbqt_par_QWebView( 1 )->stop();
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

