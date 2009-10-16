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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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

#include "hbapi.h"
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 37/40 [ 92.50% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QWebFrame *> childFrames () const
 *  QMultiMap<QString, QString> metaData () const
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  // void load ( const QWebNetworkRequest & req )
 */

#include <QtCore/QPointer>

#include <QtWebKit/QWebFrame>
#include <QtWebKit/QWebSecurityOrigin>

/*
 *
 */

HB_FUNC( QT_QWEBFRAME )
{
}
/*
 * void addToJavaScriptWindowObject ( const QString & name, QObject * object )
 */
HB_FUNC( QT_QWEBFRAME_ADDTOJAVASCRIPTWINDOWOBJECT )
{
   hbqt_par_QWebFrame( 1 )->addToJavaScriptWindowObject( hbqt_par_QString( 2 ), hbqt_par_QObject( 3 ) );
}

/*
 * void addToJavaScriptWindowObject ( const QString & name, QObject * object, QScriptEngine::ValueOwnership own )
 */
HB_FUNC( QT_QWEBFRAME_ADDTOJAVASCRIPTWINDOWOBJECT_1 )
{
   hbqt_par_QWebFrame( 1 )->addToJavaScriptWindowObject( hbqt_par_QString( 2 ), hbqt_par_QObject( 3 ), ( QScriptEngine::ValueOwnership ) hb_parni( 4 ) );
}

/*
 * QSize contentsSize () const
 */
HB_FUNC( QT_QWEBFRAME_CONTENTSSIZE )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QSize( hbqt_par_QWebFrame( 1 )->contentsSize() ) ) );
}

/*
 * QString frameName () const
 */
HB_FUNC( QT_QWEBFRAME_FRAMENAME )
{
   hb_retc( hbqt_par_QWebFrame( 1 )->frameName().toAscii().data() );
}

/*
 * QRect geometry () const
 */
HB_FUNC( QT_QWEBFRAME_GEOMETRY )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QRect( hbqt_par_QWebFrame( 1 )->geometry() ) ) );
}

/*
 * QWebHitTestResult hitTestContent ( const QPoint & pos ) const
 */
HB_FUNC( QT_QWEBFRAME_HITTESTCONTENT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QWebHitTestResult( hbqt_par_QWebFrame( 1 )->hitTestContent( *hbqt_par_QPoint( 2 ) ) ) ) );
}

/*
 * QIcon icon () const
 */
HB_FUNC( QT_QWEBFRAME_ICON )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QIcon( hbqt_par_QWebFrame( 1 )->icon() ) ) );
}

/*
 * void load ( const QUrl & url )
 */
HB_FUNC( QT_QWEBFRAME_LOAD )
{
   hbqt_par_QWebFrame( 1 )->load( *hbqt_par_QUrl( 2 ) );
}

/*
 * QWebPage * page () const
 */
HB_FUNC( QT_QWEBFRAME_PAGE )
{
   hb_retptr( ( QWebPage* ) hbqt_par_QWebFrame( 1 )->page() );
}

/*
 * QWebFrame * parentFrame () const
 */
HB_FUNC( QT_QWEBFRAME_PARENTFRAME )
{
   hb_retptr( ( QWebFrame* ) hbqt_par_QWebFrame( 1 )->parentFrame() );
}

/*
 * QPoint pos () const
 */
HB_FUNC( QT_QWEBFRAME_POS )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QPoint( hbqt_par_QWebFrame( 1 )->pos() ) ) );
}

/*
 * void render ( QPainter * painter, const QRegion & clip )
 */
HB_FUNC( QT_QWEBFRAME_RENDER )
{
   hbqt_par_QWebFrame( 1 )->render( hbqt_par_QPainter( 2 ), *hbqt_par_QRegion( 3 ) );
}

/*
 * void render ( QPainter * painter )
 */
HB_FUNC( QT_QWEBFRAME_RENDER_1 )
{
   hbqt_par_QWebFrame( 1 )->render( hbqt_par_QPainter( 2 ) );
}

/*
 * QString renderTreeDump () const
 */
HB_FUNC( QT_QWEBFRAME_RENDERTREEDUMP )
{
   hb_retc( hbqt_par_QWebFrame( 1 )->renderTreeDump().toAscii().data() );
}

/*
 * void scroll ( int dx, int dy )
 */
HB_FUNC( QT_QWEBFRAME_SCROLL )
{
   hbqt_par_QWebFrame( 1 )->scroll( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * int scrollBarMaximum ( Qt::Orientation orientation ) const
 */
HB_FUNC( QT_QWEBFRAME_SCROLLBARMAXIMUM )
{
   hb_retni( hbqt_par_QWebFrame( 1 )->scrollBarMaximum( ( Qt::Orientation ) hb_parni( 2 ) ) );
}

/*
 * int scrollBarMinimum ( Qt::Orientation orientation ) const
 */
HB_FUNC( QT_QWEBFRAME_SCROLLBARMINIMUM )
{
   hb_retni( hbqt_par_QWebFrame( 1 )->scrollBarMinimum( ( Qt::Orientation ) hb_parni( 2 ) ) );
}

/*
 * Qt::ScrollBarPolicy scrollBarPolicy ( Qt::Orientation orientation ) const
 */
HB_FUNC( QT_QWEBFRAME_SCROLLBARPOLICY )
{
   hb_retni( ( Qt::ScrollBarPolicy ) hbqt_par_QWebFrame( 1 )->scrollBarPolicy( ( Qt::Orientation ) hb_parni( 2 ) ) );
}

/*
 * int scrollBarValue ( Qt::Orientation orientation ) const
 */
HB_FUNC( QT_QWEBFRAME_SCROLLBARVALUE )
{
   hb_retni( hbqt_par_QWebFrame( 1 )->scrollBarValue( ( Qt::Orientation ) hb_parni( 2 ) ) );
}

/*
 * QPoint scrollPosition () const
 */
HB_FUNC( QT_QWEBFRAME_SCROLLPOSITION )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QPoint( hbqt_par_QWebFrame( 1 )->scrollPosition() ) ) );
}

/*
 * QWebSecurityOrigin securityOrigin () const
 */
HB_FUNC( QT_QWEBFRAME_SECURITYORIGIN )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QWebSecurityOrigin( hbqt_par_QWebFrame( 1 )->securityOrigin() ) ) );
}

/*
 * void setContent ( const QByteArray & data, const QString & mimeType = QString(), const QUrl & baseUrl = QUrl() )
 */
HB_FUNC( QT_QWEBFRAME_SETCONTENT )
{
   hbqt_par_QWebFrame( 1 )->setContent( *hbqt_par_QByteArray( 2 ), hbqt_par_QString( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QUrl( 4 ) : QUrl() ) );
}

/*
 * void setHtml ( const QString & html, const QUrl & baseUrl = QUrl() )
 */
HB_FUNC( QT_QWEBFRAME_SETHTML )
{
   hbqt_par_QWebFrame( 1 )->setHtml( hbqt_par_QString( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QUrl( 3 ) : QUrl() ) );
}

/*
 * void setScrollBarPolicy ( Qt::Orientation orientation, Qt::ScrollBarPolicy policy )
 */
HB_FUNC( QT_QWEBFRAME_SETSCROLLBARPOLICY )
{
   hbqt_par_QWebFrame( 1 )->setScrollBarPolicy( ( Qt::Orientation ) hb_parni( 2 ), ( Qt::ScrollBarPolicy ) hb_parni( 3 ) );
}

/*
 * void setScrollBarValue ( Qt::Orientation orientation, int value )
 */
HB_FUNC( QT_QWEBFRAME_SETSCROLLBARVALUE )
{
   hbqt_par_QWebFrame( 1 )->setScrollBarValue( ( Qt::Orientation ) hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setScrollPosition ( const QPoint & pos )
 */
HB_FUNC( QT_QWEBFRAME_SETSCROLLPOSITION )
{
   hbqt_par_QWebFrame( 1 )->setScrollPosition( *hbqt_par_QPoint( 2 ) );
}

/*
 * void setTextSizeMultiplier ( qreal factor )
 */
HB_FUNC( QT_QWEBFRAME_SETTEXTSIZEMULTIPLIER )
{
   hbqt_par_QWebFrame( 1 )->setTextSizeMultiplier( hb_parnd( 2 ) );
}

/*
 * void setUrl ( const QUrl & url )
 */
HB_FUNC( QT_QWEBFRAME_SETURL )
{
   hbqt_par_QWebFrame( 1 )->setUrl( *hbqt_par_QUrl( 2 ) );
}

/*
 * void setZoomFactor ( qreal factor )
 */
HB_FUNC( QT_QWEBFRAME_SETZOOMFACTOR )
{
   hbqt_par_QWebFrame( 1 )->setZoomFactor( hb_parnd( 2 ) );
}

/*
 * qreal textSizeMultiplier () const
 */
HB_FUNC( QT_QWEBFRAME_TEXTSIZEMULTIPLIER )
{
   hb_retnd( hbqt_par_QWebFrame( 1 )->textSizeMultiplier() );
}

/*
 * QString title () const
 */
HB_FUNC( QT_QWEBFRAME_TITLE )
{
   hb_retc( hbqt_par_QWebFrame( 1 )->title().toAscii().data() );
}

/*
 * QString toHtml () const
 */
HB_FUNC( QT_QWEBFRAME_TOHTML )
{
   hb_retc( hbqt_par_QWebFrame( 1 )->toHtml().toAscii().data() );
}

/*
 * QString toPlainText () const
 */
HB_FUNC( QT_QWEBFRAME_TOPLAINTEXT )
{
   hb_retc( hbqt_par_QWebFrame( 1 )->toPlainText().toAscii().data() );
}

/*
 * QUrl url () const
 */
HB_FUNC( QT_QWEBFRAME_URL )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QUrl( hbqt_par_QWebFrame( 1 )->url() ) ) );
}

/*
 * qreal zoomFactor () const
 */
HB_FUNC( QT_QWEBFRAME_ZOOMFACTOR )
{
   hb_retnd( hbqt_par_QWebFrame( 1 )->zoomFactor() );
}

/*
 * QVariant evaluateJavaScript ( const QString & scriptSource )
 */
HB_FUNC( QT_QWEBFRAME_EVALUATEJAVASCRIPT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QVariant( hbqt_par_QWebFrame( 1 )->evaluateJavaScript( hbqt_par_QString( 2 ) ) ) ) );
}

/*
 * void print ( QPrinter * printer ) const
 */
HB_FUNC( QT_QWEBFRAME_PRINT )
{
   hbqt_par_QWebFrame( 1 )->print( hbqt_par_QPrinter( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
