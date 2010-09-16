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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://harbour-project.org
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

#include "hbqtcore.h"
#include "hbqtwebkit.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 38/40 [ 95.00% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QMultiMap<QString, QString> metaData () const
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  // void load ( const QWebNetworkRequest & req )
 */

#include <QtCore/QPointer>

#include "hbqtgui.h"

#include <QtWebKit/QWebFrame>
#include <QtWebKit/QWebSecurityOrigin>

/*
 *
 */

typedef struct
{
   QPointer< QWebFrame > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QWebFrame;

HBQT_GC_FUNC( hbqt_gcRelease_QWebFrame )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QWebFrame( void * pObj, bool bNew )
{
   HBQT_GC_T_QWebFrame * p = ( HBQT_GC_T_QWebFrame * ) hb_gcAllocate( sizeof( HBQT_GC_T_QWebFrame ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QWebFrame >( ( QWebFrame * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWebFrame;
   p->type = HBQT_TYPE_QWebFrame;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QWebFrame  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QWebFrame", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QWEBFRAME )
{
   // hb_retptr( ( QWebFrame* ) new QWebFrame() );
}

/*
 * void addToJavaScriptWindowObject ( const QString & name, QObject * object )
 */
HB_FUNC( QT_QWEBFRAME_ADDTOJAVASCRIPTWINDOWOBJECT )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      void * pText;
      ( p )->addToJavaScriptWindowObject( hb_parstr_utf8( 2, &pText, NULL ), hbqt_par_QObject( 3 ) );
      hb_strfree( pText );
   }
}

/*
 * void addToJavaScriptWindowObject ( const QString & name, QObject * object, QScriptEngine::ValueOwnership own )
 */
HB_FUNC( QT_QWEBFRAME_ADDTOJAVASCRIPTWINDOWOBJECT_1 )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      void * pText;
      ( p )->addToJavaScriptWindowObject( hb_parstr_utf8( 2, &pText, NULL ), hbqt_par_QObject( 3 ), ( QScriptEngine::ValueOwnership ) hb_parni( 4 ) );
      hb_strfree( pText );
   }
}

/*
 * QList<QWebFrame *> childFrames () const
 */
HB_FUNC( QT_QWEBFRAME_CHILDFRAMES )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QWebFrame *>( ( p )->childFrames() ), true ) );
   }
}

/*
 * QSize contentsSize () const
 */
HB_FUNC( QT_QWEBFRAME_CONTENTSSIZE )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->contentsSize() ), true ) );
   }
}

/*
 * QString frameName () const
 */
HB_FUNC( QT_QWEBFRAME_FRAMENAME )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->frameName().toUtf8().data() );
   }
}

/*
 * QRect geometry () const
 */
HB_FUNC( QT_QWEBFRAME_GEOMETRY )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->geometry() ), true ) );
   }
}

/*
 * QWebHitTestResult hitTestContent ( const QPoint & pos ) const
 */
HB_FUNC( QT_QWEBFRAME_HITTESTCONTENT )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWebHitTestResult( new QWebHitTestResult( ( p )->hitTestContent( *hbqt_par_QPoint( 2 ) ) ), true ) );
   }
}

/*
 * QIcon icon () const
 */
HB_FUNC( QT_QWEBFRAME_ICON )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) );
   }
}

/*
 * void load ( const QUrl & url )
 */
HB_FUNC( QT_QWEBFRAME_LOAD )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      ( p )->load( *hbqt_par_QUrl( 2 ) );
   }
}

/*
 * QWebPage * page () const
 */
HB_FUNC( QT_QWEBFRAME_PAGE )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWebPage( ( p )->page(), false ) );
   }
}

/*
 * QWebFrame * parentFrame () const
 */
HB_FUNC( QT_QWEBFRAME_PARENTFRAME )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWebFrame( ( p )->parentFrame(), false ) );
   }
}

/*
 * QPoint pos () const
 */
HB_FUNC( QT_QWEBFRAME_POS )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos() ), true ) );
   }
}

/*
 * void render ( QPainter * painter, const QRegion & clip )
 */
HB_FUNC( QT_QWEBFRAME_RENDER )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      ( p )->render( hbqt_par_QPainter( 2 ), *hbqt_par_QRegion( 3 ) );
   }
}

/*
 * void render ( QPainter * painter )
 */
HB_FUNC( QT_QWEBFRAME_RENDER_1 )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      ( p )->render( hbqt_par_QPainter( 2 ) );
   }
}

/*
 * QString renderTreeDump () const
 */
HB_FUNC( QT_QWEBFRAME_RENDERTREEDUMP )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->renderTreeDump().toUtf8().data() );
   }
}

/*
 * void scroll ( int dx, int dy )
 */
HB_FUNC( QT_QWEBFRAME_SCROLL )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      ( p )->scroll( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * int scrollBarMaximum ( Qt::Orientation orientation ) const
 */
HB_FUNC( QT_QWEBFRAME_SCROLLBARMAXIMUM )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retni( ( p )->scrollBarMaximum( ( Qt::Orientation ) hb_parni( 2 ) ) );
   }
}

/*
 * int scrollBarMinimum ( Qt::Orientation orientation ) const
 */
HB_FUNC( QT_QWEBFRAME_SCROLLBARMINIMUM )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retni( ( p )->scrollBarMinimum( ( Qt::Orientation ) hb_parni( 2 ) ) );
   }
}

/*
 * Qt::ScrollBarPolicy scrollBarPolicy ( Qt::Orientation orientation ) const
 */
HB_FUNC( QT_QWEBFRAME_SCROLLBARPOLICY )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retni( ( Qt::ScrollBarPolicy ) ( p )->scrollBarPolicy( ( Qt::Orientation ) hb_parni( 2 ) ) );
   }
}

/*
 * int scrollBarValue ( Qt::Orientation orientation ) const
 */
HB_FUNC( QT_QWEBFRAME_SCROLLBARVALUE )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retni( ( p )->scrollBarValue( ( Qt::Orientation ) hb_parni( 2 ) ) );
   }
}

/*
 * QPoint scrollPosition () const
 */
HB_FUNC( QT_QWEBFRAME_SCROLLPOSITION )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->scrollPosition() ), true ) );
   }
}

/*
 * QWebSecurityOrigin securityOrigin () const
 */
HB_FUNC( QT_QWEBFRAME_SECURITYORIGIN )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWebSecurityOrigin( new QWebSecurityOrigin( ( p )->securityOrigin() ), true ) );
   }
}

/*
 * void setContent ( const QByteArray & data, const QString & mimeType = QString(), const QUrl & baseUrl = QUrl() )
 */
HB_FUNC( QT_QWEBFRAME_SETCONTENT )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      void * pText;
      ( p )->setContent( *hbqt_par_QByteArray( 2 ), hb_parstr_utf8( 3, &pText, NULL ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QUrl( 4 ) : QUrl() ) );
      hb_strfree( pText );
   }
}

/*
 * void setHtml ( const QString & html, const QUrl & baseUrl = QUrl() )
 */
HB_FUNC( QT_QWEBFRAME_SETHTML )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      void * pText;
      ( p )->setHtml( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QUrl( 3 ) : QUrl() ) );
      hb_strfree( pText );
   }
}

/*
 * void setScrollBarPolicy ( Qt::Orientation orientation, Qt::ScrollBarPolicy policy )
 */
HB_FUNC( QT_QWEBFRAME_SETSCROLLBARPOLICY )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      ( p )->setScrollBarPolicy( ( Qt::Orientation ) hb_parni( 2 ), ( Qt::ScrollBarPolicy ) hb_parni( 3 ) );
   }
}

/*
 * void setScrollBarValue ( Qt::Orientation orientation, int value )
 */
HB_FUNC( QT_QWEBFRAME_SETSCROLLBARVALUE )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      ( p )->setScrollBarValue( ( Qt::Orientation ) hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void setScrollPosition ( const QPoint & pos )
 */
HB_FUNC( QT_QWEBFRAME_SETSCROLLPOSITION )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      ( p )->setScrollPosition( *hbqt_par_QPoint( 2 ) );
   }
}

/*
 * void setTextSizeMultiplier ( qreal factor )
 */
HB_FUNC( QT_QWEBFRAME_SETTEXTSIZEMULTIPLIER )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      ( p )->setTextSizeMultiplier( hb_parnd( 2 ) );
   }
}

/*
 * void setUrl ( const QUrl & url )
 */
HB_FUNC( QT_QWEBFRAME_SETURL )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      ( p )->setUrl( *hbqt_par_QUrl( 2 ) );
   }
}

/*
 * void setZoomFactor ( qreal factor )
 */
HB_FUNC( QT_QWEBFRAME_SETZOOMFACTOR )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      ( p )->setZoomFactor( hb_parnd( 2 ) );
   }
}

/*
 * qreal textSizeMultiplier () const
 */
HB_FUNC( QT_QWEBFRAME_TEXTSIZEMULTIPLIER )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retnd( ( p )->textSizeMultiplier() );
   }
}

/*
 * QString title () const
 */
HB_FUNC( QT_QWEBFRAME_TITLE )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->title().toUtf8().data() );
   }
}

/*
 * QString toHtml () const
 */
HB_FUNC( QT_QWEBFRAME_TOHTML )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->toHtml().toUtf8().data() );
   }
}

/*
 * QString toPlainText () const
 */
HB_FUNC( QT_QWEBFRAME_TOPLAINTEXT )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->toPlainText().toUtf8().data() );
   }
}

/*
 * QUrl url () const
 */
HB_FUNC( QT_QWEBFRAME_URL )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->url() ), true ) );
   }
}

/*
 * qreal zoomFactor () const
 */
HB_FUNC( QT_QWEBFRAME_ZOOMFACTOR )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      hb_retnd( ( p )->zoomFactor() );
   }
}

/*
 * QVariant evaluateJavaScript ( const QString & scriptSource )
 */
HB_FUNC( QT_QWEBFRAME_EVALUATEJAVASCRIPT )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->evaluateJavaScript( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * void print ( QPrinter * printer ) const
 */
HB_FUNC( QT_QWEBFRAME_PRINT )
{
   QWebFrame * p = hbqt_par_QWebFrame( 1 );
   if( p )
   {
      ( p )->print( hbqt_par_QPrinter( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
