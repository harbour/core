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
 *  enum Extension { ChooseMultipleFilesExtension }
 *  enum FindFlag { FindBackward, FindCaseSensitively, FindWrapsAroundDocument }
 *  enum LinkDelegationPolicy { DontDelegateLinks, DelegateExternalLinks, DelegateAllLinks }
 *  enum NavigationType { NavigationTypeLinkClicked, NavigationTypeFormSubmitted, NavigationTypeBackOrForward, NavigationTypeReload, NavigationTypeFormResubmitted, NavigationTypeOther }
 *  enum WebAction { NoWebAction, OpenLink, OpenLinkInNewWindow, OpenFrameInNewWindow, ..., SelectAll }
 *  enum WebWindowType { WebBrowserWindow, WebModalDialog }
 *  flags FindFlags
 */


#include <QtWebKit/QWebPage>
#include <QtCore/QVariant>


/*
 * QWebPage ( QObject * parent = 0 )
 * ~QWebPage ()
 */
HB_FUNC( QT_QWEBPAGE )
{
   hb_retptr( new QWebPage( hbqt_par_QWidget( 2 ) ) );
}

/*
 * DESTRUCTOR
 */
HB_FUNC( QT_QWEBPAGE_DESTROY )
{
   delete hbqt_par_QWebPage( 1 );
}

/*
 * QAction * action ( WebAction action ) const
 */
HB_FUNC( QT_QWEBPAGE_ACTION )
{
   hb_retptr( ( QAction* ) hbqt_par_QWebPage( 1 )->action( ( QWebPage::WebAction ) hb_parni( 2 ) ) );
}

/*
 * quint64 bytesReceived () const
 */
HB_FUNC( QT_QWEBPAGE_BYTESRECEIVED )
{
   hb_retnint( hbqt_par_QWebPage( 1 )->bytesReceived() );
}

/*
 * QMenu * createStandardContextMenu ()
 */
HB_FUNC( QT_QWEBPAGE_CREATESTANDARDCONTEXTMENU )
{
   hb_retptr( ( QMenu* ) hbqt_par_QWebPage( 1 )->createStandardContextMenu() );
}

/*
 * QWebFrame * currentFrame () const
 */
HB_FUNC( QT_QWEBPAGE_CURRENTFRAME )
{
   hb_retptr( ( QWebFrame* ) hbqt_par_QWebPage( 1 )->currentFrame() );
}

/*
 * bool findText ( const QString & subString, FindFlags options = 0 )
 */
HB_FUNC( QT_QWEBPAGE_FINDTEXT )
{
   hb_retl( hbqt_par_QWebPage( 1 )->findText( hbqt_par_QString( 2 ), ( QWebPage::FindFlags ) hb_parni( 3 ) ) );
}

/*
 * bool focusNextPrevChild ( bool next )
 */
HB_FUNC( QT_QWEBPAGE_FOCUSNEXTPREVCHILD )
{
   hb_retl( hbqt_par_QWebPage( 1 )->focusNextPrevChild( hb_parl( 2 ) ) );
}

/*
 * bool forwardUnsupportedContent () const
 */
HB_FUNC( QT_QWEBPAGE_FORWARDUNSUPPORTEDCONTENT )
{
   hb_retl( hbqt_par_QWebPage( 1 )->forwardUnsupportedContent() );
}

/*
 * QWebHistory * history () const
 */
HB_FUNC( QT_QWEBPAGE_HISTORY )
{
   hb_retptr( ( QWebHistory* ) hbqt_par_QWebPage( 1 )->history() );
}

/*
 * QVariant inputMethodQuery ( Qt::InputMethodQuery property ) const
 */
HB_FUNC( QT_QWEBPAGE_INPUTMETHODQUERY )
{
   hb_retptr( new QVariant( hbqt_par_QWebPage( 1 )->inputMethodQuery( ( Qt::InputMethodQuery ) hb_parni( 2 ) ) ) );
}

/*
 * bool isContentEditable () const
 */
HB_FUNC( QT_QWEBPAGE_ISCONTENTEDITABLE )
{
   hb_retl( hbqt_par_QWebPage( 1 )->isContentEditable() );
}

/*
 * bool isModified () const
 */
HB_FUNC( QT_QWEBPAGE_ISMODIFIED )
{
   hb_retl( hbqt_par_QWebPage( 1 )->isModified() );
}

/*
 * LinkDelegationPolicy linkDelegationPolicy () const
 */
HB_FUNC( QT_QWEBPAGE_LINKDELEGATIONPOLICY )
{
   hb_retni( ( QWebPage::LinkDelegationPolicy ) hbqt_par_QWebPage( 1 )->linkDelegationPolicy() );
}

/*
 * QWebFrame * mainFrame () const
 */
HB_FUNC( QT_QWEBPAGE_MAINFRAME )
{
   hb_retptr( ( QWebFrame* ) hbqt_par_QWebPage( 1 )->mainFrame() );
}

/*
 * QNetworkAccessManager * networkAccessManager () const
 */
HB_FUNC( QT_QWEBPAGE_NETWORKACCESSMANAGER )
{
   hb_retptr( ( QNetworkAccessManager* ) hbqt_par_QWebPage( 1 )->networkAccessManager() );
}

/*
 * QPalette palette () const
 */
HB_FUNC( QT_QWEBPAGE_PALETTE )
{
   hb_retptr( new QPalette( hbqt_par_QWebPage( 1 )->palette() ) );
}

/*
 * QWebPluginFactory * pluginFactory () const
 */
HB_FUNC( QT_QWEBPAGE_PLUGINFACTORY )
{
   hb_retptr( ( QWebPluginFactory* ) hbqt_par_QWebPage( 1 )->pluginFactory() );
}

/*
 * QString selectedText () const
 */
HB_FUNC( QT_QWEBPAGE_SELECTEDTEXT )
{
   hb_retc( hbqt_par_QWebPage( 1 )->selectedText().toAscii().data() );
}

/*
 * void setContentEditable ( bool editable )
 */
HB_FUNC( QT_QWEBPAGE_SETCONTENTEDITABLE )
{
   hbqt_par_QWebPage( 1 )->setContentEditable( hb_parl( 2 ) );
}

/*
 * void setForwardUnsupportedContent ( bool forward )
 */
HB_FUNC( QT_QWEBPAGE_SETFORWARDUNSUPPORTEDCONTENT )
{
   hbqt_par_QWebPage( 1 )->setForwardUnsupportedContent( hb_parl( 2 ) );
}

/*
 * void setLinkDelegationPolicy ( LinkDelegationPolicy policy )
 */
HB_FUNC( QT_QWEBPAGE_SETLINKDELEGATIONPOLICY )
{
   hbqt_par_QWebPage( 1 )->setLinkDelegationPolicy( ( QWebPage::LinkDelegationPolicy ) hb_parni( 2 ) );
}

/*
 * void setNetworkAccessManager ( QNetworkAccessManager * manager )
 */
HB_FUNC( QT_QWEBPAGE_SETNETWORKACCESSMANAGER )
{
   hbqt_par_QWebPage( 1 )->setNetworkAccessManager( hbqt_par_QNetworkAccessManager( 2 ) );
}

/*
 * void setPalette ( const QPalette & palette )
 */
HB_FUNC( QT_QWEBPAGE_SETPALETTE )
{
   hbqt_par_QWebPage( 1 )->setPalette( *hbqt_par_QPalette( 2 ) );
}

/*
 * void setPluginFactory ( QWebPluginFactory * factory )
 */
HB_FUNC( QT_QWEBPAGE_SETPLUGINFACTORY )
{
   hbqt_par_QWebPage( 1 )->setPluginFactory( hbqt_par_QWebPluginFactory( 2 ) );
}

/*
 * void setView ( QWidget * view )
 */
HB_FUNC( QT_QWEBPAGE_SETVIEW )
{
   hbqt_par_QWebPage( 1 )->setView( hbqt_par_QWidget( 2 ) );
}

/*
 * void setViewportSize ( const QSize & size ) const
 */
HB_FUNC( QT_QWEBPAGE_SETVIEWPORTSIZE )
{
   hbqt_par_QWebPage( 1 )->setViewportSize( *hbqt_par_QSize( 2 ) );
}

/*
 * QWebSettings * settings () const
 */
HB_FUNC( QT_QWEBPAGE_SETTINGS )
{
   hb_retptr( ( QWebSettings* ) hbqt_par_QWebPage( 1 )->settings() );
}

/*
 * virtual bool supportsExtension ( Extension extension ) const
 */
HB_FUNC( QT_QWEBPAGE_SUPPORTSEXTENSION )
{
   hb_retl( hbqt_par_QWebPage( 1 )->supportsExtension( ( QWebPage::Extension ) hb_parni( 2 ) ) );
}

/*
 * bool swallowContextMenuEvent ( QContextMenuEvent * event )
 */
HB_FUNC( QT_QWEBPAGE_SWALLOWCONTEXTMENUEVENT )
{
   hb_retl( hbqt_par_QWebPage( 1 )->swallowContextMenuEvent( hbqt_par_QContextMenuEvent( 2 ) ) );
}

/*
 * quint64 totalBytes () const
 */
HB_FUNC( QT_QWEBPAGE_TOTALBYTES )
{
   hb_retnint( hbqt_par_QWebPage( 1 )->totalBytes() );
}

/*
 * virtual void triggerAction ( WebAction action, bool checked = false )
 */
HB_FUNC( QT_QWEBPAGE_TRIGGERACTION )
{
   hbqt_par_QWebPage( 1 )->triggerAction( ( QWebPage::WebAction ) hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * QUndoStack * undoStack () const
 */
HB_FUNC( QT_QWEBPAGE_UNDOSTACK )
{
   hb_retptr( ( QUndoStack* ) hbqt_par_QWebPage( 1 )->undoStack() );
}

/*
 * void updatePositionDependentActions ( const QPoint & pos )
 */
HB_FUNC( QT_QWEBPAGE_UPDATEPOSITIONDEPENDENTACTIONS )
{
   hbqt_par_QWebPage( 1 )->updatePositionDependentActions( *hbqt_par_QPoint( 2 ) );
}

/*
 * QWidget * view () const
 */
HB_FUNC( QT_QWEBPAGE_VIEW )
{
   hb_retptr( ( QWidget* ) hbqt_par_QWebPage( 1 )->view() );
}

/*
 * QSize viewportSize () const
 */
HB_FUNC( QT_QWEBPAGE_VIEWPORTSIZE )
{
   hb_retptr( new QSize( hbqt_par_QWebPage( 1 )->viewportSize() ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
