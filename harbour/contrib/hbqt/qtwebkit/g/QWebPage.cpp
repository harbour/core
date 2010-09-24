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
 *  enum Extension { ChooseMultipleFilesExtension }
 *  enum FindFlag { FindBackward, FindCaseSensitively, FindWrapsAroundDocument }
 *  enum LinkDelegationPolicy { DontDelegateLinks, DelegateExternalLinks, DelegateAllLinks }
 *  enum NavigationType { NavigationTypeLinkClicked, NavigationTypeFormSubmitted, NavigationTypeBackOrForward, NavigationTypeReload, NavigationTypeFormResubmitted, NavigationTypeOther }
 *  enum WebAction { NoWebAction, OpenLink, OpenLinkInNewWindow, OpenFrameInNewWindow, ..., SelectAll }
 *  enum WebWindowType { WebBrowserWindow, WebModalDialog }
 *  flags FindFlags
 */

/*
 *  Constructed[ 31/31 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // virtual bool extension ( Extension extension, const ExtensionOption * option = 0, ExtensionReturn * output = 0 )
 *  // QNetworkAccessManager * networkAccessManager () const
 *  // void setNetworkAccessManager ( QNetworkAccessManager * manager )
 *  //QUndoStack * undoStack () const
 */

#include <QtCore/QPointer>

#include "hbqtgui.h"

#include <QtWebKit/QWebPage>
#include <QtCore/QVariant>


/*
 * QWebPage ( QObject * parent = 0 )
 * ~QWebPage ()
 */

typedef struct
{
   QPointer< QWebPage > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QWebPage;

HBQT_GC_FUNC( hbqt_gcRelease_QWebPage )
{
   QWebPage  * ph = NULL ;
   HBQT_GC_T_QWebPage * p = ( HBQT_GC_T_QWebPage * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QWebPage   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QWebPage   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QWebPage          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QWebPage    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QWebPage    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QWebPage( void * pObj, bool bNew )
{
   HBQT_GC_T_QWebPage * p = ( HBQT_GC_T_QWebPage * ) hb_gcAllocate( sizeof( HBQT_GC_T_QWebPage ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QWebPage >( ( QWebPage * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWebPage;
   p->type = HBQT_TYPE_QWebPage;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QWebPage  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QWebPage", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QWEBPAGE )
{
   QWebPage * pObj = NULL;

   pObj = new QWebPage( hbqt_par_QWidget( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QWebPage( ( void * ) pObj, true ) );
}

/*
 * QAction * action ( WebAction action ) const
 */
HB_FUNC( QT_QWEBPAGE_ACTION )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->action( ( QWebPage::WebAction ) hb_parni( 2 ) ), false ) );
   }
}

/*
 * quint64 bytesReceived () const
 */
HB_FUNC( QT_QWEBPAGE_BYTESRECEIVED )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      hb_retnint( ( p )->bytesReceived() );
   }
}

/*
 * QMenu * createStandardContextMenu ()
 */
HB_FUNC( QT_QWEBPAGE_CREATESTANDARDCONTEXTMENU )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->createStandardContextMenu(), false ) );
   }
}

/*
 * QWebFrame * currentFrame () const
 */
HB_FUNC( QT_QWEBPAGE_CURRENTFRAME )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWebFrame( ( p )->currentFrame(), false ) );
   }
}

/*
 * bool findText ( const QString & subString, FindFlags options = 0 )
 */
HB_FUNC( QT_QWEBPAGE_FINDTEXT )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->findText( hb_parstr_utf8( 2, &pText, NULL ), ( QWebPage::FindFlags ) hb_parni( 3 ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool focusNextPrevChild ( bool next )
 */
HB_FUNC( QT_QWEBPAGE_FOCUSNEXTPREVCHILD )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      hb_retl( ( p )->focusNextPrevChild( hb_parl( 2 ) ) );
   }
}

/*
 * bool forwardUnsupportedContent () const
 */
HB_FUNC( QT_QWEBPAGE_FORWARDUNSUPPORTEDCONTENT )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      hb_retl( ( p )->forwardUnsupportedContent() );
   }
}

/*
 * QWebHistory * history () const
 */
HB_FUNC( QT_QWEBPAGE_HISTORY )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWebHistory( ( p )->history(), false ) );
   }
}

/*
 * QVariant inputMethodQuery ( Qt::InputMethodQuery property ) const
 */
HB_FUNC( QT_QWEBPAGE_INPUTMETHODQUERY )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->inputMethodQuery( ( Qt::InputMethodQuery ) hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * bool isContentEditable () const
 */
HB_FUNC( QT_QWEBPAGE_ISCONTENTEDITABLE )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      hb_retl( ( p )->isContentEditable() );
   }
}

/*
 * bool isModified () const
 */
HB_FUNC( QT_QWEBPAGE_ISMODIFIED )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      hb_retl( ( p )->isModified() );
   }
}

/*
 * LinkDelegationPolicy linkDelegationPolicy () const
 */
HB_FUNC( QT_QWEBPAGE_LINKDELEGATIONPOLICY )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      hb_retni( ( QWebPage::LinkDelegationPolicy ) ( p )->linkDelegationPolicy() );
   }
}

/*
 * QWebFrame * mainFrame () const
 */
HB_FUNC( QT_QWEBPAGE_MAINFRAME )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWebFrame( ( p )->mainFrame(), false ) );
   }
}

/*
 * QPalette palette () const
 */
HB_FUNC( QT_QWEBPAGE_PALETTE )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->palette() ), true ) );
   }
}

/*
 * QWebPluginFactory * pluginFactory () const
 */
HB_FUNC( QT_QWEBPAGE_PLUGINFACTORY )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWebPluginFactory( ( p )->pluginFactory(), false ) );
   }
}

/*
 * QString selectedText () const
 */
HB_FUNC( QT_QWEBPAGE_SELECTEDTEXT )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->selectedText().toUtf8().data() );
   }
}

/*
 * void setContentEditable ( bool editable )
 */
HB_FUNC( QT_QWEBPAGE_SETCONTENTEDITABLE )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      ( p )->setContentEditable( hb_parl( 2 ) );
   }
}

/*
 * void setForwardUnsupportedContent ( bool forward )
 */
HB_FUNC( QT_QWEBPAGE_SETFORWARDUNSUPPORTEDCONTENT )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      ( p )->setForwardUnsupportedContent( hb_parl( 2 ) );
   }
}

/*
 * void setLinkDelegationPolicy ( LinkDelegationPolicy policy )
 */
HB_FUNC( QT_QWEBPAGE_SETLINKDELEGATIONPOLICY )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      ( p )->setLinkDelegationPolicy( ( QWebPage::LinkDelegationPolicy ) hb_parni( 2 ) );
   }
}

/*
 * void setPalette ( const QPalette & palette )
 */
HB_FUNC( QT_QWEBPAGE_SETPALETTE )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      ( p )->setPalette( *hbqt_par_QPalette( 2 ) );
   }
}

/*
 * void setPluginFactory ( QWebPluginFactory * factory )
 */
HB_FUNC( QT_QWEBPAGE_SETPLUGINFACTORY )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      ( p )->setPluginFactory( hbqt_par_QWebPluginFactory( 2 ) );
   }
}

/*
 * void setView ( QWidget * view )
 */
HB_FUNC( QT_QWEBPAGE_SETVIEW )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      ( p )->setView( hbqt_par_QWidget( 2 ) );
   }
}

/*
 * void setViewportSize ( const QSize & size ) const
 */
HB_FUNC( QT_QWEBPAGE_SETVIEWPORTSIZE )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      ( p )->setViewportSize( *hbqt_par_QSize( 2 ) );
   }
}

/*
 * QWebSettings * settings () const
 */
HB_FUNC( QT_QWEBPAGE_SETTINGS )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWebSettings( ( p )->settings(), false ) );
   }
}

/*
 * virtual bool supportsExtension ( Extension extension ) const
 */
HB_FUNC( QT_QWEBPAGE_SUPPORTSEXTENSION )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      hb_retl( ( p )->supportsExtension( ( QWebPage::Extension ) hb_parni( 2 ) ) );
   }
}

/*
 * bool swallowContextMenuEvent ( QContextMenuEvent * event )
 */
HB_FUNC( QT_QWEBPAGE_SWALLOWCONTEXTMENUEVENT )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      hb_retl( ( p )->swallowContextMenuEvent( hbqt_par_QContextMenuEvent( 2 ) ) );
   }
}

/*
 * quint64 totalBytes () const
 */
HB_FUNC( QT_QWEBPAGE_TOTALBYTES )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      hb_retnint( ( p )->totalBytes() );
   }
}

/*
 * virtual void triggerAction ( WebAction action, bool checked = false )
 */
HB_FUNC( QT_QWEBPAGE_TRIGGERACTION )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      ( p )->triggerAction( ( QWebPage::WebAction ) hb_parni( 2 ), hb_parl( 3 ) );
   }
}

/*
 * void updatePositionDependentActions ( const QPoint & pos )
 */
HB_FUNC( QT_QWEBPAGE_UPDATEPOSITIONDEPENDENTACTIONS )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      ( p )->updatePositionDependentActions( *hbqt_par_QPoint( 2 ) );
   }
}

/*
 * QWidget * view () const
 */
HB_FUNC( QT_QWEBPAGE_VIEW )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->view(), false ) );
   }
}

/*
 * QSize viewportSize () const
 */
HB_FUNC( QT_QWEBPAGE_VIEWPORTSIZE )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->viewportSize() ), true ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
