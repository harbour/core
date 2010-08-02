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
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->action( ( QWebPage::WebAction ) hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_ACTION FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->action( ( QWebPage::WebAction ) hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * quint64 bytesReceived () const
 */
HB_FUNC( QT_QWEBPAGE_BYTESRECEIVED )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retnint( ( p )->bytesReceived() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_BYTESRECEIVED FP=hb_retnint( ( p )->bytesReceived() ); p is NULL" ) );
   }
}

/*
 * QMenu * createStandardContextMenu ()
 */
HB_FUNC( QT_QWEBPAGE_CREATESTANDARDCONTEXTMENU )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->createStandardContextMenu(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_CREATESTANDARDCONTEXTMENU FP=hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->createStandardContextMenu(), false ) ); p is NULL" ) );
   }
}

/*
 * QWebFrame * currentFrame () const
 */
HB_FUNC( QT_QWEBPAGE_CURRENTFRAME )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebFrame( ( p )->currentFrame(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_CURRENTFRAME FP=hb_retptrGC( hbqt_gcAllocate_QWebFrame( ( p )->currentFrame(), false ) ); p is NULL" ) );
   }
}

/*
 * bool findText ( const QString & subString, FindFlags options = 0 )
 */
HB_FUNC( QT_QWEBPAGE_FINDTEXT )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retl( ( p )->findText( QWebPage::tr( hb_parc( 2 ) ), ( QWebPage::FindFlags ) hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_FINDTEXT FP=hb_retl( ( p )->findText( QWebPage::tr( hb_parc( 2 ) ), ( QWebPage::FindFlags ) hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * bool focusNextPrevChild ( bool next )
 */
HB_FUNC( QT_QWEBPAGE_FOCUSNEXTPREVCHILD )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retl( ( p )->focusNextPrevChild( hb_parl( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_FOCUSNEXTPREVCHILD FP=hb_retl( ( p )->focusNextPrevChild( hb_parl( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool forwardUnsupportedContent () const
 */
HB_FUNC( QT_QWEBPAGE_FORWARDUNSUPPORTEDCONTENT )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retl( ( p )->forwardUnsupportedContent() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_FORWARDUNSUPPORTEDCONTENT FP=hb_retl( ( p )->forwardUnsupportedContent() ); p is NULL" ) );
   }
}

/*
 * QWebHistory * history () const
 */
HB_FUNC( QT_QWEBPAGE_HISTORY )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebHistory( ( p )->history(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_HISTORY FP=hb_retptrGC( hbqt_gcAllocate_QWebHistory( ( p )->history(), false ) ); p is NULL" ) );
   }
}

/*
 * QVariant inputMethodQuery ( Qt::InputMethodQuery property ) const
 */
HB_FUNC( QT_QWEBPAGE_INPUTMETHODQUERY )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->inputMethodQuery( ( Qt::InputMethodQuery ) hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_INPUTMETHODQUERY FP=hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->inputMethodQuery( ( Qt::InputMethodQuery ) hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * bool isContentEditable () const
 */
HB_FUNC( QT_QWEBPAGE_ISCONTENTEDITABLE )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retl( ( p )->isContentEditable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_ISCONTENTEDITABLE FP=hb_retl( ( p )->isContentEditable() ); p is NULL" ) );
   }
}

/*
 * bool isModified () const
 */
HB_FUNC( QT_QWEBPAGE_ISMODIFIED )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retl( ( p )->isModified() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_ISMODIFIED FP=hb_retl( ( p )->isModified() ); p is NULL" ) );
   }
}

/*
 * LinkDelegationPolicy linkDelegationPolicy () const
 */
HB_FUNC( QT_QWEBPAGE_LINKDELEGATIONPOLICY )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retni( ( QWebPage::LinkDelegationPolicy ) ( p )->linkDelegationPolicy() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_LINKDELEGATIONPOLICY FP=hb_retni( ( QWebPage::LinkDelegationPolicy ) ( p )->linkDelegationPolicy() ); p is NULL" ) );
   }
}

/*
 * QWebFrame * mainFrame () const
 */
HB_FUNC( QT_QWEBPAGE_MAINFRAME )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebFrame( ( p )->mainFrame(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_MAINFRAME FP=hb_retptrGC( hbqt_gcAllocate_QWebFrame( ( p )->mainFrame(), false ) ); p is NULL" ) );
   }
}

/*
 * QPalette palette () const
 */
HB_FUNC( QT_QWEBPAGE_PALETTE )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->palette() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_PALETTE FP=hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->palette() ), true ) ); p is NULL" ) );
   }
}

/*
 * QWebPluginFactory * pluginFactory () const
 */
HB_FUNC( QT_QWEBPAGE_PLUGINFACTORY )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebPluginFactory( ( p )->pluginFactory(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_PLUGINFACTORY FP=hb_retptrGC( hbqt_gcAllocate_QWebPluginFactory( ( p )->pluginFactory(), false ) ); p is NULL" ) );
   }
}

/*
 * QString selectedText () const
 */
HB_FUNC( QT_QWEBPAGE_SELECTEDTEXT )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retc( ( p )->selectedText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_SELECTEDTEXT FP=hb_retc( ( p )->selectedText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void setContentEditable ( bool editable )
 */
HB_FUNC( QT_QWEBPAGE_SETCONTENTEDITABLE )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      ( p )->setContentEditable( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_SETCONTENTEDITABLE FP=( p )->setContentEditable( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setForwardUnsupportedContent ( bool forward )
 */
HB_FUNC( QT_QWEBPAGE_SETFORWARDUNSUPPORTEDCONTENT )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      ( p )->setForwardUnsupportedContent( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_SETFORWARDUNSUPPORTEDCONTENT FP=( p )->setForwardUnsupportedContent( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setLinkDelegationPolicy ( LinkDelegationPolicy policy )
 */
HB_FUNC( QT_QWEBPAGE_SETLINKDELEGATIONPOLICY )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      ( p )->setLinkDelegationPolicy( ( QWebPage::LinkDelegationPolicy ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_SETLINKDELEGATIONPOLICY FP=( p )->setLinkDelegationPolicy( ( QWebPage::LinkDelegationPolicy ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPalette ( const QPalette & palette )
 */
HB_FUNC( QT_QWEBPAGE_SETPALETTE )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      ( p )->setPalette( *hbqt_par_QPalette( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_SETPALETTE FP=( p )->setPalette( *hbqt_par_QPalette( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPluginFactory ( QWebPluginFactory * factory )
 */
HB_FUNC( QT_QWEBPAGE_SETPLUGINFACTORY )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      ( p )->setPluginFactory( hbqt_par_QWebPluginFactory( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_SETPLUGINFACTORY FP=( p )->setPluginFactory( hbqt_par_QWebPluginFactory( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setView ( QWidget * view )
 */
HB_FUNC( QT_QWEBPAGE_SETVIEW )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      ( p )->setView( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_SETVIEW FP=( p )->setView( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setViewportSize ( const QSize & size ) const
 */
HB_FUNC( QT_QWEBPAGE_SETVIEWPORTSIZE )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      ( p )->setViewportSize( *hbqt_par_QSize( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_SETVIEWPORTSIZE FP=( p )->setViewportSize( *hbqt_par_QSize( 2 ) ); p is NULL" ) );
   }
}

/*
 * QWebSettings * settings () const
 */
HB_FUNC( QT_QWEBPAGE_SETTINGS )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebSettings( ( p )->settings(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_SETTINGS FP=hb_retptrGC( hbqt_gcAllocate_QWebSettings( ( p )->settings(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual bool supportsExtension ( Extension extension ) const
 */
HB_FUNC( QT_QWEBPAGE_SUPPORTSEXTENSION )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retl( ( p )->supportsExtension( ( QWebPage::Extension ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_SUPPORTSEXTENSION FP=hb_retl( ( p )->supportsExtension( ( QWebPage::Extension ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool swallowContextMenuEvent ( QContextMenuEvent * event )
 */
HB_FUNC( QT_QWEBPAGE_SWALLOWCONTEXTMENUEVENT )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retl( ( p )->swallowContextMenuEvent( hbqt_par_QContextMenuEvent( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_SWALLOWCONTEXTMENUEVENT FP=hb_retl( ( p )->swallowContextMenuEvent( hbqt_par_QContextMenuEvent( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * quint64 totalBytes () const
 */
HB_FUNC( QT_QWEBPAGE_TOTALBYTES )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retnint( ( p )->totalBytes() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_TOTALBYTES FP=hb_retnint( ( p )->totalBytes() ); p is NULL" ) );
   }
}

/*
 * virtual void triggerAction ( WebAction action, bool checked = false )
 */
HB_FUNC( QT_QWEBPAGE_TRIGGERACTION )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      ( p )->triggerAction( ( QWebPage::WebAction ) hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_TRIGGERACTION FP=( p )->triggerAction( ( QWebPage::WebAction ) hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void updatePositionDependentActions ( const QPoint & pos )
 */
HB_FUNC( QT_QWEBPAGE_UPDATEPOSITIONDEPENDENTACTIONS )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      ( p )->updatePositionDependentActions( *hbqt_par_QPoint( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_UPDATEPOSITIONDEPENDENTACTIONS FP=( p )->updatePositionDependentActions( *hbqt_par_QPoint( 2 ) ); p is NULL" ) );
   }
}

/*
 * QWidget * view () const
 */
HB_FUNC( QT_QWEBPAGE_VIEW )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->view(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_VIEW FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->view(), false ) ); p is NULL" ) );
   }
}

/*
 * QSize viewportSize () const
 */
HB_FUNC( QT_QWEBPAGE_VIEWPORTSIZE )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->viewportSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBPAGE_VIEWPORTSIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->viewportSize() ), true ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
