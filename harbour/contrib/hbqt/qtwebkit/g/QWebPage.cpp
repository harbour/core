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
   HBQT_GC_T_QWebPage * p = ( HBQT_GC_T_QWebPage * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QWebPage * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
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

   return p;
}

HB_FUNC( QT_QWEBPAGE )
{
   QWebPage * pObj = NULL;

   pObj = new QWebPage( hbqt_par_QWidget( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QWebPage( ( void * ) pObj, true ) );
}

/* QAction * action ( WebAction action ) const */
HB_FUNC( QT_QWEBPAGE_ACTION )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->action( ( QWebPage::WebAction ) hb_parni( 2 ) ), false ) );
}

/* quint64 bytesReceived () const */
HB_FUNC( QT_QWEBPAGE_BYTESRECEIVED )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retnint( ( p )->bytesReceived() );
}

/* QMenu * createStandardContextMenu () */
HB_FUNC( QT_QWEBPAGE_CREATESTANDARDCONTEXTMENU )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->createStandardContextMenu(), false ) );
}

/* QWebFrame * currentFrame () const */
HB_FUNC( QT_QWEBPAGE_CURRENTFRAME )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebFrame( ( p )->currentFrame(), false ) );
}

/* bool findText ( const QString & subString, FindFlags options = 0 ) */
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

/* bool focusNextPrevChild ( bool next ) */
HB_FUNC( QT_QWEBPAGE_FOCUSNEXTPREVCHILD )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retl( ( p )->focusNextPrevChild( hb_parl( 2 ) ) );
}

/* bool forwardUnsupportedContent () const */
HB_FUNC( QT_QWEBPAGE_FORWARDUNSUPPORTEDCONTENT )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retl( ( p )->forwardUnsupportedContent() );
}

/* QWebHistory * history () const */
HB_FUNC( QT_QWEBPAGE_HISTORY )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebHistory( ( p )->history(), false ) );
}

/* QVariant inputMethodQuery ( Qt::InputMethodQuery property ) const */
HB_FUNC( QT_QWEBPAGE_INPUTMETHODQUERY )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->inputMethodQuery( ( Qt::InputMethodQuery ) hb_parni( 2 ) ) ), true ) );
}

/* bool isContentEditable () const */
HB_FUNC( QT_QWEBPAGE_ISCONTENTEDITABLE )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retl( ( p )->isContentEditable() );
}

/* bool isModified () const */
HB_FUNC( QT_QWEBPAGE_ISMODIFIED )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retl( ( p )->isModified() );
}

/* LinkDelegationPolicy linkDelegationPolicy () const */
HB_FUNC( QT_QWEBPAGE_LINKDELEGATIONPOLICY )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retni( ( QWebPage::LinkDelegationPolicy ) ( p )->linkDelegationPolicy() );
}

/* QWebFrame * mainFrame () const */
HB_FUNC( QT_QWEBPAGE_MAINFRAME )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebFrame( ( p )->mainFrame(), false ) );
}

/* QPalette palette () const */
HB_FUNC( QT_QWEBPAGE_PALETTE )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->palette() ), true ) );
}

/* QWebPluginFactory * pluginFactory () const */
HB_FUNC( QT_QWEBPAGE_PLUGINFACTORY )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebPluginFactory( ( p )->pluginFactory(), false ) );
}

/* QString selectedText () const */
HB_FUNC( QT_QWEBPAGE_SELECTEDTEXT )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retstr_utf8( ( p )->selectedText().toUtf8().data() );
}

/* void setContentEditable ( bool editable ) */
HB_FUNC( QT_QWEBPAGE_SETCONTENTEDITABLE )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      ( p )->setContentEditable( hb_parl( 2 ) );
}

/* void setForwardUnsupportedContent ( bool forward ) */
HB_FUNC( QT_QWEBPAGE_SETFORWARDUNSUPPORTEDCONTENT )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      ( p )->setForwardUnsupportedContent( hb_parl( 2 ) );
}

/* void setLinkDelegationPolicy ( LinkDelegationPolicy policy ) */
HB_FUNC( QT_QWEBPAGE_SETLINKDELEGATIONPOLICY )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      ( p )->setLinkDelegationPolicy( ( QWebPage::LinkDelegationPolicy ) hb_parni( 2 ) );
}

/* void setPalette ( const QPalette & palette ) */
HB_FUNC( QT_QWEBPAGE_SETPALETTE )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      ( p )->setPalette( *hbqt_par_QPalette( 2 ) );
}

/* void setPluginFactory ( QWebPluginFactory * factory ) */
HB_FUNC( QT_QWEBPAGE_SETPLUGINFACTORY )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      ( p )->setPluginFactory( hbqt_par_QWebPluginFactory( 2 ) );
}

/* void setView ( QWidget * view ) */
HB_FUNC( QT_QWEBPAGE_SETVIEW )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      ( p )->setView( hbqt_par_QWidget( 2 ) );
}

/* void setViewportSize ( const QSize & size ) const */
HB_FUNC( QT_QWEBPAGE_SETVIEWPORTSIZE )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      ( p )->setViewportSize( *hbqt_par_QSize( 2 ) );
}

/* QWebSettings * settings () const */
HB_FUNC( QT_QWEBPAGE_SETTINGS )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebSettings( ( p )->settings(), false ) );
}

/* virtual bool supportsExtension ( Extension extension ) const */
HB_FUNC( QT_QWEBPAGE_SUPPORTSEXTENSION )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retl( ( p )->supportsExtension( ( QWebPage::Extension ) hb_parni( 2 ) ) );
}

/* bool swallowContextMenuEvent ( QContextMenuEvent * event ) */
HB_FUNC( QT_QWEBPAGE_SWALLOWCONTEXTMENUEVENT )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retl( ( p )->swallowContextMenuEvent( hbqt_par_QContextMenuEvent( 2 ) ) );
}

/* quint64 totalBytes () const */
HB_FUNC( QT_QWEBPAGE_TOTALBYTES )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retnint( ( p )->totalBytes() );
}

/* virtual void triggerAction ( WebAction action, bool checked = false ) */
HB_FUNC( QT_QWEBPAGE_TRIGGERACTION )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      ( p )->triggerAction( ( QWebPage::WebAction ) hb_parni( 2 ), hb_parl( 3 ) );
}

/* void updatePositionDependentActions ( const QPoint & pos ) */
HB_FUNC( QT_QWEBPAGE_UPDATEPOSITIONDEPENDENTACTIONS )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      ( p )->updatePositionDependentActions( *hbqt_par_QPoint( 2 ) );
}

/* QWidget * view () const */
HB_FUNC( QT_QWEBPAGE_VIEW )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->view(), false ) );
}

/* QSize viewportSize () const */
HB_FUNC( QT_QWEBPAGE_VIEWPORTSIZE )
{
   QWebPage * p = hbqt_par_QWebPage( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->viewportSize() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
