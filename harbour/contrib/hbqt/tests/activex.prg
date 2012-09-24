/*
 * $Id$
 */

/*             W A R N I N G : Windows Only Code
 *
 *  Demonstrate the usage of Windows ActiveX functionality
 *  available in HbQt via QWidget():winID() call. This demonstration
 *  is one of the many benefits :winID() facilitates without breaking
 *  compatilibility. The programmer ( me, here ) knows what he/she is
 *  doing. On the contrary, I am free TO do mathematical operations
 *  with :winID(), like, IF o:winID() > 99999999 ; WOW() ; ELSE sigh()
 *
 *  To build I am inclusing GtWVG as it already has class to construct activex window.
 *  This is just to cut the time short. Anyway we will be using activeX on windows only
 *
 *  This can be argued that if this all can be done in pure Windows, then why through HbQt ?
 *
 *  The answer is simple: Harbour's WINAPI related infrastructure is so inadequate that it is
 *  extremely difficult IF not impossible. There are two reasons: 1. Harbour does not have
 *  any upper level GUI library, 2. Structure support in Harbour, as of current, is broken.
 *  So the effective GUI library is HbQt, complete with all GUI stuff, which can be exploited
 *  to harness the power of Windows exclusive functionality via winID() call.
 *
 *  This less than 70 lines of code demonstrate what can be achieved with WINAPI in hundreds...
 *
 *  Pritpal Bedi   23Sep2012
 */

/*----------------------------------------------------------------------*/

#include "hbqtgui.ch"
#include "hbtrace.ch"

/*----------------------------------------------------------------------*/

FUNCTION Main()
   LOCAL oUI, oActiveX

   hbqt_errorsys()

   oUI := hbqtui_activex()

   oActiveX := BuildActiveX( oUI )

   oUI:connect( QEvent_Resize, {|| oActiveX:setSize( { oUI:frameActiveX:width(), oUI:frameActiveX:height() } ) } )
   oUI:editNavigate:setText( "http://hbide.vouch.info" )
   oUI:btnNavigate:connect( "clicked()", {|| HandleNavigation( oUI, oActiveX ) } )
   oUI:btnRefresh:connect( "clicked()" , {|| oActiveX:refresh() } )

   oUI:treeUrls:setHeaderHidden( .T. )
   oUI:treeUrls:setTooltip( "Click on any url to navigate !" )
   oUI:treeUrls:connect( "itemPressed(QTreeWidgetItem*,int)", {|q,i| oActiveX:navigate( q:text( i ) ) } )

   oUI:splitterOne:connect( "splitterMoved(int,int)", {|| oActiveX:setSize( { oUI:frameActiveX:width(), oUI:frameActiveX:height() } ) } )

   oUI:editNavigate:connect( "returnPressed()", {|| oUI:btnNavigate:click() } )
   oUI:editNavigate:setFocus()

   oUI:show()

   oUI:btnNavigate:click()
   oUI:resize( 1000, 500 )
   oUI:treeUrls:setFocus()

   QApplication():exec()

   RETURN oActiveX

/*----------------------------------------------------------------------*/

FUNCTION BuildActiveX( oUI )
   LOCAL oXbp, oActiveX

   oXbp := WvgWindow():new( , , { 0, 0 }, { 640, 400 }, , .T. )
   oXbp:hWnd := oUI:frameActiveX:winID()

   oActiveX := WvgActiveXControl():new( oXbp, , { 0, 0 }, { 100, 100 }, , .t. )

   oActiveX:CLSID := "Shell.Explorer.2"
   oActiveX:mapEvent( 269, {|| oUI:labelStatus:setText( ' E X P L O R E R - 2 6 9' ) } )
   oActiveX:mapEvent( 105, {|| oUI:labelStatus:setText( ' E X P L O R E R - 105'   ) } )

   oActiveX:create()

   RETURN oActiveX

/*----------------------------------------------------------------------*/

FUNCTION HandleNavigation( oUI, oActiveX )
   LOCAL qItm := QTreeWidgetItem()

   oActiveX:navigate( oUI:editNavigate:text() )

   qItm:setText( 0, oUI:editNavigate:text() )

   oUI:treeUrls:addTopLevelItem( qItm )

   RETURN NIL

/*----------------------------------------------------------------------*/

