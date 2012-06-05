/*
 * $Id$
 */


FUNCTION Main() 
   LOCAL oWnd 

   hbqt_errorsys()

   oWnd := QMainWindow() 
   oWnd:resize( 400,200 )

   BuildMenuBar( oWnd ) 
   AddLabel( oWnd )

   oWnd:show() 

   QApplication():exec() 

   RETURN NIL


STATIC FUNCTION BuildMenuBar( oWnd ) 
   LOCAL oMenu1, oMenuBar

   // Why do we need these two variables in scope ?
   // Because :connect needs that Harbour object must 
   //  always be accessible.
   //
   STATIC oItemIns, oItemMod
   
   oMenuBar := QMenuBar( oWnd ) 

   // if we do not construct a widget without a parent
   // it will be deleted once RETURN is his, so always 
   // pass it its parent as an argument.
   //
   oMenu1 := QMenu( oMenuBar ) 
   //
   oMenu1:setTitle( "&Dialogs" ) 

   oItemIns := QAction( oMenu1 ) 
   oItemIns:setText( "&MessageBox()" ) 
   oItemIns:connect( "triggered()", {|| DlgMBox( "Yes" ) } )

   oItemMod := QAction( oMenu1 ) 
   oItemMod:setText( "&FileDialog()" ) 
   oItemMod:connect( "triggered()", {|| DlgFiles( "*.prg" ) } )

   oMenu1:addAction( oItemIns ) 
   oMenu1:addAction( oItemMod ) 

   oMenuBar:addMenu( oMenu1 ) 

   oWnd:setMenuBar( oMenuBar )

   RETURN NIL


FUNCTION AddLabel( oWnd )
   LOCAL oL

   oL := QLabel( oWnd )
   oL:move( 180,95 )
   oL:setText( "Harbour Qt" )

   // oL is local, still it is visible to the appln throughout.
   // Also memory is released automatically once its parent 
   // will go out of scope.

   RETURN NIL


FUNCTION DlgMBox( cMsg )
   LOCAL oMB 

   oMB := QMessageBox()
   oMB:setText( cMsg )
   oMB:exec()

   // oMB is local and QMessageBox() is not constructed with any 
   // parent, so it will be deleted once RETURN is hit, and memory 
   // will be reclaimed properly. Try to click on this option 
   // repeatedly and see for yourself that memory remains constant.

   RETURN NIL


FUNCTION DlgFiles( cMask )
   LOCAL oFD

   oFD := QFileDialog()
   oFD:exec()

   // oFD is local and QMessageBox() is not constructed with any 
   // parent, so it will be deleted once RETURN is hit, and memory 
   // will be reclaimed properly. Try to click on this option 
   // repeatedly and see for yourself that memory remains constant.

   RETURN cMask

