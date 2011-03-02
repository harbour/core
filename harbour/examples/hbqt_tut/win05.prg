#include "hbqtgui.ch"

PROCEDURE Main()

   LOCAL oWnd
   LOCAL oSBar

   oWnd := QMainWindow()
   oWnd:show()

   oWnd:setWindowTitle( "Giovanni" )
   oWnd:resize( 300, 200 )

   oSBar := QStatusBar( oWnd )
   oWnd:setStatusBar( oSBar )

   oSBar:showMessage( "Harbour-QT Statusbar Ready!" )

   QApplication():exec()

   RETURN
