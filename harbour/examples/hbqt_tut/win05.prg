#include "hbqtgui.ch"

PROCEDURE Main()

   LOCAL s_qApp
   LOCAL oWnd
   LOCAL oSBar

   s_qApp := QApplication()

   oWnd := QMainWindow()
   oWnd:show()

   oWnd:setWindowTitle( "Giovanni" )
   oWnd:resize( 300, 200 )

   oSBar := QStatusBar( oWnd )
   oWnd:setStatusBar( oSBar )

   oSBar:showMessage( "Harbour-QT Statusbar Ready!" )

   s_qApp:exec()

   RETURN
