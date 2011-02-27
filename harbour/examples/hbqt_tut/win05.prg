#include "hbqtgui.ch"

STATIC s_qApp

PROCEDURE Main()

   LOCAL oWnd
   LOCAL oSBar

   s_qApp := QApplication()
   oWnd := QMainWindow()
   oWnd:show()
   oWnd:setWindowTitle( "Harbour-Qt Implementation Test Dialog" )
   oWnd:resize( 640, 480 )
   oSBar := QStatusBar( oWnd )
   oWnd:setStatusBar( oSBar )
   oSBar:showMessage( "Harbour-QT Statusbar Ready!" )
   s_qApp:exec()

   RETURN
