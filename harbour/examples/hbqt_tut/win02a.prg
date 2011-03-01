#include "hbqtgui.ch"

PROCEDURE Main()

   LOCAL s_qApp
   LOCAL oWnd
   LOCAL testo

   s_qApp := QApplication()

   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Finestra di Giovanni" )
   oWnd:resize( 300, 200 )

   testo := Qlabel( oWnd )
   testo:setText( "Hello World" )
   testo:move( 100, 100 )

   oWnd:show()
   s_qApp:exec()

   RETURN
