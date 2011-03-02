#include "hbqtgui.ch"

PROCEDURE Main()

   LOCAL oWnd
   LOCAL testo

   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Finestra di Giovanni" )
   oWnd:resize( 300, 200 )

   testo := Qlabel( oWnd )
   testo:setText( "Hello World" )
   testo:move( 100, 100 )

   oWnd:show()
   QApplication():exec()

   RETURN
