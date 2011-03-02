#include "hbqtgui.ch"

PROCEDURE Main()

   LOCAL oWnd
   LOCAL testo

   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Finestra di Giovanni" )
   oWnd:resize( 300, 200 )

   testo := Qlabel( oWnd )
   testo:setText( "<font color=#FF0000 size=7>Gio</font>" )
   testo:move( 10, 10 )
   testo:resize( 280, 100 )

   oWnd:show()
   QApplication():exec()

   RETURN
