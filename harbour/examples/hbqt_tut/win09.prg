#include "hbqtgui.ch"

PROCEDURE Main()

   LOCAL oWnd
   LOCAL Pulsante1

   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Prova dei pulsanti" )
   oWnd:resize( 300, 200 )

   Pulsante1 := QPushButton( oWnd )
   Pulsante1:setText( "Quit" )
   Pulsante1:move( 50, 50 )
   Pulsante1:Connect( "clicked()", { || QApplication():quit() } )
   Pulsante1:setStyleSheet( "background-color: yellow; border: 2px solid # FF0000;")

   oWnd:show()
   QApplication():exec()

   RETURN
