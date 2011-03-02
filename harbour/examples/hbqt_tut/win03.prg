#include "hbqtgui.ch"

PROCEDURE Main()

   LOCAL oWnd
   LOCAL pulsante1

   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Finestra di Giovanni" )
   oWnd:resize( 400, 300 )

   Pulsante1 := QPushButton( oWnd )
   Pulsante1:setText( "Premi per messaggio" )
   Pulsante1:resize( 300, 50 )
   Pulsante1:move( 50, 50 )
   Pulsante1:Connect( "clicked()", { || messaggio() } )

   oWnd:show()
   QApplication():exec()

   RETURN

PROCEDURE messaggio

   LOCAL oBox

   oBox := QMessageBox()
   oBox:setInformativeText( "attenzione!!! " )
   oBox:setWindowTitle( "Informazione" )
   oBox:exec()

   RETURN
