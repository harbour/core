#include "hbqtgui.ch"

STATIC s_qApp
STATIC oWnd
STATIC pulsante1

PROCEDURE Main()

   s_qApp := QApplication()
   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Finestra di Giovanni" )
   oWnd:resize( 640, 480 )
   Pulsante1 := QPushButton( oWnd )
   Pulsante1:setText( "Premi per messaggio" )
   Pulsante1:resize( 300, 50 )
   Pulsante1:move( 50, 50 )
   Pulsante1:Connect( "clicked()", { || messaggio() } )
   oWnd:show()
   s_qApp:exec()

   RETURN

PROCEDURE messaggio

   STATIC oBox

   oBox := QMessageBox()
   oBox:setInformativeText( "attenzione!!! " )
   oBox:setWindowTitle( "Informazione" )
   oBox:show()

   RETURN
