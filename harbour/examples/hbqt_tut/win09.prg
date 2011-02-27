#include "hbqtgui.ch"

STATIC s_qApp
STATIC oWnd

PROCEDURE Main()

   LOCAL Pulsante1

   s_qApp := QApplication()
   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Prova dei pulsanti" )
   oWnd:resize( 300, 200 )
   Pulsante1 := QPushButton( oWnd )
   Pulsante1:setText( "Quit" )
   Pulsante1:move( 50, 50 )
   Pulsante1:Connect( "clicked()", { || s_qApp:quit() } )
   Pulsante1:setStyleSheet( "background-color: yellow; border: 2px solid #FF0000;" )
   oWnd:show()
   s_qApp:exec()

   RETURN
