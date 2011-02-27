#include "hbqtgui.ch"

STATIC s_qApp
STATIC oWnd

PROCEDURE Main()

   LOCAL Pulsante1, Pulsante2

   s_qApp := QApplication()
   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Prova dei pulsanti" )
   oWnd:resize( 640, 480 )
   Pulsante1 := QPushButton( oWnd )
   Pulsante1:setText( "Quit" )
   Pulsante1:move( 50, 50 )
   Pulsante1:Connect( "clicked()", { || s_qApp:quit() } )
   Pulsante2 := QPushButton( oWnd )
   Pulsante2:setText( "Premere per modificare la barra del titolo" )
   Pulsante2:move( 50, 100 )
   Pulsante2:setIcon( QIcon( "Star_32.bmp" ) )
   Pulsante2:resize( 300, 50 )
   Pulsante2:Connect( "clicked()", { || modifica() } )
   oWnd:show()
   s_qApp:exec()

   RETURN

PROCEDURE modifica()

   oWnd:setWindowTitle( "Evviva, ci sono riuscito !!!!!!!!!!!!!" )

   RETURN
