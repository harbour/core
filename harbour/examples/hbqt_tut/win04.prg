#include "hbqtgui.ch"
STATIC oWnd

PROCEDURE Main()

   LOCAL Pulsante1, Pulsante2

   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Prova dei pulsanti" )
   oWnd:resize( 640, 480 )

   Pulsante1 := QPushButton( oWnd )
   Pulsante1:setText( "Quit" )
   Pulsante1:move( 50, 50 )
   Pulsante1:Connect( "clicked()", { || QApplication():quit() } )

   Pulsante2 := QPushButton( oWnd )
   Pulsante2:setText( "Premere per modificare la barra del titolo" )
   Pulsante2:move( 50, 100 )
   Pulsante2:setIcon( QIcon( "Star_32.bmp" ) )
   Pulsante2:resize( 300, 50 )
   Pulsante2:Connect( "clicked()", { || modifica() } )

   oWnd:show()
   QApplication():exec()

   RETURN

PROCEDURE modifica()

   oWnd:setWindowTitle( "Evviva, ci sono riuscito !!!!!!!!!" )

   RETURN
