#include "hbqtgui.ch"

PROCEDURE Main()

   LOCAL finestra, pulsante_diminuisci, pulsante_aumenta, lcd

   finestra := QMainWindow()
   finestra:resize( 300, 200 )
   finestra:setWindowTitle( "Giovanni" )
   lcd := QLCDNumber( finestra )
   lcd:move( 50, 50 )
   lcd:resize( 200, 50 )
   pulsante_diminuisci := QPushButton( finestra )
   pulsante_diminuisci:resize( 30, 30 )
   pulsante_diminuisci:move( 70, 130 )
   pulsante_diminuisci:setText( "-" )
   pulsante_diminuisci:Connect( "clicked()", { || decrementa( lcd ) } )
   pulsante_aumenta := QPushButton( finestra )
   pulsante_aumenta:resize( 30, 30 )
   pulsante_aumenta:move( 200, 130 )
   pulsante_aumenta:setText( "+" )
   pulsante_aumenta:Connect( "clicked()", { || incrementa( lcd ) } )
   finestra:show()
   QApplication():exec()

   RETURN

PROCEDURE incrementa( lcd )

   LOCAL x

   x := lcd:value()
   x ++
   lcd:display( x )

   RETURN

PROCEDURE decrementa( lcd )

   LOCAL x

   x := lcd:value()
   x --
   lcd:display( x )

   RETURN
