#include "hbqtgui.ch"

STATIC s_qApp
STATIC finestra
STATIC lcd
STATIC pulsante_diminuisci, pulsante_aumenta

PROCEDURE Main()

   s_qApp := QApplication()
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
   pulsante_diminuisci:Connect( "clicked()", { || decrementa() } )
   pulsante_aumenta := QPushButton( finestra )
   pulsante_aumenta:resize( 30, 30 )
   pulsante_aumenta:move( 200, 130 )
   pulsante_aumenta:setText( "+" )
   pulsante_aumenta:Connect( "clicked()", { || incrementa() } )
   finestra:show()
   s_qApp:exec()

   RETURN

PROCEDURE incrementa()

   LOCAL x

   x := lcd:value()
   x++
   lcd:display( x )

   RETURN

PROCEDURE decrementa()

   LOCAL x

   x := lcd:value()
   x--
   lcd:display( x )

   RETURN
