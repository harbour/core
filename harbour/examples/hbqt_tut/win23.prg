#include "hbqtgui.ch"

STATIC s_qApp
STATIC finestra
STATIC bottone1, bottone2, bottone3

PROCEDURE Main()

   s_qApp := QApplication()

   finestra := QMainWindow()
   finestra:resize( 400, 300 )
   finestra:setWindowTitle( "Giovanni" )

   bottone1 := QRadioButton( finestra )
   bottone1:move( 100, 50 )
   bottone1:setText( "Si" )
   bottone1:Connect( "clicked()", { || messaggio( "SI" ) } )

   bottone2 := QRadioButton( finestra )
   bottone2:move( 100, 80 )
   bottone2:setText( "No" )
   bottone2:Connect( "clicked()", { || messaggio( "NO" ) } )

   bottone3 := QRadioButton( finestra )
   bottone3:move( 100, 110 )
   bottone3:setText( "Non so" )
   bottone3:Connect( "clicked()", { || messaggio( "NON SO" ) } )

   finestra:show()

   s_qApp:exec()

   RETURN

PROCEDURE messaggio( msg )

   STATIC oBox

   oBox := QMessageBox()
   oBox:setInformativeText( msg )
   oBox:setWindowTitle( "Informazione" )
   oBox:show()

   RETURN
