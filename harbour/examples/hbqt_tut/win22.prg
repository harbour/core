#include "hbqtgui.ch"
STATIC edit1, edit2, edit3

PROCEDURE Main()

   LOCAL finestra
   LOCAL testo
   LOCAL calcola

   SET DATE ITALIAN

   finestra := QMainWindow()
   finestra:resize( 400, 300 )
   finestra:setWindowTitle( "Giovanni" )

   testo := QLabel( finestra )
   testo:setText( "Difference between two dates" )
   testo:move( 130, 20 )
   testo:resize( 171, 16 )

   edit1 := QLineEdit( finestra )
   edit1:resize( 113, 20 )
   edit1:move( 140, 100 )

   edit2 := QLineEdit( finestra )
   edit2:resize( 113, 20 )
   edit2:move( 140, 130 )
   edit3 := QLineEdit( finestra )
   edit3:resize( 113, 20 )
   edit3:move( 140, 180 )

   calcola := QPushButton( finestra )
   calcola:resize( 75, 23 )
   calcola:move( 270, 180 )
   calcola:setText( "Calculate" )
   calcola:Connect( "clicked()", { || calcola() } )

   finestra:show()

   QApplication():exec()

   RETURN

PROCEDURE calcola()

   LOCAL differenza

   differenza = Abs( CToD( edit1:text() ) - CToD( edit2:text() ) )
   edit3:setText( AllTrim( Str(differenza ) ) )

   RETURN
