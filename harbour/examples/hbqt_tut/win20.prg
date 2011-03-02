#include "hbqtgui.ch"
STATIC testo
STATIC casella

PROCEDURE Main()

   LOCAL finestra
   LOCAL font

   finestra := QMainWindow()
   finestra:resize( 320, 200 )
   finestra:setWindowTitle( "Giovanni" )

   font := QFont()

   testo := Qlabel( finestra )
   testo:setText( "Ciao a tutti" )
   testo:resize( 200, 80 )
   testo:move( 50, 20 )
   testo:setfont( font )

   casella := QFontComboBox( finestra )
   casella:move( 50, 100 )
   casella:resize( 200, 25 )
   casella:Connect( "currentFontChanged(QFont)", { || cambia_testo() } )

   finestra:show()

   QApplication():exec()

   RETURN

PROCEDURE cambia_testo

   testo:setFont( casella:currentFont() )

   RETURN
