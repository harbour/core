#include "hbqtgui.ch"
STATIC font
STATIC testo
STATIC modificatore

PROCEDURE Main()

   LOCAL finestra

   finestra := QMainWindow()
   finestra:resize( 320, 200 )
   finestra:setWindowTitle( "Giovanni" )

   font := QFont()
   font:setPointSize( 30 )
   testo := QLabel( finestra )
   testo:setText( "Testo" )
   testo:move( 10, 10 )
   testo:resize( 280, 100 )
   testo:setfont( font )

   modificatore := QSpinBox( finestra )
   modificatore:move( 50, 150 )
   modificatore:resize( 50, 25 )
   modificatore:Connect( "valueChanged(int)", { || cambia_dimensione() } )
   modificatore:setMinimum( 1 )
   modificatore:setMaximum( 72 )
   modificatore:setSingleStep( 1 )
   modificatore:setValue( 30 )

   finestra:show()

   QApplication():exec()

   RETURN

PROCEDURE cambia_dimensione()

   font:setPointSize( modificatore:value )
   testo:setfont( font )

   RETURN
