#include "hbqtgui.ch"

PROCEDURE Main()

   LOCAL finestra
   LOCAL logo

   finestra := QmainWindow()
   finestra:SetFixedSize( 400, 300 )
   finestra:setWindowTitle( "Finestra Giovanni" )

   logo := QLabel( finestra )
   logo:move( 50, 50 )
   logo:resize( 300, 200 )
   logo:SetPixmap( QPixmap( "test.jpg" ) )
   logo:setStyleSheet( "border: 2px solid #0000ff;" )

   finestra:show()

   QApplication():exec()

   RETURN
