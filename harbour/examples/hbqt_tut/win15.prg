#include "hbqtgui.ch"

PROCEDURE Main()

   LOCAL s_qApp
   LOCAL finestra
   LOCAL logo

   s_qApp := QApplication()

   finestra := QmainWindow()
   finestra:SetFixedSize( 400, 300 )
   finestra:setWindowTitle( "Finestra Giovanni" )

   logo := QLabel( finestra )
   logo:move( 50, 50 )
   logo:resize( 300, 200 )
   logo:SetPixmap( QPixmap( "test.jpg" ) )
   logo:setStyleSheet( "border: 2px solid #0000ff;" )

   finestra:show()

   s_qApp:exec()

   RETURN
