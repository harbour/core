#include "hbqtgui.ch"

STATIC s_qApp
STATIC finestra
STATIC casella

PROCEDURE Main()

   s_qApp := QApplication()
   finestra := QMainWindow()
   finestra:resize( 320, 200 )
   finestra:setWindowTitle( "Giovanni" )
   casella := QComboBox( finestra )
   casella:move( 100, 50 )
   casella:resize( 100, 25 )
   casella:addItem( "Francia" )
   casella:addItem( "Italia" )
   casella:addItem( "U.S.A." )
   casella:addItem( "Germania" )
   casella:addItem( "Belgio" )
   casella:addItem( "Spagna" )
   casella:addItem( "Portogallo" )
   casella:addItem( "Islanda" )
   finestra:show()
   s_qApp:exec()

   RETURN
