#include "hbqtgui.ch"

STATIC s_qApp
STATIC oWnd

PROCEDURE Main()

   s_qApp := QApplication()
   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Finestra di Giovanni" )
   oWnd:resize( 640, 480 )
   oWnd:show()
   s_qApp:exec()

   RETURN
