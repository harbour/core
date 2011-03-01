#include "hbqtgui.ch"

PROCEDURE Main()

   LOCAL s_qApp
   LOCAL oWnd

   s_qApp := QApplication()

   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Finestra di Giovanni" )
   oWnd:resize( 300, 200 )

   oWnd:show()
   s_qApp:exec()

   RETURN
