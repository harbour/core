#include "hbqtgui.ch"

PROCEDURE Main()

   LOCAL oWnd

   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Finestra di Giovanni" )
   oWnd:resize( 300, 200 )

   oWnd:show()
   QApplication():exec()

   RETURN
