#include "hbqtgui.ch"

PROCEDURE Main()

   LOCAL oWnd
   LOCAL stringa
   LOCAL dialogo

   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Finestra di Giovanni" )
   oWnd:resize( 400, 300 )

   oWnd:show()

   dialogo := QInputDialog()
   stringa = dialogo:getText( oWnd, "Titolo", "Come ti chiami?" )
   oWnd:setWindowTitle( stringa )

   QApplication():exec()

   RETURN
