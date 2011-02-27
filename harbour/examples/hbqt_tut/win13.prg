#include "hbqtgui.ch"

STATIC s_qApp
STATIC oWnd
STATIC stringa
STATIC dialogo

PROCEDURE Main()

   s_qApp := QApplication()
   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Finestra di Giovanni" )
   oWnd:resize( 400, 300 )
   oWnd:show()
   dialogo := QInputDialog()
   stringa = dialogo:getText( oWnd, "Titolo", "Come ti chiami?" )
   oWnd:setWindowTitle( stringa )
   s_qApp:exec()

   RETURN
