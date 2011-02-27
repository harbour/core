#include "hbqtgui.ch"

STATIC s_qApp
STATIC oWnd
STATIC testo
STATIC orolo

PROCEDURE Main()

   s_qApp := QApplication()
   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Finestra di Giovanni" )
   oWnd:resize( 640, 480 )
   testo := Qlabel( oWnd )
   testo:setText( "clocking..." )
   testo:move( 100, 100 )
   orolo := QTimer()
   orolo:Connect( "timeout()", { || stampa_orologio() } )
   orolo:start( 1000 )
   oWnd:show()
   s_qApp:exec()

   RETURN

PROCEDURE stampa_orologio

   testo:setText( Time() )

   RETURN
