#include "hbqtgui.ch"
STATIC testo

PROCEDURE Main()

   LOCAL oWnd
   LOCAL orolo

   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Finestra di Giovanni" )
   oWnd:resize( 250, 150 )

   testo := Qlabel( oWnd )
   testo:setText( "clocking..." )
   testo:move( 100, 100 )

   orolo := QTimer()
   orolo:Connect( "timeout()", { || stampa_orologio() } )
   orolo:start( 1000 )
   oWnd:show()
   QApplication():exec()

   RETURN

PROCEDURE stampa_orologio

   testo:setText( Time() )

   RETURN
