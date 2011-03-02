#include "hbqtgui.ch"
STATIC testo
STATIC orolo

PROCEDURE Main()

   LOCAL oWnd
   LOCAL pulsante_start, pulsante_stop

   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Finestra di Giovanni" )
   oWnd:resize( 300, 200 )
   testo := Qlabel( oWnd )
   testo:setText( "clocking..." )
   testo:move( 50, 50 )
   testo:resize( 200, 100 )
   orolo := QTimer()
   orolo:Connect( "timeout()", { || stampa_orologio() } )
   pulsante_start := QPushButton( oWnd )
   pulsante_start:move( 150, 50 )
   pulsante_start:setText( "Start" )
   pulsante_start:connect( "pressed()", { || start() } )
   pulsante_stop := QPushButton( oWnd )
   pulsante_stop:move( 150, 100 )
   pulsante_stop:setText( "Stop" )
   pulsante_stop:connect( "pressed()", { || stop() } )
   oWnd:show()
   QApplication():exec()

   RETURN

PROCEDURE stampa_orologio

   testo:setText( Time() )

   RETURN

PROCEDURE start

   orolo:start( 1000 )

   RETURN

PROCEDURE stop

   orolo:stop()

   RETURN
