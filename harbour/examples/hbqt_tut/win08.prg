#include "hbqtgui.ch"

STATIC s_qApp
STATIC oWnd
STATIC testo
STATIC orolo
STATIC pulsante_start, pulsante_stop

PROCEDURE Main()

   s_qApp := QApplication()
   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Finestra di Giovanni" )
   oWnd:resize( 640, 480 )
   testo := Qlabel( oWnd )
   testo:setText( "clocking..." )
   testo:move( 100, 100 )
   testo:resize( 200, 100 )
   orolo := QTimer()
   orolo:Connect( "timeout()", { || stampa_orologio() } )
   pulsante_start := QPushButton( oWnd )
   pulsante_start:move( 300, 100 )
   pulsante_start:setText( "Start" )
   pulsante_start:connect( "pressed()", { || start() } )
   pulsante_stop := QPushButton( oWnd )
   pulsante_stop:move( 300, 200 )
   pulsante_stop:setText( "Stop" )
   pulsante_stop:connect( "pressed()", { || stop() } )
   oWnd:show()
   s_qApp:exec()

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
