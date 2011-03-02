#include "hbqtgui.ch"

PROCEDURE Main()

   LOCAL oWnd
   LOCAL calendario

   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Finestra di Giovanni" )
   oWnd:resize( 400, 300 )

   calendario := QCalendarWidget( oWnd )
   calendario:resize( 250, 200 )
   calendario:move( 50, 50 )
   calendario:setFirstDayOfWeek( 1 )
   calendario:setGridVisible( .T. )

   oWnd:show()
   QApplication():exec()

   RETURN
