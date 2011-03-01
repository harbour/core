#include "hbqtgui.ch"

PROCEDURE Main()

   LOCAL s_qApp
   LOCAL oWnd
   LOCAL calendario

   s_qApp := QApplication()

   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Finestra di Giovanni" )
   oWnd:resize( 400, 300 )

   calendario := QCalendarWidget( oWnd )
   calendario:resize( 250, 200 )
   calendario:move( 50, 50 )
   calendario:setFirstDayOfWeek( 1 )
   calendario:setGridVisible( .T. )

   oWnd:show()
   s_qApp:exec()

   RETURN
