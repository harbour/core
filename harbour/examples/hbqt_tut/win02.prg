#include "hbqtgui.ch"

STATIC s_qApp
STATIC oWnd
STATIC calendario

PROCEDURE Main()

   s_qApp := QApplication()
   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Finestra di Giovanni" )
   oWnd:resize( 640, 480 )
   calendario := QCalendarWidget( oWnd )
   calendario:resize( 250, 200 )
   calendario:move( 50, 50 )
   calendario:setFirstDayOfWeek( 1 )
   calendario:setGridVisible( .T. )
   oWnd:show()
   s_qApp:exec()

   RETURN
