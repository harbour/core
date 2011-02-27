#include "hbqtgui.ch"

STATIC applicazione, finestra, ui, file

PROCEDURE Main()

   applicazione := QApplication()
   file := QFile( "prova.ui" )
   file:open( 1 )
   ui := QUiLoader()
   finestra := ui:load( file )
   file:close()
   finestra:show()
   applicazione:exec()
   applicazione:quit()

   RETURN
