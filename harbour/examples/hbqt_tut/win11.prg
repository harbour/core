#include "hbqtgui.ch"

PROCEDURE Main()

   LOCAL applicazione, finestra, ui, file

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
