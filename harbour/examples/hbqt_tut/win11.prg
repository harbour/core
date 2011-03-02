#include "hbqtgui.ch"

PROCEDURE Main()

   LOCAL finestra, ui, file

   file := QFile( "prova.ui" )
   file:open( 1 )
   ui := QUiLoader()

   finestra := ui:load( file )
   file:close()
   finestra:show()

   QApplication():exec()

   RETURN
