#include "hbqtgui.ch"

PROCEDURE Main()

   LOCAL barra_progresso
   LOCAL k

   barra_progresso := QProgressBar()
   barra_progresso:resize( 400, 50 )
   barra_progresso:move( 50, 50 )
   barra_progresso:setRange( 1, 500000 )
   barra_progresso:setWindowTitle( "Elaborazione in corso" )
   barra_progresso:Show()
   barra_progresso:repaint()

   for k = 1 TO 500000
      barra_progresso:setValue( k )
   next k

   barra_progresso:quit()
   QApplication():exec()

   RETURN
