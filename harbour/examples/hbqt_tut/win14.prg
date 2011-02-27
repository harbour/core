#include "hbqtgui.ch"

STATIC s_qApp
STATIC barra_progresso

PROCEDURE Main()

   LOCAL k

   s_qApp := QApplication()
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
   s_qApp:exec()

   RETURN
