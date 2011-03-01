#include "hbqtgui.ch"

PROCEDURE Main()

   LOCAL s_qApp
   LOCAL oWnd
   LOCAL barra_schede
   LOCAL clienti, fornitori, magazzino

   s_qApp := QApplication()

   oWnd := QMainWindow()
   oWnd:setWindowTitle( "Finestra di Giovanni" )
   oWnd:resize( 500, 300 )

   barra_schede := QTabWidget( oWnd )
   barra_schede:resize( 400, 200 )
   barra_schede:move( 50, 50 )

   clienti  := QWidget()
   fornitori := QWidget()
   magazzino := QWidget()

   barra_schede:addTab( clienti, "Scheda Clienti" )
   barra_schede:addTab( fornitori, "Scheda Fornitori" )
   barra_schede:addTab( magazzino, "Scheda Magazzino" )

   oWnd:show()
   s_qApp:exec()

   RETURN
