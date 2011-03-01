#include "hbqtgui.ch"
STATIC tabella

PROCEDURE Main()

   //------------Dichiarazioni--------------
   LOCAL applicazione, finestra
   LOCAL cella, valore
   LOCAL num_campi, num_record, intestazioni
   LOCAL x, y
   LOCAL pulsante_primo, pulsante_ultimo
   applicazione := QApplication()
   finestra := QMainWindow()
   finestra:resize( 800, 600 )
   //------------Apre DBF e conta campi e record------------
   creafiletest()
   USE test
   num_record = RecCount()
   num_campi = FCount()
   //------------Dimensiona tabella--------------
   tabella := QTableWidget( finestra )
   tabella:move( 50, 50 )
   tabella:resize( 700, 450 )
   tabella:setRowCount( num_record )
   tabella:setColumnCount( num_campi )
   tabella:setColumnWidth( 0, 200 )
   //------------Riempie tabella--------------
   for x = 1 TO num_record
      for y = 1 TO num_campi
         cella := QTableWidgetItem()
         valore = FieldGet( y )
         DO CASE
         CASE ValType( valore ) = "C"
            cella:setText( valore )
         CASE ValType( valore ) = "N"
            cella:setText( AllTrim( Str(valore ) ) )
         CASE ValType( valore ) = "D"
            cella:setText( Dtoc( valore ) )
         CASE ValType( valore ) = "L"
            cella:setText( iif( valore = .T. ,"Yes","No" ) )
         end CASE
         tabella:setItem( x - 1, y - 1, cella )
      next y
      SKIP
   next x
   //------------Crea intestazioni tabella--------------
   intestazioni := QStringList()
   for x = 1 TO 50
      intestazioni:append( field( x ) )
   next k
   tabella:setHorizontalHeaderLabels( intestazioni )
   USE
   //------------Pulsanti--------------
   pulsante_primo := QPushButton( finestra )
   pulsante_primo:move( 100, 520 )
   pulsante_primo:setText( "Primo" )
   pulsante_primo:Connect( "clicked()", { || primo() } )
   pulsante_ultimo := QPushButton( finestra )
   pulsante_ultimo:move( 300, 520 )
   pulsante_ultimo:setText( "Ultimo" )
   pulsante_ultimo:Connect( "clicked()", { || ultimo() } )
   //------------Esecuzione--------------
   finestra:show()
   applicazione:exec()
   applicazione:quit()

   RETURN

PROCEDURE primo()

   tabella:scrollToTop()
   tabella:setCurrentCell( 0, 0 )
   tabella:setFocus()

   RETURN

PROCEDURE ultimo()

   tabella:scrollToBottom()
   tabella:setCurrentCell( tabella:rowCount() - 1, 0 )
   tabella:setFocus()

   RETURN

PROCEDURE creafiletest()

   LOCAL aCampi, i, m_num

   IF .NOT. File( "test.dbf" )
      aCampi := {}
      AAdd( aCampi, { "num", "N", 6, 0 } )
      AAdd( aCampi, { "nome", "C", 20, 0 } )
      AAdd( aCampi, { "cognome", "C", 25, 0 } )
      AAdd( aCampi, { "dt_nasc", "D", 8, 0 } )
      AAdd( aCampi, { "yesno", "L", 1, 0 } )
      dbCreate( "test.dbf", aCampi, "DBFNTX" )
   ENDIF
   USE test
   GO BOTTOM
   m_num := test -> num
   for i := 1 TO 50
      APPEND BLANK
      test -> num := ++ m_num
      test -> nome := "pippo"
      test -> cognome := "pluto"
      test -> dt_nasc := CToD( "10/12/2001" )
      test -> yesno := .F.
   next
   CLOSE test

   RETURN
