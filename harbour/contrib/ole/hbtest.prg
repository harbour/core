*
*  HBTEST.PRG
*
*  Este ejemplo es para probar con Harbour en modo consola, sin FiveWin
*  para Harbour.
*
*  This example is done for testing with Harbour in console mode, without
*  FiveWin for Harbour.
*


#define CRLF Chr( 13 ) + Chr( 10 )


PROCEDURE MAIN()

   LOCAL nOption

   CLS
   SetColor("W+/R")
   @  6, 25 TO 16, 55 DOUBLE
   @  8, 28 SAY "Test hbole.lib with ..."

   While .t.
      @ 10, 32 PROMPT "Excel"
      @ 11, 32 PROMPT "Word"
      @ 12, 32 PROMPT "Internet Explorer"
      @ 13, 32 PROMPT "Outlook"
      @ 14, 32 PROMPT "Quit"

      MENU TO nOption

      IF nOption == 0
         nOption := 5
      ELSEIF nOption == 1
         EXCEL97()
      ELSEIF nOption == 2
         WORD97()
      ELSEIF nOption == 3
         IEXPLORER()
      ELSEIF nOption == 4
         OUTLOOK()
      ELSEIF nOption == 5
         EXIT
      ENDIF
   End

   SetColor("W/N")
   CLS

RETURN

//--------------------------------------------------------------------

STATIC PROCEDURE EXCEL97()

   LOCAL oExcel, oHoja

   oExcel := TOleAuto():New( "Excel.Application" )

   oExcel:WorkBooks:Add()

   oHoja := oExcel:ActiveSheet()

   oHoja:Cells:Font:Name := "Arial"
   oHoja:Cells:Font:Size := 12

   oHoja:Cells( 3, 1 ):Value := "Text:"
   oHoja:Cells( 3, 2 ):Value := "This is sample text"
   oHoja:Cells( 4, 1 ):Value := "Numeric:"
   oHoja:Cells( 4, 2 ):NumberFormat := "#.##0,00"
   oHoja:Cells( 4, 2 ):Value := 1234.50
   oHoja:Cells( 5, 1 ):Value := "Logical:"
   oHoja:Cells( 5, 2 ):Value := .T.
   oHoja:Cells( 6, 1 ):Value := "Date:"
   oHoja:Cells( 6, 2 ):Value := DATE()

   oHoja:Columns( 1 ):Font:Bold := .T.
   oHoja:Columns( 2 ):HorizontalAlignment := -4152  // xlRight

   oHoja:Columns( 1 ):AutoFit()
   oHoja:Columns( 2 ):AutoFit()

   oHoja:Cells( 1, 1 ):Value := "OLE in Harbour"
   oHoja:Cells( 1, 1 ):Font:Size := 16
   oHoja:Range( "A1:B1" ):HorizontalAlignment := 7

   oHoja:Cells( 1, 1 ):Select()
   oExcel:Visible := .T.

   oHoja:End()
   oExcel:End()

RETURN

//--------------------------------------------------------------------

STATIC PROCEDURE WORD97()

   LOCAL oWord, oTexto

   oWord:=TOleAuto():New( "Word.Application" )

   oWord:Documents:Add()

   oTexto := oWord:Selection()

   oTexto:Text := "Harbour hbOLE.lib"+CRLF+"Word Sample"+CRLF
   oTexto:Font:Name := "Arial"
   oTexto:Font:Size := 48
   oTexto:Font:Bold := .T.

   oWord:Visible := .T.
   oWord:WindowState := 1  // Maximize

   oTexto:End()
   oWord:End()

RETURN

//--------------------------------------------------------------------

STATIC PROCEDURE IEXPLORER()

   LOCAL oIE

   oIE:=TOleAuto():New( "InternetExplorer.Application" )

   oIE:Visible := .T.

   oIE:Navigate( "http://www.harbour-project.org" )

   oIE:End()

RETURN

//--------------------------------------------------------------------

STATIC PROCEDURE OUTLOOK()

   LOCAL oOL, oLista, oMail, i

   oOL := TOleAuto():New( "Outlook.Application.9" )

   IF Ole2TxtError() != "S_OK"

      Alert("Outlook is not available", "Error")

   ELSE

      oMail := oOL:CreateItem( 0 )  // olMailItem

      FOR i := 1 TO 10
         oMail:Recipients:Add( "Contact" + LTRIM( STR( i, 2 ) ) + ;
               "<contact" + LTRIM( STR( i, 2 ) ) + "@server.com>" )
      NEXT

      oLista := oOL:CreateItem( 7 )  // olDistributionListItem
      oLista:DLName := "Test with distribution list"
      oLista:Display( .F. )
      oLista:AddMembers( oMail:Recipients )
      oLista:Save()
      oLista:Close( 0 )

      oMail:End()
      oLista:End()
      oOL:End()

   ENDIF

RETURN
