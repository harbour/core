/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration code for FOR EACH used for OLE objects
 *    this code needs HBWIN library
 *
 * Copyright 2007 Enrico Maria Giordano e.m.giordano at emagsoftware.it
 * www - http://www.harbour-project.org
 *
 */

/* Explicit usage of OLE DEFAULT Method when syntax implies it. */
#xtranslate :<!Method!>( <args,...> ) := => :<Method>( <args> ):Value :=

PROCEDURE Main()

   LOCAL nOption

   CLS

   @  6, 25 TO 19, 55 DOUBLE
   @  8, 28 SAY "Test Harbour OLE with..."

   DO WHILE .T.
      @ 10, 32 PROMPT "MS Excel"
      @ 11, 32 PROMPT "MS Word"
      @ 12, 32 PROMPT "MS Outlook (1)"
      @ 13, 32 PROMPT "MS Outlook (2)"
      @ 14, 32 PROMPT "Internet Explorer"
      @ 15, 32 PROMPT "XP CDO"
      @ 16, 32 PROMPT "OpenOffice"
      @ 17, 32 PROMPT "Quit"

      MENU TO nOption

      IF nOption == 0
         nOption := 8
      ELSEIF nOption == 1
         Exm_MSExcel()
      ELSEIF nOption == 2
         Exm_MSWord()
      ELSEIF nOption == 3
         Exm_MSOutlook()
      ELSEIF nOption == 4
         Exm_MSOutlook2()
      ELSEIF nOption == 5
         Exm_IExplorer()
      ELSEIF nOption == 6
         Exm_CDO()
      ELSEIF nOption == 7
         Exm_OpenOffice()
      ELSEIF nOption == 8
         EXIT
      ENDIF
   ENDDO

   CLS

   RETURN

// ; Requires Windows XP

STATIC PROCEDURE Exm_CDO()

   LOCAL oCDOMsg
   LOCAL oCDOConf

   IF ( oCDOMsg  := CreateObject( "CDO.Message" ) ) != NIL .AND. ;
      ( oCDOConf := CreateObject( "CDO.Configuration" ) ) != NIL

      oCDOConf:Fields("http://schemas.microsoft.com/cdo/configuration/sendusing") := 2 /* cdoSendUsingPort */
      oCDOConf:Fields("http://schemas.microsoft.com/cdo/configuration/smtpserver") := "localhost"
      oCDOConf:Fields("http://schemas.microsoft.com/cdo/configuration/smtpserverport") := 25
      oCDOConf:Fields("http://schemas.microsoft.com/cdo/configuration/smtpconnectiontimeout") := 120
      oCDOConf:Fields:Update()

      oCDOMsg:Configuration := oCDOConf
      oCDOMsg:BodyPart:Charset := "iso-8859-2" // "iso-8859-1" "utf-8"
      oCDOMsg:To := "test@localhost"
      oCDOMsg:From := "sender@localhost"
      oCDOMsg:Subject := "Test message"
      oCDOMsg:TextBody := "Test message body"

      oCDOMsg:Send()
   ELSE
      Alert( "Error: CDO subsystem not available. [" + OLEErrorText()+ "]" )
   ENDIF

   RETURN

STATIC PROCEDURE Exm_IExplorer()

   LOCAL oIE

   IF ( oIE := CreateObject( "InternetExplorer.Application" ) ) != NIL
      oIE:Visible := .T.
      oIE:Navigate( "http://www.harbour-project.org" )
   ELSE
      Alert( "Error: IExplorer not available. [" + OLEErrorText()+ "]" )
   ENDIF

   RETURN

STATIC PROCEDURE Exm_MSExcel()

   LOCAL oExcel
   LOCAL oWorkBook
   LOCAL oWorkSheet
   LOCAL oAS

   IF ( oExcel := CreateObject( "Excel.Application" ) ) != NIL

      oWorkBook := oExcel:WorkBooks:Add()

      FOR EACH oWorkSheet IN oWorkBook:WorkSheets
         ? oWorkSheet:Name
      NEXT

      oAS := oExcel:ActiveSheet()

      oAS:Cells:Font:Name := "Arial"
      oAS:Cells:Font:Size := 12

      // Explicit use of DEFAULT method by means of #xtranslate above!!!
      oAS:Cells( 3, 1 ) := "Explict DEFAULT Method Text:"

      // Array notation seem to have REVERSED indexs for the Cells Collections!!!
      // Implicitly using DEFAULT Method
      oAS:Cells[ 2, 3 ] := "Implicit DEFAULT Method using *reversed* array index notation"

      // Operator overloading will attempt explict resolutin using :OleValue
      oAS:Cells[ 2, 3 ] += "!"

      oAS:Cells( 4, 1 ):Value := "Numeric:"
      oAS:Cells( 4, 2 ):NumberFormat := "#.##0,00"

      oAS:Cells[ 2, 4 ] := 1234.50
      oAS:Cells[ 2, 4 ] *= 4
      ? oAS:Cells[ 2, 4 ], oAS:Cells[ 2, 4 ]:Value
      oAS:Cells[ 2, 4 ] /= 2
      ? oAS:Cells[ 2, 4 ], oAS:Cells[ 2, 4 ]:Value

      oAS:Cells[ 2, 4 ]++
      ? oAS:Cells[ 2, 4 ], oAS:Cells[ 2, 4 ]:Value
      oAS:Cells[ 2, 4 ]--
      ? oAS:Cells[ 2, 4 ], oAS:Cells[ 2, 4 ]:Value

      oAS:Cells( 5, 1 ):Value := "Logical:"
      oAS:Cells( 5, 2 ):Value := .T.
      oAS:Cells( 6, 1 ):Value := "Date:"
      oAS:Cells( 6, 2 ):Value := DATE()

      oAS:Columns( 1 ):Font:Bold := .T.
      oAS:Columns( 2 ):HorizontalAlignment := -4152  // xlRight

      oAS:Columns( 1 ):AutoFit()
      oAS:Columns( 2 ):AutoFit()

      oAS:Cells( 1, 1 ):Value := "OLE from Harbour"
      oAS:Cells( 1, 1 ):Font:Size := 16
      oAS:Range( "A1:B1" ):HorizontalAlignment := 7

      oAS:Cells( 1, 1 ):Select()

      oExcel:Visible := .T.

      oExcel:Quit()
   ELSE
      Alert( "Error: MS Excel not available. [" + OLEErrorText()+ "]" )
   ENDIF

   RETURN

STATIC PROCEDURE Exm_MSWord()

   LOCAL oWord
   LOCAL oText

   IF ( oWord := CreateObject( "Word.Application" ) ) != NIL

      oWord:Documents:Add()

      oText := oWord:Selection()

      oText:Text := "OLE from Harbour" + hb_OSNewLine()
      oText:Font:Name := "Arial"
      oText:Font:Size := 48
      oText:Font:Bold := .T.

      oWord:Visible := .T.
      oWord:WindowState := 1 /* Maximize */
   ELSE
      Alert( "Error: MS Word not available. [" + OLEErrorText()+ "]" )
   ENDIF

   RETURN

STATIC PROCEDURE Exm_MSOutlook()

   LOCAL oOL
   LOCAL oList

   IF ( oOL := CreateObject( "Outlook.Application" ) ) != NIL
      oList := oOL:CreateItem( 7 /* olDistributionListItem */ )
      oList:DLName := "Distribution List"
      oList:Display( .F. )
   ELSE
      Alert( "Error: MS Outlook not available. [" + OLEErrorText()+ "]" )
   ENDIF

   RETURN

STATIC PROCEDURE Exm_MSOutlook2()

   LOCAL oOL
   LOCAL oLista
   LOCAL oMail
   LOCAL i

   IF ( oOL := hb_OleAuto():New( "Outlook.Application.9" ) ) != NIL

      oMail := oOL:CreateItem( 0 /* olMailItem */ )

      FOR i := 1 TO 10
         oMail:Recipients:Add( "Contact" + LTRIM( STR( i, 2 ) ) + ;
               "<contact" + LTRIM( STR( i, 2 ) ) + "@server.com>" )
      NEXT

      oLista := oOL:CreateItem( 7 /* olDistributionListItem */ )
      oLista:DLName := "Test with distribution list"
      oLista:Display( .F. )
      oLista:AddMembers( oMail:Recipients )
      oLista:Save()
      oLista:Close( 0 )

      oMail:End()
      oLista:End()
      oOL:End()
   ELSE
      Alert("Outlook is not available", "Error")
   ENDIF

   RETURN

STATIC PROCEDURE Exm_OpenOffice()

   LOCAL oOO_ServiceManager
   LOCAL oOO_Desktop
   LOCAL oOO_PropVal01
   LOCAL oOO_Doc

   IF ( oOO_ServiceManager := CreateObject( "com.sun.star.ServiceManager" ) ) != NIL

      IF ( oOO_Desktop := oOO_ServiceManager:createInstance( "com.sun.star.frame.Desktop" ) ) != NIL
         IF ( oOO_PropVal01 := oOO_ServiceManager:Bridge_GetStruct( "com.sun.star.beans.PropertyValue" ) ) != NIL
            IF ( oOO_Doc := oOO_Desktop:loadComponentFromURL( OO_ConvertToURL( hb_FNameMerge( hb_dirBase(), "sample.odt" ) ), "_blank", 0, { oOO_PropVal01 } ) ) != NIL

              // ...

              oOO_Doc:Close( .T. )
              oOO_Doc := NIL
            ELSE
               Alert( "Error: #3: " + OO_ConvertToURL( hb_FNameMerge( hb_dirBase(), "sample.odt" ) ) )
            ENDIF

            oOO_PropVal01 := NIL
         ELSE
            Alert( "Error: #2" )
         ENDIF

         oOO_Desktop:Terminate()
         oOO_Desktop := NIL
      ELSE
         Alert( "Error: #1" )
      ENDIF

      oOO_ServiceManager := NIL

   ELSE
      Alert( "Error: OpenOffice not available. [" + OLEErrorText()+ "]" )
   ENDIF

   RETURN

STATIC FUNCTION OO_ConvertToURL( cString )

   // ; Handle UNC paths
   IF !( Left( cString, 2 ) == "\\" )
      cString := StrTran( cString, ":", "|" )
      cString := "///" + cString
   ENDIF

   cString := StrTran( cString, "\", "/" )
   cString := StrTran( cString, " ", "%20" )

   RETURN "file:" + cString
