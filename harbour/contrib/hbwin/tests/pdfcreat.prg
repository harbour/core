/*
 * $Id$
 */

/*
 * Harbour Project source code
 * Demonstration code for generating .pdf documents using PDFCreator 
 *   COM interface.
 *
 * You should install PDFCreator to be able to run this test
 *
 * Download site:
 * http://sourceforge.net/projects/pdfcreator/
 *
 * COM interface docs:
 * http://www.pdfforge.org/content/com-interface
 *
 * Copyright 2010 Mindaugas Kavaliauskas <dbtopas / at / dbtopas.lt>
 * www - http://harbour-project.org
 *
 */

PROC main()
LOCAL oPC, nTime, cDefaultPrinter, cFilename, oPrinter, nEvent := 0

   IF EMPTY( oPC := WIN_OLECreateObject( "PDFCreator.clsPDFCreator" ) )
      ? "Unable to create PDFCreator COM object"
      RETURN
   ENDIF

   cFilename := HB_PROGNAME()

   /* Setup event notification */
   oPC:__hSink := __AxRegisterHandler( oPC:__hObj, {|X| nEvent := X}, "{58B69879-9ED8-468D-879F-787161FA105F}")

   oPC:cStart( "/NoProcessingAtStartup" )
   oPC:_cOption( "UseAutosave", 1 )
   oPC:_cOption( "UseAutosaveDirectory", 1 )
   oPC:_cOption( "AutosaveDirectory", LEFT( cFileName, RAT( HB_PS(), cFilename ) - 1 ) )
   oPC:_cOption( "AutosaveFilename", "pdfcreat.pdf" )
   oPC:_cOption( "AutosaveFormat", 0 )

   cDefaultPrinter := oPC:cDefaultPrinter
   oPC:cDefaultPrinter := "PDFCreator"
   oPC:cClearCache()

   /* You can do any printing here using WinAPI or 
      call a 3rd party application to do printing */
#if 1
   oPrinter := Win_Prn():New( "PDFCreator" )
   oPrinter:Create()
   oPrinter:startDoc( "Harbour print job via PDFCreator" )
   oPrinter:NewLine()
   oPrinter:NewLine()
   oPrinter:TextOut( "Hello, PDFCreator! This is Harbour :)" )
   oPrinter:EndDoc()
   oPrinter:Destroy()
#else
   ? "Do some printing to PDFCreator printer and press any key..."
   INKEY(0)
#endif

   oPC:cPrinterStop := .F.

   nTime := hb_milliseconds()
   DO WHILE nEvent == 0 .AND. hb_milliseconds() - nTime < 10000
      hb_idleSleep( 0.5 )
      /* The following dummy line is required to allow COM server to send event [Mindaugas] */
      oPC:cOption("UseAutosave") 
   ENDDO

   IF nEvent == 0
      ? "Print timeout"
   ELSEIF nEvent == 1
      ? "Printed successfully"
   ELSEIF nEvent == 2
      ? "Error:", oPC:cError():Description
   ELSE
      ? "Unknown event"
   ENDIF

   oPC:cDefaultPrinter := cDefaultPrinter
   oPC:cClose()
   oPC := NIL
RETURN
