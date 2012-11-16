/*
 * $Id$
 */

#require "hbwin"

#include "hbwin.ch"

PROCEDURE Main( cPar1 )

   LOCAL nPrn := 1
   LOCAL cBMPFile := Space( 40 )
   LOCAL aPrn := win_printerList()
   LOCAL GetList := {}

   CLS

   IF Empty( aPrn )
      Alert( "No printers installed - Cannot continue" )
      QUIT
   ENDIF

   DO WHILE nPrn != 0
      CLS
      @ 0, 0 SAY "Win_Prn() Class test program. Choose a printer to test"
      @ 1, 0 SAY "Bitmap file name" GET cBMPFile PICT "@K"
      READ
      @ 2, 0 TO MaxRow(), MaxCol()
      nPrn := AChoice( 3, 1, MaxRow() - 1, MaxCol() - 1, aPrn, .T.,, nPrn )
      IF nPrn != 0
         PrnTest( aPrn[ nPrn ], cBMPFile, iif( HB_ISSTRING( cPar1 ) .AND. Lower( cPar1 ) == "ask", .T., NIL ) )
      ENDIF
   ENDDO

   RETURN

STATIC PROCEDURE PrnTest( cPrinter, cBMPFile, lAsk )

   LOCAL oPrinter := win_Prn():New( cPrinter )
   LOCAL aFonts
   LOCAL x
   LOCAL nColFixed
   LOCAL nColTTF
   LOCAL nColCharSet

   oPrinter:Landscape := .F.
   oPrinter:FormType  := WIN_DMPAPER_A4
   oPrinter:Copies    := 1
   IF HB_ISLOGICAL( lAsk )
      oPrinter:AskProperties := lAsk
   ENDIF

   IF ! oPrinter:Create()
      Alert( "Cannot Create Printer" )
   ELSE
      IF ! oPrinter:startDoc( "Win_Prn(Doc name in Printer Properties)" )
         Alert( "StartDoc() failed" )
      ELSE
         oPrinter:SetPen( WIN_PS_SOLID, 1, HB_WIN_RGB_RED )
         oPrinter:Bold( WIN_FW_EXTRABOLD )
         oPrinter:TextOut( oPrinter:PrinterName + ": MaxRow() = " + Str( oPrinter:MaxRow(), 4 ) + "   MaxCol() = " + Str( oPrinter:MaxCol(), 4 ) )
         oPrinter:Bold( WIN_FW_DONTCARE )
         oPrinter:NewLine()
         oPrinter:TextOut( "   Partial list of available fonts that are available for OEM_" )
         oPrinter:NewLine()
         oPrinter:UnderLine( .T. )
         oPrinter:Italic( .T. )
//       oPrinter:SetFont( "Courier New", 7, { 3, -50 } )  // Compressed print
         nColFixed   := 40 * oPrinter:CharWidth
         nColTTF     := 48 * oPrinter:CharWidth
         nColCharSet := 60 * oPrinter:CharWidth
         oPrinter:TextOut( "FontName" )
         oPrinter:SetPos( nColFixed )
         oPrinter:TextOut( "Fixed?" )
         oPrinter:SetPos( nColTTF )
         oPrinter:TextOut( "TrueType?" )
         oPrinter:SetPos( nColCharset )
         oPrinter:TextOut( "CharSet#", .T. )
         oPrinter:NewLine()
         oPrinter:Italic( .F. )
         oPrinter:UnderLine( .F. )
         aFonts := oPrinter:GetFonts()
         oPrinter:NewLine()
         FOR x := 1 TO Len( aFonts ) STEP 2
            oPrinter:CharSet( aFonts[ x, 4 ] )
            IF oPrinter:SetFont( aFonts[ x, 1 ] )       // Could use "IF oPrinter:SetFontOk" after call to oPrinter:SetFont()
               IF oPrinter:FontName == aFonts[ x, 1 ]  // Make sure Windows didn't pick a different font
                  oPrinter:TextOut( aFonts[ x, 1 ] )
                  oPrinter:SetPos( nColFixed )
                  oPrinter:TextOut( iif( aFonts[ x, 2 ], "Yes", "No" ) )
                  oPrinter:SetPos( nColTTF )
                  oPrinter:TextOut( iif( aFonts[ x, 3 ], "Yes", "No" ) )
                  oPrinter:SetPos( nColCharSet )
                  oPrinter:TextOut( Str( aFonts[ x, 4 ], 5 ) )
                  oPrinter:SetPos( oPrinter:LeftMargin, oPrinter:PosY + ( oPrinter:CharHeight * 2 ) )
                  IF oPrinter:PRow() > oPrinter:MaxRow() - 16  // Could use "oPrinter:NewPage()" to start a new page
                     EXIT
                  ENDIF
               ENDIF
            ENDIF
            oPrinter:Line( 0, oPrinter:PosY + 5, 2000, oPrinter:PosY + 5 )
         NEXT
         oPrinter:SetFont( "Lucida Console", 8, { 3, -50 } )  // Alternative Compressed print
         oPrinter:CharSet( 0 )  // Reset default charset
         oPrinter:Bold( WIN_FW_EXTRABOLD )
         oPrinter:NewLine()
         oPrinter:TextOut( "This is on line" + Str( oPrinter:PRow(), 4 ) + ", Printed bold, " )
         oPrinter:TextOut( " finishing at Column: " )
         oPrinter:TextOut( Str( oPrinter:PCol(), 4 ) )
         oPrinter:SetPRC( oPrinter:PRow() + 3, 0 )
         oPrinter:Bold( WIN_FW_DONTCARE )
         oPrinter:TextOut( "Notice: UNDERLINE only prints correctly if there is a blank line after", .T. )
         oPrinter:TextOut( "        it. This is because of :LineHeight and the next line", .T. )
         oPrinter:TextOut( "        printing over top of the underline. To avoid this happening", .T. )
         oPrinter:TextOut( "        you can to alter :LineHeight or use a smaller font, or use :SetBkMode( WIN_TRANSPARENT )" )
         oPrinter:NewLine()
         oPrinter:NewLine()
         oPrinter:SetFont( "Lucida Console", 18, 0 )  // Large print
         oPrinter:SetColor( HB_WIN_RGB_GREEN )
         oPrinter:TextOut( "Finally some larger print" )
         oPrinter:Box(   0, oPrinter:PosY + 100, 100, oPrinter:PosY + 200 )
         oPrinter:Arc( 200, oPrinter:PosY + 100, 300, oPrinter:PosY + 200 )
         oPrinter:Ellipse( 400, oPrinter:PosY + 100, 500, oPrinter:PosY + 200 )
         oPrinter:FillRect( 600, oPrinter:PosY + 100, 700, oPrinter:PosY + 200, HB_WIN_RGB_RED )

         //       To print a barcode;
         //       Replace 'BCod39HN' with your own bar code font or any other font
         //         oPrinter:TextAtFont( oPrinter:MM_TO_POSX( 30 ), oPrinter:MM_TO_POSY( 60 ), "1234567890", "BCod39HN", 24, 0 )
         //
         PrintBitMap( oPrinter, cBMPFile )

         oPrinter:EndDoc()
      ENDIF
      oPrinter:Destroy()
   ENDIF

   RETURN

STATIC PROCEDURE PrintBitMap( oPrn, cBitFile )

   LOCAL oBMP

   IF Empty( cBitFile )
      //
   ELSEIF ! hb_FileExists( cBitFile )
      Alert( cBitFile + " not found " )
   ELSE
      oBMP := win_BMP():New()
      IF oBmp:loadFile( cBitFile )

         oBmp:Draw( oPrn, { 200, 200, 2000, 1500 } )

         // Note: Can also use this method to print bitmap
         //   oBmp:Rect := { 200, 200, 2000, 1500 }
         //   oPrn:DrawBitMap( oBmp )

      ENDIF
      oBMP:Destroy()
   ENDIF

   RETURN
