/*
 * $Id$ 
 */

PROCEDURE Main()

   LOCAL cDosScrn
   LOCAL nDosRow
   LOCAL nDosCol
   LOCAL lColor
   LOCAL nMaxRow
   LOCAL nType

   // color variables
   LOCAL cNormH, cNormN
   LOCAL cWindH, cWindN
   LOCAL cErrH, cErrN

   // main routine starts here
   SET SCOREBOARD OFF

   lColor := .T.

   cNormH := iif( lColor, "W+/BG", "W+/N" )
   cNormN := iif( lColor, "N/BG" , "W/N"  )
   cWindH := iif( lColor, "W+/B", "W+/N" )
   cWindN := iif( lColor, "W/B" , "W/N"  )
   cErrH  := iif( lColor, "W+/R", "W+/N" )
   cErrN  := iif( lColor, "W/R" , "W/N"  )

   cDosScrn := SaveScreen()
   nDosRow := Row()
   nDosCol := Col()
   SetColor( "W/N" )
   CLS
   nMaxRow := MaxRow()
   SetBlink( .F. )
   SetColor( cWindN + "*" )
   CLS
   SetColor( cNormN )

   FT_DispMsg( { { "[Esc] To Abort Changes   [PgDn] To Continue" }, { cNormN, , cNormH } }, , nMaxRow - 5 )

   FT_DispMsg( { { "[E]dit     [P]rint    [D]elete", ;
      "[Esc]ape       [Alt-Q]" }, ;
      { cErrN, cErrN, cErrH } }, , 2 )

   nType := FT_DispMsg( { { ;
      "Create Or Edit [I]nvoice"    ,;
      "Create Or Edit [O]rder"      ,;
      "Create Or Edit [B]ack Order" ,;
      "Create Or Edit [Q]uote"      ,;
      "[Esc] To Exit" }             ,;
      { cWindN, , , , , cWindH } }, "BIOQ" + Chr( K_ESC ) )

   HB_SYMBOL_UNUSED( nType )

   SetColor( "W/N" )
   SetCursor( SC_NORMAL )
   SetBlink( .T. )
   RestScreen( , , , , cDosScrn )
   SetPos( nDosRow, nDosCol )
   QUIT

