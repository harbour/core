#require "hbnf"

#include "inkey.ch"
#include "setcurs.ch"

PROCEDURE Main()

   LOCAL lColor := .T.

   // color variables
   LOCAL cNormH := iif( lColor, "W+/BG", "W+/N" )
   LOCAL cNormN := iif( lColor, "N/BG", "W/N"  )
   LOCAL cWindH := iif( lColor, "W+/B", "W+/N" )
   LOCAL cWindN := iif( lColor, "W/B", "W/N"  )
   LOCAL cErrH  := iif( lColor, "W+/R", "W+/N" )
   LOCAL cErrN  := iif( lColor, "W/R", "W/N"  )

   LOCAL cOldScrn := SaveScreen()
   LOCAL nOldRow := Row()
   LOCAL nOldCol := Col()

   LOCAL nMaxRow
   LOCAL nType

   // main routine starts here
   Set( _SET_SCOREBOARD, .F. )

   SetColor( "W/N" )
   CLS
   nMaxRow := MaxRow()
   SetBlink( .F. )
   SetColor( cWindN + "*" )
   CLS
   SetColor( cNormN )

   ft_DispMsg( { { "[Esc] To Abort Changes   [PgDn] To Continue" }, { cNormN, , cNormH } }, , nMaxRow - 5 )

   ft_DispMsg( { { "[E]dit     [P]rint    [D]elete", ;
      "[Esc]ape       [Alt-Q]" }, ;
      { cErrN, cErrN, cErrH } }, , 2 )

   nType := ft_DispMsg( { { ;
      "Create Or Edit [I]nvoice", ;
      "Create Or Edit [O]rder", ;
      "Create Or Edit [B]ack Order", ;
      "Create Or Edit [Q]uote", ;
      "[Esc] To Exit" }, ;
      { cWindN, , , , , cWindH } }, "BIOQ" + hb_BChar( K_ESC ) )

   HB_SYMBOL_UNUSED( nType )

   SetColor( "W/N" )
   SetCursor( SC_NORMAL )
   SetBlink( .T. )
   RestScreen( , , , , cOldScrn )
   SetPos( nOldRow, nOldCol )

   RETURN
