/*
 * $Id$
 */

#require "hbnf"

#include "setcurs.ch"

PROCEDURE Main( cVidMode )

   LOCAL nRowDos := Row()
   LOCAL nColDos := Col()
   LOCAL aEnvDos := FT_SaveSets()
   LOCAL cScrDos := SaveScreen( 0, 0, MaxRow(), MaxCol() )
   LOCAL lColour
   LOCAL aClrs

   __defaultNIL( @cVidMode, "" )

   NoSnow( ( "NOSNOW" $ Upper( cVidMode ) ) )
   IF "VGA" $ Upper( cVidMode )
      SetMode( 50, 80 )
   ENDIF
   IF "EGA" $ Upper( cVidMode )
      SetMode( 43, 80 )
   ENDIF
   lColour := iif( "MONO" $ Upper( cVidMode ), .F., IsColor() )

   SET SCOREBOARD OFF
   SetCursor( SC_NONE )
   SetBlink( .F. )

   //.... a typical application might have the following different settings
   //     normally these would be stored in a .dbf/.dbv
   aClrs := { ;
      { "Desktop",        "N/BG",                          "D", hb_UTF8ToStr( "▒" ) }, ;
      { "Title",          "N/W",                           "T" }, ;
      { "Top Menu",       "N/BG,N/W,W+/BG,W+/N,GR+/N",     "M" }, ;
      { "Sub Menu",       "W+/N*,GR+/N*,GR+/N*,W+/R,G+/R", "M" }, ;
      { "Standard Gets",  "W/B,  W+/N,,, W/N",             "G" }, ;
      { "Nested Gets",    "N/BG, W+/N,,, W/N",             "G" }, ;
      { "Help",           "N/G,  W+/N,,, W/N",             "W" }, ;
      { "Error Messages", "W+/R*,N/GR*,,,N/R*",            "W" }, ;
      { "Database Query", "N/BG, N/GR*,,,N+/BG",           "B" }, ;
      { "Pick List",      "N/GR*,W+/B,,, BG/GR*",          "A" } }

   aClrs := FT_ClrSel( aClrs, lColour )

   HB_SYMBOL_UNUSED( aClrs )

   //.... restore the DOS environment
   FT_RestSets( aEnvDos )
   RestScreen( 0, 0, MaxRow(), MaxCol(), cScrDos )
   SetPos( nRowDos, nColDos )
   SetBlink( .F. )  // doesn't appear to be reset from FT_RestSets

   RETURN
