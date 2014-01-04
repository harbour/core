#require "hbnf"

#include "setcurs.ch"

PROCEDURE Main( cVidMode )

   LOCAL nRowOri := Row()
   LOCAL nColOri := Col()
   LOCAL aEnvOri := ft_SaveSets()
   LOCAL cScrOri := SaveScreen( 0, 0, MaxRow(), MaxCol() )
   LOCAL lColor
   LOCAL aClrs

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   __defaultNIL( @cVidMode, "" )

   NoSnow( ( "NOSNOW" $ Upper( cVidMode ) ) )
   IF "VGA" $ Upper( cVidMode )
      SetMode( 50, 80 )
   ENDIF
   IF "EGA" $ Upper( cVidMode )
      SetMode( 43, 80 )
   ENDIF
   lColor := iif( "MONO" $ Upper( cVidMode ), .F., IsColor() )

   SET SCOREBOARD OFF
   SetCursor( SC_NONE )
   SetBlink( .F. )

   // .... a typical application might have the following different settings
   //     normally these would be stored in a .dbf/.dbv
   aClrs := { ;
      { "Desktop",        "N/BG",                          "D", hb_UTF8ToStrBox( "░" ) }, ;
      { "Title",          "N/W",                           "T" }, ;
      { "Top Menu",       "N/BG,N/W,W+/BG,W+/N,GR+/N",     "M" }, ;
      { "Sub Menu",       "W+/N*,GR+/N*,GR+/N*,W+/R,G+/R", "M" }, ;
      { "Standard Gets",  "W/B,  W+/N,,, W/N",             "G" }, ;
      { "Nested Gets",    "N/BG, W+/N,,, W/N",             "G" }, ;
      { "Help",           "N/G,  W+/N,,, W/N",             "W" }, ;
      { "Error Messages", "W+/R*,N/GR*,,,N/R*",            "W" }, ;
      { "Database Query", "N/BG, N/GR*,,,N+/BG",           "B" }, ;
      { "Pick List",      "N/GR*,W+/B,,, BG/GR*",          "A" } }

   aClrs := ft_ClrSel( aClrs, lColor )

   HB_SYMBOL_UNUSED( aClrs )

   // .... restore the original environment
   ft_RestSets( aEnvOri )
   RestScreen( 0, 0, MaxRow(), MaxCol(), cScrOri )
   SetPos( nRowOri, nColOri )
   SetBlink( .F. )  // doesn't appear to be reset from FT_RestSets

   RETURN
