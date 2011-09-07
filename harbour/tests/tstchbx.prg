/*
 * $Id$
 */

FUNCTION Main()

   LOCAL GetList := {}

   LOCAL lx := .F.
   LOCAL ly := .F.
   LOCAL cItem := "Windows NT/2000"

   LOCAL aItems[ 4 ]

   aItems[ 1 ] := RADIOBUTTO( 3, 3, "&Windows NT/2000" )
   aItems[ 2 ] := RADIOBUTTO( 4, 3, "W&indows 9x" )
   aItems[ 3 ] := RADIOBUTTO( 5, 3, "&Linux" )
   aItems[ 4 ] := RADIOBUTTO( 6, 3, "&Mac OS" )

   CLS
   SetColor( "W/B+,R/B,G+/R,B+/R+,BG/N+,W/BG,RB/BG" )

   @  2, 2, 7,40 GET cItem RADIOGROUP aItems COLOR "W/B+,R/B,G/B+" MESSAGE "Select your OS"

   @  8, 3 SAY "Married"
   @  8,12 GET lx CHECKBOX COLOR "W/B+,W/B,W+/R,W/G+" MESSAGE "Is you married?"
   @  9, 3 SAY "Singer"
   @  9,12 GET ly CHECKBOX COLOR "W/B+,W/B,W+/R,W/G+" MESSAGE "Are you a singer"

   READ MSG AT MaxRow(), 0, MaxCol() MSG COLOR "W/B+"

   ? "Is the person married:", iif( lx, "Yes", "No" )
   ? "Is the person a singer:", iif( ly, "Yes", "No" )
   ? "Your OS is", cItem

   RETURN NIL
