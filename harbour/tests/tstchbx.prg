/*
 * $Id$
 */

PROCEDURE Main()

   LOCAL GetList := {}

   LOCAL cItem := "Windows"
   LOCAL lx := .F.
   LOCAL ly := .F.

   LOCAL aItems[ 3 ]

   aItems[ 1 ] := RadioButto( 3, 3, "&Windows" )
   aItems[ 2 ] := RadioButto( 4, 3, "&Linux" )
   aItems[ 3 ] := RadioButto( 5, 3, "&OS X" )

   CLS
   SetColor( "W/B+,R/B,G+/R,B+/R+,BG/N+,W/BG,RB/BG" )

   @  2,  2, 6, 40 GET cItem RADIOGROUP aItems COLOR "W/B+,R/B,G/B+" MESSAGE "Select your OS"

   @  8,  3 SAY "Married"
   @  8, 12 GET lx CHECKBOX COLOR "W/B+,W/B,W+/R,W/G+" MESSAGE "Are you married?"
   @  9,  3 SAY "Singer"
   @  9, 12 GET ly CHECKBOX COLOR "W/B+,W/B,W+/R,W/G+" MESSAGE "Are you a singer?"

   READ MSG AT MaxRow(), 0, MaxCol() MSG COLOR "W/B+"

   ? "Is the person married:", iif( lx, "Yes", "No" )
   ? "Is the person a singer:", iif( ly, "Yes", "No" )
   ? "Your OS is", cItem

   RETURN
