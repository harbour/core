#require "hbnf"

#include "setcurs.ch"

PROCEDURE Main( cCmdLine )

   LOCAL sDosScrn, nDosRow, nDosCol, lColor

   // my approach to color variables
   // see colorchg.arc on NANFORUM
   LOCAL cNormN
   LOCAL cWindN
   LOCAL cErrH
   LOCAL cErrN

   // options on menu bar
   LOCAL aColors
   LOCAL aBar     := { " ENTER/EDIT ", " REPORTS ", " DISPLAY ", " MAINTENANCE ", " QUIT " }
   LOCAL aOptions[ Len( aBar ) ]

   LOCAL nMaxRow

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   AEval( aBar, {| x, i | HB_SYMBOL_UNUSED( x ), aOptions[ i ] := { {}, {}, {} } } )

   cCmdLine := iif( cCmdLine == NIL, "", cCmdLine )

   lColor := iif( "MONO" $ Upper( cCmdLine ), .F., IsColor() )

   // Border, Box, Bar, Current, Unselected
   aColors := iif( lColor, { "W+/G", "N/G", "N/G", "N/W", "N+/G" }, ;
      { "W+/N", "W+/N", "W/N", "N/W", "W/N" } )

   ft_Fill( aOptions[ 1 ], "A. Execute A Dummy Procedure"        , {|| fubar() }, .T. )
   ft_Fill( aOptions[ 1 ], "B. Enter Daily Charge/Credit Slips"  , {|| .T. }, .T. )
   ft_Fill( aOptions[ 1 ], "C. Enter Payments On Accounts"       , {|| .T. }, .F. )
   ft_Fill( aOptions[ 1 ], "D. Edit Daily Transactions"          , {|| .T. }, .T. )
   ft_Fill( aOptions[ 1 ], "E. Enter/Update Member File"         , {|| .T. }, .T. )
   ft_Fill( aOptions[ 1 ], "F. Update Code File"                 , {|| .T. }, .F. )
   ft_Fill( aOptions[ 1 ], "G. Add/Update Auto Charge File"      , {|| .T. }, .T. )
   ft_Fill( aOptions[ 1 ], "H. Post All Transactions To A/R File", {|| .T. }, .T. )
   ft_Fill( aOptions[ 1 ], "I. Increment Next Posting Date"      , {|| .T. }, .T. )

   ft_Fill( aOptions[ 2 ], "A. Print Member List"                , {|| .T. }, .T. )
   ft_Fill( aOptions[ 2 ], "B. Print Active Auto Charges"        , {|| .T. }, .T. )
   ft_Fill( aOptions[ 2 ], "C. Print Edit List"                  , {|| .T. }, .T. )
   ft_Fill( aOptions[ 2 ], "D. Print Pro-Usage Report"           , {|| .T. }, .T. )
   ft_Fill( aOptions[ 2 ], "E. Print A/R Transaction Report"     , {|| .T. }, .T. )
   ft_Fill( aOptions[ 2 ], "F. Aging Report Preparation"         , {|| .T. }, .T. )
   ft_Fill( aOptions[ 2 ], "G. Add Interest Charges"             , {|| .T. }, .T. )
   ft_Fill( aOptions[ 2 ], "H. Print Aging Report"               , {|| .T. }, .T. )
   ft_Fill( aOptions[ 2 ], "I. Print Monthly Statements"         , {|| .T. }, .T. )
   ft_Fill( aOptions[ 2 ], "J. Print Mailing Labels"             , {|| .T. }, .T. )
   ft_Fill( aOptions[ 2 ], "K. Print Transaction Totals"         , {|| .T. }, .T. )
   ft_Fill( aOptions[ 2 ], "L. Print Transaction Codes File"     , {|| .T. }, .T. )
   ft_Fill( aOptions[ 2 ], "M. Print No-Activity List"           , {|| .T. }, .T. )

   ft_Fill( aOptions[ 3 ], "A. Transaction Totals Display"       , {|| .T. }, .T. )
   ft_Fill( aOptions[ 3 ], "B. Display Invoice Totals"           , {|| .T. }, .T. )
   ft_Fill( aOptions[ 3 ], "C. Accounts Receivable Display"      , {|| .T. }, .T. )

   ft_Fill( aOptions[ 4 ], "A. Backup Database Files"            , {|| .T. }, .T. )
   ft_Fill( aOptions[ 4 ], "B. Reindex Database Files"           , {|| .T. }, .T. )
   ft_Fill( aOptions[ 4 ], "C. Set System Parameters"            , {|| .T. }, .T. )
   ft_Fill( aOptions[ 4 ], "D. This EXITs Too"                   , {|| .F. }, .T. )

   ft_Fill( aOptions[ 5 ], "A. Does Nothing"                     , {|| .T. }, .T. )
   ft_Fill( aOptions[ 5 ], "B. Exit Application"                 , {|| .F. }, .T. )

   // main routine starts here
   SET SCOREBOARD OFF

   cNormN := iif( lColor, "N/G" , "W/N"  )
   cWindN := iif( lColor, "W/B" , "W/N"  )
   cErrH  := iif( lColor, "W+/R", "W+/N" )
   cErrN  := iif( lColor, "W/R" , "W/N"  )

   SAVE SCREEN TO sDosScrn
   nDosRow := Row()
   nDosCol := Col()
   SetColor( "w/n" )
   CLS
   NoSnow( "NOSNOW" $ Upper( cCmdLine ) )
   IF "VGA" $ Upper( cCmdLine )
      SetMode( 50, 80 )
   ENDIF
   nMaxRow := MaxRow()
   SetBlink( .F. )
   SetColor( cWindN + "*" )
   CLS
   SetColor( cNormN )
   @ nMaxRow, 0
   @ nMaxRow, 0 SAY hb_UTF8ToStr( " FT_MENU1 1.0 │ " )
   @ nMaxRow, 16 SAY "WRITTEN BY PAUL FERRARA [76702,556] FOR NANFORUM.LIB"
   @ nMaxRow, 69 SAY hb_UTF8ToStr( "│ " ) + DToC( Date() )

   SetColor( cErrH )
   @ nMaxRow - 11, 23, nMaxRow - 3, 56 BOX hb_UTF8ToStr( "┌─┐│┘─└│ " )
   @ nMaxRow - 9, 23 SAY hb_UTF8ToStr( "├────────────────────────────────┤" )
   SetColor( cErrN )
   @ nMaxRow - 10, 33 SAY "Navigation Keys"
   @ nMaxRow - 8, 25 SAY "LeftArrow   RightArrow   Alt-E"
   @ nMaxRow - 7, 25 SAY "Home        End          Alt-R"
   @ nMaxRow - 6, 25 SAY "Tab         Shift-Tab    Alt-D"
   @ nMaxRow - 5, 25 SAY "PgUp        PgDn         Alt-M"
   @ nMaxRow - 4, 25 SAY "Enter       ESCape       Alt-Q"
   SetColor( cNormN )

   ft_Menu1( aBar, aOptions, aColors )

   SetColor( "W/N" )
   SetCursor( SC_NORMAL )
   SetBlink( .T. )
   IF "VGA" $ Upper( cCmdLine )
      SetMode( 25, 80 )
   ENDIF
   RESTORE SCREEN FROM sDosScrn
   SetPos( nDosRow, nDosCol )

   RETURN

FUNCTION fubar()

   LOCAL OldColor := SetColor( "W/N" )

   CLS
   QOut( "Press Any Key" )
   Inkey( 0 )
   SetColor( OldColor )

   RETURN .T.
