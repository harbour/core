/*
 * $Id$ 
 */

PROCEDURE Main()

   LOCAL aNames := {}

   Set( _SET_SCOREBOARD, .F. )
   Set( _SET_COLOR, "W/B" )
   CLS

   // Demo will create an array of names and display in 3 columns
   // _ftRow() and _ftCol() will calculate the screen co-ordinates
   // by evaluating the element number

   AAdd( aNames, "Adams"        )
   AAdd( aNames, "Addams"       )
   AAdd( aNames, "Atoms"        )
   AAdd( aNames, "Adamson"      )
   AAdd( aNames, "Cajun"        )
   AAdd( aNames, "Cagen"        )
   AAdd( aNames, "Cochy"        )
   AAdd( aNames, "Cocci"        )
   AAdd( aNames, "Smith"        )
   AAdd( aNames, "Smythe"       )
   AAdd( aNames, "Naylor"       )
   AAdd( aNames, "Nailer"       )
   AAdd( aNames, "Holberry"     )
   AAdd( aNames, "Wholebary"    )
   AAdd( aNames, "Jackson"      )
   AAdd( aNames, "Jekksen"      )
   AAdd( aNames, "The Source"   )
   AAdd( aNames, "The Sores"    )
   AAdd( aNames, "Jones"        )
   AAdd( aNames, "Johns"        )
   AAdd( aNames, "Lennon"       )
   AAdd( aNames, "Lenin"        )
   AAdd( aNames, "Fischer"      )
   AAdd( aNames, "Fisher"       )
   AAdd( aNames, "O'Donnell"    )
   AAdd( aNames, "O Donald"     )
   AAdd( aNames, "Pugh"         )
   AAdd( aNames, "Pew"          )
   AAdd( aNames, "Heimendinger" )
   AAdd( aNames, "Hymendinker"  )
   AAdd( aNames, "Knight"       )
   AAdd( aNames, "Nite"         )
   AAdd( aNames, "Lamb"         )
   AAdd( aNames, "Lamb Chops"   )
   AAdd( aNames, "Stephens"     )
   AAdd( aNames, "Stevens"      )
   AAdd( aNames, "Neilson"      )
   AAdd( aNames, "Nelson"       )
   AAdd( aNames, "Tchaikovski"  )
   AAdd( aNames, "Chikofski"    )
   AAdd( aNames, "Caton"        )
   AAdd( aNames, "Wright"       )
   AAdd( aNames, "Write"        )
   AAdd( aNames, "Right"        )
   AAdd( aNames, "Manual"       )
   AAdd( aNames, "Now"          )
   AAdd( aNames, "Wheatabix"    )
   AAdd( aNames, "Science"      )
   AAdd( aNames, "Cinzano"      )
   AAdd( aNames, "Lucy"         )
   AAdd( aNames, "Reece"        )
   AAdd( aNames, "Righetti"     )
   AAdd( aNames, "Oppermann"    )
   AAdd( aNames, "Bookkeeper"   )
   AAdd( aNames, "McGill"       )
   AAdd( aNames, "Magic"        )
   AAdd( aNames, "McLean"       )
   AAdd( aNames, "McLane"       )
   AAdd( aNames, "Maclean"      )
   AAdd( aNames, "Exxon"        )

   // display names and metaphones in 3 columns on screen
   AEval( aNames, ;
      {| cName, nElem | ;
         SetPos( _ftRow( nElem ), _ftCol( nElem ) ), ;
         QQOut( PadR( cName, 18, "." ) + FT_METAPH( cName ) ) ;
      } )

   SetPos( 21, 00 )
   QUIT

//------------------------------------------------

STATIC FUNCTION _ftRow( nElem )  //  Determine which row to print on

   RETURN iif( nElem > 40, nElem - 40, iif( nElem > 20, nElem - 20, nElem ) )

//------------------------------------------------

STATIC FUNCTION _ftCol( nElem )  //  Determine which column to start print

   RETURN iif( nElem > 40,  55, iif( nElem > 20, 28, 1 ) )

//------------------------------------------------

