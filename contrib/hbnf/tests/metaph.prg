#require "hbnf"

PROCEDURE Main()

   // Demo will create an array of names and display in 3 columns
   // _ftRow() and _ftCol() will calculate the screen co-ordinates
   // by evaluating the element number

   LOCAL aNames := { ;
      "Adams", ;
      "Addams", ;
      "Atoms", ;
      "Adamson", ;
      "Cajun", ;
      "Cagen", ;
      "Cochy", ;
      "Cocci", ;
      "Smith", ;
      "Smythe", ;
      "Naylor", ;
      "Nailer", ;
      "Holberry", ;
      "Wholebary", ;
      "Jackson", ;
      "Jekksen", ;
      "The Source", ;
      "The Sores", ;
      "Jones", ;
      "Johns", ;
      "Lennon", ;
      "Lenin", ;
      "Fischer", ;
      "Fisher", ;
      "O'Donnell", ;
      "O Donald", ;
      "Pugh", ;
      "Pew", ;
      "Heimendinger", ;
      "Hymendinker", ;
      "Knight", ;
      "Nite", ;
      "Lamb", ;
      "Lamb Chops", ;
      "Stephens", ;
      "Stevens", ;
      "Neilson", ;
      "Nelson", ;
      "Tchaikovski", ;
      "Chikofski", ;
      "Caton", ;
      "Wright", ;
      "Write", ;
      "Right", ;
      "Manual", ;
      "Now", ;
      "Wheatabix", ;
      "Science", ;
      "Cinzano", ;
      "Lucy", ;
      "Reece", ;
      "Righetti", ;
      "Oppermann", ;
      "Bookkeeper", ;
      "McGill", ;
      "Magic", ;
      "McLean", ;
      "McLane", ;
      "Maclean", ;
      "Exxon" }

   Set( _SET_SCOREBOARD, .F. )
   SetColor( "W/B" )
   CLS

   // display names and metaphones in 3 columns on screen
   AEval( aNames, ;
      {| cName, nElem | ;
      SetPos( _ftRow( nElem ), _ftCol( nElem ) ), ;
      QQOut( PadR( cName, 18, "." ) + ft_Metaph( cName ) ) ;
      } )

   SetPos( 21, 0 )

   RETURN

STATIC FUNCTION _ftRow( nElem )  //  Determine which row to print on

   RETURN iif( nElem > 40, nElem - 40, iif( nElem > 20, nElem - 20, nElem ) )

STATIC FUNCTION _ftCol( nElem )  //  Determine which column to start print

   RETURN iif( nElem > 40,  55, iif( nElem > 20, 28, 1 ) )
