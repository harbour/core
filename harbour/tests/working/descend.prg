#include "set.ch"

function main()

  LOCAL dDate

  QOut( Descend( "HARBOUR POWER & MAGIC" ) )
  QOut( Descend( Descend( "HARBOUR POWER & MAGIC" ) ) )
  QOut( Descend( .f. ) )
  QOut( Descend( .t. ) )
  QOut( Descend( 1 ) )
  QOut( Descend( -1 ) )
  QOut( Descend( Descend( 256 ) ) )
  QOut( Descend( 2.5 ) )
  QOut( Descend( -100.35 ) )

  SET( _SET_DATEFORMAT, "dd/mm/yyyy" )
  dDate := cToD( "31/12/2999" )
  QOut( dDate, dtos( dDate ), Descend( dDate ) )

  dDate := cToD( "1/1/0100" )
  QOut( dDate, dtos( dDate ), Descend( dDate ) )

  QOut( date(), dtos( date() ), Descend( date() ) )
  QOut( date(), dtos( date() ), Descend( Descend( date() ) ) )
  QOut( date()+1, dtos( date()+1 ), Descend( date()+1 ) )
  QOut( date()+2, dtos( date()+2 ), Descend( date()+2 ) )

  QOut( Asc( Descend( chr(0) ) ) )

  QOut( Descend( { "A", "B" } ) )
  QOut( Descend( nil ) )


return nil
