//
// $Id$
//

#include "set.ch"

function main()

  LOCAL dDate

  OutSpec( Descend( "HARBOUR POWER & MAGIC" ) )
  OutSpec( Descend( Descend( "HARBOUR POWER & MAGIC" ) ) )
  OutSpec( Descend( .f. ) )
  OutSpec( Descend( .t. ) )
  OutSpec( Descend( 1 ) )
  OutSpec( Descend( -1 ) )
  OutSpec( Descend( Descend( 256 ) ) )
  OutSpec( Descend( 2.0 ) )
  OutSpec( Descend( 2.5 ) )
  OutSpec( Descend( -100.35 ) )
  OutSpec( Descend( -740.354 ) )
  OutSpec( Descend( -740.359 ) )

  SET( _SET_DATEFORMAT, "dd/mm/yyyy" )
  dDate := cToD( "31/12/2999" )
  OutSpec( dDate, dtos( dDate ), Descend( dDate ) )

  dDate := cToD( "1/1/0100" )
  OutSpec( dDate, dtos( dDate ), Descend( dDate ) )

  OutSpec( date(), dtos( date() ), Descend( date() ) )
  OutSpec( date(), dtos( date() ), Descend( Descend( date() ) ) )
  OutSpec( date()+1, dtos( date()+1 ), Descend( date()+1 ) )
  OutSpec( date()+2, dtos( date()+2 ), Descend( date()+2 ) )

  OutSpec( Asc( Descend( "" ) ) )
  OutSpec( Descend( "" ) )
  OutSpec( Asc( Descend( chr(0) ) ) )
  OutSpec( Asc( Descend( chr(0) + "Hello" ) ) )
  OutSpec( Descend( chr(0) + "Hello" ) )
  OutSpec( Asc( Descend( "Hello" + Chr(0) + "world" ) ) )
  OutSpec( Descend( "Hello" + Chr(0) + "world" ) )

  OutSpec( Descend( { "A", "B" } ) )
  OutSpec( ValType( Descend( { "A", "B" } ) ))
  OutSpec( Descend( nil ) )
  OutSpec( ValType( Descend( nil ) ))

  OutSpec( Descend() )
  OutSpec( ValType( Descend() ) )

return nil

STATIC FUNCTION OutSpec( cString )

     OutStd( cString )
     OutStd( Chr(13) + Chr(10) )

     RETURN NIL

