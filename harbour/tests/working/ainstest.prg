//
// $Id$
//

//
// Array test aIns / aDel / aSize / aFill
//
// Date : 26/4/99
// Time : 09:30
//
function Main()

    local aFirst
    local aSecond
    local aMore

    aFirst := aClone( { 1,2,4 } )
    aIns( aFirst, 3 )
    aFirst[3] = "3"
    QQOut( "Testing aIns .. " )
    aDump( aFirst )

    aSecond := { 1,2,4 }
    aSize( aSecond, 4 )
    QQOut( "Testing aSize .. " )
    aDump( aSecond )

    aSecond := { 1,2,4 }
    aSize( aSecond, 4 )
    aIns( aSecond, 3 )
    aSecond[3] = "3"
    QQOut( "Testing aSize + aIns .. " )
    aDump( aSecond )

    aSecond := { 1,2,3,3,4,5 }
    aDel( aSecond, 3 )
    QQOut( "Testing aDel .. " )
    aDump( aSecond )

    aSecond := { 1,2,3,3,4,5 }
    aDel( aSecond, 3 )
    aSize( aSecond, len(aSecond) - 1 )
    QQOut( "Testing aSize + aDel .. " )
    aDump( aSecond )

    aFill( aSecond, "!" )
    QQOut( "Testing aFill .. " )
    aDump( aSecond )

    aMore := { 1,2,3,4,5,6 }
    aFill( aMore, "X", 3 )
    QQOut( "Testing aFill with start .. " )
    aDump( aMore )

    aMore := { 1,2,3,4,5,6 }
    aFill( aMore, "X", 3, 2 )
    QQOut( "Testing aFill with start and count .. " )
    aDump( aMore )

    aMore := { {1,2}, {3,4} }
    aDel( aMore, 1 )
    aDump( aMore )
return nil

function aDump( aShow )

   local n
   local CRLF := chr(13)+chr(10)

   QQOut( "Len=", ALLTRIM( STR( len( aShow ) ) ) )
   QQOut( ": " )
   for n=1 to len(aShow)

      QQOut( "[" )
      QQOut( ALLTRIM (STR (n)) )
      QQOut( "]= " )
      QQOut( ValType( aShow[n] ) )
      QQOut( ":" )
      if ValType( aShow[n] ) == "A"             /* Iterate array         */
         QQOut( CRLF )
         QQOut("[")
         aDump( aShow[n] )
         QQOut("]")
      else
         QQOut( aShow[n] )
      endif

      if n != len(aShow)
         QQOut( ", " )
      endif

   next n
   QQOut( CRLF )
return nil
