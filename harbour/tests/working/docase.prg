// Testing Harbour Do case

function Main()

   local n := 2

   QOut( "testing Harbour Do case" )

   do case
      case n == 1
           QOut( "n is 1" )
           QOut( "first case" )

      case n == 2
           QOut( "n is 2" )
           QOut( "second case" )

      case n == 3
           QOut( "n is 3" )
           QOut( "third case" )

      otherwise
           QOut( "Sorry, I don't know what n is :-)" )
           QOut( "otherwise" )
   endcase

   QOut( "Ok!" )

return nil
