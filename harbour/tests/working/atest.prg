// releasing arrays test

function Main()

   local a := { 1 }

   a[ 1 ] = a
   a[ 1 ] = nil

   QOut( "The array will try to be released now..." )

return nil
