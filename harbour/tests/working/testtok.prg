#include "set.ch"

procedure main()
   local a
   local i

   set( _SET_EXACT, .T. )               /* Stupid command !!!!
                                           Always causing bugs !!*/
   a := strtoarray("this is a great big test of strtoken")
   for i := 1 to len(a)
      qout( a[i] )
   next i
return

function strtoarray(s)
   local aResult := {}
   local t, l

   QOut( "Here" )
   while( s <> "" )
      QOut( "This" )
      t := strtoken(s, 1,, @l)


      aadd(aResult, t)
      s := substr(s, l + 2) // skip the delimiter

      qout( t, str(l), s )
   end
return aResult
