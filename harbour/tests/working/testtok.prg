procedure main()
   local a := strtoarray("this is a great big test of strtoken")
   local i

   for i := 1 to len(a)
      qout( a[i] )
   next
return

function strtoarray(s)
   local aResult := {}
   local t, l

   while( s <> "" )
      t := strtoken(s, 1,, @l)


      aadd(aResult, t)
      s := substr(s, l + 2) // skip the delimiter

      qout( t, str(l), s )
   end
return aResult
