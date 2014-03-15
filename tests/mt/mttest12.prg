/*
 * Harbour Project source code:
 *    demonstration/test code for thread static variables
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

thread static t_var1 := { "A", "B", "C" }
thread static t_var2 := { "A" => "qwe", "B" => "asd" }
thread static t_var3 := 1235.567
thread static t_var4 := "text"

proc main()
   testvar("initial main thread values")
   AAdd( t_var1, "X" )
   t_var2[ "X" ] := "zxc"
   t_var3 += 100000
   t_var4 := Upper( t_var4 )
   testvar("modified main thread values")
   hb_threadJoin( hb_threadStart( @thFunc() ) )
   testvar("modified main thread values")
   return

proc testvar( cMsg )
   ? cMsg
   ? "t_var1:", hb_ValToExp( t_var1 )
   ? "t_var2:", hb_ValToExp( t_var2 )
   ? "t_var3:", hb_ValToExp( t_var3 )
   ? "t_var4:", hb_ValToExp( t_var4 )
   wait
   ?
   return

proc thFunc()
   testvar("initial child thread values")
   AAdd( t_var1, "Y" )
   t_var2[ "Y" ] := "abc"
   t_var3 -= 500000
   t_var4 += " CHANGED"
   testvar("modified child thread values")
   return
