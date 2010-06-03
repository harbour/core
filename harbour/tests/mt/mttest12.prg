/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for thread static variables
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

thread static s_var1 := { "A", "B", "C" }
thread static s_var2 := { "A" => "qwe", "B" => "asd" }
thread static s_var3 := 1235.567
thread static s_var4 := "text"

proc main()
testvar("initial main thread values")
aadd( s_var1, "X" )
s_var2[ "X" ] := "zxc"
s_var3 += 100000
s_var4 := upper( s_var4 )
testvar("modified main thread values")
hb_threadJoin( hb_threadStart( @thFunc() ) )
testvar("modified main thread values")
return

proc testvar( cMsg )
? cMsg
? "s_var1:", hb_valtoExp( s_var1 )
? "s_var2:", hb_valtoExp( s_var2 )
? "s_var3:", hb_valtoExp( s_var3 )
? "s_var4:", hb_valtoExp( s_var4 )
wait
?
return

proc thFunc()
testvar("initial child thread values")
aadd( s_var1, "Y" )
s_var2[ "Y" ] := "abc"
s_var3 -= 500000
s_var4 += " CHANGED"
testvar("modified child thread values")
return
