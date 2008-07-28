/*
 * $Id$
 */

Function Main()
 Local Program := { , }, Condition := 1, body := 2, Counter := 1, TheEnd := 1000000, stop, start

 Program[ condition] := { || Counter == TheEnd}
 Program[      body] := { || Counter++}
 ? start := Second()

 // in Clipper :
 // While !Program[ condition]:Eval() ; Program[ body]:Eval()
 // why Harbour CodeBlocks don't have Eval() method ?!

 // Now It is supported.

 While !Eval( Program[ condition]) ; Eval( Program[ body])
 End
 ? stop := Second()
 ? '==============='
 ? stop - start
 Return NIL
