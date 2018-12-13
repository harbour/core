PROCEDURE Main()

   LOCAL Program := {, }, Condition := 1, body := 2, Counter := 1, TheEnd := 1000000, stop, start

   Program[ condition ] := {|| Counter == TheEnd }
   Program[      body ] := {|| Counter++ }
   ? start := Second()

   // in Clipper :
   // WHILE ! Program[ condition ]:Eval() ; Program[ body ]:Eval()
   // why Harbour CodeBlocks don't have Eval() method ?!

   // Now It is supported.

   DO WHILE ! Eval( Program[ condition ] )
      Eval( Program[ body ] )
   ENDDO
   ? stop := Second()
   ? "==============="
   ? stop - start

   RETURN
