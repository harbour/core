/*
 * $Id$
 */

PROCEDURE Main()

   LOCAL i, bBlock[ 3 ]

   MEMVAR  Var1, Var2, Var3, Macro
   PRIVATE Var1, Var2, Var3, Macro

   M->Var1 := "Var1"
   M->Var2 := "Var2"
   M->Var3 := "Var3"

   CLS

   FOR i := 1 TO 3
      M->Macro := "Var" + Str( i, 1 )
      bBlock[ i ] := {|| &Macro }
   NEXT

   ? "Early (Simple):"

   FOR i := 1 TO 3
      ? Eval( bBlock[ i ] )
   NEXT

   FOR i := 1 TO 3
      M->Macro := "Var" + Str( i, 1 )
      bBlock[ i ] := {|| &Macro + "!" }
   NEXT

   ?
   ? "Early (Complex):"

   FOR i := 1 TO 3
      ? Eval( bBlock[ i ] )
   NEXT

   FOR i := 1 TO 3
      M->Macro := "Var" + Str( i, 1 )
      bBlock[ i ] := {|| &( Macro ) }
   NEXT

   ?
   ? "Late:"

   FOR i := 1 TO 3
      ? Eval( bBlock[ i ] )
   NEXT

   RETURN
