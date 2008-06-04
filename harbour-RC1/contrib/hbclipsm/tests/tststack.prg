/*
 * $Id$
 */

function Test()

   LOCAL a := StackNew()

   ? Valtype( a )
   ? Len( a )           // 0
   StackPush( a, "POWER" )
   ? Len( a )           // 1
   StackPush( a, "HARBOUR" )
   ? Len( a )           // 2
   ? StackPop( a )      // HARBOUR
   ? Len( a )           // 1
   ? StackPop( a )      // POWER
   ? Len( a )           // 0
   ? StackPop( a )      // NIL
   ? Len( a )           // 0
   ? StackIsEmpty( a )  // TRUE
   StackPush( a, "HARBOUR" )
   ? StackTop( a )      // HARBOUR
   ? StackIsEmpty( a )  // FALSE

return nil
