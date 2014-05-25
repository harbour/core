// Statics overlapped!
//
// Compile statics1.prg, statics2.prg and link both files

STATIC uOne
STATIC uTwo

PROCEDURE Main()

   ? "Statics overlapped!"
   ? "==================="
   ?
   ? "INSIDE", __FILE__
   ? "   STATIC uOne, uTwo"
   ?
   ? "   uOne, uTwo =>", uOne, ",", uTwo
   uOne := 1
   uTwo := 2
   ? "   uOne := 1"
   ? "   uOne := 2"
   ? "   uOne, uTwo =>", uOne, ",", uTwo
   ?
   Test()
   ? "INSIDE", __FILE__
   ? "   uOne, uTwo =>", uOne, ",", uTwo
   ?

   RETURN
