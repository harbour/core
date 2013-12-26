// Statics overlapped!
//
// Compile statics1.prg, statics2.prg and link both files

STATIC uOne
STATIC uTwo

PROCEDURE Main()

   ? "Statics overlapped!"
   ? "==================="
   ?
   ? "INSIDE statics1.prg"
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
   ? "INSIDE statics1.prg"
   ? "   uOne, uTwo =>", uOne, ",", uTwo
   ?

   RETURN
