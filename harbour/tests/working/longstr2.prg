//NOTEST - You'll want to test this with the output redirected to a file!
//
// $Id$
//


function Main()

   local short := "1234567890"
   local i, long, very_long, cNewLine

   cNewLine := OS_NewLine()

   long := short
   for i := 1 TO 12
      long += long
   next

   very_long := long
   for i := 1 to 5
      very_long += very_long
   next

   OutErr (len(short), len(long), len(very_long))
   Qout   (len(short), len(long), len(very_long))

   OutStd (cNewLine)
   OutStd (len(short), len(long), len(very_long))

   OutStd (cNewLine)
   OutStd (cNewLine)
   OutStd (short)

   OutStd (cNewLine)
   OutStd (cNewLine)
   OutStd (long)

   OutStd (cNewLine)
   OutStd (cNewLine)
   OutStd (very_long)

return nil
