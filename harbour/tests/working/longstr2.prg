//NOTEST - You'll want to test this with the output redirected to a file!

function Main()

   local short := "1234567890"
   local i, long, very_long

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

   CrLf()
   OutStd (len(short), len(long), len(very_long))

   CrLf()
   CrLf()
   OutStd (short)

   CrLf()
   CrLf()
   OutStd (long)

   CrLf()
   CrLf()
   OutStd (very_long)

return nil

procedure CrLf
   #ifdef __HARBOUR__
      OutStd (chr(10))
   #else
      OutStd (chr(13)+chr(10))
   #endif
return

procedure Pause
   __Accept("Pause: ")
return