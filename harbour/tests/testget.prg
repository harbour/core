Procedure Main()

   LOCAL   GetList := {}, cVar := "Hello"
   MEMVAR  aVar, nIndex, cMacro
   PRIVATE aVar := { "World", "Again" }, nIndex := 1, cMacro := "cEarly", cEarly := "Early", cLate := "Late!"

   CLS

   ? "2nd GET should say 'Early'."

   @ 10,10 SAY "cVar            :" GET cVar PICTURE "@K!"
   @ 12,10 SAY "cMacro          :" GET &cMacro
   nIndex := 2
   @ 14,10 SAY "aVar            :" GET aVar[nIndex]
   @ 16,10 SAY "Picture of GET-1:" GET GetList[1]:Picture
   nIndex := 3
   cMacro := "cLate"
   READ

   CLS

   ? "This GET should say 'Late!'."
   cMacro := "cEarly"
   @ 10,10 SAY "cMacro          :" GET &(cMacro)
   cMacro := "cLate"
   READ

RETURN


