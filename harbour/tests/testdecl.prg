#INCLUDE "HBCLASS.CH"

#TRANSLATE AS NEW <ClassName> => AS CLASS <ClassName> := <ClassName>():New()

DECLARE nMyFun() AS NUM
DECLARE MyClass cVar AS CHAR

Function Main()

    MEMVAR Var1, Var2, aVar
    LOCAL MyObj AS NEW MyClass

    DECLARE Var1
    DECLARE Var2 := 2
    DECLARE aVar[2]

    M->Var1 := nMyFun()
    ? M->Var1

    ? M->Var2

    M->aVar[1] := 'Array Element'
    ? M->aVar[1]

    MyObj:cVar := 'Hello'
    ? MyObj:cVar

RETURN NIL

CLASS MyClass
   VAR cVar
ENDCLASS

Function nMyFun()
RETURN 1
