// Test for DECLARE statement
// $Id$
//
#include "hbclass.ch"

#TRANSLATE AS NEW <ClassName> => AS CLASS <ClassName> := <ClassName>():New()

DECLARE nMyFun() AS NUM
DECLARE MyClass cVar AS CHAR

Function Main()

    MEMVAR Var1, Var2, aVar
    MEMVAR Var3, aVar5, aVar8, Var7
    LOCAL MyObj AS NEW MyClass

    DECLARE Var1
    DECLARE Var2 := 2
    DECLARE aVar[2]
    DECLARE Var3 := 'Var9', Var4, aVar5[1]
    DECLARE Var6, Var7:=7, aVar8[8]
    DECLARE &var3
    DECLARE &Var3.
    DECLARE &Var3. ; DECLARE &Var3.&Var3
    DECLARE &Var3.var3
    DECLARE Var3&Var3
    DECLARE Var3&Var3.
    DECLARE Var3&Var3&Var3

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
