REQUEST HBSTRUC
#INCLUDE "HBSTRUC.CH"

STRUCTURE MyStruct Var1 As Char, Var2 As Num, sNext AS Stru MyStruct

STRUCTURE OtherStruct sNested As Stru MyStruct, sNext AS Structure OtherStruct

PROCEDURE MAIN()

  LOCAL sTest AS New MyStruct, sTest2 As New OtherStruct

  sTest:Var1 := 'Working'       // No problem here. (Var1 of myStruct is Char)

  ? sTest:Var1                  // No Problem here.

  sTest:Var1 := 8             // Warning Here as Expected.  -> Var1 of MyStruct is Char not Num

  sTest2:sNested:Var1 := 8      // Warning Here Expected -> Var1 of MyStruct is Char not Num

  sTest2:sNested:sNext := 8    // Warning Here Expected -> sNext of MyStruct is MyStruct not Num


  /* Run TIME ERROR Here */
  sTest2:sNested:sNested := 8  // Warning Here Expected ->  MyStruct has no sNested Var

  /* Run TIME ERROR Here */
  sTest:NoSuchVar := "Error"  // Warning here as expected. ->  MyStruct has no NoSuchVar Var.

RETURN
