STRUCTURE MyStruct Var1 As Char, Var2 As Num

PROCEDURE MAIN()

  LOCAL sTest AS New MyStruct

  sTest:Var1 := 8             // Warning Here as Expected.
  sTest:NoSuchVar := "Error"  // Warning here as expected.

  sTest:Var1 := 'Working'     // No problem here.

  ? sTest:Var1                // Np Problem here.

RETURN
