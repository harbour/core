
FUNCTION Main()

// ? "1", __copyfile("COPYFROM.FIL")
? "2", __copyfile("COPYFROM.FIL", "COPYTO.FIL")
? "3", __copyfile("C.PRG", "B.PRG")
? "4", __copyfile("COPYFROM.FIL", "..")

RETURN NIL
