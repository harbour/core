
FUNCTION Main()

// ? "1", __copyfile("COPYFROM.TXT")
? "2", __copyfile("COPYFROM.TXT", "COPYTO.TXT")
? "3", __copyfile("C.PRG", "B.PRG")
? "4", __copyfile("COPYFROM.TXT", "..")

RETURN NIL
