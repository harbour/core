//
// $Id$
//


FUNCTION Main()

// ? "1", __copyfile("COPYFROM.PRG")
? "2", __copyfile("COPYFROM.PRG", "COPYTO.TMP")
? "3", __copyfile("_NOTHERE.$$$", "COPYTO.TMP")
? "4", __copyfile("COPYFROM.PRG", "..")

RETURN NIL
