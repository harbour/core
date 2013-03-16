// see also hrb.prg

FUNCTION Msg()

   ? "Function called from .hrb"

   RETURN .T.

FUNCTION Msg2()
   RETURN Msg()

INIT PROCEDURE MsgInit()

   ? "INIT PROCEDURE in .hrb"

   RETURN
