// see also hrb.prg

FUNCTION Msg()  /* must be a public function */

   ? "Function called from .hrb"

   RETURN .T.

FUNCTION Msg2()  /* must be a public function */
   RETURN Msg()

INIT PROCEDURE MsgInit()

   ? "INIT PROCEDURE in .hrb"

   RETURN
