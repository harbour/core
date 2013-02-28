/*
 * $Id$
 */

// see also hrb.prg

FUNCTION Msg()

   ? "Function called from HRB file"

   RETURN .T.

FUNCTION msg2()

   RETURN Msg()
