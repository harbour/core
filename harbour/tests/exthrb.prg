//
// $Id$
//

// see also testhrb.prg


Function Msg()
? "Function called from HRB file"
Return .T.

Function msg2()
Return Msg()
