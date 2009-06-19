//
// $Id: extmsg.prg 0001 2009-06-18 20:20:41 j. lefebvre $
//

// see also testhrb.prg


Function Msg()
? "Function called from HRB file"
Return .T.

Function msg2()
Return Msg()
