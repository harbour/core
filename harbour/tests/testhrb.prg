//
// $Id$
//

// see also exthrb.prg


#include "hbhrb.ch"

Procedure Main(x)
Local pHrb, cExe := "Msg2()", n

  n:=iif(x==NIL,0,val(x))

  ? "calling Msg ... From exe here !"
  Msg()
  ? "========================="

//  ? "Loading('exthrb.hrb' )"
//  pHrb := hb_HrbLoad("exthrb.hrb" )

//  ? "Loading(HB_HRB_DEFAULT,'exthrb.hrb' )"
//  pHrb := hb_HrbLoad(HB_HRB_DEFAULT,"exthrb.hrb" )

//  ? "Loading(HB_HRB_KEEP_LOCAL,'exthrb.hrb' )"
//  pHrb := hb_HrbLoad(HB_HRB_KEEP_LOCAL,"exthrb.hrb" )

  ? "Loading("+iif(n=0,"HB_HRB_DEFAULT",iif(n=1,"HB_HRB_KEEP_LOCAL","HB_HRB_KEEP_GLOBAL"))+",'exthrb.hrb' )"
  pHrb := hb_HrbLoad(n,"exthrb.hrb" )

  ? "========================="

  ? "calling Msg ... DEFAULT=From exe, LOCAL=From exe, GLOBAL=From HRB"
  Msg()
  ? "========================="

  ? "calling Msg ... DEFAULT=From exe, LOCAL=From HRB, GLOBAL=From HRB"
  &cExe  //
  ? "========================="

  hb_HrbUnload( pHrb ) // should do nothing in case of GLOBAL

  ? "calling Msg ... DEFAULT=From exe, LOCAL=From exe, GLOBAL=From HRB"
  Msg() // test unload protection when using GLOBAL ... then Hrb not anymore unloadable
  ? "========================="

  ?  "END"

Return


Function Msg()
? "Function called from Exe"
Return .T.
