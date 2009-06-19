//
// $Id: testhrb.prg 0001 2009-06-18 20:20:41 j. lefebvre $
//

// see also exthrb.prg


#include "hbhrb.ch"
     
Procedure Main(x)
Local pHrb, cExe := "Msg2()", n

  n:=iif(x==NIL,0,val(x))

  ? "calling Msg ... From exe here !"
  Msg()
  ? "========================="

//  ? "Loading('Exthrb.hrb' )"   
//  pHrb := hb_HrbLoad("Exthrb.hrb" )

//  ? "Loading(HB_HRB_DEFAULT,'Exthrb.hrb' )"   
//  pHrb := hb_HrbLoad(HB_HRB_DEFAULT,"Exthrb.hrb" )

//  ? "Loading(HB_HRB_KEEP_LOCAL,'Exthrb.hrb' )"   
//  pHrb := hb_HrbLoad(HB_HRB_KEEP_LOCAL,"Exthrb.hrb" )

  ? "Loading("+iif(n=0,"HB_HRB_DEFAULT",iif(n=1,"HB_HRB_KEEP_LOCAL","HB_HRB_KEEP_GLOBAL"))+",'Exthrb.hrb' )"
  pHrb := hb_HrbLoad(n,"Exthrb.hrb" )

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

