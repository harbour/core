//
// $Id$
//


function main()
  local cString

  cString := "[HARBOUR]" + chr(141)+chr(10) + ;
             "[POWER]"   + chr(141)+chr(10) + ;
             "[WHATEVER]" + chr(141) + ;
             "[MAGIC]"

  OutSpec( cString )
  OutSpec( HardCR( cString ) )

  cString := "[HAR" + Chr(0) + "BOUR]" + chr(141)+chr(10) + ;
             Chr(0) + "[POWER]"   + chr(141)+chr(10) + ;
             "[WHATEVER]" + chr(141) + ;
             "[MAGIC]" + Chr(0)

  OutSpec( cString )
  OutSpec( HardCR( cString ) )

return nil

STATIC FUNCTION OutSpec( cString )

     cString := StrTran(cString, Chr(13), "!")
     cString := StrTran(cString, Chr(10), "?")

     OutStd( ">" + cString + "<" + Chr(13) + Chr(10) )

     RETURN NIL

