//
// $Id$
//


function main()
  LOCAL cString

  cString := "[HARBOUR]" + chr(141)+chr(10) + ;
             "[POWER]"   + chr(13)+chr(10) + ;
             "[MAGIC]"

  OutSpec( cString )
  OutSpec( MemoTran( cString ) )

  cString := "[HAR" + Chr(0) + "OUR]" + chr(141)+chr(10) + ;
             "[POWER]"   + chr(13)+chr(10) + ;
             "[MA" + Chr(0) + "IC]"

  OutSpec( cString )
  OutSpec( MemoTran( cString ) )

  cString := "Mr. Chandler " + Chr(13) +;
             " something " + Chr(13) + Chr(10) +;
             " wonderful " + Chr(141) +;
             " will " + Chr(141) + Chr(10) +;
             " happen" + Chr(13)

  OutSpec( cString )
  OutSpec( MemoTran( cString ) )
  OutSpec( MemoTran( cString, "111", "222" ) )
  OutSpec( MemoTran( cString, "", "" ) )
  OutSpec( MemoTran() )
  OutSpec( MemoTran( 100 ) )

return nil

STATIC FUNCTION OutSpec( cString )

     cString := StrTran(cString, Chr(13), "!")
     cString := StrTran(cString, Chr(10), "?")

     OutStd( ">" + cString + "<" + Chr(13) + Chr(10) )

     RETURN NIL

