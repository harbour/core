/*
**
**    (c) 1999-2000 Manos Aspradakis, Greece
**    eMail : maspr@otenet.gr
**
*/

#include "hbclass.ch"
#include "html.ch"
#include "default.ch"


/****
*
*     ::debug()
*
*     method of all objects
*
*/
/*
METHOD FUNCTION __clsDebug( opObj )
LOCAL saArr      := {}
LOCAL aData
LOCAL aMethods
LOCAL cColor := ""
LOCAL i, cStr
Local lIsoObject := .F.

IF opObj != NIL
  IF VALTYPE( opObj ) == "O"
     Self := opObj
  ENDIF
ENDIF

IF ! (VALTYPE( self ) == "O")
   RETURN NIL
ENDIF

aData      := getoData( Self )
aMethods   := aoMethods( Self )

saArr     := {}

AADD( saArr, "<BR><BR>")
AADD( saArr, "<FONT FACE=Verdana COLOR='black' SIZE='1'>"+CRLF() )
AADD( saArr, "<center>")
AADD( saArr, '<TABLE COLS="1" BORDER bgcolor="gray" WIDTH="90%"><TR bgcolor="gray" bordercolorlight="#000000" bordercolordark="#FFFFFF"><TD><B><CENTER>Class Viewer</CENTER></B></TD></TR><TR bgcolor="gray"><TD>'+CRLF() )
AADD( saArr, "<center>")
AADD( saArr, '<TABLE COLS="2" BORDER WIDTH="85%" bordercolorlight="#000000" bordercolordark="#FFFFFF"> '+CRLF() )
AADD( saArr, "<TR bgcolor='black'>" )
AADD( saArr, "<TD><font size='2' COLOR='white'><b>CLASS "+ Self:ClassName() + "</b></TD>" + ;
             "<TD><font size='2' COLOR='white'><b>DATA ("+ltrim(str(len(aData)))+") - Methods ("+ ltrim(str(len(aMethods)))+")</b></TD>" )
AADD( saArr, "</TR>" )
AADD( saArr, "<TR bgcolor='red'>" )

AADD( saArr, "<TD><b>Data Name</b></TD><TD><b>Value</b></TD>" )
//AADD( saArr, "</TD></TR></TR>" )
AADD( saArr, "</TR>" )


FOR i=1 to len( aData )
     cColor := if( i%2=0, "lightyellow", "lightblue" )
     cColor := if( aData[i,2] == 4, 'red', cColor )
     cStr := "<TR "+"bgcolor='"+cColor+"'>"
     cStr += "<TD><FONT SIZE='2' COLOR='blue'><b>"+aData[i,1]+"</b></font></TD>"
     cStr += "<TD><FONT SIZE='2' COLOR='black'>"


     if aData[i,2] == 2
        if aData[i,3] == 0
           cStr += "-Method-"
        else
           cStr += "-Inline-"
        endif
     elseif aData[i,2] == 4
        cStr += htmlspace(2)
     else
        cStr += oTransform( oGetData( Self, aData[i,1]) )
     endif

     cStr += "</font></TD></TR>"

     AADD( saArr, cStr )
NEXT

AADD( saArr, "</TABLE>" )
AADD( saArr, "</center>")
AADD( saArr, "</TD></TR></TABLE>" )
AADD( saArr, "</FONT>" )
AADD( saArr, "</center>")

AEVAL( saArr, {|e| oPage():QQOut(e) } )

RETURN Self



  */
/****
*
*    aoData()
*
*
*
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
STATIC FUNCTION aoData( oObject )
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
local aInfo    := ASort( __ClassSel( oObject:ClassH() ) )
local aData    := {}
local aMethods := {}
local i        := 1
local lExact   := Set( _SET_EXACT, .t. )

   while SubStr( aInfo[i], 1, 1 ) != "_"
      if ASCAN( aInfo, "_" + SubStr( aInfo[i], 1, 9 ), i+1 ) != 0
         AAdd( aData, aInfo[i] )
      else
         AAdd( aMethods, aInfo[i] )
      endif
      i++
   end

   Set( _SET_EXACT, lExact )

RETURN { aData, aMethods }


/****
*
*    __aoData()
*
*
*
*/

STATIC FUNCTION __aoDATA( oObject )
   local aInfo  := ASort( __ClassSel( oObject:ClassH() ) )
   local aData  := {}
   local i      := 1
   local lExact := Set( _SET_EXACT, .t. )

   while SubStr( aInfo[i], 1, 1 ) != "_"
      if ASCAN( aInfo, "_" + SubStr( aInfo[i], 1, 9 ), i+1 ) != 0
         AADD( aData, aInfo[i] )
      endif
      i++
   end

   Set( _SET_EXACT, lExact )

RETURN aData


/****
*
*    getOData()
*
*
*
*/

STATIC FUNCTION GetOData( o )
LOCAL i
LOCAL aObjData:= aoData( o )
LOCAL aData   := aObjData[1]
LOCAL aMeth   := aObjData[2]
LOCAL aInline := {}
LOCAL aRet    := {}

LOCAL slIsOObject := .F.

IF ASCAN( aData, "DICT" ) > 0    // Is oObject class !!!
    IF VALTYPE( o:Dict ) == "A"
          slIsOObject := .T.
    ENDIF
ENDIF

// oObject Classes
IF slIsOObject == .T.
     for i=1 to LEN( o:Dict[_CLASS_DATA] )

         IF VALTYPE( o:Dict[_CLASS_DATA][i,3] ) == "B"         // INLINE-BLOCK
            AADD( aInline, LOWER( o:Dict[_CLASS_DATA][i,1] ) )
         ELSE
            AADD( aRet, { o:Dict[_CLASS_DATA][i,1], 1, 0 } )
         ENDIF
     NEXT

     aadd( aRet, { "<font color='white'><b>"+"METHODS</font></b>",    4, 0 })
     AEVAL( aInline               , {|e| aadd( aRet, { LOWER( e ),    2, 1 })})
     AEVAL( o:Dict[_CLASS_METHODS], {|e| AADD( aRet, { LOWER( e[1] ), 2, 0 })})

// Normal classes
ELSE
     AEVAL( aData, {|e| AADD( aRet, {        e  , 1, 0 } ) } )
     AEVAL( aMeth, {|e| AADD( aRet, { LOWER( e ), 2, 0 } ) } )
ENDIf
RETURN aRet



/****
*
*      aoMethods()
*
*
*
*/

STATIC FUNCTION aoMETHODS( oObject )
   local aInfo  := ASort( __ClassSel( oObject:ClassH() ) )
   local aData  := {}
   local i      := 1
   local lExact := Set( _SET_EXACT, .t. )

   while SubStr( aInfo[i], 1, 1 ) != "_"
      if ASCAN( aInfo, "_" + SubStr( aInfo[i], 1, 9 ), i+1 ) == 0
         AADD( aData, aInfo[i] )
      endif
      i++
   end

   Set( _SET_EXACT, lExact )

RETURN aData



/****
*
*     oGetData()
*
*
*
*/

STATIC FUNCTION oGETDATA( oObject, cIVar )
LOCAL oErr := ERRORBLOCK( {|o| break(o) } )
LOCAL xRet
BEGIN SEQUENCE
xRet :=  EVAL( &( "{ | o | o:" + cIVar + "}" ), oObject )
RECOVER USING oErr
xRet := "<error>"
END SEQUENCE
ERRORBLOCK( oErr )
RETURN xRet

/****
*
*      oSetData()
*
*
*
*/

STATIC FUNCTION oSETDATA( oObject, cIVar, xValue )
EVAL(&("{ | o, x | o:_" + cIVar + "( x ) }"), oObject, xValue)
RETURN ( Nil )


/****
*
*     oTransform()
*
*
*
*/

STATIC FUNCTION oTRANSFORM(xVal)
    Local cType:= ValType(xVal)

    Do Case
    Case cType == "C"
        RETURN '"' + xVal + '"'
    Case cType == "N"
        RETURN LTrim(Str(xVal))
    Case cType == "D"
        RETURN "CTOD('" + DToC(xVal) + "')"
    Case cType == "A"
        RETURN "{ ... }"
    Case cType == "B"
        RETURN "{|| ... }"
    Case cType == "L"
        RETURN IIf(xVal, ".T.", ".F.")
    Case cType == "M"
        RETURN "<Memo>"
    Case cType == "O"
        RETURN "-Object-"
    EndCase
    RETURN "-NIL-"
RETURN NIL


*** EOF ***

