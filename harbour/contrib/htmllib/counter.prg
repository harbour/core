#include "html.ch"
#include "forms.ch"
#include "default.ch"



PROC CounterCGI()
LOCAL lIsPost := .F.
LOCAL cCounterDat := "counter.dat"
LOCAL oFrm, oEd, oSub

LOCAL oHtm, oCgi

SET DATE BRITISH

IF "POST" $ UPPER(GETENV("REQUEST_METHOD"))
   lIsPost := .T.
      oCgi := oCGI():New()     // get server parameters/variables
ENDIF

oHtm := HTML():CGINew( "Counter.htm", "My Counter File" )

oHtm:SetPageColor(CLR_DARK_YELLOW )  //"#FFFFCC")
oHtm:SetTextColor(CLR_WHITE)
oHtm:SetbgImage("/images/back/bg32.bmp")
oHtm:Setcenter( .f. )
oHtm:qout( "" )
SET FONT "Verdana" SIZE 3 OF oHtm

DEFINE FORM oFrm ;
       CAPTION "A Simple Counter" ;
       NAME "MyForm";
       ACTION "/cgi-bin/counter.exe?" ;
       FRAME

CONTROL EDIT NAME "User" ;
       MAXCHARS 20 ;
       SIZE     20 ;
       CAPTION "User Name:" ;
       IN oFrm

LINE BREAK IN oFrm
LINE IN oFrm
LINE BREAK IN oFrm
SPACE 80 IN oFrm

CONTROL SUBMIT NAME cSubmit VALUE ("    Ok    ") IN oFrm

ACTIVATE oFrm
                   
SET FONT "Verdana" SIZE 3 OF oHtm
oHtm:defineTable( 2,, 90,, "#9196A0" )
oHtm:newTableRow()
oHtm:newTableCell()
oHtm:QOut( "Page Visited :<b>" + TRANSFORM( incCounter(), "999,999,999" ) + +htmlSpace(2)+"</b>Times")
oHtm:EndTableCell()
oHtm:EndTableRow()
oHtm:EndTable()

oHtm:cgiClose()
RETURN


//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
FUNCTION IncCounter()
LOCAL n := 0

IF FILE("Counter.dat")
   n := VAL(MEMOREAD("counter.dat"))
ELSE
   n := 0
ENDIF
MEMOWRIT( "counter.dat", STR( n+1 ) )
RETURN n


//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
 FUNCTION GetCounter()
LOCAL n := 0

IF FILE("Counter.dat")
   n := VAL(MEMOREAD("counter.dat"))
ELSE
   incCounter()
ENDIF
RETURN n


