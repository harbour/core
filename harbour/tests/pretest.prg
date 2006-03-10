#command TEXT TO VAR <v> => #pragma __stream|%s||<v>:=
#command CTEXT TO VAR <v> => #pragma __cstream|%s||<v>:=

PROCEDURE MAIN()
LOCAL in, out, pre
LOCAL nCnt:=0
LOCAL nRes:=0

  __PP_INIT()
  
/* ---------------------------------------------------------------------*/

  in := "#xtranslate CCC <v> => QOUT( <v>[2] [, <v>[<v>][3]] )"+HB_OSNewLine()+;
        "CCC b"
  pre := "QOUT(b[2] ,bb[3] )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )
/*------------*/
  in := "#xtranslate AAA [A <a> [B <b>] ] => Qout([ <a> ][, <b>] )"
  __PreProcess( in )
/*------------*/
  in:= "AAA"
  pre :="Qout()"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

/*------------*/
  in:= "AAA A a"
  pre :="Qout(a )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

/*------------*/
  in:= "AAA A a B b"
  pre :="Qout(a ,b )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

/*------------*/
CTEXT TO VAR in
#define RED {255,0,0}\n
#xcommand SET TOOLTIP TO <color> OF <form> => 
SM( TTH (<"form">), 1, RGB(<color>[1], 
<color>\[2\],
<color>[, <color>[ 3 ] ]), 0)\n
SET TOOLTIP TO RED OF form1
ENDTEXT
TEXT TO VAR pre    
SM(TTH ("form1"),1,RGB({255,0,0}[1],{255,0,0}[2],{255,0,0},{255,0,0}[ 3 ] ),0)
ENDTEXT
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

/*------------*/
  in :="#command ZZZ [<v>] => QOUT( [ <v>[1] ] )"
  __PreProcess( in )
  in :="ZZZ a"
  pre :="QOUT(a[1] )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

/*------------*/
  in :="ZZZ"
  pre :="QOUT()"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

/*------------*/
  in := "ZZZ a[1]+2"
  pre := "QOUT(a[1]+2[1] )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

/*------------*/
CTEXT TO VAR in
#xtranslate _HMG_a  =>  _HMG\[137\]\n
v:= _bro[ a( _HMG_a [i] ) ]
ENDTEXT
  pre :="v:= _bro[ a( _HMG[137] [i] ) ]"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

/*------------*/
CTEXT TO VAR in
#define clas( x )   ( x )\n
#xtranslate ( <!name!>{ [<p,...>] } => (<name>():New( <p> )\n
a :=clas( TEST{ 1, 2,3} )
ENDTEXT
  pre := "a :=(TEST():New(1,2,3 ) )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

/*------------*/
CTEXT TO VAR in
#define DATENEW   1\n
#define DATEOLD(x)   x\n
#define datediff(x,y) ( DATEOLD(x) - DATENEW )\n
\n
#command datediff1(<x>,<y>) => ( DATEOLD(<x>) - DATENEW )\n
x := datediff( x, y )
ENDTEXT
  pre := "x := (x - 1 )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

/*------------*/




/* ---------------------------------------------------------------------*/
  __PP_FREE()
  
  ? "Total count   =", nCnt
  ? "Valid results =", nRes
  ? "Failed results=", nCnt - nRes
  
  RETURN

PROCEDURE PrePrepare( in )
LOCAL len, i, cin, cout

  len := MLCOUNT( in )
  FOR i:=1 TO len
    cin := ALLTRIM( MEMOLINE(in, 192, i) )
    __PreProcess( cin )
  NEXT

RETURN

FUNCTION PreRun( in, pre )
LOCAL len, i, cin, cout
LOCAL out:=''

  len := MLCOUNT( in )
  FOR i:=1 TO len
    cin := ALLTRIM( MEMOLINE(in, 1024, i) )
    out += __PreProcess( cin )
  NEXT

RETURN out
  
FUNCTION PreResult( pre, out, pCnt )

  pCnt++
  IF( pre == out )
    RETURN 1
  ELSE
    ? pre
    ? out
    ? " => FAILED"
  ENDIF
  
RETURN 0