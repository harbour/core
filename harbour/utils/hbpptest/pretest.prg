/*
 * $Id$
 */

#ifndef __HARBOUR__
   #xtranslate HB_OSNewLine() => ( Chr( 13 ) + Chr( 10 ) )
#endif

#command TEXT TO VAR <v> => #pragma __stream|%s||<v>:=
#command CTEXT TO VAR <v> => #pragma __cstream|%s||<v>:=

/* Testing preprocessor */

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
  in := "#xtranslate AAA [A <a> [B <b>] ] => Qout([<a>][, <b>])"
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
  in :="#command ZZZ [<v>] => QOUT([<v>[1]])"
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
#define clas( x )   (x)\n
#xtranslate ( <!name!>{ [<p,...>] } => (<name>():New(<p>)\n
a :=clas( TEST{ 1,2,3} )
ENDTEXT
  pre := "a :=(TEST():New(1,2,3) )"
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
	//REGULAR
  in := "#command _REGULAR_(<z>) => rm( <z> )"
  __PreProcess( in )

  in := "_REGULAR_(a)"
  pre :="rm(a )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )
  
  in := '_REGULAR_("a")'
  pre :='rm("a" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_REGULAR_('a')"
  pre :='rm("a" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

CTEXT TO VAR in
_REGULAR_(["'a'"])
ENDTEXT
CTEXT TO VAR pre
rm(["'a'"] )
ENDTEXT
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_REGULAR_(&a.1)"
  pre :="rm(&a.1 )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_REGULAR_(&a)"
  pre :="rm(&a )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_REGULAR_(&a.)"
  pre :="rm(&a. )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_REGULAR_(&(a))"
  pre :="rm(&(a) )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_REGULAR_(&a[1])"
  pre :="rm(&a[1] )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_REGULAR_(a[1])"
  pre :="rm(a[1] )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

CTEXT TO VAR in
_REGULAR_("['']")
ENDTEXT
CTEXT TO VAR pre
rm("['']" )
ENDTEXT
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )


	//NORMAL
  in := '#command _NORMAL_M(<z>) => nm( <"z"> )'
  __PreProcess( in )
  
  in := "_NORMAL_M(a)"
  pre :='nm("a" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := '_NORMAL_M("a")'
  pre :=[nm('"a"' )]
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_NORMAL_M('a')"
  pre :=[nm('"a"' )]
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

CTEXT TO VAR in
_NORMAL_M(["'a'"])
ENDTEXT
CTEXT TO VAR pre
nm([["'a'"]] )
ENDTEXT
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_NORMAL_M(&a.1)"
  pre :='nm("&a.1" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_NORMAL_M(&a)"
  pre :="nm(a )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_NORMAL_M(&a.)"
  pre :="nm(a )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_NORMAL_M(&(a))"
  pre :="nm((a) )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_NORMAL_M(&a[1])"
  pre :='nm("&a[1]" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_NORMAL_M(a[1])"
  pre :='nm("a[1]" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

CTEXT TO VAR in
_NORMAL_M("['']")
ENDTEXT
CTEXT TO VAR pre
nm(["['']"] )
ENDTEXT
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )


	//SMART
  in := '#command _SMART_M(<z>) => sm( <(z)> )'
  __PreProcess( in )
  
  in := "_SMART_M(a)"
  pre :='sm("a" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := '_SMART_M("a")'
  pre :='sm("a" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_SMART_M('a')"
  pre :='sm("a" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

CTEXT TO VAR in
_SMART_M(["'a'"])
ENDTEXT
CTEXT TO VAR pre
sm(["'a'"] )
ENDTEXT
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_SMART_M(&a.1)"
  pre :='sm("&a.1" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_SMART_M(&a)"
  pre :='sm(a )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_SMART_M(&a.)"
  pre :='sm(a )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_SMART_M(&(a))"
  pre :='sm((a) )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_SMART_M(&a[1])"
  pre :='sm("&a[1]" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_SMART_M(a[1])"
  pre :='sm("a[1]" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

CTEXT TO VAR in
_SMART_M("['']")
ENDTEXT
CTEXT TO VAR pre
sm("['']" )
ENDTEXT
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )



	//DUMB
  in := '#command _DUMB_M(<z>) => dm( #<z> )'
  __PreProcess( in )

  in := "_DUMB_M(a)"
  pre :='dm("a" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := '_DUMB_M("a")'
  pre :=[dm('"a"' )]
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_DUMB_M('a')"
  pre :=[dm('"a"' )]
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

CTEXT TO VAR in
_DUMB_M(["'a'"])
ENDTEXT
CTEXT TO VAR pre
dm([["'a'"]] )
ENDTEXT
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_DUMB_M(&a.1)"
  pre :='dm("&a.1" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_DUMB_M(&a)"
  pre :='dm("&a" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_DUMB_M(&a.)"
  pre :='dm("&a." )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_DUMB_M(&(a))"
  pre :='dm("&(a)" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_DUMB_M(&a[1])"
  pre :='dm("&a[1]" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "_DUMB_M(a[1])"
  pre :='dm("a[1]" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

CTEXT TO VAR in
_DUMB_M("['']")
ENDTEXT
CTEXT TO VAR pre
dm(["['']"] )
ENDTEXT
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )


  // REGULAR list
CTEXT TO VAR in
#command _REGULAR_L(<z,...>)	=> rl( <z> )\n
_REGULAR_L(a,"a",'a',["'a'"],"['a']",'["a"]',&a.1,&a,&a.,&a.  ,&(a),&a[1],&a.[1],&a.  [2],&a&a, &a.a,  a, a)
ENDTEXT
CTEXT TO VAR pre
rl(a,"a","a",["'a'"],"['a']",'["a"]',&a.1,&a,&a.,&a.  ,&(a),&a[1],&a.[1],&a.  [2],&a&a, &a.a,  a, a )
ENDTEXT
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  // NORMAL list
CTEXT TO VAR in
#command _NORMAL_L(<z,...>) => nl( <"z"> )\n
_NORMAL_L(n,"n",'a',["'a'"],"['a']",'["a"]',&a.1,&a,&a.,&a.  ,&(a),&a[1],&a.[1],&a.  [2],&a&a, &.a, &a.a,  a, a)
ENDTEXT
CTEXT TO VAR pre
nl("n",'"n"','"a"',[["'a'"]],["['a']"],['["a"]'],"&a.1",a,a,a,(a),"&a[1]","&a.[1]","&a.  [2]","&a&a","&.a","&a.a","a","a" )
ENDTEXT
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  // SMART list
CTEXT TO VAR in
#command _SMART_L(<z,...>) => sl( <(z)> )\n
_SMART_L(a,"a",'a',["'a'"],"['a']",'["a"]',&a.1,&a,&a.,&a.  ,&(a),&a[1],&a.[1],&a.  [2],&a&a, &.a, &a.a,  a, a)
ENDTEXT
CTEXT TO VAR pre
sl("a","a","a",["'a'"],"['a']",'["a"]',"&a.1",a,a,a,(a),"&a[1]","&a.[1]","&a.  [2]","&a&a","&.a","&a.a","a","a" )
ENDTEXT
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  // DUMB list
CTEXT TO VAR in
#command _DUMB_L(<z,...>) => dl( #<z> )\n
_DUMB_L(a,"a",'a',["'a'"],"['a']",'["a"]',&a.1,&a,&a.,&a.  ,&(a),&a[1],&a.[1],&a.  [2],&a&a, &.a, &a.a,  a, a)
ENDTEXT
CTEXT TO VAR pre
dl([a,"a","a",["'a'"],"['a']",'["a"]',&a.1,&a,&a.,&a.  ,&(a),&a[1],&a.[1],&a.  [2],&a&a, &.a, &a.a,  a, a] )
ENDTEXT
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "
index on LEFT(   f1  ,  10   )      to _tst"
CTEXT TO VAR pre
dbCreateIndex( "_tst", "LEFT(   f1  ,  10   )", {||LEFT(   f1  ,  10   )}, if( .F., .t., NIL ) )
ENDTEXT
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

TEXT TO VAR in
#xcommand SET <var1> [, <varN>] WITH <val> => 
<var1>:=<val>[; <varN>:=<val>]
ENDTEXT
  __PreProcess( in )
  in := "SET v1, v2, v3 WITH 0"
  pre := "v1:=0; v2:=0 ; v3:=0 "
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

TEXT TO VAR in
#xcommand INSERT INTO <table> ( <uField1> [, <uFieldN> ] ) VALUES ( <uVal1> 
[, <uValN> ] ) => if <table>->( dbappend() ) <table>-><uField1> := <uVal1> [ <table>-><uFieldN> := <uValN> ] <table>->( dbunlock() ) endif

ENDTEXT  
  __PreProcess( in )
TEXT TO VAR in
insert into test ( FIRST, LAST, STREET ) values ( "first", "last", "street" )
ENDTEXT
TEXT TO VAR pre
if test->(dbappend() ) test->FIRST := "first"  test->LAST := "last"   test->STREET := "street"   test->(dbunlock() ) endif
ENDTEXT
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

TEXT TO VAR in
#xcommand INSERT2 INTO <table> ( <uField1> [, <uFieldN> ] ) VALUES ( <uVal1> 
[, <uValN> ] ) => 
if <table>->( dbappend() ) ;
 <table>-><uField1> := <uVal1> ;
 [ <table>-><uFieldN> := <uValN> ; ] 
 <table>->( dbunlock() ) ;
endif
ENDTEXT  
  __PreProcess( in )
TEXT TO VAR in
insert2 into test ( FIRST, LAST, STREET )
 values ( "first", "last", "street" )
ENDTEXT
TEXT TO VAR pre
if test->(dbappend() ) ; test->FIRST := "first" ;  test->LAST := "last" ;   test->STREET := "street" ;   test->(dbunlock() ) ;endif
ENDTEXT
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "#define F1( n ) F2( n, N )"
  __PreProcess( in )
  in := "F1( 1 )"
  pre := "F2(1 ,N )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "#define F3( nN, Nn ) F2( nN, Nn, NN, nn, N, n )"
  __PreProcess( in )
  in := "F3( 1, 2 )"
  pre := "F2(1,2 ,NN,nn,N,n )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

TEXT TO VAR in
#command MYCOMMAND [<mylist,...>] [MYCLAUSE <myval>] => 
   MyFunction( {<mylist>} [, <myval>] )
ENDTEXT
  __PreProcess( in )
  in := 'MYCOMMAND MYCLAUSE 321 "HELLO"'
  pre := 'MyFunction({"HELLO"} ,321  )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )
   
  in := 'MYCOMMAND MYCLAUSE 321 "HELLO","all"'
  pre := 'MyFunction({"HELLO","all"} ,321  )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := 'MYCOMMAND "HELLO","all" MYCLAUSE 321'
  pre := 'MyFunction({"HELLO","all"} ,321  )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

TEXT TO VAR in
#command MYCOMMAND2 [<mylist,...>] [MYCLAUSE <myval>] [ALL] => 
   MyFunction( {<mylist>} [, <myval>] )
ENDTEXT
  __PreProcess( in )
  pre := 'MyFunction({"HELLO"} ,321  )'
  in := 'MYCOMMAND2 MYCLAUSE 321 "HELLO"'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )
  in := 'MYCOMMAND2 MYCLAUSE 321 "HELLO" ALL'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := 'MYCOMMAND2 ALL MYCLAUSE 321 "HELLO"'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := 'MYCOMMAND2 MYCLAUSE 321 "HELLO" ALL' 
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := 'MYCOMMAND2 MYCLAUSE 321 ALL "HELLO"'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )


TEXT TO VAR in
#command MYCOMMAND3 [<mylist,...>] [MYCLAUSE <myval>] [<all:ALL>] => 
   MyFunction( {<mylist>} [, <myval>] [,<.all.>] )
ENDTEXT
  __PreProcess( in )
  in := 'MYCOMMAND3 ALL MYCLAUSE 321 "HELLO","WORLD"'
  pre := 'MyFunction({"HELLO","WORLD"} ,321  ,.T.  )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := 'MYCOMMAND3 MYCLAUSE 321 ALL "HELLO"'
  pre := 'MyFunction({"HELLO"} ,321  ,.T.  )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := 'MYCOMMAND3 MYCLAUSE 321 "HELLO" ALL'
  pre := 'MyFunction({"HELLO"} ,321  ,.T.  )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := 'MYCOMMAND3 MYCLAUSE 321 "HELLO"'
  pre := 'MyFunction({"HELLO"} ,321   )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

   
  /* Special restricted macro match marker (used in SET FILTER TO command */
  in := "SET FILTER TO &cVar."
  pre := "if ( Empty(cVar) ) ;    dbClearFilter() ; else ;    dbSetFilter({||&cVar.},cVar) ; end"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "SET FILTER TO &(cVar .AND. &cVar)"
  pre := "if ( Empty((cVar .AND. &cVar)) ) ;    dbClearFilter() ; else ;    dbSetFilter({||&(cVar .AND. &cVar)},(cVar .AND. &cVar)) ; end"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "SET FILTER TO &cVar. .AND. cVar"
  pre := 'dbSetFilter( {||&cVar. .AND. cVar}, "&cVar. .AND. cVar" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

CTEXT TO VAR in
#xtranslate XTRANS(<x>( => normal( <(x)> )\n
#xtranslate XTRANS(<x:&>( => macro( <(x)> )
ENDTEXT
  PrePrepare( in )

  in := "XTRANS( cVar ("
  pre := 'normal("cVar" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "XTRANS( &cVar ("
  pre := 'macro(cVar )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "XTRANS( &cVar+1 ("
  pre := 'normal("&cVar+1" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "XTRANS( &cVar. ("
  pre := 'macro(cVar )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "XTRANS( (&cVar.) ("
  pre := 'XTRANS( (&cVar.) ('
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "XTRANS( &(cVar) ("
  pre := 'macro((cVar) )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "XTRANS( &cVar[3] ("
  pre := 'normal("&cVar[3]" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "XTRANS( &cVar.  [3] ("
  pre := 'normal("&cVar.  [3]" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "XTRANS( &(cVar  [3],&cvar) ("
  pre := 'macro((cVar  [3],&cvar) )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "XTRANS( (&cVar.  [3],&cvar) ("
  pre := 'XTRANS( (&cVar.  [3],&cvar) ('
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "XTRANS( &cVar.1+5 ("
  pre := 'normal("&cVar.1+5" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "XTRANS( &cVar .AND. cVar ("
  pre := 'normal("&cVar .AND. cVar" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "XTRANS( &cVar. .AND. cVar ("
  pre := 'normal("&cVar. .AND. cVar" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

CTEXT TO VAR in
#xtranslate MXCALL <x:&>  => (<x>)\n
#xtranslate MYCALL <x:&> <y>  => <x>( <y>, 'mycall' )\n
#xtranslate MZCALL <x> <y>  => <x>( <y>, "mzcall" )\n
#command   FOO <x:&> FOO <y:&> => <(x)>+<(y)>\n
#translate BAR <x:&> BAR <y:&> => <(x)>+<(y)>
ENDTEXT
  PrePrepare( in )

  in := "MYCALL &cVar ++cVar"
  pre := '&cVar(++cVar,"mycall" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MZCALL &cVar ++cVar"
  pre := '&cVar ++(cVar,"mzcall" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MYCALL &cVar+1 &cVar"
  pre := '&cVar(+1,"mycall" ) &cVar'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MZCALL &cVar+1 &cVar"
  pre := '&cVar+1(&cVar,"mzcall" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MXCALL &cVar" 
  pre := '(&cVar)'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MXCALL &cVar." 
  pre := '(&cVar.)'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MXCALL &cVar.1" 
  pre := '(&cVar.1)'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MXCALL &cVar + 1" 
  pre := '(&cVar) + 1'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MYCALL &cVar &cVar"
  pre := '&cVar(&cVar,"mycall" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MYCALL &cVar. &cVar."
  pre := '&cVar.(&cVar.,"mycall" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MYCALL &cVar.1 &cVar.1"
  pre := '&cVar.1(&cVar.1,"mycall" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MYCALL &cVar ++cVar"
  pre := '&cVar(++cVar,"mycall" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MYCALL &cVar. --cVar"
  pre := '&cVar.(--cVar,"mycall" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MYCALL &cVar.1 !cVar"
  pre := '&cVar.1(!cVar,"mycall" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MYCALL &cVar+1 &cVar"
  pre := '&cVar(+1,"mycall" ) &cVar'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MYCALL &cVar.+1 &cVar."
  pre := '&cVar.(+1,"mycall" ) &cVar.'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MYCALL &cVar.1+1 &cVar.1"
  pre := '&cVar.1(+1,"mycall" ) &cVar.1'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MYCALL &cVar +1 &cVar"
  pre := '&cVar(+1,"mycall" ) &cVar'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MYCALL &cVar. +1 &cVar."
  pre := '&cVar.(+1,"mycall" ) &cVar.'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MYCALL &cVar.1 +1 &cVar.1"
  pre := '&cVar.1(+1,"mycall" ) &cVar.1'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MYCALL &cVar +1"
  pre := '&cVar(+1,"mycall" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MYCALL &cVar. +1"
  pre := '&cVar.(+1,"mycall" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MYCALL &cVar.1 +1" 
  pre := '&cVar.1(+1,"mycall" )'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "FOO &cVar FOO &var."
  pre := 'cVar+var'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "BAR &cVar BAR &var."
  pre := 'cVar+var'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "FOO &cVar FOO &var.+1"
  pre := 'FOO &cVar FOO &var.+1'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "BAR &cVar BAR &var.+1"
  pre := 'cVar+var+1'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MXCALL &cVar()" 
  pre := '(&cVar)()'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MXCALL &cVar++" 
  pre := '(&cVar)++'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "(MXCALL &cVar)++" 
  pre := '((&cVar))++'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MXCALL &cVar.()" 
  pre := '(&cVar.)()'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MXCALL &cVar.++" 
  pre := '(&cVar.)++'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "(MXCALL &cVar.)++" 
  pre := '((&cVar.))++'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MXCALL &cVar.1 ()" 
  pre := '(&cVar.1) ()'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MXCALL &cVar.1 ++" 
  pre := '(&cVar.1) ++'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "(MXCALL &cVar.1) ++" 
  pre := '((&cVar.1)) ++'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

CTEXT TO VAR in
#translate MTRANSLATE <x> => normal_t(<"x">)\n
#translate MTRANSLATE <x:&>  => macro_t(<(x)>)\n
#command MCOMMAND <x> => normal_c(<"x">)\n
#command MCOMMAND <x:&>  => macro_c(<(x)>)
ENDTEXT
  PrePrepare( in )

  in := "MTRANSLATE &cVar"
  pre :='macro_t(cVar)'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE &cVar."
  pre :='macro_t(cVar)'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE &(cVar)"
  pre :='macro_t((cVar))'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE & (cVar)"
  pre :='macro_t((cVar))'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE &cVar&cVar"
  pre :='macro_t("&cVar&cVar")'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE &cVar+1"
  pre :='macro_t(cVar)+1'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE &cVar.+1"
  pre :='macro_t(cVar)+1'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE &cVar. .AND.  .T."
  pre :='macro_t(cVar) .AND.  .T.'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE &cVar++"
  pre :='macro_t(cVar)++'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE &cVar.++" 
  pre :='macro_t(cVar)++'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE &cVar+=1"
  pre :='macro_t(cVar)+=1'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE &cVar.-=2" 
  pre :='macro_t(cVar)-=2'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE &cVar*=1"
  pre :='macro_t(cVar)*=1'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE &cVar/=2"
  pre :='macro_t(cVar)/=2'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE &cVar%=1"
  pre :='macro_t(cVar)%=1'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE &cVar^=2"
  pre :='macro_t(cVar)^=2'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE &cVar:=1" 
  pre :='macro_t(cVar):=1'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE &cVar. .AND.  .T."
  pre :='macro_t(cVar) .AND.  .T.'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE &cVar .AND.  .T."
  pre :='macro_t(cVar) .AND.  .T.'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE &(cVar) +1"
  pre :='macro_t((cVar)) +1'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE & (cVar) +1"
  pre :='macro_t((cVar)) +1'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE &cVar.&cVar."
  pre :='macro_t("&cVar.&cVar.")'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE &cVar.&cVar.&cVar&cVar"
  pre :='macro_t("&cVar.&cVar.&cVar&cVar")'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MTRANSLATE &cVar.&(cVar)"
  pre :='macro_t("&cVar.&")(cVar)'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )


   /* command */
  in := "MCOMMAND &cVar"
  pre :='macro_c(cVar)'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND &cVar."
  pre :='macro_c(cVar)'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND &(cVar)"
  pre :='macro_c((cVar))'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND & (cVar)"
  pre :='macro_c((cVar))'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND &cVar&cVar"
  pre :='macro_c("&cVar&cVar")'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND &cVar+1"
  pre :='normal_c("&cVar+1")'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND &cVar.+1"
  pre :='normal_c("&cVar.+1")'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND &cVar. .AND.  .T."
  pre :='normal_c("&cVar. .AND.  .T.")'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND &cVar++"
  pre :='normal_c("&cVar++")'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND &cVar.++" 
  pre :='normal_c("&cVar.++")'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND &cVar+=1"
  pre :='normal_c("&cVar+=1")'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND &cVar.-=2" 
  pre :='normal_c("&cVar.-=2")'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND &cVar*=1"
  pre :='normal_c("&cVar*=1")'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND &cVar/=2" 
  pre :='normal_c("&cVar/=2")'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND &cVar%=12"
  pre :='normal_c("&cVar%=12")'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND &cVar^=2"
  pre :='normal_c("&cVar^=2")'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )
  
  in := "MCOMMAND &cVar:=1"
  pre :='normal_c("&cVar:=1")'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND &cVar. .AND.  .T."
  pre :='normal_c("&cVar. .AND.  .T.")'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND &cVar .AND.  .T."
  pre :='normal_c("&cVar .AND.  .T.")'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND &(cVar) +1"
  pre :='normal_c((cVar) +1)'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND & (cVar) +1"
  pre :='normal_c( (cVar) +1)'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND &cVar.&cVar."
  pre :='macro_c("&cVar.&cVar.")'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND &cVar.&cVar.&cVar&cVar2"
  pre :='macro_c("&cVar.&cVar.&cVar&cVar2")'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "MCOMMAND &cVar.&(cVar)"
  pre :='normal_c("&cVar.&(cVar)")'
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

   /* repeated optional clauses */
TEXT TO VAR in
#xcommand SET <var1> [, <varN>] WITH <val> => 
<var1>:=<val> [; <varN>:=<val>]
ENDTEXT
  __PreProcess( in )
  in := "SET v1 WITH 0"
  pre := "v1:=0 "
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "SET v1, v2 WITH 0"
  pre := "v1:=0 ; v2:=0 "
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "SET v1, v2, v3 WITH 0"
  pre := "v1:=0 ; v2:=0 ; v3:=0 "
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "SET v1, v2, v3, v4 WITH 0"
  pre := "v1:=0 ; v2:=0 ; v3:=0 ; v4:=0 "
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

TEXT TO VAR in
#command AVG <x1> [, <xn>] TO <v1> [, <vn>]  =>
   AVERAGE( {||<v1>:=<v1>+<x1>} [, {||<vn>:=<vn>+<xn>} ] )
ENDTEXT
  __PreProcess( in )
  in := "AVG f1 TO s1"
  pre := "AVERAGE({||s1:=s1+f1}  )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )
  
  in := "AVG f1, f2 TO s1, s2"
  pre := "AVERAGE({||s1:=s1+f1} ,{||s2:=s2+f2}   )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )

  in := "AVG f1, f2, f3 TO s1, s2, s3"
  pre := "AVERAGE({||s1:=s1+f1} ,{||s2:=s2+f2}  ,{||s3:=s3+f3}   )"
  nRes += PreResult( pre, PreRun( in, pre ), @nCnt )


  
/* ---------------------------------------------------------------------*/
  __PP_FREE()
  
  OutStd( "Total count   =", nCnt, hb_OSNewLine() )
  OutStd( "Valid results =", nRes, hb_OSNewLine() )
  OutStd( "Failed results=", nCnt - nRes, hb_OSNewLine() )
  
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
LOCAL i

  pCnt++
  IF( pre == out )
    RETURN 1
  ELSE
    OutStd( pre, hb_OSNewLine() )
    OutStd( out, hb_OSNewLine() )
    OutStd( " => FAILED in LINE: ", PROCLINE(1), hb_OSNewLine() )
    i := 1
    WHILE SUBSTR(pre,i,1) == SUBSTR(out,i,1)
      i++
    ENDDO
    OutStd( SUBSTR( pre, i ), hb_OSNewLine() )
    OutStd( SUBSTR( out, i ), hb_OSNewLine() )
  ENDIF
  
RETURN 0
