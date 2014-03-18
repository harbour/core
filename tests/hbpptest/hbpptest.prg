#ifndef __HARBOUR__
   #xtranslate hb_eol() => ( Chr( 13 ) + Chr( 10 ) )
#endif

/* #command TEXT TO VAR <v> => #pragma __stream|<v>:=%s */
#command TEXT TO VAR <v> => #pragma __text|<v>+=%s;<v>:=""
#command CTEXT TO VAR <v> => #pragma __cstream|<v>:=%s
#command XTEXT TO VAR <v> => #pragma __text|<v>+=%s+hb_eol();<v>:=""

/* Testing preprocessor */

PROCEDURE Main()
  LOCAL in, pre
  LOCAL nCnt := 0
  LOCAL nRes := 0
  LOCAL pp

  pp := __pp_Init()

/* ---------------------------------------------------------------------*/
  in := "#xtranslate CCC <v> => QOut( <v>[2] [, <v>\[<v>\]\[3\]] )"+hb_eol()+;
        "CCC b"
  pre := "QOut(b[2] ,bb[3] )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )
/*------------*/
  in := "#xtranslate AAA [A <a> [B <b>] ] => QOut([<a>][, <b>])"
  __pp_Process( pp, in )
/*------------*/
  in:= "AAA"
  pre :="QOut()"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

/*------------*/
  in:= "AAA A a"
  pre :="QOut(a )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

/*------------*/
  in:= "AAA A a B b"
  pre :="QOut(a ,b )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

/*------------*/
XTEXT TO VAR in
#define RED {255,0,0}
#xcommand SET TOOLTIP TO <color> OF <form> => SM( TTH (<"form">), 1, RGB(<color>\[1], <color>\[2\], <color>[, <color>\[ 3 \] ]), 0)
SET TOOLTIP TO RED OF form1
ENDTEXT
TEXT TO VAR pre
SM(TTH ("form1"),1,RGB({255,0,0}[1],{255,0,0}[2],{255,0,0},{255,0,0}[ 3 ] ),0)
ENDTEXT
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

/*------------*/
  in :="#command ZZZ [<v>] => QOut([<v>\[1\]])"
  __pp_Process( pp, in )
  in :="ZZZ a"
  pre :="QOut(a[1] )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

/*------------*/
  in :="ZZZ"
  pre :="QOut()"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

/*------------*/
  in := "ZZZ a[1]+2"
  pre := "QOut(a[1]+2[1] )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

/*------------*/
XTEXT TO VAR in
#xtranslate _HMG_a  =>  _HMG\[137\]
v:= _bro[ a( _HMG_a [i] ) ]
ENDTEXT
  pre :="v:= _bro[ a( _HMG[137] [i] ) ]"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

/*------------*/
XTEXT TO VAR in
#define clas( x )   (x)
#xtranslate ( <!name!>{ [<p,...>] } => (<name>():New(<p>)
a :=clas( TEST{ 1,2,3} )
ENDTEXT
  pre := "a :=(TEST():New(1,2,3) )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

/*------------*/
XTEXT TO VAR in
#define DATENEW   1
#define DATEOLD(x)   x
#define datediff(x,y) ( DATEOLD(x) - DATENEW )

#command datediff1(<x>,<y>) => ( DATEOLD(<x>) - DATENEW )
x := datediff( x, y )
ENDTEXT
  pre := "x := (x - 1 )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

/*------------*/
      //REGULAR
  in := "#command _REGULAR_(<z>) => rm( <z> )"
  __pp_Process( pp, in )

  in := "_REGULAR_(a)"
  pre :="rm(a )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := '_REGULAR_("a")'
  pre :='rm("a" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_REGULAR_('a')"
  pre :='rm("a" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

CTEXT TO VAR in
_REGULAR_(["'a'"])
ENDTEXT
CTEXT TO VAR pre
rm(["'a'"] )
ENDTEXT
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_REGULAR_(&a.1)"
  pre :="rm(&a.1 )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_REGULAR_(&a)"
  pre :="rm(&a )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_REGULAR_(&a.)"
  pre :="rm(&a. )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_REGULAR_(&(a))"
  pre :="rm(&(a) )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_REGULAR_(&a[1])"
  pre :="rm(&a[1] )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_REGULAR_(a[1])"
  pre :="rm(a[1] )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

CTEXT TO VAR in
_REGULAR_("['']")
ENDTEXT
CTEXT TO VAR pre
rm("['']" )
ENDTEXT
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )


      //NORMAL
  in := '#command _NORMAL_M(<z>) => nm( <"z"> )'
  __pp_Process( pp, in )

  in := "_NORMAL_M(a)"
  pre :='nm("a" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := '_NORMAL_M("a")'
  pre :=[nm('"a"' )]
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_NORMAL_M('a')"
  pre :=[nm('"a"' )]
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

CTEXT TO VAR in
_NORMAL_M(["'a'"])
ENDTEXT
CTEXT TO VAR pre
nm([["'a'"]] )
ENDTEXT
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_NORMAL_M(&a.1)"
  pre :='nm("&a.1" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_NORMAL_M(&a)"
  pre :="nm(a )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_NORMAL_M(&a.)"
  pre :="nm(a )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_NORMAL_M(&(a))"
  pre :="nm((a) )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_NORMAL_M(&a[1])"
  pre :='nm("&a[1]" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_NORMAL_M(a[1])"
  pre :='nm("a[1]" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

CTEXT TO VAR in
_NORMAL_M("['']")
ENDTEXT
CTEXT TO VAR pre
nm(["['']"] )
ENDTEXT
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )


      //SMART
  in := '#command _SMART_M(<z>) => sm( <(z)> )'
  __pp_Process( pp, in )

  in := "_SMART_M(a)"
  pre :='sm("a" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := '_SMART_M("a")'
  pre :='sm("a" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_SMART_M('a')"
  pre :='sm("a" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

CTEXT TO VAR in
_SMART_M(["'a'"])
ENDTEXT
CTEXT TO VAR pre
sm(["'a'"] )
ENDTEXT
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_SMART_M(&a.1)"
  pre :='sm("&a.1" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_SMART_M(&a)"
  pre :='sm(a )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_SMART_M(&a.)"
  pre :='sm(a )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_SMART_M(&(a))"
  pre :='sm((a) )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_SMART_M(&a[1])"
  pre :='sm("&a[1]" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_SMART_M(a[1])"
  pre :='sm("a[1]" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

CTEXT TO VAR in
_SMART_M("['']")
ENDTEXT
CTEXT TO VAR pre
sm("['']" )
ENDTEXT
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )



      //DUMB
  in := '#command _DUMB_M(<z>) => dm( #<z> )'
  __pp_Process( pp, in )

  in := "_DUMB_M(a)"
  pre :='dm("a" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := '_DUMB_M("a")'
  pre :=[dm('"a"' )]
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_DUMB_M('a')"
  pre :=[dm('"a"' )]
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

CTEXT TO VAR in
_DUMB_M(["'a'"])
ENDTEXT
CTEXT TO VAR pre
dm([["'a'"]] )
ENDTEXT
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_DUMB_M(&a.1)"
  pre :='dm("&a.1" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_DUMB_M(&a)"
  pre :='dm("&a" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_DUMB_M(&a.)"
  pre :='dm("&a." )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_DUMB_M(&(a))"
  pre :='dm("&(a)" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_DUMB_M(&a[1])"
  pre :='dm("&a[1]" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "_DUMB_M(a[1])"
  pre :='dm("a[1]" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

CTEXT TO VAR in
_DUMB_M("['']")
ENDTEXT
CTEXT TO VAR pre
dm(["['']"] )
ENDTEXT
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )


  // REGULAR list
XTEXT TO VAR in
#command _REGULAR_L(<z,...>)  => rl( <z> )
_REGULAR_L(a,"a",'a',["'a'"],"['a']",'["a"]',&a.1,&a,&a.,&a.  ,&(a),&a[1],&a.[1],&a.  [2],&a&a, &a.a,  a, a)
ENDTEXT
CTEXT TO VAR pre
rl(a,"a","a",["'a'"],"['a']",'["a"]',&a.1,&a,&a.,&a.  ,&(a),&a[1],&a.[1],&a.  [2],&a&a, &a.a,  a, a )
ENDTEXT
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  // NORMAL list
XTEXT TO VAR in
#command _NORMAL_L(<z,...>) => nl( <"z"> )
_NORMAL_L(n,"n",'a',["'a'"],"['a']",'["a"]',&a.1,&a,&a.,&a.  ,&(a),&a[1],&a.[1],&a.  [2],&a&a, &.a, &a.a,  a, a)
ENDTEXT
CTEXT TO VAR pre
nl("n",'"n"','"a"',[["'a'"]],["['a']"],['["a"]'],"&a.1",a,a,a,(a),"&a[1]","&a.[1]","&a.  [2]","&a&a","&.a","&a.a","a","a" )
ENDTEXT
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  // SMART list
XTEXT TO VAR in
#command _SMART_L(<z,...>) => sl( <(z)> )
_SMART_L(a,"a",'a',["'a'"],"['a']",'["a"]',&a.1,&a,&a.,&a.  ,&(a),&a[1],&a.[1],&a.  [2],&a&a, &.a, &a.a,  a, a)
ENDTEXT
CTEXT TO VAR pre
sl("a","a","a",["'a'"],"['a']",'["a"]',"&a.1",a,a,a,(a),"&a[1]","&a.[1]","&a.  [2]","&a&a","&.a","&a.a","a","a" )
ENDTEXT
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  // DUMB list
XTEXT TO VAR in
#command _DUMB_L(<z,...>) => dl( #<z> )
_DUMB_L(a,"a",'a',["'a'"],"['a']",'["a"]',&a.1,&a,&a.,&a.  ,&(a),&a[1],&a.[1],&a.  [2],&a&a, &.a, &a.a,  a, a)
ENDTEXT
CTEXT TO VAR pre
dl([a,"a","a",["'a'"],"['a']",'["a"]',&a.1,&a,&a.,&a.  ,&(a),&a[1],&a.[1],&a.  [2],&a&a, &.a, &a.a,  a, a] )
ENDTEXT
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "index on Left(   f1  ,  10   )      to _tst"
CTEXT TO VAR pre
dbCreateIndex( "_tst", "Left(   f1  ,  10   )", {|| Left(   f1  ,  10   )}, if( .F., .T., NIL ) )
ENDTEXT
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

TEXT TO VAR in
#xcommand SET <var1> [, <varN>] WITH <val> =>
<var1>:=<val>[; <varN>:=<val>]
ENDTEXT
  __pp_Process( pp, in )
  in := "SET v1, v2, v3 WITH 0"
  pre := "v1:=0; v2:=0 ; v3:=0 "
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

TEXT TO VAR in
#xcommand INSERT INTO <table> ( <uField1> [, <uFieldN> ] ) VALUES ( <uVal1> [, <uValN> ] ) => if <table>->( dbAppend() ) <table>-><uField1> := <uVal1> [ <table>-><uFieldN> := <uValN> ] <table>->( dbUnlock() ) endif
ENDTEXT
  __pp_Process( pp, in )
TEXT TO VAR in
insert into test ( FIRST, LAST, STREET ) values ( "first", "last", "street" )
ENDTEXT
TEXT TO VAR pre
if test->(dbAppend() ) test->FIRST := "first"  test->LAST := "last"   test->STREET := "street"   test->(dbUnlock() ) endif
ENDTEXT
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

TEXT TO VAR in
#xcommand INSERT2 INTO <table> ( <uField1> [, <uFieldN> ] ) VALUES ( <uVal1> [, <uValN> ] ) =>
if <table>->( dbAppend() ) ;
 <table>-><uField1> := <uVal1> ;
 [ <table>-><uFieldN> := <uValN> ; ]
 <table>->( dbUnlock() ) ;
endif
ENDTEXT
  __pp_Process( pp, in )
TEXT TO VAR in
insert2 into test ( FIRST, LAST, STREET )
 values ( "first", "last", "street" )
ENDTEXT
TEXT TO VAR pre
if test->(dbAppend() ) ; test->FIRST := "first" ;  test->LAST := "last" ;   test->STREET := "street" ;   test->(dbUnlock() ) ;endif
ENDTEXT
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "#define F1( n ) F2( n, N )"
  __pp_Process( pp, in )
  in := "F1( 1 )"
  pre := "F2(1 ,N )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "#define F3( nN, Nn ) F2( nN, Nn, NN, nn, N, n )"
  __pp_Process( pp, in )
  in := "F3( 1, 2 )"
  pre := "F2(1,2 ,NN,nn,N,n )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

TEXT TO VAR in
#command MYCOMMAND [<mylist,...>] [MYCLAUSE <myval>] =>
   MyFunction( {<mylist>} [, <myval>] )
ENDTEXT
  __pp_Process( pp, in )
  in := 'MYCOMMAND MYCLAUSE 321 "HELLO"'
  pre := 'MyFunction({"HELLO"} ,321  )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := 'MYCOMMAND MYCLAUSE 321 "HELLO","all"'
  pre := 'MyFunction({"HELLO","all"} ,321  )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := 'MYCOMMAND "HELLO","all" MYCLAUSE 321'
  pre := 'MyFunction({"HELLO","all"} ,321  )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

TEXT TO VAR in
#command MYCOMMAND2 [<mylist,...>] [MYCLAUSE <myval>] [ALL] =>
   MyFunction( {<mylist>} [, <myval>] )
ENDTEXT
  __pp_Process( pp, in )
  pre := 'MyFunction({"HELLO"} ,321  )'
  in := 'MYCOMMAND2 MYCLAUSE 321 "HELLO"'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )
  in := 'MYCOMMAND2 MYCLAUSE 321 "HELLO" ALL'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := 'MYCOMMAND2 ALL MYCLAUSE 321 "HELLO"'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := 'MYCOMMAND2 MYCLAUSE 321 "HELLO" ALL'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := 'MYCOMMAND2 MYCLAUSE 321 ALL "HELLO"'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )


TEXT TO VAR in
#command MYCOMMAND3 [<mylist,...>] [MYCLAUSE <myval>] [<all:ALL>] =>
   MyFunction( {<mylist>} [, <myval>] [,<.all.>] )
ENDTEXT
  __pp_Process( pp, in )
  in := 'MYCOMMAND3 ALL MYCLAUSE 321 "HELLO","WORLD"'
  pre := 'MyFunction({"HELLO","WORLD"} ,321  ,.T.  )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := 'MYCOMMAND3 MYCLAUSE 321 ALL "HELLO"'
  pre := 'MyFunction({"HELLO"} ,321  ,.T.  )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := 'MYCOMMAND3 MYCLAUSE 321 "HELLO" ALL'
  pre := 'MyFunction({"HELLO"} ,321  ,.T.  )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := 'MYCOMMAND3 MYCLAUSE 321 "HELLO"'
  pre := 'MyFunction({"HELLO"} ,321   )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

TEXT TO VAR in
#command MYCOMMAND2 [<myList,...>]
   [MYCLAUSE <myVal>] [MYOTHER <myOther>] => MyFunction( {<myList>}, <myVal>, <myOther> )
ENDTEXT
  __pp_Process( pp, in )

  /* Special restricted macro match marker (used in SET FILTER TO command */
  in := "SET FILTER TO &cVar."
  pre := "if ( Empty(cVar) ) ;    dbClearFilter() ; else ;    dbSetFilter({|| &cVar.},cVar) ; end"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "SET FILTER TO &(cVar .AND. &cVar)"
  pre := "if ( Empty((cVar .AND. &cVar)) ) ;    dbClearFilter() ; else ;    dbSetFilter({|| &(cVar .AND. &cVar)},(cVar .AND. &cVar)) ; end"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "SET FILTER TO &cVar. .AND. cVar"
  pre := 'dbSetFilter( {|| &cVar. .AND. cVar}, "&cVar. .AND. cVar" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

XTEXT TO VAR in
#xtranslate XTRANS(<x>( => normal( <(x)> )
#xtranslate XTRANS(<x:&>( => macro( <(x)> )
ENDTEXT
  PrePrepare( pp, in )

  in := "XTRANS( cVar ("
  pre := 'normal("cVar" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "XTRANS( &cVar ("
  pre := 'macro(cVar )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "XTRANS( &cVar+1 ("
  pre := 'normal("&cVar+1" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "XTRANS( &cVar. ("
  pre := 'macro(cVar )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "XTRANS( (&cVar.) ("
  pre := 'XTRANS( (&cVar.) ('
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "XTRANS( &(cVar) ("
  pre := 'macro((cVar) )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "XTRANS( &cVar[3] ("
  pre := 'normal("&cVar[3]" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "XTRANS( &cVar.  [3] ("
  pre := 'normal("&cVar.  [3]" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "XTRANS( &(cVar  [3],&cvar) ("
  pre := 'macro((cVar  [3],&cvar) )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "XTRANS( (&cVar.  [3],&cvar) ("
  pre := 'XTRANS( (&cVar.  [3],&cvar) ('
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "XTRANS( &cVar.1+5 ("
  pre := 'normal("&cVar.1+5" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "XTRANS( &cVar .AND. cVar ("
  pre := 'normal("&cVar .AND. cVar" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "XTRANS( &cVar. .AND. cVar ("
  pre := 'normal("&cVar. .AND. cVar" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

XTEXT TO VAR in
#xtranslate MXCALL <x:&>  => (<x>)
#xtranslate MYCALL <x:&> <y>  => <x>( <y>, 'mycall' )
#xtranslate MZCALL <x> <y>  => <x>( <y>, "mzcall" )
#command   FOO <x:&> FOO <y:&> => <(x)>+<(y)>
#translate BAR <x:&> BAR <y:&> => <(x)>+<(y)>
ENDTEXT
  PrePrepare( pp, in )

  in := "MYCALL &cVar ++cVar"
  pre := '&cVar(++cVar,"mycall" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MZCALL &cVar ++cVar"
  pre := '&cVar ++(cVar,"mzcall" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MYCALL &cVar+1 &cVar"
  pre := '&cVar(+1,"mycall" ) &cVar'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MZCALL &cVar+1 &cVar"
  pre := '&cVar+1(&cVar,"mzcall" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MXCALL &cVar"
  pre := '(&cVar)'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MXCALL &cVar."
  pre := '(&cVar.)'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MXCALL &cVar.1"
  pre := '(&cVar.1)'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MXCALL &cVar + 1"
  pre := '(&cVar) + 1'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MYCALL &cVar &cVar"
  pre := '&cVar(&cVar,"mycall" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MYCALL &cVar. &cVar."
  pre := '&cVar.(&cVar.,"mycall" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MYCALL &cVar.1 &cVar.1"
  pre := '&cVar.1(&cVar.1,"mycall" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MYCALL &cVar ++cVar"
  pre := '&cVar(++cVar,"mycall" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MYCALL &cVar. --cVar"
  pre := '&cVar.(--cVar,"mycall" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MYCALL &cVar.1 !cVar"
  pre := '&cVar.1(!cVar,"mycall" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MYCALL &cVar+1 &cVar"
  pre := '&cVar(+1,"mycall" ) &cVar'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MYCALL &cVar.+1 &cVar."
  pre := '&cVar.(+1,"mycall" ) &cVar.'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MYCALL &cVar.1+1 &cVar.1"
  pre := '&cVar.1(+1,"mycall" ) &cVar.1'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MYCALL &cVar +1 &cVar"
  pre := '&cVar(+1,"mycall" ) &cVar'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MYCALL &cVar. +1 &cVar."
  pre := '&cVar.(+1,"mycall" ) &cVar.'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MYCALL &cVar.1 +1 &cVar.1"
  pre := '&cVar.1(+1,"mycall" ) &cVar.1'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MYCALL &cVar +1"
  pre := '&cVar(+1,"mycall" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MYCALL &cVar. +1"
  pre := '&cVar.(+1,"mycall" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MYCALL &cVar.1 +1"
  pre := '&cVar.1(+1,"mycall" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "FOO &cVar FOO &var."
  pre := 'cVar+var'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "BAR &cVar BAR &var."
  pre := 'cVar+var'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "FOO &cVar FOO &var.+1"
  pre := 'FOO &cVar FOO &var.+1'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "BAR &cVar BAR &var.+1"
  pre := 'cVar+var+1'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MXCALL &cVar()"
  pre := '(&cVar)()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MXCALL &cVar++"
  pre := '(&cVar)++'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "(MXCALL &cVar)++"
  pre := '((&cVar))++'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MXCALL &cVar.()"
  pre := '(&cVar.)()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MXCALL &cVar.++"
  pre := '(&cVar.)++'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "(MXCALL &cVar.)++"
  pre := '((&cVar.))++'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MXCALL &cVar.1 ()"
  pre := '(&cVar.1) ()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MXCALL &cVar.1 ++"
  pre := '(&cVar.1) ++'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "(MXCALL &cVar.1) ++"
  pre := '((&cVar.1)) ++'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

XTEXT TO VAR in
#translate MTRANSLATE <x> => normal_t(<"x">)
#translate MTRANSLATE <x:&>  => macro_t(<(x)>)
#command MCOMMAND <x> => normal_c(<"x">)
#command MCOMMAND <x:&>  => macro_c(<(x)>)
ENDTEXT
  PrePrepare( pp, in )

  in := "MTRANSLATE &cVar"
  pre :='macro_t(cVar)'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE &cVar."
  pre :='macro_t(cVar)'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE &(cVar)"
  pre :='macro_t((cVar))'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE & (cVar)"
  pre :='macro_t((cVar))'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE &cVar&cVar"
  pre :='macro_t("&cVar&cVar")'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE &cVar+1"
  pre :='macro_t(cVar)+1'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE &cVar.+1"
  pre :='macro_t(cVar)+1'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE &cVar. .AND.  .T."
  pre :='macro_t(cVar) .AND.  .T.'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE &cVar++"
  pre :='macro_t(cVar)++'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE &cVar.++"
  pre :='macro_t(cVar)++'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE &cVar+=1"
  pre :='macro_t(cVar)+=1'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE &cVar.-=2"
  pre :='macro_t(cVar)-=2'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE &cVar*=1"
  pre :='macro_t(cVar)*=1'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE &cVar/=2"
  pre :='macro_t(cVar)/=2'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE &cVar%=1"
  pre :='macro_t(cVar)%=1'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE &cVar^=2"
  pre :='macro_t(cVar)^=2'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE &cVar:=1"
  pre :='macro_t(cVar):=1'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE &cVar. .AND.  .T."
  pre :='macro_t(cVar) .AND.  .T.'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE &cVar .AND.  .T."
  pre :='macro_t(cVar) .AND.  .T.'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE &(cVar) +1"
  pre :='macro_t((cVar)) +1'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE & (cVar) +1"
  pre :='macro_t((cVar)) +1'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE &cVar.&cVar."
  pre :='macro_t("&cVar.&cVar.")'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE &cVar.&cVar.&cVar&cVar"
  pre :='macro_t("&cVar.&cVar.&cVar&cVar")'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MTRANSLATE &cVar.&(cVar)"
  pre :='macro_t("&cVar.&")(cVar)'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )


   /* command */
  in := "MCOMMAND &cVar"
  pre :='macro_c(cVar)'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND &cVar."
  pre :='macro_c(cVar)'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND &(cVar)"
  pre :='macro_c((cVar))'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND & (cVar)"
  pre :='macro_c((cVar))'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND &cVar&cVar"
  pre :='macro_c("&cVar&cVar")'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND &cVar+1"
  pre :='normal_c("&cVar+1")'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND &cVar.+1"
  pre :='normal_c("&cVar.+1")'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND &cVar. .AND.  .T."
  pre :='normal_c("&cVar. .AND.  .T.")'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND &cVar++"
  pre :='normal_c("&cVar++")'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND &cVar.++"
  pre :='normal_c("&cVar.++")'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND &cVar+=1"
  pre :='normal_c("&cVar+=1")'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND &cVar.-=2"
  pre :='normal_c("&cVar.-=2")'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND &cVar*=1"
  pre :='normal_c("&cVar*=1")'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND &cVar/=2"
  pre :='normal_c("&cVar/=2")'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND &cVar%=12"
  pre :='normal_c("&cVar%=12")'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND &cVar^=2"
  pre :='normal_c("&cVar^=2")'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND &cVar:=1"
  pre :='normal_c("&cVar:=1")'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND &cVar. .AND.  .T."
  pre :='normal_c("&cVar. .AND.  .T.")'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND &cVar .AND.  .T."
  pre :='normal_c("&cVar .AND.  .T.")'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND &(cVar) +1"
  pre :='normal_c((cVar) +1)'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND & (cVar) +1"
  pre :='normal_c( (cVar) +1)'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND &cVar.&cVar."
  pre :='macro_c("&cVar.&cVar.")'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND &cVar.&cVar.&cVar&cVar2"
  pre :='macro_c("&cVar.&cVar.&cVar&cVar2")'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "MCOMMAND &cVar.&(cVar)"
  pre :='normal_c("&cVar.&(cVar)")'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

   /* repeated optional clauses */
TEXT TO VAR in
#xcommand SET <var1> [, <varN>] WITH <val> =>
<var1>:=<val> [; <varN>:=<val>]
ENDTEXT
  __pp_Process( pp, in )
  in := "SET v1 WITH 0"
  pre := "v1:=0 "
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "SET v1, v2 WITH 0"
  pre := "v1:=0 ; v2:=0 "
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "SET v1, v2, v3 WITH 0"
  pre := "v1:=0 ; v2:=0 ; v3:=0 "
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "SET v1, v2, v3, v4 WITH 0"
  pre := "v1:=0 ; v2:=0 ; v3:=0 ; v4:=0 "
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

TEXT TO VAR in
#command AVG <x1> [, <xn>] TO <v1> [, <vn>]  =>
   AVERAGE( {||<v1>:=<v1>+<x1>} [, {||<vn>:=<vn>+<xn>} ] )
ENDTEXT
  __pp_Process( pp, in )
  in := "AVG f1 TO s1"
  pre := "AVERAGE({||s1:=s1+f1}  )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "AVG f1, f2 TO s1, s2"
  pre := "AVERAGE({||s1:=s1+f1} ,{||s2:=s2+f2}   )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := "AVG f1, f2, f3 TO s1, s2, s3"
  pre := "AVERAGE({||s1:=s1+f1} ,{||s2:=s2+f2}  ,{||s3:=s3+f3}   )"
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

/* ---------------------------------------------------------------------*/
  in := "COPY STRUCTURE EXTENDED TO teststru"
  pre := '__dbCopyXStruct( "teststru" )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

/* ---------------------------------------------------------------------*/
TEXT TO VAR in
#command @ <row>, <col> GET <var>
                        [PICTURE <pic>]
                        [VALID <valid>]
                        [WHEN <when>]
                        [CAPTION <caption>]
                        [MESSAGE <message>]
                        [SEND <msg>]

      => SetPos( <row>, <col> )
       ; AAdd( GetList,
              _GET_( <var>, <"var">, <pic>, <{valid}>, <{when}> ) )
      [; ATail(GetList):Caption := <caption>]
      [; ATail(GetList):CapRow  := ATail(Getlist):row
       ; ATail(GetList):CapCol  := ATail(Getlist):col -
                              __CapLength(<caption>) - 1]
      [; ATail(GetList):message := <message>]
      [; ATail(GetList):<msg>]
       ; ATail(GetList):Display()
ENDTEXT
  __pp_Process( pp, in )

  in := '@ 0,1 GET a'
  pre := 'SetPos(0,1 ) ; AAdd(GetList,_GET_(a,"a",,, ) )     ; ATail(GetList):Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := '@ 0,2 GET a PICTURE "X"'
  pre := 'SetPos(0,2 ) ; AAdd(GetList,_GET_(a,"a","X",, ) )     ; ATail(GetList):Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := '@ 0,3 GET a PICTURE "X" VALID .T.'
  pre := 'SetPos(0,3 ) ; AAdd(GetList,_GET_(a,"a","X",{|| .T.}, ) )     ; ATail(GetList):Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := '@ 0,4 GET a PICTURE "X" VALID .T. WHEN .T.'
  pre := 'SetPos(0,4 ) ; AAdd(GetList,_GET_(a,"a","X",{|| .T.},{|| .T.} ) )     ; ATail(GetList):Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := '@ 0,5 GET a PICTURE "X" VALID .T. WHEN .T. CAPTION "myget"'
  pre := 'SetPos(0,5 ) ; AAdd(GetList,_GET_(a,"a","X",{|| .T.},{|| .T.} ) ) ; ATail(GetList):Caption := "myget"  ; ATail(GetList):CapRow := ATail(Getlist):row ; ATail(GetList):CapCol := ATail(Getlist):col - __CapLength("myget") - 1    ; ATail(GetList):Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := '@ 0,6 GET a PICTURE "X" VALID .T. WHEN .T. CAPTION "myget" MESSAGE "mymess"'
  pre := 'SetPos(0,6 ) ; AAdd(GetList,_GET_(a,"a","X",{|| .T.},{|| .T.} ) ) ; ATail(GetList):Caption := "myget"  ; ATail(GetList):CapRow := ATail(Getlist):row ; ATail(GetList):CapCol := ATail(Getlist):col - __CapLength("myget") - 1  ; ATail(GetList):message := "mymess"   ; ATail(GetList):Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in := '@ 0,7 GET a PICTURE "X" VALID .T. WHEN .T. CAPTION "myget" MESSAGE "mymess" SEND send()'
  pre := 'SetPos(0,7 ) ; AAdd(GetList,_GET_(a,"a","X",{|| .T.},{|| .T.} ) ) ; ATail(GetList):Caption := "myget"  ; ATail(GetList):CapRow := ATail(Getlist):row ; ATail(GetList):CapCol := ATail(Getlist):col - __CapLength("myget") - 1  ; ATail(GetList):message := "mymess"  ; ATail(GetList):send()  ; ATail(GetList):Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

/* ---------------------------------------------------------------------*/
  in :='@ 1,1 GET a RANGE 0,100'
  pre := 'SetPos(1,1 ) ; AAdd(GetList,_GET_(a,"a",,{|_1| RangeCheck(_1,, 0, 100)}, ) )     ; ATail(GetList):Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 1,2 GET a PICTURE "X" RANGE 0,100'
  pre := 'SetPos(1,2 ) ; AAdd(GetList,_GET_(a,"a","X",{|_1| RangeCheck(_1,, 0, 100)}, ) )     ; ATail(GetList):Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

   /* NOTE: Clipper fails here */
  in :='@ 1,3 GET a PICTURE "X" VALID .T. RANGE 0,100'
  pre := 'SetPos(1,3 ) ; AAdd(GetList,_GET_(a,"a","X",{|| .T.}, ) )     ; ATail(GetList):Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 1,4 GET a PICTURE "X" WHEN .T. RANGE 0,100'
  pre := 'SetPos(1,4 ) ; AAdd(GetList,_GET_(a,"a","X",{|_1| RangeCheck(_1,, 0, 100)},{|| .T.} ) )     ; ATail(GetList):Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 1,5 GET a PICTURE "X" WHEN .T. CAPTION "myget" RANGE 0,100'
  pre := 'SetPos(1,5 ) ; AAdd(GetList,_GET_(a,"a","X",{|_1| RangeCheck(_1,, 0, 100)},{|| .T.} ) ) ; ATail(GetList):Caption := "myget"  ; ATail(GetList):CapRow := ATail(Getlist):row ; ATail(GetList):CapCol := ATail(Getlist):col - __CapLength("myget") - 1    ; ATail(GetList):Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 1,6 GET a PICTURE "X" WHEN .T. CAPTION "myget" MESSAGE "mymess" RANGE 0,100'
  pre := 'SetPos(1,6 ) ; AAdd(GetList,_GET_(a,"a","X",{|_1| RangeCheck(_1,, 0, 100)},{|| .T.} ) ) ; ATail(GetList):Caption := "myget"  ; ATail(GetList):CapRow := ATail(Getlist):row ; ATail(GetList):CapCol := ATail(Getlist):col - __CapLength("myget") - 1  ; ATail(GetList):message := "mymess"   ; ATail(GetList):Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 1,7 GET a PICTURE "X" WHEN .T. CAPTION "myget" MESSAGE "mymess" SEND send() RANGE 0,100'
  pre := 'SetPos(1,7 ) ; AAdd(GetList,_GET_(a,"a","X",{|_1| RangeCheck(_1,, 0, 100)},{|| .T.} ) ) ; ATail(GetList):Caption := "myget"  ; ATail(GetList):CapRow := ATail(Getlist):row ; ATail(GetList):CapCol := ATail(Getlist):col - __CapLength("myget") - 1  ; ATail(GetList):message := "mymess"  ; ATail(GetList):send()  ; ATail(GetList):Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 2,1 GET a'
  pre := 'SetPos(2,1 ) ; AAdd(GetList,_GET_(a,"a",,, ) )     ; ATail(GetList):Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 2,2 GET a RANGE 0,100 PICTURE "X"'
  pre := 'SetPos(2,2 ) ; AAdd(GetList,_GET_(a,"a","X",{|_1| RangeCheck(_1,, 0, 100)}, ) )     ; ATail(GetList):Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 2,3 GET a PICTURE "X" RANGE 0,100'
  pre := 'SetPos(2,3 ) ; AAdd(GetList,_GET_(a,"a","X",{|_1| RangeCheck(_1,, 0, 100)}, ) )     ; ATail(GetList):Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 2,4 GET a PICTURE "X" RANGE 0,100 WHEN .T.'
  pre := 'SetPos(2,4 ) ; AAdd(GetList,_GET_(a,"a","X",{|_1| RangeCheck(_1,, 0, 100)},{|| .T.} ) )     ; ATail(GetList):Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 2,5 GET a PICTURE "X" RANGE 0,100 WHEN .T. CAPTION "myget"'
  pre := 'SetPos(2,5 ) ; AAdd(GetList,_GET_(a,"a","X",{|_1| RangeCheck(_1,, 0, 100)},{|| .T.} ) ) ; ATail(GetList):Caption := "myget"  ; ATail(GetList):CapRow := ATail(Getlist):row ; ATail(GetList):CapCol := ATail(Getlist):col - __CapLength("myget") - 1    ; ATail(GetList):Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 2,6 GET a PICTURE "X" RANGE 0,100 WHEN .T. CAPTION "myget" MESSAGE "mymess"'
  pre := 'SetPos(2,6 ) ; AAdd(GetList,_GET_(a,"a","X",{|_1| RangeCheck(_1,, 0, 100)},{|| .T.} ) ) ; ATail(GetList):Caption := "myget"  ; ATail(GetList):CapRow := ATail(Getlist):row ; ATail(GetList):CapCol := ATail(Getlist):col - __CapLength("myget") - 1  ; ATail(GetList):message := "mymess"   ; ATail(GetList):Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 2,7 GET a PICTURE "X" RANGE 0,100 WHEN .T. CAPTION "myget" MESSAGE "mymess" SEND send()'
  pre := 'SetPos(2,7 ) ; AAdd(GetList,_GET_(a,"a","X",{|_1| RangeCheck(_1,, 0, 100)},{|| .T.} ) ) ; ATail(GetList):Caption := "myget"  ; ATail(GetList):CapRow := ATail(Getlist):row ; ATail(GetList):CapCol := ATail(Getlist):col - __CapLength("myget") - 1  ; ATail(GetList):message := "mymess"  ; ATail(GetList):send()  ; ATail(GetList):Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )


/* ---------------------------------------------------------------------*/
TEXT TO VAR in
#command @ <row>, <col> GET <var>
                        PUSHBUTTON
                        [VALID <valid>]
                        [WHEN <when>]
                        [CAPTION <caption>]
                        [MESSAGE <message>]
                        [COLOR <color>]
                        [FOCUS <fblock>]
                        [STATE <sblock>]
                        [STYLE <style>]
                        [SEND <msg>]
                        [GUISEND <guimsg>]
                        [SIZE X <sizex> Y <sizey>]
                        [CAPOFF X <capxoff> Y <capyoff>]
                        [BITMAP <bitmap>]
                        [BMPOFF X <bmpxoff> Y <bmpyoff>]

      => SetPos( <row>, <col> )
       ; AAdd( GetList,
              _GET_( <var>, <(var)>, NIL, <{valid}>, <{when}> ) )
       ; ATail(GetList):Control := _PUSHBUTT_( <caption>, <message>,
                       <color>, <{fblock}>, <{sblock}>, <style>,
                       <sizex>, <sizey>, <capxoff>, <capyoff>,
                       <bitmap>, <bmpxoff>, <bmpyoff> )
       ; ATail(GetList):reader  := { | a, b, c, d |
                                    GUIReader( a, b, c, d ) }
      [; ATail(GetList):<msg>]
      [; ATail(GetList):Control:<guimsg>]
       ; ATail(GetList):Control:Display()
ENDTEXT
  __pp_Process( pp, in )

  in :='@ 4,1 GET a PUSHBUTTON'
  pre := 'SetPos(4,1 ) ; AAdd(GetList,_GET_(a,"a",NIL,, ) ) ; ATail(GetList):Control := _PUSHBUTT_(,,,,,,,,,,,, ) ; ATail(GetList):reader := { | a,b,c,d | GUIReader(a,b,c,d ) }   ; ATail(GetList):Control:Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 4,1 GET a PUSHBUTTON VALID valid()'
  pre := 'SetPos(4,1 ) ; AAdd(GetList,_GET_(a,"a",NIL,{|| valid()}, ) ) ; ATail(GetList):Control := _PUSHBUTT_(,,,,,,,,,,,, ) ; ATail(GetList):reader := { | a,b,c,d | GUIReader(a,b,c,d ) }   ; ATail(GetList):Control:Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 4,1 GET a PUSHBUTTON VALID valid() WHEN when()'
  pre := 'SetPos(4,1 ) ; AAdd(GetList,_GET_(a,"a",NIL,{|| valid()},{|| when()} ) ) ; ATail(GetList):Control := _PUSHBUTT_(,,,,,,,,,,,, ) ; ATail(GetList):reader := { | a,b,c,d | GUIReader(a,b,c,d ) }   ; ATail(GetList):Control:Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap"'
  pre := 'SetPos(4,1 ) ; AAdd(GetList,_GET_(a,"a",NIL,{|| valid()},{|| when()} ) ) ; ATail(GetList):Control := _PUSHBUTT_("cap",,,,,,,,,,,, ) ; ATail(GetList):reader := { | a,b,c,d | GUIReader(a,b,c,d ) }   ; ATail(GetList):Control:Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes"'
  pre := 'SetPos(4,1 ) ; AAdd(GetList,_GET_(a,"a",NIL,{|| valid()},{|| when()} ) ) ; ATail(GetList):Control := _PUSHBUTT_("cap","mes",,,,,,,,,,, ) ; ATail(GetList):reader := { | a,b,c,d | GUIReader(a,b,c,d ) }   ; ATail(GetList):Control:Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes" COLOR color()'
  pre := 'SetPos(4,1 ) ; AAdd(GetList,_GET_(a,"a",NIL,{|| valid()},{|| when()} ) ) ; ATail(GetList):Control := _PUSHBUTT_("cap","mes",color(),,,,,,,,,, ) ; ATail(GetList):reader := { | a,b,c,d | GUIReader(a,b,c,d ) }   ; ATail(GetList):Control:Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes" COLOR color() FOCUS focus()'
  pre := 'SetPos(4,1 ) ; AAdd(GetList,_GET_(a,"a",NIL,{|| valid()},{|| when()} ) ) ; ATail(GetList):Control := _PUSHBUTT_("cap","mes",color(),{|| focus()},,,,,,,,, ) ; ATail(GetList):reader := { | a,b,c,d | GUIReader(a,b,c,d ) }   ; ATail(GetList):Control:Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes" COLOR color() FOCUS focus() STATE state()'
  pre := 'SetPos(4,1 ) ; AAdd(GetList,_GET_(a,"a",NIL,{|| valid()},{|| when()} ) ) ; ATail(GetList):Control := _PUSHBUTT_("cap","mes",color(),{|| focus()},{|| state()},,,,,,,, ) ; ATail(GetList):reader := { | a,b,c,d | GUIReader(a,b,c,d ) }   ; ATail(GetList):Control:Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes" COLOR color() FOCUS focus() STATE state() STYLE style()'
  pre := 'SetPos(4,1 ) ; AAdd(GetList,_GET_(a,"a",NIL,{|| valid()},{|| when()} ) ) ; ATail(GetList):Control := _PUSHBUTT_("cap","mes",color(),{|| focus()},{|| state()},style(),,,,,,, ) ; ATail(GetList):reader := { | a,b,c,d | GUIReader(a,b,c,d ) }   ; ATail(GetList):Control:Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes" COLOR color() FOCUS focus() STATE state() STYLE style() SEND send()'
  pre := 'SetPos(4,1 ) ; AAdd(GetList,_GET_(a,"a",NIL,{|| valid()},{|| when()} ) ) ; ATail(GetList):Control := _PUSHBUTT_("cap","mes",color(),{|| focus()},{|| state()},style(),,,,,,, ) ; ATail(GetList):reader := { | a,b,c,d | GUIReader(a,b,c,d ) } ; ATail(GetList):send()   ; ATail(GetList):Control:Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes" COLOR color() FOCUS focus() STATE state() STYLE style() SEND send() GUISEND guisend()'
  pre := 'SetPos(4,1 ) ; AAdd(GetList,_GET_(a,"a",NIL,{|| valid()},{|| when()} ) ) ; ATail(GetList):Control := _PUSHBUTT_("cap","mes",color(),{|| focus()},{|| state()},style(),,,,,,, ) ; ATail(GetList):reader := { | a,b,c,d | GUIReader(a,b,c,d ) } ; ATail(GetList):send()  ; ATail(GetList):Control:guisend()  ; ATail(GetList):Control:Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes" COLOR color() FOCUS focus() STATE state() STYLE style() SEND send() GUISEND guisend() SIZE X 100 Y 100'
  pre := 'SetPos(4,1 ) ; AAdd(GetList,_GET_(a,"a",NIL,{|| valid()},{|| when()} ) ) ; ATail(GetList):Control := _PUSHBUTT_("cap","mes",color(),{|| focus()},{|| state()},style(),100,100,,,,, ) ; ATail(GetList):reader := { | a,b,c,d | GUIReader(a,b,c,d ) } ; ATail(GetList):send()  ; ATail(GetList):Control:guisend()  ; ATail(GetList):Control:Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes" COLOR color() FOCUS focus() STATE state() STYLE style() SEND send() GUISEND guisend() SIZE X 100 Y 100 CAPOFF X 10 Y 10'
  pre := 'SetPos(4,1 ) ; AAdd(GetList,_GET_(a,"a",NIL,{|| valid()},{|| when()} ) ) ; ATail(GetList):Control := _PUSHBUTT_("cap","mes",color(),{|| focus()},{|| state()},style(),100,100,10,10,,, ) ; ATail(GetList):reader := { | a,b,c,d | GUIReader(a,b,c,d ) } ; ATail(GetList):send()  ; ATail(GetList):Control:guisend()  ; ATail(GetList):Control:Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes" COLOR color() FOCUS focus() STATE state() STYLE style() SEND send() GUISEND guisend() SIZE X 100 Y 100 CAPOFF X 10 Y 10 BITMAP bitmap()'
  pre := 'SetPos(4,1 ) ; AAdd(GetList,_GET_(a,"a",NIL,{|| valid()},{|| when()} ) ) ; ATail(GetList):Control := _PUSHBUTT_("cap","mes",color(),{|| focus()},{|| state()},style(),100,100,10,10,bitmap(),, ) ; ATail(GetList):reader := { | a,b,c,d | GUIReader(a,b,c,d ) } ; ATail(GetList):send()  ; ATail(GetList):Control:guisend()  ; ATail(GetList):Control:Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes" COLOR color() FOCUS focus() STATE state() STYLE style() SEND send() GUISEND guisend() SIZE X 100 Y 100 CAPOFF X 10 Y 10 BITMAP bitmap() BMPOFF X 2 Y 2'
  pre := 'SetPos(4,1 ) ; AAdd(GetList,_GET_(a,"a",NIL,{|| valid()},{|| when()} ) ) ; ATail(GetList):Control := _PUSHBUTT_("cap","mes",color(),{|| focus()},{|| state()},style(),100,100,10,10,bitmap(),2,2 ) ; ATail(GetList):reader := { | a,b,c,d | GUIReader(a,b,c,d ) } ; ATail(GetList):send()  ; ATail(GetList):Control:guisend()  ; ATail(GetList):Control:Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 4,1 GET a PUSHBUTTON COLOR "W/N"'
  pre := 'SetPos(4,1 ) ; AAdd(GetList,_GET_(a,"a",NIL,, ) ) ; ATail(GetList):Control := _PUSHBUTT_(,,"W/N",,,,,,,,,, ) ; ATail(GetList):reader := { | a,b,c,d | GUIReader(a,b,c,d ) }   ; ATail(GetList):Control:Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 4,1 GET a PUSHBUTTON COLOR "W/N" SIZE X 100 Y 100 BMPOFF X 2 Y 2 VALID valid() GUISEND guisend() WHEN when() MESSAGE "mes"'
  pre := 'SetPos(4,1 ) ; AAdd(GetList,_GET_(a,"a",NIL,{|| valid()},{|| when()} ) ) ; ATail(GetList):Control := _PUSHBUTT_(,"mes","W/N",,,,100,100,,,,2,2 ) ; ATail(GetList):reader := { | a,b,c,d | GUIReader(a,b,c,d ) }  ; ATail(GetList):Control:guisend()  ; ATail(GetList):Control:Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 4,1 GET a PUSHBUTTON SIZE X 100 Y 100 BMPOFF X 2 Y 2 VALID valid() GUISEND guisend() WHEN when() MESSAGE "mes" COLOR "W/N"'
  pre := 'SetPos(4,1 ) ; AAdd(GetList,_GET_(a,"a",NIL,{|| valid()},{|| when()} ) ) ; ATail(GetList):Control := _PUSHBUTT_(,"mes","W/N",,,,100,100,,,,2,2 ) ; ATail(GetList):reader := { | a,b,c,d | GUIReader(a,b,c,d ) }  ; ATail(GetList):Control:guisend()  ; ATail(GetList):Control:Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

  in :='@ 4,1 GET a PUSHBUTTON SIZE X 100 Y 100 BMPOFF X 2 Y 2 VALID valid() GUISEND guisend() WHEN when() MESSAGE "mes" COLOR "W/N" CAPOFF X 10 Y 10 FOCUS focus() STATE state() STYLE style() SEND send() BITMAP bitmap() CAPTION "cap"'
  pre := 'SetPos(4,1 ) ; AAdd(GetList,_GET_(a,"a",NIL,{|| valid()},{|| when()} ) ) ; ATail(GetList):Control := _PUSHBUTT_("cap","mes","W/N",{|| focus()},{|| state()},style(),100,100,10,10,bitmap(),2,2 ) ; ATail(GetList):reader := { | a,b,c,d | GUIReader(a,b,c,d ) } ; ATail(GetList):send()  ; ATail(GetList):Control:guisend()  ; ATail(GetList):Control:Display()'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )

TEXT TO VAR in
#command DEFINE CLIPBOARD <oClp>
   [ FORMAT <format:TEXT,OEMTEXT,BITMAP,DIF> ]
   [ OF <oWnd> ]
   =>
   <oClp> := TClipboard():New( [Upper(<(format)>)], <oWnd> )
ENDTEXT
  __pp_Process( pp, in )
  in:= "DEFINE CLIPBOARD oC OF oD FORMAT TEXT"
  pre :='oC := TClipboard():New(Upper("TEXT") ,oD )'
  nRes += PreResult( pre, PreRun( pp, in, pre ), @nCnt )



/* ---------------------------------------------------------------------*/

  OutStd( "Total count   =", nCnt, hb_eol() )
  OutStd( "Valid results =", nRes, hb_eol() )
  OutStd( "Failed results=", nCnt - nRes, hb_eol() )

  RETURN

PROCEDURE PrePrepare( pp, in )

   LOCAL len, i, cin

   len := MLCount( in )
   FOR i := 1 TO len
      cin := AllTrim( MemoLine( in, 192, i ) )
      __pp_Process( pp, cin )
   NEXT

   RETURN

FUNCTION PreRun( pp, in, pre )

   LOCAL len, i, cin
   LOCAL out := ''

#ifdef __HARBOUR__
   HB_SYMBOL_UNUSED( pre )
#endif

   len := MLCount( in, 1024 )
   FOR i := 1 TO len
      cin := AllTrim( MemoLine( in, 1024, i ) )
      out += __pp_Process( pp, cin )
   NEXT

   RETURN out

FUNCTION PreResult( pre, out, pCnt )

   LOCAL i

   pCnt++
   pre := StrTran( pre, " " )
   out := StrTran( out, " " )
   IF pre == out
      RETURN 1
   ELSE
      OutStd( pre, hb_eol() )
      OutStd( out, hb_eol() )
      OutStd( " => FAILED in LINE:", ProcLine( 1 ), hb_eol() )
      i := 1
      DO WHILE SubStr( pre, i, 1 ) == SubStr( out, i, 1 )
         i++
      ENDDO
      OutStd( SubStr( pre, i ), hb_eol() )
      OutStd( SubStr( out, i ), hb_eol() )
   ENDIF

   RETURN 0
