/*
 * Test file to check the preprocessor.
 *
 * Usage:
 * 1) Execute compare.hb script for a comparison between Harbour and CA-Cl*pper:
 *       'hbrun compare'
 * 2) Results will be stored in pp_test.dif
 *
 * Notes:
 * - clipper.exe (5.x) and diff executable need to be in the path.
 * - requires 32-bit Windows OS
 *
 * Ignore most of errors reported during compilation.
 *
 */

PROCEDURE Main()

#ifdef __HARBOUR__
#xtranslate CCC <v> => QOUT( <v>[2] [, <v>[<v>]\[3\]] )
CCC b
#endif

#xtranslate CCCC <v> => QOUT(  <v><v>\[3\] )
CCCC c

#xtranslate AAA [A <a> [B <b>] ] => Qout([<a>][, <b>])
  AAA
  AAA A a
  AAA A a B b

#define RED {255,0,0}
#ifdef __HARBOUR__
/* ---------------------------------------------------------------------*/
/* obligatory clause on left side used as optional clause on right side
 * Clipper doesn't allow for it
 */
#xcommand SET COOLTIP TO <color> OF <form> => ;
   SM( TTH (<"form">), 1, RGB(<color>\[1\], ;
   <color>\[2\], ;
   <color>[, <color>[ 3 ] ]), 0)

   SET COOLTIP TO RED OF form1
#endif

#xcommand SET TOOLTIP TO <color> OF <form> => ;
   SM( TTH (<"form">), 1, RGB(<color>\[1\], ;
   <color>\[2\], ;
   <color>\[ 3 \]), 0)

   SET TOOLTIP TO RED OF form1

/* ---------------------------------------------------------------------*/
/* simple optional clause */
#command ZZZ [<v>] => QOUT( [ <v>\[1\] ] )
   ZZZ a
   ZZZ
   ZZZ a[1]+2
   ZZZ 1
   ZZZ 'z'
   ZZZ "z"

#translate TZZZ [<v>] => QOUT( [ <v>\[1\] ] )
   TZZZ a
   TZZZ
   TZZZ a[1]+2
   TZZZ 1
   TZZZ 'z'
   TZZZ "z"

/* ---------------------------------------------------------------------*/
#xtranslate _HMG_a  =>  _HMG\[137\]
   ? _bro[ a( _HMG_a [i] ) ]

/* ---------------------------------------------------------------------*/
#define clas( x )   (x)
#ifdef __HARBOUR__
/* <!name!> is harbour extension match marker used to allow identifiers
 * only
 */
#xtranslate ( <!name!>{ [<p,...>] } => (<name>():New(<p>)
   a :=clas( TesT{ 1,2,3} )
   a :=clas( a+3{ 11,2,3} )
   a :=clas( a(){ 11,2,3} )
#endif

#xtranslate ( <name>{ [<p,...>] } => (<name>():New(<p>)
   a :=clas( TEST{ 1,2,3} )
   a :=clas( a+3{ 11,2,3} )
   a :=clas( a(){ 11,2,3} )

/* ---------------------------------------------------------------------*/
#define DATENEW   1
#define DATEOLD(x)   x
#define datediff(x,y) ( DATEOLD(x) - DATENEW )
#command datediff1(<x>,<y>) => ( DATEOLD(<x>) - DATENEW )
  ? datediff( x, y )

/* ---------------------------------------------------------------------*/
/* test for regular match marker */
#command _REGULAR_(<z>) => rm( <z> )
  _REGULAR_(a)
  _REGULAR_("a")
  _REGULAR_('a')
  _REGULAR_(["'a'"])
  _REGULAR_(&a.1)
  _REGULAR_(&a)
  _REGULAR_(&a.)
  _REGULAR_(&(a))
  _REGULAR_(&a[1])
  _REGULAR_(a[1])
  _REGULAR_("['']")

      //NORMAL
/* test for normal match marker */
#command _NORMAL_M(<z>) => nm( <"z"> )
  _NORMAL_M(a)
  _NORMAL_M("a")
  _NORMAL_M('a')
  _NORMAL_M(["'a'"])
  _NORMAL_M(&a.1)
  _NORMAL_M(&a)
  _NORMAL_M(&a.)
  _NORMAL_M(&(a))
  _NORMAL_M(&a[1])
  _NORMAL_M(a[1])
  _NORMAL_M("['']")

      //SMART
/* test for smart match marker */
#command _SMART_M(<z>) => sm( <(z)> )
  _SMART_M(a)
  _SMART_M("a")
  _SMART_M('a')
  _SMART_M(["'a'"])
  _SMART_M(&a.1)
  _SMART_M(&a)
  _SMART_M(&a.)
  _SMART_M(&(a))
  _SMART_M(&a[1])
  _SMART_M(a[1])
  _SMART_M("['']")

      //DUMB
/* test for dumb match marker */
#command _DUMB_M(<z>) => dm( #<z> )
  _DUMB_M(a)
  _DUMB_M("a")
  _DUMB_M('a')
  _DUMB_M(["'a'"])
  _DUMB_M(&a.1)
  _DUMB_M(&a)
  _DUMB_M(&a.)
  _DUMB_M(&(a))
  _DUMB_M(&a[1])
  _DUMB_M(a[1])
  _DUMB_M("['']")

/* ---------------------------------------------------------------------*/
  // REGULAR list
/* test for regular match marker */
#command _REGULAR_L(<z,...>) => rl( <z> )
_REGULAR_L(a,"a",'a',["'a'"],"['a']",'["a"]',&a.1,&a,&a.,&a.  ,&(a),&a[1],&a.[1],&a.  [2],&a&a, &a.a,  a, a)

  // NORMAL list
/* test for normal match marker */
#command _NORMAL_L(<z,...>) => nl( <"z"> )
_NORMAL_L(n,"n",'a',["'a'"],"['a']",'["a"]',&a.1,&a,&a.,&a.  ,&(a),&a[1],&a.[1],&a.  [2],&a&a, &.a, &a.a,  a, a)

  // SMART list
/* test for smart match marker */
#command _SMART_L(<z,...>) => sl( <(z)> )
_SMART_L(a,"a",'a',["'a'"],"['a']",'["a"]',&a.1,&a,&a.,&a.  ,&(a),&a[1],&a.[1],&a.  [2],&a&a, &.a, &a.a,  a, a)

  // DUMB list
/* test for dumb match marker */
#command _DUMB_L(<z,...>) => dl( #<z> )
_DUMB_L(a,"a",'a',["'a'"],"['a']",'["a"]',&a.1,&a,&a.,&a.  ,&(a),&a[1],&a.[1],&a.  [2],&a&a, &.a, &a.a,  a, a)

/* ---------------------------------------------------------------------*/
/* test of preserving spaces in expressions */
/* Notice that Clipper and Harbour doesn't remove spaces from
 * expressions passed for stingify
*/
 index on LEFT(   f1  ,  10   )      to _tst

/* ---------------------------------------------------------------------*/
/* repeated optinal clauses */
#xcommand INSERT INTO <table> ( <uField1> [, <uFieldN> ] ) VALUES ( <uVal1> [, <uValN> ] ) => ;
if <table>->( dbappend() ) ;;
 replace <table>-><uField1> with <uVal1> ;;
 [ replace <table>-><uFieldN> with <uValN> ; ] ;
 <table>->( dbunlock() ) ;;
endif
   insert into test ( FIRST, LAST, STREET ) values ( "first", "last", "street" )

#xcommand INSERT2 INTO <table> ( <uField1> [, <uFieldN> ] ) VALUES ( <uVal1> [, <uValN> ] ) => ;
if <table>->( dbappend() ) ;;
 <table>-><uField1> := <uVal1> ;;
 [ <table>-><uFieldN> := <uValN> ; ] ;
 <table>->( dbunlock() ) ;;
endif
   insert2 into test ( FIRST, LAST, STREET ) ;
   values ( "first", "last", "street" )

/* ---------------------------------------------------------------------*/
/* test for case sensitivity in define */
#define F1( n ) F2( n, N )
  F1( 1 )

#define F3( nN, Nn ) F2( nN, Nn, NN, nn, N, n )
  F3( 1, 2 )

/* ---------------------------------------------------------------------*/
/* test for different order of optional clauses */
#command MYCOMMAND [<mylist,...>] [MYCLAUSE <myval>] => ;
   MyFunction( {<mylist>} [, <myval>] )
  MYCOMMAND MYCLAUSE 321 "HELLO"
  MYCOMMAND MYCLAUSE 321 "HELLO","all"
  MYCOMMAND "HELLO","all" MYCLAUSE 321

#command MYCOMMAND2 [<mylist,...>] [MYCLAUSE <myval>] [ALL] => ;
   MyFunction2( {<mylist>} [, <myval>] )
  MYCOMMAND2 MYCLAUSE 321 "HELLO"
  MYCOMMAND2 MYCLAUSE 321 "HELLO" ALL
  MYCOMMAND2 ALL MYCLAUSE 321 "HELLO"
  MYCOMMAND2 MYCLAUSE 321 "HELLO" ALL
  MYCOMMAND2 MYCLAUSE 321 ALL "HELLO"

#xcommand MYCOMMAND3 [<myList,...>] ;
   [MYCLAUSE <myVal>] [MYOTHER <myOther>] => MyFunction3( {<myList>}, <myVal>, <myOther> )

   MYCOMMAND3 MYCLAUSE 322 "Hello" MYOTHER 1
   MYCOMMAND3 MYOTHER 1 MYCLAUSE 322 "Hello"
   MYCOMMAND3 "Hello" MYOTHER 1 MYCLAUSE 322
   MYCOMMAND3 MYOTHER 1 "Hello" MYCLAUSE 322

/* ---------------------------------------------------------------------*/
  /* Special restricted macro match marker (used in SET FILTER TO command */
  /* <x:&> is matched when &variable or &(expression) are used
  */
  SET FILTER TO &cVar.
  SET FILTER TO &(cVar .AND. &cVar)
  SET FILTER TO &cVar. .AND. cVar

/* ---------------------------------------------------------------------*/
#xtranslate XTRANS(<x>( => normal_match( <(x)> )
#xtranslate XTRANS(<x:&>( => macro_match( <(x)> )

  XTRANS( cVar (
  XTRANS( &cVar (
  XTRANS( &cVar+1 (
  XTRANS( &cVar. (
  XTRANS( &cVar&cVar (
  XTRANS( &cVar.&cVar (
  XTRANS( &cVar.&cVar. (
  XTRANS( (&cVar.) (
  XTRANS( &(cVar) (
  XTRANS( &cVar[3] (
  XTRANS( &cVar.  [3] (
  XTRANS( &(cVar  [3],&cvar) (
  XTRANS( (&cVar.  [3],&cvar) (
  XTRANS( &cVar.1+5 (
  XTRANS( &cVar .AND. cVar (
  XTRANS( &cVar. .AND. cVar (

/* ---------------------------------------------------------------------*/
#xtranslate MXCALL <x:&>  => (<x>)
#xtranslate MYCALL <x:&> <y>  => <x>( <y>, 'mycall' )
#xtranslate MZCALL <x> <y>  => <x>( <y>, "mzcall" )
#command   FOO <x:&> FOO <y:&> => <(x)>+<(y)>
#translate BAR <x:&> BAR <y:&> => <(x)>+<(y)>
  MYCALL &cVar ++cVar
  MZCALL &cVar ++cVar
  MYCALL &cVar+1 &cVar
  MZCALL &cVar+1 &cVar
  MXCALL &cVar
  MXCALL &cVar.
  MXCALL &cVar.1
  MXCALL &cVar + 1
  MYCALL &cVar &cVar
  MYCALL &cVar. &cVar.
  MYCALL &cVar.1 &cVar.1
  MYCALL &cVar ++cVar
  MYCALL &cVar. --cVar
  MYCALL &cVar.1 !cVar
  MYCALL &cVar+1 &cVar
  MYCALL &cVar.+1 &cVar.
  MYCALL &cVar.1+1 &cVar.1
  MYCALL &cVar +1 &cVar
  MYCALL &cVar. +1 &cVar.
  MYCALL &cVar.1 +1 &cVar.1
  MYCALL &cVar +1
  MYCALL &cVar. +1
  MYCALL &cVar.1 +1
  FOO &cVar FOO &var.
  BAR &cVar BAR &var.
  FOO &cVar FOO &var.+1
  BAR &cVar BAR &var.+1
  MXCALL &cVar()
  MXCALL &cVar++
  (MXCALL &cVar)++
  MXCALL &cVar.()
  MXCALL &cVar.++
  (MXCALL &cVar.)++
  MXCALL &cVar.1 ()
  MXCALL &cVar.1 ++
  (MXCALL &cVar.1) ++

/* ---------------------------------------------------------------------*/
#translate MTRANSLATE <x> => normal_t(<"x">)
#translate MTRANSLATE <x:&>  => macro_t(<(x)>)
#command MCOMMAND <x> => normal_c(<"x">)
#command MCOMMAND <x:&>  => macro_c(<(x)>)
  MTRANSLATE &cVar
  MTRANSLATE &cVar.
  MTRANSLATE &(cVar)
  MTRANSLATE & (cVar)
  MTRANSLATE &cVar&cVar
  MTRANSLATE &cVar+1
  MTRANSLATE &cVar.+1
  MTRANSLATE &cVar. .AND.  .T.
  MTRANSLATE &cVar++
  MTRANSLATE &cVar.++
  MTRANSLATE &cVar+=1
  MTRANSLATE &cVar.-=2
  MTRANSLATE &cVar*=1
  MTRANSLATE &cVar/=2
  MTRANSLATE &cVar%=1
  MTRANSLATE &cVar^=2
  MTRANSLATE &cVar:=1
  MTRANSLATE &cVar. .AND.  .T.
  MTRANSLATE &cVar .AND.  .T.
  MTRANSLATE &(cVar) +1
  MTRANSLATE & (cVar) +1
  MTRANSLATE &cVar.&cVar.
  MTRANSLATE &cVar.&cVar.&cVar&cVar
  MTRANSLATE &cVar.&(cVar)

   /* command */
  MCOMMAND &cVar
  MCOMMAND &cVar.
  MCOMMAND &(cVar)
  MCOMMAND & (cVar)
  MCOMMAND &cVar&cVar
  MCOMMAND &cVar+1
  MCOMMAND &cVar.+1
  MCOMMAND &cVar. .AND.  .T.
  MCOMMAND &cVar++
  MCOMMAND &cVar.++
  MCOMMAND &cVar+=1
  MCOMMAND &cVar.-=2
  MCOMMAND &cVar*=1
  MCOMMAND &cVar/=2
  MCOMMAND &cVar%=12
  MCOMMAND &cVar^=2
  MCOMMAND &cVar:=1
  MCOMMAND &cVar. .AND.  .T.
  MCOMMAND &cVar .AND.  .T.
  MCOMMAND &(cVar) +1
  MCOMMAND & (cVar) +1
  MCOMMAND &cVar.&cVar.
  MCOMMAND &cVar.&cVar.&cVar&cVar2
  MCOMMAND &cVar.&(cVar)

/* ---------------------------------------------------------------------*/
/* repeated optional clauses */
#xcommand SET <var1> [, <varN>] WITH <val> => <var1>:=<val> [; <varN>:=<val>]
  SET v1 WITH 0
  SET v1, v2 WITH 0
  SET v1, v2, v3 WITH 0
  SET v1, v2, v3, v4 WITH 0

#command AVG <x1> [, <xn>] TO <v1> [, <vn>]  =>;
   AVERAGE( {||<v1>:=<v1>+<x1>} [, {||<vn>:=<vn>+<xn>} ] )
  AVG f1 TO s1
  AVG f1, f2 TO s1, s2
  AVG f1, f2, f3 TO s1, s2, s3

/* ---------------------------------------------------------------------*/
  COPY STRUCTURE EXTENDED TO teststru

/* ---------------------------------------------------------------------*/
#ifdef __HARBOUR__
#command @ <row>, <col> GET <var>                                       ;
                        [PICTURE <pic>]                                 ;
                        [VALID <valid>]                                 ;
                        [WHEN <when>]                                   ;
                        [CAPTION <caption>]                             ;
                        [MESSAGE <message>]                             ;
                        [SEND <msg>]                                    ;
                                                                        ;
      => SetPos( <row>, <col> )                                         ;
       ; AAdd( GetList,                                                 ;
              _GET_( <var>, <"var">, <pic>, <{valid}>, <{when}> ) )     ;
      [; ATail(GetList):Caption := <caption>]                           ;
      [; ATail(GetList):CapRow  := ATail(Getlist):row                   ;
       ; ATail(GetList):CapCol  := ATail(Getlist):col -                 ;
                              __CapLength(<caption>) - 1]               ;
      [; ATail(GetList):message := <message>]                           ;
      [; ATail(GetList):<msg>]                                          ;
       ; ATail(GetList):Display()
#endif
  @ 0,1 GET a
  @ 0,2 GET a PICTURE "X"
  @ 0,3 GET a PICTURE "X" VALID .T.
  @ 0,4 GET a PICTURE "X" VALID .T. WHEN .T.
  @ 0,5 GET a PICTURE "X" VALID .T. WHEN .T. CAPTION "myget"
  @ 0,6 GET a PICTURE "X" VALID .T. WHEN .T. CAPTION "myget" MESSAGE "mymess"
  @ 0,7 GET a PICTURE "X" VALID .T. WHEN .T. CAPTION "myget" MESSAGE "mymess" SEND send()
/* ---------------------------------------------------------------------*/
  @ 1,1 GET a RANGE 0,100
  @ 1,2 GET a PICTURE "X" RANGE 0,100
   /* NOTE: Clipper fails here */
  @ 1,3 GET a PICTURE "X" VALID .T. RANGE 0,100
  @ 1,4 GET a PICTURE "X" WHEN .T. RANGE 0,100
  @ 1,5 GET a PICTURE "X" WHEN .T. CAPTION "myget" RANGE 0,100
  @ 1,6 GET a PICTURE "X" WHEN .T. CAPTION "myget" MESSAGE "mymess" RANGE 0,100
  @ 1,7 GET a PICTURE "X" WHEN .T. CAPTION "myget" MESSAGE "mymess" SEND send() RANGE 0,100
  @ 2,1 GET a
  @ 2,2 GET a RANGE 0,100 PICTURE "X"
  @ 2,3 GET a PICTURE "X" RANGE 0,100
  @ 2,4 GET a PICTURE "X" RANGE 0,100 WHEN .T.
  @ 2,5 GET a PICTURE "X" RANGE 0,100 WHEN .T. CAPTION "myget"
  @ 2,6 GET a PICTURE "X" RANGE 0,100 WHEN .T. CAPTION "myget" MESSAGE "mymess"
  @ 2,7 GET a PICTURE "X" RANGE 0,100 WHEN .T. CAPTION "myget" MESSAGE "mymess" SEND send()

/* ---------------------------------------------------------------------*/
#ifdef __HARBOUR__
#command @ <row>, <col> GET <var>                                           ;
                        PUSHBUTTON                                          ;
                        [VALID <valid>]                                     ;
                        [WHEN <when>]                                       ;
                        [CAPTION <caption>]                                 ;
                        [MESSAGE <message>]                                 ;
                        [COLOR <color>]                                     ;
                        [FOCUS <fblock>]                                    ;
                        [STATE <sblock>]                                    ;
                        [STYLE <style>]                                     ;
                        [SEND <msg>]                                        ;
                        [GUISEND <guimsg>]                                  ;
                        [SIZE X <sizex> Y <sizey>]                          ;
                        [CAPOFF X <capxoff> Y <capyoff>]                    ;
                        [BITMAP <bitmap>]                                   ;
                        [BMPOFF X <bmpxoff> Y <bmpyoff>]                    ;
                                                                            ;
      => SetPos( <row>, <col> )                                             ;
       ; AAdd( GetList,                                                     ;
              _GET_( <var>, <(var)>, NIL, <{valid}>, <{when}> ) )           ;
       ; ATail(GetList):Control := _PushButt_( <caption>, <message>,        ;
                       <color>, <{fblock}>, <{sblock}>, <style>,            ;
                       <sizex>, <sizey>, <capxoff>, <capyoff>,              ;
                       <bitmap>, <bmpxoff>, <bmpyoff> )                     ;
       ; ATail(GetList):reader  := { | a, b, c, d |                         ;
                                    GuiReader( a, b, c, d ) }               ;
      [; ATail(GetList):<msg>]                                              ;
      [; ATail(GetList):Control:<guimsg>]                                   ;
       ; ATail(GetList):Control:Display()
#endif
  @ 4,1 GET a PUSHBUTTON
  @ 4,1 GET a PUSHBUTTON VALID valid()
  @ 4,1 GET a PUSHBUTTON VALID valid() WHEN when()
  @ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap"
  @ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes"
  @ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes" COLOR color()
  @ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes" COLOR color() FOCUS focus()
  @ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes" COLOR color() FOCUS focus() STATE state()
  @ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes" COLOR color() FOCUS focus() STATE state() STYLE style()
  @ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes" COLOR color() FOCUS focus() STATE state() STYLE style() SEND send()
  @ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes" COLOR color() FOCUS focus() STATE state() STYLE style() SEND send() GUISEND guisend()
  @ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes" COLOR color() FOCUS focus() STATE state() STYLE style() SEND send() GUISEND guisend() SIZE X 100 Y 100
  @ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes" COLOR color() FOCUS focus() STATE state() STYLE style() SEND send() GUISEND guisend() SIZE X 100 Y 100 CAPOFF X 10 Y 10
  @ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes" COLOR color() FOCUS focus() STATE state() STYLE style() SEND send() GUISEND guisend() SIZE X 100 Y 100 CAPOFF X 10 Y 10 BITMAP bitmap()
  @ 4,1 GET a PUSHBUTTON VALID valid() WHEN when() CAPTION "cap" MESSAGE "mes" COLOR color() FOCUS focus() STATE state() STYLE style() SEND send() GUISEND guisend() SIZE X 100 Y 100 CAPOFF X 10 Y 10 BITMAP bitmap() BMPOFF X 2 Y 2
  @ 4,1 GET a PUSHBUTTON COLOR "W/N"
  @ 4,1 GET a PUSHBUTTON COLOR "W/N" SIZE X 100 Y 100 BMPOFF X 2 Y 2 VALID valid() GUISEND guisend() WHEN when() MESSAGE "mes"
  @ 4,1 GET a PUSHBUTTON SIZE X 100 Y 100 BMPOFF X 2 Y 2 VALID valid() GUISEND guisend() WHEN when() MESSAGE "mes" COLOR "W/N"
  @ 4,1 GET a PUSHBUTTON SIZE X 100 Y 100 BMPOFF X 2 Y 2 VALID valid() GUISEND guisend() WHEN when() MESSAGE "mes" COLOR "W/N" CAPOFF X 10 Y 10 FOCUS focus() STATE state() STYLE style() SEND send() BITMAP bitmap() CAPTION "cap"

#command DEFINE CLIPBOARD <oClp>               ;
   [ FORMAT <format:TEXT,OEMTEXT,BITMAP,DIF> ] ;
   [ OF <oWnd> ]                               ;
   => ;
   <oClp> := TClipboard():New( [UPPER(<(format)>)], <oWnd> )
DEFINE CLIPBOARD oC OF oD FORMAT TEXT

#xcommand DECLARE WINDOW <w> ;
=>;
#xtranslate <w> . <p:Name,Title,f1,f2,f3,f4,f5,f6,f7,f8,f9> := <n> => SProp( <"w">, <"p"> , <n> )
#xcommand DEFINE WINDOW <w> [ON INIT <IProc>] =>;
      DECLARE WINDOW <w>  ; _DW( <"w">, <{IProc}> )

   DEFINE WINDOW &oW
   DEFINE WINDOW &oW ON INIT &oW.Title:= "My title"
   &oW.Title := "title"
   &oW.f9 := 9

/* statndard Clipper commands */
RELEASE ALL
RELEASE ALL LIKE A
RELEASE ALL EXCEPT A
SAVE ALL LIKE A TO A
SAVE TO A ALL LIKE A
SAVE TO A ALL
SAVE ALL EXCEPT A TO A
SAVE TO A ALL EXCEPT A


LIST
LIST TO PRINTER
LIST TO FILE a
LIST OFF
LIST OFF TO PRINTER
LIST OFF TO FILE a
LIST a
LIST a,b
LIST a,b,(seek(a+b),c)
LIST a TO PRINTER
LIST a TO FILE a
LIST a,b TO PRINTER
LIST a,b,(seek(a+b),c) TO FILE a

LIST a OFF TO PRINTER
LIST a OFF TO FILE a
LIST a,b OFF TO PRINTER
LIST a,b,(seek(a+b),c) OFF TO FILE a

LIST a TO PRINTER OFF
LIST a TO FILE a OFF
LIST a,b TO PRINTER OFF
LIST a,b,(seek(a+b),c) TO FILE a OFF

LIST TO PRINTER a,b,c
LIST TO FILE a a,b,c

LIST REST
LIST REST TO PRINTER
LIST REST TO FILE a
LIST REST OFF
LIST OFF REST

LIST REST ALL
LIST ALL
LIST ALL REST
LIST ALL OFF
LIST ALL TO PRINTER
LIST ALL ALL
LIST REST REST
LIST OFF OFF
LIST ALL ALL TO PRINTER
LIST REST REST TO PRINTER
LIST OFF OFF TO PRINTER

LIST (ALL) ALL
LIST (REST) REST
LIST (OFF) OFF

LIST ALL+ALL
LIST REST+REST
LIST OFF+OFF
LIST ALL+ALL ALL
LIST REST+REST REST
LIST OFF+OFF OFF

LIST ALL+ALL REST
LIST REST+REST OFF
LIST OFF+OFF ALL

RETURN
