/*
 * Harbour Project source code:
 *    HVM speed test program
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

/* Harbour MT functions used in this test */
/*
#xtranslate hb_mutexSubscribe(      => mt_mutexSubscribe(
#xtranslate hb_mutexNotify(         => mt_mutexNotify(
#xtranslate hb_mutexCreate(         => mt_mutexCreate(
#xtranslate hb_threadOnce(          => mt_threadOnce(
#xtranslate hb_threadStart(         => mt_threadStart(
#xtranslate hb_threadJoin(          => mt_threadJoin(
#xtranslate hb_threadWaitForAll(    => mt_threadWaitForAll(
*/

#define N_TESTS 56
#define N_LOOPS 1000000
#define ARR_LEN 16

#ifndef __HARBOUR__
   #ifndef __XPP__
      #ifndef __CLIP__
         #ifndef FlagShip
            #define __CLIPPER__
         #endif
      #endif
   #endif
#endif


#ifdef __CLIPPER__
   /* Clipper does not support multithreading */
   #ifndef __ST__
      #define __ST__
   #endif
   /* Clipper does not have function to extract process time */
   #xtranslate hb_secondsCPU([<x>]) => seconds()
#endif

#ifdef FlagShip
   #define __NO_OBJ_ARRAY__
   /* FlagShip does not support multithreading */
   #ifndef __ST__
      #define __ST__
   #endif
   #xtranslate hb_secondsCPU([<x>]) => secondsCPU(<x>)
   /* the FlagShip version of seconds() returns integer values */
   #xtranslate seconds() => fs_seconds()
#endif

#ifdef __XPP__
   #define __NO_OBJ_ARRAY__
   /* Has xBase++ function to extract process time? */
   #xtranslate hb_secondsCPU([<x>]) => seconds()
#endif

#ifdef __CLIP__
   #define __NO_OBJ_ARRAY__
   /* CLIP version for MT performance testing is not ready yet */
   #ifndef __ST__
      #define __ST__
   #endif
   #xtranslate hb_secondsCPU([<x>]) => secondsCPU(<x>)
#endif

#ifdef __XHARBOUR__
   /* By default build xHarbour binaries without MT support.
    * xHarbour needs separated source code versions for MT and ST mode
    * because standard MT functions are not available in ST libraries.
    */
   #ifndef __ST__
      #ifndef __MT__
         #ifndef MT
            #ifndef HB_THREAD_SUPPORT
               #define __ST__
            #endif
         #endif
      #endif
   #endif
   #xtranslate hb_secondsCPU([<x>]) => secondsCPU(<x>)
#endif

/* by default create MT version */
#ifndef __MT__
   #ifndef __ST__
      #define __MT__
   #endif
#endif


#command ? => spd_out()
#command ? <xx,...> => spd_out();spd_out(<xx>)
#command ?? <xx,...> => spd_out(<xx>)

#ifdef __HARBOUR__
   #ifdef __XHARBOUR__
      #define EOL hb_osNewLine()
   #else
      #define EOL hb_eol()
   #endif
#else
   #ifndef EOL
      #define EOL chr(10)
   #endif
#endif

#xcommand TEST <testfunc>                 ;
          [ WITH <locals,...> ]           ;
          [ STATIC <statics,...> ]        ;
          [ FIELD  <fields,...> ]         ;
          [ MEMVAR <memvars,...> ]        ;
          [ PRIVATE <privates,...> ]      ;
          [ PUBLIC <publics,...> ]        ;
          [ INIT <init> ]                 ;
          [ EXIT <exit> ]                 ;
          [ INFO <info> ]                 ;
          CODE [ <testExp,...> ] =>       ;
   func <testfunc> ;                      ;
      local time, i:=nil, x:=nil ;        ;
      [ local <locals> ; ]                ;
      [ static <statics> ; ]              ;
      [ field <fields> ; ]                ;
      [ memvar <memvars> ; ]              ;
      [ private <privates> ; ]            ;
      [ public <publics> ; ]              ;
      [ <init> ; ]                        ;
      time := hb_secondsCPU() ;           ;
      for i:=1 to N_LOOPS ;               ;
         [ ( <testExp> ) ; ]              ;
      next ;                              ;
      time := hb_secondsCPU() - time ;    ;
      [ <exit> ; ]                        ;
   return { procname() + ": " + iif( <.info.>, <(info)>, #<testExp> ), time }

STATIC s_lStdOut := .F.

#ifdef __HARBOUR__
#ifndef __XHARBOUR__
#include "hbver.ch"
#endif
#endif

#ifdef __HARBOUR__
proc main( ... )
   local aParams := hb_aparams()
#else
proc main( _p01, _p02, _p03, _p04, _p05, _p06, _p07, _p08, _p09, _p10, ;
           _p11, _p12, _p13, _p14, _p15, _p16, _p17, _p18, _p19, _p20 )
   local aParams := ;
      asize( { _p01, _p02, _p03, _p04, _p05, _p06, _p07, _p08, _p09, _p10, ;
               _p11, _p12, _p13, _p14, _p15, _p16, _p17, _p18, _p19, _p20 }, ;
             min( pCount(), 20 ) )
#endif
   local nMT, cExclude, lScale, cParam, cMemTests, lSyntax, i, j

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   lSyntax := lScale := .f.
   cMemTests := "030 031 023 025 027 041 042 044 053 054 019 022 032 033 055 056 "
   cExclude := ""
   nMT := 0
   for j := 1 to len( aParams )
      cParam := lower( aParams[ j ] )
      if left( cParam, len( "--thread" ) ) == "--thread"  /* hb_LeftIs() */
         if substr( cParam, 9, 1 ) == "="
            if isdigit( substr( cParam, 10, 1 ) )
               nMT := val( substr( cParam, 10 ) )
            elseif substr( cParam, 10 ) == "all"
               nMT := -1
            else
               lSyntax := .t.
            endif
         elseif empty( substr( cParam, 9 ) )
            nMT := -1
         else
            lSyntax := .t.
         endif
      elseif left( cParam, len( "--exclude=" ) ) == "--exclude="  /* hb_LeftIs() */
         if substr( cParam, 11 ) == "mem"
            cExclude += cMemTests
         else
            cExclude += strtran( strtran( strtran( substr( cParam, 11 ), ;
                        ".", " " ), ".", " " ), "/", " " ) + " "
         endif
      elseif left( cParam, len( "--only=" ) ) == "--only="  /* hb_LeftIs() */
         cExclude := ""
         if substr( cParam, 8 ) == "mem"
            cParam := cMemTests
         endif
         for i := 1 to N_TESTS
            if ! strzero( i, 3 ) $ cParam
               cExclude += strzero( i, 3 ) + " "
            endif
         next
      elseif cParam == "--scale"
         lScale := .t.
      elseif cParam == "--stdout"
         s_lStdOut := .t.
      else
         lSyntax := .t.
      endif
      if lSyntax
         ? "Unknown option:", cParam
         ? "syntax: speedtst [--thread[=<num>]] [--only=<test(s)>] [--exclude=<test(s)>]"
         ?
         return
      endif
   next

   IF ! s_lStdOut
      set alternate to ( spd_logfile() ) additive
      set alternate on
   ENDIF
   // set console off

   test( nMT, cExclude, lScale )

   IF ! s_lStdOut
      set alternate off
      set alternate to
   ENDIF

return

STATIC PROCEDURE spd_out( p1, p2, p3, p4, p5, p6 )
   LOCAL nPCount := PCount()

   IF s_lStdOut
      DO CASE
      CASE nPCount == 0 ; OutStd( EOL )
      CASE nPCount == 1 ; OutStd( p1 )
      CASE nPCount == 2 ; OutStd( p1, p2 )
      CASE nPCount == 3 ; OutStd( p1, p2, p3 )
      CASE nPCount == 4 ; OutStd( p1, p2, p3, p4 )
      CASE nPCount == 5 ; OutStd( p1, p2, p3, p4, p5 )
      CASE nPCount == 6 ; OutStd( p1, p2, p3, p4, p5, p6 )
      ENDCASE
   ELSE
      DO CASE
      CASE nPCount == 0 ; QOut()
      CASE nPCount == 1 ; QQOut( p1 )
      CASE nPCount == 2 ; QQOut( p1, p2 )
      CASE nPCount == 3 ; QQOut( p1, p2, p3 )
      CASE nPCount == 4 ; QQOut( p1, p2, p3, p4 )
      CASE nPCount == 5 ; QQOut( p1, p2, p3, p4, p5 )
      CASE nPCount == 6 ; QQOut( p1, p2, p3, p4, p5, p6 )
      ENDCASE
   ENDIF
   RETURN

STATIC FUNCTION spd_logfile()
#ifndef __HARBOUR__
   RETURN "speedtst.txt"
#else
   LOCAL cName
   hb_FNameSplit( hb_ArgV( 0 ),, @cName )
   RETURN hb_FNameMerge( , cName, ".txt" )
#endif

/*** TESTS ***/

TEST t000 INFO "empty loop overhead" CODE

TEST t001 WITH L_C:=dtos(date()) CODE x := L_C

TEST t002 WITH L_N:=112345.67    CODE x := L_N

TEST t003 WITH L_D:=date()       CODE x := L_D

TEST t004 STATIC s_once := NIL, S_C ;
          INIT hb_threadOnce( @s_once, {|| S_C := dtos( date() ) } ) ;
          CODE x := S_C

TEST t005 STATIC s_once := NIL, S_N ;
          INIT hb_threadOnce( @s_once, {|| S_N := 112345.67 } ) ;
          CODE x := S_N

TEST t006 STATIC s_once := NIL, S_D ;
          INIT hb_threadOnce( @s_once, {|| S_D := date() } ) ;
          CODE x := S_D

TEST t007 MEMVAR M_C ;
          PRIVATE M_C := dtos( date() ) ;
          CODE x := M->M_C

TEST t008 MEMVAR M_N ;
          PRIVATE M_N := 112345.67 ;
          CODE x := M->M_N

TEST t009 MEMVAR M_D ;
          PRIVATE M_D := date() ;
          CODE x := M->M_D

TEST t010 STATIC s_once := NIL ;
          MEMVAR P_C ;
          PUBLIC P_C ;
          INIT hb_threadOnce( @s_once, {|| M->P_C := dtos( date() ) } ) ;
          CODE x := M->P_C

TEST t011 STATIC s_once := NIL ;
          MEMVAR P_N ;
          PUBLIC P_N ;
          INIT hb_threadOnce( @s_once, {|| M->P_N := 112345.67 } ) ;
          CODE x := M->P_N

TEST t012 STATIC s_once := NIL ;
          MEMVAR P_D ;
          PUBLIC P_D ;
          INIT hb_threadOnce( @s_once, {|| M->P_D := date() } ) ;
          CODE x := M->P_D

TEST t013 FIELD  F_C ;
          INIT use_dbsh() EXIT close_db() ;
          CODE x := F_C

TEST t014 FIELD F_N ;
          INIT use_dbsh() EXIT close_db() ;
          CODE x := F_N

TEST t015 FIELD F_D ;
          INIT use_dbsh() EXIT close_db() ;
          CODE x := F_D

TEST t016 WITH o := errorNew() CODE x := o:Args

TEST t017 WITH o := errorArray() CODE x := o[2]

TEST t018 CODE round( i / 1000, 2 )

TEST t019 CODE str( i / 1000 )

TEST t020 WITH s := stuff( dtos( date() ), 7, 0, "." ) CODE val( s )

TEST t021 WITH a := afill( array( ARR_LEN ), ;
                           stuff( dtos( date() ), 7, 0, "." ) ) ;
          CODE val( a [ i % ARR_LEN + 1 ] )

TEST t022 WITH d := date() CODE dtos( d - i % 10000 )

TEST t023 CODE eval( {|| i % ARR_LEN } )

TEST t024 WITH bc := {|| i % ARR_LEN } ;
          INFO eval( bc := {|| i % ARR_LEN } ) ;
          CODE eval( bc )

TEST t025 CODE eval( {| x | x % ARR_LEN }, i )

TEST t026 WITH bc := {| x | x % ARR_LEN } ;
          INFO eval( bc := {| x | x % ARR_LEN }, i ) ;
          CODE eval( bc, i )

TEST t027 CODE eval( {| x | f1( x ) }, i )

TEST t028 WITH bc := {| x | f1( x ) } ;
          INFO eval( bc := {| x | f1( x ) }, i ) ;
          CODE eval( bc, i )

TEST t029 WITH bc := mkBlock( "{| x | f1( x ) }" ) ;
          INFO eval( bc := &("{| x | f1( x ) }"), i ) ;
          CODE eval( bc, i )

TEST t030 CODE x := &( 'f1(' + str(i) + ')' )

TEST t031 WITH bc CODE bc := &( '{| x |f1(x)}' ), eval( bc, i )

TEST t032 CODE x := valtype( x ) +  valtype( i )

TEST t033 WITH a := afill( array( ARR_LEN ), ;
                           stuff( dtos( date() ), 7, 0, "." ) ) ;
          CODE x := strzero( i % 100, 2 ) $ a[ i % ARR_LEN + 1 ]

TEST t034 WITH a := array( ARR_LEN ), s := dtos( date() ) ;
          INIT aeval( a, {| x, i | a[i] := left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] == s

TEST t035 WITH a := array( ARR_LEN ), s := dtos( date() ) ;
          INIT aeval( a, {| x, i | a[i] := left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] = s  /* hb_LeftIs() */

TEST t036 WITH a := array( ARR_LEN ), s := dtos( date() ) ;
          INIT aeval( a, {| x, i | a[i] := left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] >= s

TEST t037 WITH a := array( ARR_LEN ), s := dtos( date() ) ;
          INIT aeval( a, {| x, i | a[i] := left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] <= s

TEST t038 WITH a := array( ARR_LEN ), s := dtos( date() ) ;
          INIT aeval( a, {| x, i | a[i] := left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] < s

TEST t039 WITH a := array( ARR_LEN ), s := dtos( date() ) ;
          INIT aeval( a, {| x, i | a[i] := left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] > s

TEST t040 WITH a := array( ARR_LEN ) ;
          INIT aeval( a, {| x, i | a[i] := i, x } ) ;
          CODE ascan( a, i % ARR_LEN )

TEST t041 WITH a := array( ARR_LEN ) ;
          INIT aeval( a, {| x, i | a[i] := i, x } ) ;
          CODE ascan( a, {| x | x == i % ARR_LEN } )

TEST t042 WITH a := {}, a2 := { 1, 2, 3 }, bc := {| x | f1(x) }, ;
               s := dtos( date() ), s2 := "static text" ;
          CODE iif( i%1000==0, a:={}, ) , aadd(a,{i,1,.t.,s,s2,a2,bc})

TEST t043 WITH a := {} CODE x := a

TEST t044 CODE x := {}

TEST t045 CODE f0()

TEST t046 CODE f1( i )

TEST t047 WITH c := dtos( date() ) ;
          INFO "f2( c[1...8] )" ;
          CODE f2( c )

TEST t048 WITH c := replicate( dtos( date() ), 5000 ) ;
          INFO "f2( c[1...40000] )" ;
          CODE f2( c )

TEST t049 WITH c := replicate( dtos( date() ), 5000 ) ;
          INFO "f2( @c[1...40000] )" ;
          CODE f2( @c )

TEST t050 WITH c := replicate( dtos( date() ), 5000 ), c2 ;
          INFO "f2( @c[1...40000] ), c2 := c" ;
          CODE f2( @c ), c2 := c

TEST t051 WITH a := {}, a2 := { 1, 2, 3 }, bc := {| x | f1(x) }, ;
               s := dtos( date() ), s2 := "static text", n := 1.23 ;
          CODE f3( a, a2, s, i, s2, bc, i, n, x )

TEST t052 WITH a := { 1, 2, 3 } CODE f2( a )

TEST t053 CODE x := f4()

TEST t054 CODE x := f5()

TEST t055 CODE x := space(16)

TEST t056 WITH c := dtos( date() ) CODE f_prv( c )

/*** end of tests ***/


#ifdef __MT__

function thTest( mtxJobs, aResults )
   local xJob := NIL
   while .T.
      hb_mutexSubscribe( mtxJobs,, @xJob )
      if xJob == NIL
         exit
      endif
      aResults[ xJob ] := &( "t" + strzero( xJob, 3 ) )()
   enddo
return nil

function thTestScale( mtxJobs, mtxResults )
   local xJob := NIL
   while .T.
      hb_mutexSubscribe( mtxJobs,, @xJob )
      if xJob == NIL
         exit
      endif
      hb_mutexNotify( mtxResults, &( "t" + strzero( xJob, 3 ) )() )
   enddo
return nil

#endif


proc test( nMT, cExclude, lScale )
local nLoopOverHead, nTimes, nSeconds, cNum, aThreads, aResults, ;
      mtxJobs, mtxResults, nTimeST, nTimeMT, nTimeTotST, nTimeTotMT, ;
      cTest, x, i, j

create_db()

#ifdef __HARBOUR__
   #include "hbmemory.ch"
   if MEMORY( HB_MEM_USEDMAX ) != 0
      ? "Warning !!! Memory statistic enabled."
      ?
   endif
   if type( "__DBGENTRY()" ) == "UI"
      ? "Warning !!! HVM debugger enabled."
      ?
   endif
#endif

//? "Startup loop to increase CPU clock..."
//x := seconds() + 5; while x > seconds(); enddo

#ifdef __MT__
if ! hb_mtvm()
#else
if .t.
#endif
   if lScale
      ? "scale test available only in MULTI THREAD mode"
      ?
      return
   endif
   if nMT != 0
      ? "SINGLE THREAD mode, number of threads set to 0"
      nMT := 0
   endif
endif
? date(), time(), os()
? version() + iif( hb_mtvm(), " (MT)" + iif( nMT != 0, "+", "" ), "" ), ""
#ifdef __HARBOUR__
   ?? hb_compiler(), ""
#endif
?? spd_cpu()

if lScale .and. nMT < 1
   nMT := 1
endif

? "THREADS:", iif( nMT < 0, "all->" + ltrim( str( N_TESTS ) ), ltrim( str( nMT ) ) )
? "N_LOOPS:", ltrim( str( N_LOOPS ) )
if ! empty( cExclude )
   ? "excluded tests:", cExclude
endif

x :=t000()
nLoopOverHead := x[2]

if lScale
   ? space(56) + "1 th." + str(nMT,3) + " th.  factor"
   ? replicate("=",76)
else
   ? dsp_result( x, 0 )
   ? replicate("=",68)
endif

nSeconds := seconds()
nTimes := hb_secondsCPU()

nTimeTotST := nTimeTotMT := 0

#ifdef __MT__
   if lScale
      aThreads := array( nMT )
      mtxJobs := hb_mutexCreate()
      mtxResults := hb_mutexCreate()
      for i:=1 to nMT
         aThreads[ i ] := hb_threadStart( "thTestScale", mtxJobs, mtxResults )
      next
      for i:=1 to N_TESTS
         cTest := strzero( i, 3 )
         if ! cTest $ cExclude

            /* linear execution */
            nTimeST := seconds()
            for j:=1 to nMT
               hb_mutexNotify( mtxJobs, i )
               hb_mutexSubscribe( mtxResults,, @x )
               cTest := x[ 1 ]
            next
            nTimeST := seconds() - nTimeST
            nTimeTotST += nTimeST

            /* simultaneous execution */
            nTimeMT := seconds()
            for j:=1 to nMT
               hb_mutexNotify( mtxJobs, i )
            next
            for j:=1 to nMT
               hb_mutexSubscribe( mtxResults,, @x )
               cTest := x[ 1 ]
            next
            nTimeMT := seconds() - nTimeMT
            nTimeTotMT += nTimeMT

            ? dsp_scaleResult( cTest, nTimeST, nTimeMT, nMT, nLoopOverHead )
         endif

      next
      for i:=1 to nMT
         hb_mutexNotify( mtxJobs, NIL )
      next
      hb_threadWaitForAll( aThreads )
   elseif nMT < 0
      aThreads := array( N_TESTS )
      for i:=1 to N_TESTS
         cNum := strzero( i, 3 )
         if ! cNum $ cExclude
            aThreads[ i ] := hb_threadStart( "t" + cNum )
         endif
      next
      for i:=1 to N_TESTS
         if aThreads[ i ] != NIL .and. hb_threadJoin( aThreads[ i ], @x )
            ? dsp_result( x, nLoopOverHead )
         endif
      next
   elseif nMT > 0
      aThreads := array( nMT )
      aResults := array( N_TESTS )
      mtxJobs := hb_mutexCreate()
      for i:=1 to nMT
         aThreads[ i ] := hb_threadStart( "thTest", mtxJobs, aResults )
      next
      for i:=1 to N_TESTS
         if ! strzero( i, 3 ) $ cExclude
            hb_mutexNotify( mtxJobs, i )
         endif
      next
      for i:=1 to nMT
         hb_mutexNotify( mtxJobs, NIL )
      next
      hb_threadWaitForAll( aThreads )
      for i:=1 to N_TESTS
         if aResults[ i ] != NIL
            ? dsp_result( aResults[ i ], nLoopOverHead )
         endif
      next
      mtxJobs := NIL
   else
      for i:=1 to N_TESTS
         cNum := strzero( i, 3 )
         if ! cNum $ cExclude
            ? dsp_result( &( "t" + cNum )(), nLoopOverHead )
         endif
      next
   endif
#else
   for i:=1 to N_TESTS
      cNum := strzero( i, 3 )
      if ! cNum $ cExclude
         ? dsp_result( &( "t" + cNum )(), nLoopOverHead )
      endif
   next
#endif

nTimes := hb_secondsCPU() - nTimes
nSeconds := seconds() - nSeconds

if lScale
   ? replicate("=",76)
   ? dsp_scaleResult( "  TOTAL  ", nTimeTotST, nTimeTotMT, nMT, 0 )
   ? replicate("=",76)
else
   ? replicate("=",68)
endif
? dsp_result( { "total application time:", nTimes }, 0)
? dsp_result( { "total real time:", nSeconds }, 0 )
?

remove_db()
return

function f0()
return nil

function f1(x)
return x

function f2(x)
return nil

function f3(a,b,c,d,e,f,g,h,i)
return nil

function f4()
return space(4000)

function f5()
return space(5)

function f_prv(x)
   memvar PRV_C
   private PRV_C := x
return nil

/*
function f_pub(x)
   memvar PUB_C
   public PUB_C := x
return nil

function f_stat(x)
   static STAT_C
   STAT_C := x
return nil
*/

static function mkBlock(x)
return &x

static function errorArray()
#ifdef __NO_OBJ_ARRAY__
   return array(16)
#else
   return errorNew()
#endif

static func dsp_result( aResult, nLoopOverHead )
   return padr( "[ " + left( aResult[ 1 ], 56 ) + " ]", 60, "." ) + ;
          strtran( str( max( aResult[ 2 ] - nLoopOverHead, 0 ), 8, 2 ), " ", "." )

static func dsp_scaleResult( cTest, nTimeST, nTimeMT, nMT, nLoopOverHead )
   if .f.
      nTimeST := max( 0, nTimeST - nMT * nLoopOverHead )
      nTimeMT := max( 0, nTimeMT - nMT * nLoopOverHead )
   endif
   return padr( "[ " + left( cTest, 50 ) + " ]", 54, "_" ) + ;
          str( nTimeST, 6, 2 ) + " " + str( nTimeMT, 6, 2 ) + " ->" + ;
          str( nTimeST / nTimeMT, 6, 2 )


#define TMP_FILE "_tst_tmp.dbf"
static proc create_db()
   remove_db()
   dbcreate( TMP_FILE, { {"F_C", "C", 10, 0},;
                         {"F_N", "N", 10, 2},;
                         {"F_D", "D",  8, 0} } )
   use TMP_FILE exclusive
   dbappend()
   field->F_C := dtos(date())
   field->F_N := 112345.67
   field->F_D := date()
   dbclosearea()
return

static proc remove_db()
   ferase( TMP_FILE )
return

static proc close_db()
   dbclosearea()
return

static proc use_dbsh()
   use TMP_FILE shared
return

#ifdef __HARBOUR__
#ifndef __XHARBOUR__
   static function spd_cpu()
   return hb_version( HB_VERSION_CPU )
#endif
#endif
#ifdef __CLIPPER__
   static function spd_cpu()
   return "x86"
#endif
#ifdef FlagShip
   static function spd_cpu()
   return "?"
#endif
#ifdef __CLIP__
   static function spd_cpu()
   return "?"
#endif
#ifdef __XPP__
   static function spd_cpu()
   return "x86"
#endif
#ifdef __XHARBOUR__
   static function spd_cpu()
   return "?"
#endif

#ifdef __CLIPPER__
   static function hb_mtvm()
   return .f.                 /* Clipper does not support MT */
#endif
#ifdef FlagShip
   static function hb_mtvm()
   return .f.                 /* FlagShip does not support MT */
#endif
#ifdef __CLIP__
   static function hb_mtvm()
   return .t.                 /* CLIP always uses VM with MT support */
#endif
#ifdef __XPP__
   static function hb_mtvm()
   return .t.                 /* xBase++ always uses VM with MT support */
#endif
#ifdef __XHARBOUR__
   static function hb_mtvm()
   return hb_multiThread()    /* check for MT support in xHarbour VM */
#endif


#ifndef __MT__

   /* trivial single thread version of once execution */
   static function hb_threadOnce( xOnceControl, bAction )
      local lFirstCall := .f.
      if xOnceControl == NIL
         if bAction != NIL
            eval( bAction )
         endif
         xOnceControl := .t.
         lFirstCall := .t.
      endif
   return lFirstCall

#else

   /* Add support for MT functions for used compiler
    */

#ifdef __XHARBOUR__

   static function hb_mutexSubscribe( mtx, nTimeOut, xSubscribed )
      local lSubscribed
      if valtype( nTimeOut ) == "N"
         nTimeOut := round( nTimeOut * 1000, 0 )
         xSubscribed := Subscribe( mtx, nTimeOut, @lSubscribed )
      else
         xSubscribed := Subscribe( mtx )
         lSubscribed := .t.
      endif
   return lSubscribed

   static function hb_mutexNotify( mtx, xValue )
      Notify( mtx, xValue )
   return nil

   /* In xHarbour there is race condition in JoinThread() which fails if
    * thread have ended before call to JoinThread() so we cannot use it.
    * Exactly the same problem exists in GetThreadId().
    * As workaround we will use mutexes as thread IDs and notify/subscribe
    * mechanism to simulate thread join operation and passing thread return
    * value.
    */
   static function hb_threadStart( ... )
      local thId
      thId := hb_mutexCreate()
      /* For some reasons codeblocks as thread startup entry are broken
       * in xHarbour so we use intermediate function instead
       */
      StartThread( @_thFuncFirst(), thId, hb_aParams() )
   return thId

   static function _thFuncFirst( thID, aParams )
      Notify( thId, hb_execFromArray( aParams ) )
   return nil

   static function hb_threadJoin( thId, xResult )
      xResult := Subscribe( thId )
   return .t.

   static function hb_threadWaitForAll()
      WaitForThreads()
   return nil

   static function hb_threadOnce( xOnceControl, bAction )
      static s_mutex
      local lFirstCall := .f.
      if s_mutex == NIL
         s_mutex := hb_mutexCreate()
      endif
      if xOnceControl == NIL
         hb_mutexLock( s_mutex )
         if xOnceControl == NIL
            if bAction != NIL
               eval( bAction )
            endif
            xOnceControl := .t.
            lFirstCall := .t.
         endif
         hb_mutexUnlock( s_mutex )
      endif
   return lFirstCall

   init proc once_init()
      /* set workareas local to thread */
      set workarea private
      /* initialize mutex in hb_threadOnce() */
      hb_threadOnce()
      /* initialize error object to reduce to chance for possible crash
       * when two threads try to create new error class simultaneously.
       * xHarbour does not have any protection against such situation
       */
      errorNew()
   return

#endif /* __XHARBOUR__ */


#ifdef __XPP__

#ifdef __HARBOUR__
   /* for testing. Harbour also can use xBase++ API in this code */
   #include "hbclass.ch"
#endif

   INIT PROCEDURE once_init()
      /* initialize sync object in hb_threadOnce() */
      hb_threadOnce()
      RETURN

   CLASS Notifier
      PROTECTED:
         VAR aQueue
         VAR oSignal
      EXPORTED:
         METHOD init
         SYNC METHOD notify
         SYNC METHOD subscribe
   ENDCLASS

   METHOD Notifier:init
      ::aQueue := {}
      ::oSignal := Signal():new()
      RETURN self

   METHOD Notifier:notify( xValue )
      AAdd( ::aQueue, xValue )
      ::oSignal:signal()
      RETURN self

   METHOD Notifier:subscribe()
      LOCAL xResult
      DO WHILE Len( ::aQueue ) == 0
         ::oSignal:wait()
      ENDDO
      xResult := ::aQueue[ 1 ]
      ADel( ::aQueue, 1 )
      ASize( ::aQueue, Len( ::aQueue ) - 1 )
      RETURN xResult


   STATIC FUNCTION hb_mutexSubscribe( mtx, nTimeOut, xResult )
      /* Ignore timeout - it's not used in this test */
      xResult := mtx:subscribe()
      RETURN .T.

   STATIC FUNCTION hb_mutexNotify( mtx, xValue )
      RETURN mtx:notify( xValue )

   STATIC FUNCTION hb_mutexCreate()
      RETURN Notifier():new()


   CLASS Once
      EXPORTED:
         SYNC METHOD onceDo
   ENDCLASS

   METHOD Once:onceDo( xOnceControl, bAction )
      LOCAL lFirstCall := .f.
      IF xOnceControl == NIL
         IF bAction != NIL
            Eval( bAction )
         ENDIF
         xOnceControl := .t.
         lFirstCall := .t.
      ENDIF
      RETURN lFirstCall

   STATIC FUNCTION hb_threadOnce( xOnceControl, bAction )
      STATIC s_oObject := NIL
      IF s_oObject == NIL
         s_oObject := Once():new()
      ENDIF
      RETURN s_oObject:onceDo( @xOnceControl, bAction )

   STATIC FUNCTION hb_threadStart( cFunc, xPar1, xPar2, xPar3 )
      LOCAL oThread
      oThread := Thread():new()
      oThread:start( cFunc, xPar1, xPar2, xPar3 )
      RETURN oThread

   STATIC FUNCTION hb_threadJoin( oThread, xResult )
      oThread:synchronize( 0 )
      xResult := oThread:result
      RETURN .T.

   STATIC FUNCTION hb_threadWaitForAll( aThreads )
      ThreadWaitAll( aThreads )
      RETURN NIL

#endif /* __XPP__ */


/*
   static function hb_threadStart( cFunc, xPar1, xPar2, xPar3 )
   return nil

   static function hb_threadJoin( thId, xResult )
   return nil

   static function hb_mutexCreate()
   return nil

   static function hb_mutexSubscribe()
   return nil
   static function hb_mutexLock()
   return nil
   static function hb_mutexUnlock()
   return nil
   static function hb_mutexNotify()
   return nil
   static function hb_threadWaitForAll()
   return nil
   static function hb_mtvm()
   return .f.
*/

#endif

#ifdef FlagShip
   static function fs_seconds()
      LOCAL_DOUBLE nret := 0
      #Cinline
      {
         #include <sys/time.h>
         struct timeval tv;
         if( gettimeofday( &tv, NULL ) == 0 )
            nret = ( double ) tv.tv_sec + ( double ) ( tv.tv_usec ) / 1000000;
      }
      #endCinline
   return nret
   #ifndef FlagShip5
      FUNCTION cursesinit()
      return nil
   #endif
#endif
