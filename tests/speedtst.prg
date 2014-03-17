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
#else
   #pragma -w1
#endif


#ifdef __CLIPPER__
   /* Clipper does not support multithreading */
   #ifndef __ST__
      #define __ST__
   #endif
   /* Clipper does not have function to extract process time */
   #xtranslate hb_SecondsCPU([<x>]) => Seconds()
#endif

#ifdef FlagShip
   #define __NO_OBJ_ARRAY__
   /* FlagShip does not support multithreading */
   #ifndef __ST__
      #define __ST__
   #endif
   #xtranslate hb_SecondsCPU([<x>]) => SecondsCPU(<x>)
   /* the FlagShip version of Seconds() returns integer values */
   #xtranslate Seconds() => fs_seconds()
#endif

#ifdef __XPP__
   #define __NO_OBJ_ARRAY__
   /* Has xBase++ function to extract process time? */
   #xtranslate hb_SecondsCPU([<x>]) => Seconds()
#endif

#ifdef __CLIP__
   #define __NO_OBJ_ARRAY__
   /* CLIP version for MT performance testing is not ready yet */
   #ifndef __ST__
      #define __ST__
   #endif
   #xtranslate hb_SecondsCPU([<x>]) => SecondsCPU(<x>)
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
   #xtranslate hb_SecondsCPU([<x>]) => SecondsCPU(<x>)
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
      #define EOL Chr( 10 )
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
   function <testfunc> ;                  ;
      local time, i := nil, x := nil ;    ;
      [ local <locals> ; ]                ;
      [ static <statics> ; ]              ;
      [ field <fields> ; ]                ;
      [ memvar <memvars> ; ]              ;
      [ private <privates> ; ]            ;
      [ public <publics> ; ]              ;
      [ <init> ; ]                        ;
      time := hb_SecondsCPU() ;           ;
      for i := 1 to N_LOOPS ;             ;
         [ ( <testExp> ) ; ]              ;
      next ;                              ;
      time := hb_SecondsCPU() - time ;    ;
      [ <exit> ; ]                        ;
      return { ProcName() + ": " + iif( <.info.>, <(info)>, #<testExp> ), time }

static s_lStdOut := .f.

#ifdef __HARBOUR__
#ifndef __XHARBOUR__
#include "hbver.ch"
#endif
#endif

#ifdef __HARBOUR__
procedure main( ... )

   local aParams := hb_AParams()
#else
procedure main( _p01, _p02, _p03, _p04, _p05, _p06, _p07, _p08, _p09, _p10, ;
                _p11, _p12, _p13, _p14, _p15, _p16, _p17, _p18, _p19, _p20 )

   local aParams := ;
      ASize( { _p01, _p02, _p03, _p04, _p05, _p06, _p07, _p08, _p09, _p10, ;
               _p11, _p12, _p13, _p14, _p15, _p16, _p17, _p18, _p19, _p20 }, ;
             Min( PCount(), 20 ) )
#endif
   local nMT, cExclude, lScale, cParam, cMemTests, lSyntax, i, j

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   lSyntax := lScale := .f.
   cMemTests := "030 031 023 025 027 041 042 044 053 054 019 022 032 033 055 056 "
   cExclude := ""
   nMT := 0
   for j := 1 to Len( aParams )
      cParam := Lower( aParams[ j ] )
      if Left( cParam, Len( "--thread" ) ) == "--thread"  /* hb_LeftEq() */
         if SubStr( cParam, 9, 1 ) == "="
            if IsDigit( SubStr( cParam, 10, 1 ) )
               nMT := Val( SubStr( cParam, 10 ) )
            elseif SubStr( cParam, 10 ) == "all"
               nMT := -1
            else
               lSyntax := .t.
            endif
         elseif Empty( SubStr( cParam, 9 ) )
            nMT := -1
         else
            lSyntax := .t.
         endif
      elseif Left( cParam, Len( "--exclude=" ) ) == "--exclude="  /* hb_LeftEq() */
         if SubStr( cParam, 11 ) == "mem"
            cExclude += cMemTests
         else
            cExclude += StrTran( StrTran( StrTran( SubStr( cParam, 11 ), ;
                        ".", " " ), ".", " " ), "/", " " ) + " "
         endif
      elseif Left( cParam, Len( "--only=" ) ) == "--only="  /* hb_LeftEq() */
         cExclude := ""
         if SubStr( cParam, 8 ) == "mem"
            cParam := cMemTests
         endif
         for i := 1 to N_TESTS
            if ! StrZero( i, 3 ) $ cParam
               cExclude += StrZero( i, 3 ) + " "
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

   if ! s_lStdOut
      set alternate to ( spd_logfile() ) additive
      set alternate on
   endif
   // set console off

   test( nMT, cExclude, lScale )

   if ! s_lStdOut
      set alternate off
      set alternate to
   endif

   return

static procedure spd_out( p1, p2, p3, p4, p5, p6 )

   local nPCount := PCount()

   if s_lStdOut
      do case
      case nPCount == 0 ; OutStd( EOL )
      case nPCount == 1 ; OutStd( p1 )
      case nPCount == 2 ; OutStd( p1, p2 )
      case nPCount == 3 ; OutStd( p1, p2, p3 )
      case nPCount == 4 ; OutStd( p1, p2, p3, p4 )
      case nPCount == 5 ; OutStd( p1, p2, p3, p4, p5 )
      case nPCount == 6 ; OutStd( p1, p2, p3, p4, p5, p6 )
      endcase
   else
      do case
      case nPCount == 0 ; QOut()
      case nPCount == 1 ; QQOut( p1 )
      case nPCount == 2 ; QQOut( p1, p2 )
      case nPCount == 3 ; QQOut( p1, p2, p3 )
      case nPCount == 4 ; QQOut( p1, p2, p3, p4 )
      case nPCount == 5 ; QQOut( p1, p2, p3, p4, p5 )
      case nPCount == 6 ; QQOut( p1, p2, p3, p4, p5, p6 )
      endcase
   endif

   return

static function spd_logfile()
#ifndef __HARBOUR__
   return "speedtst.txt"
#else
   local cName

   hb_FNameSplit( hb_argv( 0 ),, @cName )

   return hb_FNameMerge( , cName, ".txt" )
#endif

/*** TESTS ***/

TEST t000 INFO "empty loop overhead" CODE

TEST t001 WITH L_C := DToS( Date() ) CODE x := L_C

TEST t002 WITH L_N := 112345.67      CODE x := L_N

TEST t003 WITH L_D := Date()         CODE x := L_D

TEST t004 STATIC s_once := NIL, S_C ;
          INIT hb_threadOnce( @s_once, {|| S_C := DToS( Date() ) } ) ;
          CODE x := S_C

TEST t005 STATIC s_once := NIL, S_N ;
          INIT hb_threadOnce( @s_once, {|| S_N := 112345.67 } ) ;
          CODE x := S_N

TEST t006 STATIC s_once := NIL, S_D ;
          INIT hb_threadOnce( @s_once, {|| S_D := Date() } ) ;
          CODE x := S_D

TEST t007 MEMVAR M_C ;
          PRIVATE M_C := DToS( Date() ) ;
          CODE x := M->M_C

TEST t008 MEMVAR M_N ;
          PRIVATE M_N := 112345.67 ;
          CODE x := M->M_N

TEST t009 MEMVAR M_D ;
          PRIVATE M_D := Date() ;
          CODE x := M->M_D

TEST t010 STATIC s_once := NIL ;
          MEMVAR P_C ;
          PUBLIC P_C ;
          INIT hb_threadOnce( @s_once, {|| M->P_C := DToS( Date() ) } ) ;
          CODE x := M->P_C

TEST t011 STATIC s_once := NIL ;
          MEMVAR P_N ;
          PUBLIC P_N ;
          INIT hb_threadOnce( @s_once, {|| M->P_N := 112345.67 } ) ;
          CODE x := M->P_N

TEST t012 STATIC s_once := NIL ;
          MEMVAR P_D ;
          PUBLIC P_D ;
          INIT hb_threadOnce( @s_once, {|| M->P_D := Date() } ) ;
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

TEST t016 WITH o := ErrorNew() CODE x := o:Args

TEST t017 WITH o := errorArray() CODE x := o[ 2 ]

TEST t018 CODE Round( i / 1000, 2 )

TEST t019 CODE Str( i / 1000 )

TEST t020 WITH s := Stuff( DToS( Date() ), 7, 0, "." ) CODE Val( s )

TEST t021 WITH a := AFill( Array( ARR_LEN ), ;
                           Stuff( DToS( Date() ), 7, 0, "." ) ) ;
          CODE Val( a [ i % ARR_LEN + 1 ] )

TEST t022 WITH d := Date() CODE DToS( d - i % 10000 )

TEST t023 CODE Eval( {|| i % ARR_LEN } )

TEST t024 WITH bc := {|| i % ARR_LEN } ;
          INFO Eval( bc := {|| i % ARR_LEN } ) ;
          CODE Eval( bc )

TEST t025 CODE Eval( {| x | x % ARR_LEN }, i )

TEST t026 WITH bc := {| x | x % ARR_LEN } ;
          INFO Eval( bc := {| x | x % ARR_LEN }, i ) ;
          CODE Eval( bc, i )

TEST t027 CODE Eval( {| x | f1( x ) }, i )

TEST t028 WITH bc := {| x | f1( x ) } ;
          INFO Eval( bc := {| x | f1( x ) }, i ) ;
          CODE Eval( bc, i )

TEST t029 WITH bc := mkBlock( "{| x | f1( x ) }" ) ;
          INFO Eval( bc := &( "{| x | f1( x ) }" ), i ) ;
          CODE Eval( bc, i )

TEST t030 CODE x := &( 'f1(' + Str( i ) + ')' )

TEST t031 WITH bc CODE bc := &( '{| x | f1( x ) }' ), Eval( bc, i )

TEST t032 CODE x := ValType( x ) + ValType( i )

TEST t033 WITH a := AFill( Array( ARR_LEN ), ;
                           Stuff( DToS( Date() ), 7, 0, "." ) ) ;
          CODE x := StrZero( i % 100, 2 ) $ a[ i % ARR_LEN + 1 ]

TEST t034 WITH a := Array( ARR_LEN ), s := DToS( Date() ) ;
          INIT AEval( a, {| x, i | a[ i ] := Left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] == s

TEST t035 WITH a := Array( ARR_LEN ), s := DToS( Date() ) ;
          INIT AEval( a, {| x, i | a[ i ] := Left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] = s  /* hb_LeftEq() */

TEST t036 WITH a := Array( ARR_LEN ), s := DToS( Date() ) ;
          INIT AEval( a, {| x, i | a[ i ] := Left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] >= s

TEST t037 WITH a := Array( ARR_LEN ), s := DToS( Date() ) ;
          INIT AEval( a, {| x, i | a[ i ] := Left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] <= s

TEST t038 WITH a := Array( ARR_LEN ), s := DToS( Date() ) ;
          INIT AEval( a, {| x, i | a[ i ] := Left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] < s

TEST t039 WITH a := Array( ARR_LEN ), s := DToS( Date() ) ;
          INIT AEval( a, {| x, i | a[ i ] := Left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] > s

TEST t040 WITH a := Array( ARR_LEN ) ;
          INIT AEval( a, {| x, i | a[ i ] := i, x } ) ;
          CODE AScan( a, i % ARR_LEN )

TEST t041 WITH a := Array( ARR_LEN ) ;
          INIT AEval( a, {| x, i | a[ i ] := i, x } ) ;
          CODE AScan( a, {| x | x == i % ARR_LEN } )

TEST t042 WITH a := {}, a2 := { 1, 2, 3 }, bc := {| x | f1( x ) }, ;
               s := DToS( Date() ), s2 := "static text" ;
          CODE iif( i % 1000 == 0, a := {}, ), AAdd(a,{i,1,.t.,s,s2,a2,bc})

TEST t043 WITH a := {} CODE x := a

TEST t044 CODE x := {}

TEST t045 CODE f0()

TEST t046 CODE f1( i )

TEST t047 WITH c := DToS( Date() ) ;
          INFO "f2( c[1...8] )" ;
          CODE f2( c )

TEST t048 WITH c := Replicate( DToS( Date() ), 5000 ) ;
          INFO "f2( c[1...40000] )" ;
          CODE f2( c )

TEST t049 WITH c := Replicate( DToS( Date() ), 5000 ) ;
          INFO "f2( @c[1...40000] )" ;
          CODE f2( @c )

TEST t050 WITH c := Replicate( DToS( Date() ), 5000 ), c2 ;
          INFO "f2( @c[1...40000] ), c2 := c" ;
          CODE f2( @c ), c2 := c

TEST t051 WITH a := {}, a2 := { 1, 2, 3 }, bc := {| x | f1( x ) }, ;
               s := DToS( Date() ), s2 := "static text", n := 1.23 ;
          CODE f3( a, a2, s, i, s2, bc, i, n, x )

TEST t052 WITH a := { 1, 2, 3 } CODE f2( a )

TEST t053 CODE x := f4()

TEST t054 CODE x := f5()

TEST t055 CODE x := Space( 16 )

TEST t056 WITH c := DToS( Date() ) CODE f_prv( c )

/*** end of tests ***/


#ifdef __MT__

function thTest( mtxJobs, aResults )

   local xJob := nil

   while .t.
      hb_mutexSubscribe( mtxJobs,, @xJob )
      if xJob == nil
         exit
      endif
      aResults[ xJob ] := &( "t" + StrZero( xJob, 3 ) )()
   enddo

   return nil

function thTestScale( mtxJobs, mtxResults )

   local xJob := nil

   while .t.
      hb_mutexSubscribe( mtxJobs,, @xJob )
      if xJob == nil
         exit
      endif
      hb_mutexNotify( mtxResults, &( "t" + StrZero( xJob, 3 ) )() )
   enddo

   return nil

#endif


procedure test( nMT, cExclude, lScale )

   local nLoopOverHead, nTimes, nSeconds, cNum, aThreads, aResults, ;
         mtxJobs, mtxResults, nTimeST, nTimeMT, nTimeTotST, nTimeTotMT, ;
         cTest, x, i, j

   create_db()

#ifdef __HARBOUR__
   #include "hbmemory.ch"
   if Memory( HB_MEM_USEDMAX ) != 0
      ? "Warning !!! Memory statistics enabled."
      ?
   endif
   if Type( "__DBGENTRY()" ) == "UI"
      ? "Warning !!! HVM debugger enabled."
      ?
   endif
#endif

//  ? "Startup loop to increase CPU clock..."
//  x := Seconds() + 5; while x > Seconds(); enddo

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
   ? Date(), Time(), OS()
   ? Version() + iif( hb_mtvm(), " (MT)" + iif( nMT != 0, "+", "" ), "" ), ""
#ifdef __HARBOUR__
   ?? hb_Compiler(), ""
#endif
   ?? spd_cpu()

   if lScale .and. nMT < 1
      nMT := 1
   endif

   ? "THREADS:", iif( nMT < 0, "all->" + LTrim( Str( N_TESTS ) ), LTrim( Str( nMT ) ) )
   ? "N_LOOPS:", LTrim( Str( N_LOOPS ) )
   if ! Empty( cExclude )
      ? "excluded tests:", cExclude
   endif

   x := t000()
   nLoopOverHead := x[ 2 ]

   if lScale
      ? Space( 56 ) + "1 th." + Str( nMT, 3 ) + " th.  factor"
      ? Replicate( "=", 76 )
   else
      ? dsp_result( x, 0 )
      ? Replicate( "=", 68 )
   endif

   nSeconds := Seconds()
   nTimes := hb_SecondsCPU()

   nTimeTotST := nTimeTotMT := 0

#ifdef __MT__
   if lScale
      aThreads := Array( nMT )
      mtxJobs := hb_mutexCreate()
      mtxResults := hb_mutexCreate()
      for i := 1 to nMT
         aThreads[ i ] := hb_threadStart( "thTestScale", mtxJobs, mtxResults )
      next
      for i := 1 to N_TESTS
         cTest := StrZero( i, 3 )
         if ! cTest $ cExclude

            /* linear execution */
            nTimeST := Seconds()
            for j := 1 to nMT
               hb_mutexNotify( mtxJobs, i )
               hb_mutexSubscribe( mtxResults,, @x )
               cTest := x[ 1 ]
            next
            nTimeST := Seconds() - nTimeST
            nTimeTotST += nTimeST

            /* simultaneous execution */
            nTimeMT := Seconds()
            for j := 1 to nMT
               hb_mutexNotify( mtxJobs, i )
            next
            for j := 1 to nMT
               hb_mutexSubscribe( mtxResults,, @x )
               cTest := x[ 1 ]
            next
            nTimeMT := Seconds() - nTimeMT
            nTimeTotMT += nTimeMT

            ? dsp_scaleResult( cTest, nTimeST, nTimeMT, nMT, nLoopOverHead )
         endif

      next
      for i := 1 to nMT
         hb_mutexNotify( mtxJobs, nil )
      next
      hb_threadWaitForAll( aThreads )
   elseif nMT < 0
      aThreads := Array( N_TESTS )
      for i := 1 to N_TESTS
         cNum := StrZero( i, 3 )
         if ! cNum $ cExclude
            aThreads[ i ] := hb_threadStart( "t" + cNum )
         endif
      next
      for i := 1 to N_TESTS
         if aThreads[ i ] != nil .and. hb_threadJoin( aThreads[ i ], @x )
            ? dsp_result( x, nLoopOverHead )
         endif
      next
   elseif nMT > 0
      aThreads := Array( nMT )
      aResults := Array( N_TESTS )
      mtxJobs := hb_mutexCreate()
      for i := 1 to nMT
         aThreads[ i ] := hb_threadStart( "thTest", mtxJobs, aResults )
      next
      for i := 1 to N_TESTS
         if ! StrZero( i, 3 ) $ cExclude
            hb_mutexNotify( mtxJobs, i )
         endif
      next
      for i := 1 to nMT
         hb_mutexNotify( mtxJobs, nil )
      next
      hb_threadWaitForAll( aThreads )
      for i := 1 to N_TESTS
         if aResults[ i ] != nil
            ? dsp_result( aResults[ i ], nLoopOverHead )
         endif
      next
      mtxJobs := nil
   else
      for i := 1 to N_TESTS
         cNum := StrZero( i, 3 )
         if ! cNum $ cExclude
            ? dsp_result( &( "t" + cNum )(), nLoopOverHead )
         endif
      next
   endif
#else
   for i := 1 to N_TESTS
      cNum := StrZero( i, 3 )
      if ! cNum $ cExclude
         ? dsp_result( &( "t" + cNum )(), nLoopOverHead )
      endif
   next
#endif

   nTimes := hb_SecondsCPU() - nTimes
   nSeconds := Seconds() - nSeconds

   if lScale
      ? Replicate( "=", 76 )
      ? dsp_scaleResult( "  TOTAL  ", nTimeTotST, nTimeTotMT, nMT, 0 )
      ? Replicate( "=", 76 )
   else
      ? Replicate( "=", 68 )
   endif
   ? dsp_result( { "total application time:", nTimes }, 0 )
   ? dsp_result( { "total real time:", nSeconds }, 0 )
   ?

   remove_db()

   return

function f0()
   return nil

function f1( x )
   return x

function f2( x )
   return nil

function f3( a, b, c, d, e, f, g, h, i )
   return nil

function f4()
   return Space( 4000 )

function f5()
   return Space( 5 )

function f_prv( x )

   memvar PRV_C
   private PRV_C := x

   return nil

/*
function f_pub( x )
   memvar PUB_C
   public PUB_C := x
   return nil

function f_stat( x )
   static STAT_C
   STAT_C := x
   return nil
*/

static function mkBlock( x )
   return &x

static function errorArray()
#ifdef __NO_OBJ_ARRAY__
   return Array(16)
#else
   return ErrorNew()
#endif

static function dsp_result( aResult, nLoopOverHead )
   return PadR( "[ " + Left( aResult[ 1 ], 56 ) + " ]", 60, "." ) + ;
      StrTran( Str( Max( aResult[ 2 ] - nLoopOverHead, 0 ), 8, 2 ), " ", "." )

static function dsp_scaleResult( cTest, nTimeST, nTimeMT, nMT, nLoopOverHead )

   if .f.
      nTimeST := Max( 0, nTimeST - nMT * nLoopOverHead )
      nTimeMT := Max( 0, nTimeMT - nMT * nLoopOverHead )
   endif

   return PadR( "[ " + Left( cTest, 50 ) + " ]", 54, "_" ) + ;
      Str( nTimeST, 6, 2 ) + " " + Str( nTimeMT, 6, 2 ) + " ->" + ;
      Str( nTimeST / nTimeMT, 6, 2 )


#define TMP_FILE "_tst_tmp.dbf"
static procedure create_db()

   remove_db()
   dbCreate( TMP_FILE, { ;
      { "F_C", "C", 10, 0 },;
      { "F_N", "N", 10, 2 },;
      { "F_D", "D",  8, 0 } } )
   use TMP_FILE exclusive
   dbAppend()
   field->F_C := DToS( Date() )
   field->F_N := 112345.67
   field->F_D := Date()
   dbCloseArea()

   return

static procedure remove_db()

   FErase( TMP_FILE )

   return

static procedure close_db()

   dbCloseArea()

   return

static procedure use_dbsh()

   use TMP_FILE shared

   return

#ifdef __HARBOUR__
#ifndef __XHARBOUR__
static function spd_cpu()
   return hb_Version( HB_VERSION_CPU )
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
   return hb_MultiThread()    /* check for MT support in xHarbour VM */
#endif


#ifndef __MT__

/* trivial single thread version of once execution */
static function hb_threadOnce( xOnceControl, bAction )

   local lFirstCall := .f.

   if xOnceControl == nil
      if bAction != nil
         Eval( bAction )
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

   if ValType( nTimeOut ) == "N"
      nTimeOut := Round( nTimeOut * 1000, 0 )
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
   StartThread( @_thFuncFirst(), thId, hb_AParams() )

   return thId

static function _thFuncFirst( thID, aParams )

   Notify( thId, hb_ExecFromArray( aParams ) )

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

   if s_mutex == nil
      s_mutex := hb_mutexCreate()
   endif
   if xOnceControl == nil
      hb_mutexLock( s_mutex )
      if xOnceControl == nil
         if bAction != nil
            Eval( bAction )
         endif
         xOnceControl := .t.
         lFirstCall := .t.
      endif
      hb_mutexUnlock( s_mutex )
   endif

   return lFirstCall

init procedure once_init()

   /* set workareas local to thread */
   set workarea private
   /* initialize mutex in hb_threadOnce() */
   hb_threadOnce()
   /* initialize error object to reduce to chance for possible crash
    * when two threads try to create new error class simultaneously.
    * xHarbour does not have any protection against such situation
    */
   ErrorNew()

   return

#endif /* __XHARBOUR__ */


#ifdef __XPP__

#ifdef __HARBOUR__
   /* for testing. Harbour also can use xBase++ API in this code */
   #include "hbclass.ch"
#endif

init procedure once_init()
   /* initialize sync object in hb_threadOnce() */
   hb_threadOnce()
   return

class Notifier
   protected:
      var aQueue
      var oSignal
   exported:
      method init
      sync method notify
      sync method subscribe
endclass

method Notifier:init

   ::aQueue := {}
   ::oSignal := Signal():new()

   return self

method Notifier:notify( xValue )

   AAdd( ::aQueue, xValue )
   ::oSignal:signal()

   return self

method Notifier:subscribe()

   local xResult

   while Len( ::aQueue ) == 0
      ::oSignal:wait()
   enddo
   xResult := ::aQueue[ 1 ]
   ADel( ::aQueue, 1 )
   ASize( ::aQueue, Len( ::aQueue ) - 1 )

   return xResult


static function hb_mutexSubscribe( mtx, nTimeOut, xResult )

   /* Ignore timeout - it's not used in this test */
   xResult := mtx:subscribe()

   return .t.

static function hb_mutexNotify( mtx, xValue )
   return mtx:notify( xValue )

static function hb_mutexCreate()
   return Notifier():new()


class Once
   exported:
      sync method onceDo
endclass

method Once:onceDo( xOnceControl, bAction )

   local lFirstCall := .f.

   if xOnceControl == nil
      if bAction != nil
         Eval( bAction )
      endif
      xOnceControl := .t.
      lFirstCall := .t.
   endif

   return lFirstCall

static function hb_threadOnce( xOnceControl, bAction )

   static s_oObject := nil

   if s_oObject == nil
      s_oObject := Once():new()
   endif

   return s_oObject:onceDo( @xOnceControl, bAction )

static function hb_threadStart( cFunc, xPar1, xPar2, xPar3 )

   local oThread

   oThread := Thread():new()
   oThread:start( cFunc, xPar1, xPar2, xPar3 )

   return oThread

static function hb_threadJoin( oThread, xResult )

   oThread:synchronize( 0 )
   xResult := oThread:result

   return .t.

static function hb_threadWaitForAll( aThreads )

   ThreadWaitAll( aThreads )

   return nil

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
function cursesinit()
   return nil
#endif

#endif
