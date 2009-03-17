/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    HVM speed test program
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
 *
 */

#define N_TESTS 56
#define N_LOOPS 1000000
#define ARR_LEN 16

#ifndef __HARBOUR__
   #ifndef __XPP__
      #ifndef __CLIP__
        #define __CLIPPER__
      #endif
   #endif
#endif


#ifdef __CLIPPER__
   /* Clipper does not support multithreading */
   #ifndef __ST__
      #define __ST__
   #endif
#endif

#ifdef __XPP__
   #define __NO_OBJ_ARRAY__
   /* xBase++ version for MT performance testing is not ready yet */
   #ifndef __ST__
      #define __ST__
   #endif
#endif

#ifdef __CLIP__
   #define __NO_OBJ_ARRAY__
   /* CLIP version for MT performance testing is not ready yet */
   #ifndef __ST__
      #define __ST__
   #endif
#endif

#ifdef __XHARBOUR__
   /* By default build xHarbour binaries without MT support
    * xHarbour needs separated version for MT and ST mode
    * because standard MT functions are not available in
    * ST libraries.
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
#endif

/* by default create MT version */
#ifndef __MT__
   #ifndef __ST__
      #define __MT__
   #endif
#endif


#command ? => outstd(EOL)
#command ? <xx,...> => outstd(EOL);outstd(<xx>)
#command ?? <xx,...> => outstd(<xx>)

#ifdef __HARBOUR__
   #define EOL hb_OSNewLine()
#else
   #ifndef __CLIP__
      #xtranslate secondsCPU() => seconds()
   #endif
   #ifndef EOL
      #define EOL chr(10)
   #endif
#endif

#xcommand TEST <testfunc>           ;
          [ WITH <locals,...> ]     ;
          [ STATIC <statics,...> ]  ;
          [ FIELD  <fields,...> ]   ;
          [ MEMVAR <memvars,...> ]  ;
          [ PRIVATE <privates,...> ];
          [ PUBLIC <publics,...> ]  ;
          [ INIT <init> ]           ;
          [ EXIT <exit> ]           ;
          [ INFO <info> ]           ;
          CODE [ <testExp,...> ] => ;
   func <testfunc> ;                ;
      local time, i:=nil, x:=nil ;  ;
      [ local <locals> ; ]          ;
      [ static <statics> ; ]        ;
      [ field <fields> ; ]          ;
      [ memvar <memvars> ; ]        ;
      [ private <privates> ; ]      ;
      [ public <publics> ; ]        ;
      [ <init> ; ]                  ;
      time := secondscpu() ;        ;
      for i:=1 to N_LOOPS ;         ;
         [ ( <testExp> ) ] ;        ;
      next ;                        ;
      time := secondscpu() - time ; ;
      [ <exit> ; ]                  ;
   return { procname() + ": " + iif( <.info.>, <(info)>, #<testExp> ), time }

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

   lSyntax := lScale := .f.
   cMemTests := "030 031 023 025 027 041 042 044 053 054 019 022 032 033 055 056 "
   cExclude := ""
   nMT := 0
   for j := 1 to len( aParams )
      cParam := lower( aParams[ j ] )
      if cParam = "--thread"
         if substr( cParam, 9, 1 ) == "="
            if isdigit( substr( cParam, 10, 1 ) )
               nMT := val( substr( cParam, 10 ) )
            elseif substr( cParam, 10 ) == "all"
               nMT := -1
            else
               lSyntax = .t.
            endif
         elseif empty( substr( cParam, 9 ) )
            nMT := -1
         else
            lSyntax = .t.
         endif
      elseif cParam = "--exclude="
         if substr( cParam, 11 ) == "mem"
            cExclude += cMemTests
         else
            cExclude += strtran( strtran( strtran( substr( cParam, 11 ), ;
                        ".", " " ), ".", " " ), "/", " " ) + " "
         endif
      elseif cParam = "--only="
         cExclude := ""
         if substr( cParam, 8 ) == "mem"
            cParam := cMemTests
         endif
         for i := 1 to N_TESTS
            if !strzero( i, 3 ) $ cParam
               cExclude += strzero( i, 3 ) + " "
            endif
         next
      elseif cParam = "--scale"
         lScale := .t.
      else
         lSyntax = .t.
      endif
      if lSyntax
         ? "Unknown option:", cParam
         ? "syntax: speedtst [--thread[=<num>]] [--only=<test(s)>] [--exclude=<test(s)>]"
         ?
         return
      endif
   next
   test( nMT, cExclude, lScale )
return


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

TEST t023 CODE eval( { || i % ARR_LEN } )

TEST t024 WITH bc := { || i % ARR_LEN } ;
          INFO eval( bc := { || i % ARR_LEN } ) ;
          CODE eval( bc )

TEST t025 CODE eval( { |x| x % ARR_LEN }, i )

TEST t026 WITH bc := { |x| x % ARR_LEN } ;
          INFO eval( bc := { |x| x % ARR_LEN }, i ) ;
          CODE eval( bc, i )

TEST t027 CODE eval( { |x| f1( x ) }, i )

TEST t028 WITH bc := { |x| f1( x ) } ;
          INFO eval( bc := { |x| f1( x ) }, i ) ;
          CODE eval( bc, i )

TEST t029 WITH bc := mkBlock( "{ |x| f1( x ) }" ) ;
          INFO eval( bc := &("{ |x| f1( x ) }"), i ) ;
          CODE eval( bc, i )

TEST t030 CODE x := &( 'f1(' + str(i) + ')' )

TEST t031 WITH bc CODE bc := &( '{|x|f1(x)}' ), eval( bc, i )

TEST t032 CODE x := valtype( x ) +  valtype( i )

TEST t033 WITH a := afill( array( ARR_LEN ), ;
                           stuff( dtos( date() ), 7, 0, "." ) ) ;
          CODE x := strzero( i % 100, 2 ) $ a[ i % ARR_LEN + 1 ]

TEST t034 WITH a := array( ARR_LEN ), s := dtos( date() ) ;
          INIT aeval( a, { |x,i| a[i] := left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] == s

TEST t035 WITH a := array( ARR_LEN ), s := dtos( date() ) ;
          INIT aeval( a, { |x,i| a[i] := left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] = s

TEST t036 WITH a := array( ARR_LEN ), s := dtos( date() ) ;
          INIT aeval( a, { |x,i| a[i] := left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] >= s

TEST t037 WITH a := array( ARR_LEN ), s := dtos( date() ) ;
          INIT aeval( a, { |x,i| a[i] := left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] <= s

TEST t038 WITH a := array( ARR_LEN ), s := dtos( date() ) ;
          INIT aeval( a, { |x,i| a[i] := left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] < s

TEST t039 WITH a := array( ARR_LEN ), s := dtos( date() ) ;
          INIT aeval( a, { |x,i| a[i] := left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] > s

TEST t040 WITH a := array( ARR_LEN ) ;
          INIT aeval( a, { |x,i| a[i] := i, x } ) ;
          CODE ascan( a, i % ARR_LEN )

TEST t041 WITH a := array( ARR_LEN ) ;
          INIT aeval( a, { |x,i| a[i] := i, x } ) ;
          CODE ascan( a, { |x| x == i % ARR_LEN } )

TEST t042 WITH a := {}, a2 := { 1, 2, 3 }, bc := { |x| f1(x) }, ;
               s := dtos( date() ), s2 := "static text" ;
          CODE iif( i%1000==0, a:={}, ) , aadd(a,{i,1,.t.,s,s2,a2,bc})

TEST t043 WITH a := {} CODE x := a

TEST t044 CODE x := {}

TEST t045 CODE f0()

TEST t046 CODE f1( i )

TEST t047 WITH c := dtos( date() ) ;
          INFO f2( c[1...8] ) ;
          CODE f2( c )

TEST t048 WITH c := repl( dtos( date() ), 5000 ) ;
          INFO f2( c[1...40000] ) ;
          CODE f2( c )

TEST t049 WITH c := repl( dtos( date() ), 5000 ) ;
          INFO f2( @c[1...40000] ) ;
          CODE f2( c )

TEST t050 WITH c := repl( dtos( date() ),5000 ), c2 ;
          INFO "f2( @c[1...40000] ), c2 := c" ;
          CODE f2( @c ), c2 := c

TEST t051 WITH a := {}, a2 := { 1, 2, 3 }, bc := { |x| f1(x) }, ;
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
   local xJob
   while .T.
      hb_mutexSubscribe( mtxJobs,, @xJob )
      if xJob == NIL
         exit
      endif
      aResults[ xJob ] := &( "t" + strzero( xJob, 3 ) )()
   enddo
return nil

function thTestScale( mtxJobs, mtxResults )
   local xJob
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
#endif

//? "Startup loop to increase CPU clock..."
//x := seconds() + 5; while x > seconds(); enddo

#ifdef __MT__
if !hb_mtvm()
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
   ?? hb_compiler()
#endif

if lScale .and. nMT < 1
   nMT := 1
endif

? "THREADS:", iif( nMT < 0, "all->" + ltrim( str( N_TESTS ) ), ltrim( str( nMT ) ) )
? "N_LOOPS:", ltrim( str( N_LOOPS ) )
if !empty( cExclude )
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
nTimes := secondsCPU()

nTimeTotST := nTimeTotMT := 0

#ifdef __MT__
   if lScale
      mtxJobs := hb_mutexCreate()
      mtxResults := hb_mutexCreate()
      for i:=1 to nMT
         hb_threadStart( "thTestScale", mtxJobs, mtxResults )
      next
      for i:=1 to N_TESTS
         cTest := strzero( i, 3 )
         if !cTest $ cExclude

            /* linear execution */
            nTimeST := seconds()
            for j:=1 to nMT
               hb_mutexNotify( mtxJobs, i )
               hb_mutexSubscribe( mtxResults,, @x )
               cTest := x[1]
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
               cTest := x[1]
            next
            nTimeMT := seconds() - nTimeMT
            nTimeTotMT += nTimeMT

            ? dsp_scaleResult( cTest, nTimeST, nTimeMT, nMT, nLoopOverHead )
         endif

      next
      for i:=1 to nMT
         hb_mutexNotify( mtxJobs, NIL )
      next
      hb_threadWaitForAll()
   elseif nMT < 0
      aThreads := array( N_TESTS )
      for i:=1 to N_TESTS
         cNum := strzero( i, 3 )
         if !cNum $ cExclude
            aThreads[ i ] := hb_threadStart( "t" + cNum )
         endif
      next
      for i:=1 to N_TESTS
         if aThreads[ i ] != NIL .and. hb_threadJoin( aThreads[ i ], @x )
            ? dsp_result( x, nLoopOverHead )
         endif
      next
   elseif nMT > 0
      aResults := array( N_TESTS )
      mtxJobs := hb_mutexCreate()
      for i:=1 to nMT
         hb_threadStart( "thTest", mtxJobs, aResults )
      next
      for i:=1 to N_TESTS
         if !strzero( i, 3 ) $ cExclude
            hb_mutexNotify( mtxJobs, i )
         endif
      next
      for i:=1 to nMT
         hb_mutexNotify( mtxJobs, NIL )
      next
      hb_threadWaitForAll()
      for i:=1 to N_TESTS
         if aResults[ i ] != NIL
            ? dsp_result( aResults[ i ], nLoopOverHead )
         endif
      next
      mtxJobs := NIL
   else
      for i:=1 to N_TESTS
         cNum := strzero( i, 3 )
         if !cNum $ cExclude
            ? dsp_result( &( "t" + cNum )(), nLoopOverHead )
         endif
      next
   endif
#else
   for i:=1 to N_TESTS
      cNum := strzero( i, 3 )
      if !cNum $ cExclude
         ? dsp_result( &( "t" + cNum )(), nLoopOverHead )
      endif
   next
#endif

nTimes := secondsCPU() - nTimes
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
   return padr( "[ " + left( aResult[1], 56 ) + " ]", 60, "." ) + ;
          strtran( str( max( aResult[2] - nLoopOverHead, 0 ), 8, 2 ), " ", "." )

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
   replace F_C with dtos(date())
   replace F_N with 112345.67
   replace F_D with date()
   close
return

static proc remove_db()
   ferase( TMP_FILE )
return

static proc close_db()
   close
return

static proc use_dbsh()
   use TMP_FILE shared
return


#ifdef __CLIPPER__
   function hb_mtvm()
   return .f.                 /* Clipper does not support MT */
#endif
#ifdef __CLIP__
   function hb_mtvm()
   return .t.                 /* CLIP always uses VM with MT support */
#endif
#ifdef __XPP__
   function hb_mtvm()
   return .t.                 /* xBase++ always uses VM with MT support */
#endif
#ifdef __XHARBOUR__
   function hb_mtvm()
   return hb_multiThread()    /* check for MT support in xHarbour VM */
#endif


#ifndef __MT__

   /* trivial single thread version of once execution */
   function hb_threadOnce( xOnceControl, bAction )
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

   /* do not expect that this code will work with xHarbour.
    * xHarbour has many race conditions which are exploited quite fast
    * on real multi CPU machines so it crashes in different places :-(
    * probably this code should be forwared to xHarbour developers as
    * some type of MT test
    */

   /* this define is only to test if emulation function works
    * without running real test which causes that xHarbour crashes
    */
   //#define _DUMY_XHB_TEST_


   function hb_mutexSubscribe( mtx, nTimeOut, xSubscribed )
      local lSubscribed
      if valtype( nTimeOut ) == "N"
         nTimeOut := round( nTimeOut * 1000, 0 )
         xSubscribed := Subscribe( mtx, nTimeOut, @lSubscribed )
      else
         xSubscribed := Subscribe( mtx )
         lSubscribed := .t.
      endif
   return lSubscribed

   function hb_mutexNotify( mtx, xValue )
      Notify( mtx, xValue )
   return nil

   /* in xHarbour there is race condition in JoinThread() which
    * fails if thread end before we call it so we cannot use it :-(
    * this code tries to simulate it and also add support for thread
    * return value
    */

   function hb_threadStart( ... )
      local thId
      thId := StartThread( @threadFirstFunc(), hb_aParams() )
      /* Just like in JoinThread() the same race condition exists in
       * GetThreadId() so we will use HVM thread numbers internally
       */
#ifdef _DUMY_XHB_TEST_
   return val( substr( hb_aParams()[1], 2 ) )
#else
   return GetThreadId( thId )
#endif

   function hb_threadJoin( thId, xResult )
      xResult := results( thId )
   return .t.

   static function threadFirstFunc( aParams )
      local xResult
#ifdef _DUMY_XHB_TEST_
      xResult := { "skipped test " + aParams[1], val( substr( aParams[1], 2 ) ) + 0.99 }
      results( val( substr( aParams[1], 2 ) ), xResult )
#else
      xResult := hb_execFromArray( aParams )
      results( GetThreadId(), xResult )
#endif
   return nil

   static function results( nThread, xResult )
      static s_aResults
      static s_mutex
      if s_aResults == nil
         s_aResults := HSetAutoAdd( hash(), .t. )
         s_mutex := hb_mutexCreate()
      endif
      if pcount() < 2
         while ! nThread $ s_aResults
            Subscribe( s_mutex, 1000 )
         enddo
         xResult := s_aResults[ nThread ]
      else
         s_aResults[ nThread ] := xResult
         /* We cannot use NotifyAll() here because it will create
          * race condition. In this program only one thread join
          * results so we can use simple Notify() as workaround
          */
         //NotifyAll( s_mutex )
         Notify( s_mutex )
      endif
   return xResult

   function hb_threadWaitForAll()
      WaitForThreads()
   return nil

   function hb_threadOnce( xOnceControl, bAction )
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
      /* initialize mutex in hb_trheadDoOnce() */
      hb_threadOnce()
      /* initialize error object to reduce possible crashes when two
       * threads will try to create new error class simultaneously.
       * xHarbour does not have any protection against such situation
       */
      errorNew()
   return

#endif /* __XHARBOUR__ */


/*
   function hb_threadStart( cFunc, xPar1, xPar2, xPar3 )
   return nil

   function hb_threadJoin( thId, xResult )
   return nil

   function hb_mutexCreate()
   return nil

   function hb_mutexSubscribe()
   return nil
   function hb_mutexLock()
   return nil
   function hb_mutexUnlock()
   return nil
   function hb_mutexNotify()
   return nil
   function hb_threadWaitForAll()
   return nil
   function hb_mtvm()
   return .f.
*/

#endif
