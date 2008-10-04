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


#define N_TESTS 53
#define N_LOOPS 1000000
#define ARR_LEN 16

#command ? => outstd(EOL)
#command ? <xx,...> => outstd(EOL);outstd(<xx>)

#include "common.ch"

#ifdef __HARBOUR__
    #define EOL hb_OSNewLine()
#else
   #define HB_SYMBOL_UNUSED( symbol )  ( ( symbol ) )
   #ifndef __CLIP__
      #xtranslate secondsCPU() => seconds()
   #endif
   #ifndef EOL
      #define EOL chr(10)
   #endif
#endif

#xcommand _( [<cmds,...>] ) => [<cmds>]

#xcommand TEST <testfunc>           ;
          [ WITH <locals,...> ]     ;
          [ INIT <init> ]           ;
          [ EXIT <exit> ]           ;
          [ INFO <info> ]           ;
          CODE [<*testExp*>] =>     ;
   func <testfunc> ;                ;
      local time, i, x := nil ;     ;
      [ local <locals> ; ]          ;
      [ <init> ; ]                  ;
      time := secondscpu() ;        ;
      for i:=1 to N_LOOPS ;         ;
         [<testExp>;]               ;
      next ;                        ;
      time := secondscpu() - time ; ;
      [ <exit> ; ]                  ;
   return { iif( <.info.>, <(info)>, #<testExp> ), time }


proc main( par1, par2 )
   test( par1, par2 )
return


#ifdef __XHARBOUR__

   #xtranslate hb_mtvm()                  => hb_multiThread()

#ifndef __ST__

   /* do not expect that this code will work with xHarbour.
    * xHarbour has many race conditions which are exploited quite fast
    * on real multi CPU machines so it crashes in different places :-(
    * probably this code should be forwared to xHarbour developers as
    * some type of MT test
    */

   static function results( nTest, xResult )
      static s_aResults[ N_TESTS + 1 ]
      if xResult == NIL
         xResult := s_aResults[ nTest ]
      else
         s_aResults[ nTest ] := xResult
      endif
   return xResult

   /* I used function wrappers to simulate thread join which can
    * return thread results
    */
   static function do_test( cFunc )
      local x
      ? "starting: " + cFunc + "()"
      // if you set .f. then tests will be skipped but you can check
      // if this test code is executed because it greatly reduces
      // the race conditions inside xHarbour HVM
      if .t.
         x := &cFunc()
         //dsp_result( x )
      else
         x := { "skipped test " + cFunc, val( substr( cFunc, 2 ) ) + 0.99 }
      endif
      results( val( substr( cFunc, 2 ) ), x )
   return nil

   function hb_threadStart( cFunc )
   return StartThread( @do_test(), cFunc )

   function hb_threadJoin( thId, xResult )
      static s_n := 0
      local lOK
      /* in xHarbour there is race condition in JoinThread() which
       * fails if thread end before we call it so we cannot use it :-(
       */
      //lOK := JoinThread( thId )
      lOK := .t.
      if s_n == 0
         HB_SYMBOL_UNUSED( thId )
         WaitForThreads()
      endif
      xResult := results( ++s_n )
   return lOK

#else

   function hb_threadStart()
   return nil
   function hb_threadJoin()
   return nil

#endif

#endif


TEST t000 INFO "empty loop overhead" CODE

TEST t001 WITH L_C:=dtos(date()) CODE x := L_C

TEST t002 WITH L_N:=112345.67    CODE x := L_N

TEST t003 WITH L_D:=date()       CODE x := L_D

TEST t004 INIT _( static S_C ) INIT S_C:=dtos(date()) CODE x := S_C

TEST t005 INIT _( static S_N ) INIT S_N:=112345.67    CODE x := S_N

TEST t006 INIT _( static S_D ) INIT S_D:=date()       CODE x := S_D

TEST t007 INIT _( memvar M_C ) INIT _( private M_C:=dtos(date()) ) ;
          CODE x := M_C

TEST t008 INIT _( memvar M_N ) INIT _( private M_N:=112345.67 ) ;
          CODE x := M_N

TEST t009 INIT _( memvar M_D ) INIT _( private M_D:=date() ) ;
          CODE x := M_D

TEST t010 INIT _( memvar P_C ) INIT _( public P_C:=dtos(date()) ) ;
          CODE x := P_C

TEST t011 INIT _( memvar P_N ) INIT _( public P_N:=112345.67 ) ;
          CODE x := P_N

TEST t012 INIT _( memvar P_D ) INIT _( public P_D:=date() ) ;
          CODE x := P_D

TEST t013 INIT _( field F_C ) INIT use_dbsh() EXIT close_db() ;
          CODE x := F_C

TEST t014 INIT _( field F_N ) INIT use_dbsh() EXIT close_db() ;
          CODE x := F_N

TEST t015 INIT _( field F_D ) INIT use_dbsh() EXIT close_db() ;
          CODE x := F_D

TEST t016 WITH o := errorNew() CODE x := o:GenCode

TEST t017 WITH o := errorNew() CODE x := o[8]

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

TEST t029 CODE x := &( "f1(" + str(i) + ")" )

TEST t030 WITH bc CODE bc := &( "{|x|f1(x)}" ); eval( bc, i )

TEST t031 CODE x := valtype( x ) +  valtype( i )

TEST t032 WITH a := afill( array( ARR_LEN ), ;
                           stuff( dtos( date() ), 7, 0, "." ) ) ;
          CODE x := strzero( i % 100, 2 ) $ a[ i % ARR_LEN + 1 ]

TEST t033 WITH a := array( ARR_LEN ), s := dtos( date() ) ;
          INIT aeval( a, { |x,i| a[i] := left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] == s

TEST t034 WITH a := array( ARR_LEN ), s := dtos( date() ) ;
          INIT aeval( a, { |x,i| a[i] := left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] = s

TEST t035 WITH a := array( ARR_LEN ), s := dtos( date() ) ;
          INIT aeval( a, { |x,i| a[i] := left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] >= s

TEST t036 WITH a := array( ARR_LEN ), s := dtos( date() ) ;
          INIT aeval( a, { |x,i| a[i] := left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] <= s

TEST t037 WITH a := array( ARR_LEN ), s := dtos( date() ) ;
          INIT aeval( a, { |x,i| a[i] := left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] < s

TEST t038 WITH a := array( ARR_LEN ), s := dtos( date() ) ;
          INIT aeval( a, { |x,i| a[i] := left( s + s, i ), x } ) ;
          CODE x := a[ i % ARR_LEN + 1 ] > s

TEST t039 WITH a := array( ARR_LEN ) ;
          INIT aeval( a, { |x,i| a[i] := i, x } ) ;
          CODE ascan( a, i % ARR_LEN )

TEST t040 WITH a := array( ARR_LEN ) ;
          INIT aeval( a, { |x,i| a[i] := i, x } ) ;
          CODE ascan( a, { |x| x == i % ARR_LEN } )

TEST t041 WITH a := {}, a2 := { 1, 2, 3 }, bc := { |x| f1(x) }, ;
               s := dtos( date() ), s2 := "static text" ;
          CODE if i%1000==0;a:={};end; aadd(a,{i,1,.t.,s,s2,a2,bc})

TEST t042 WITH a := {} CODE x := a

TEST t043 CODE x := {}

TEST t044 CODE f0()

TEST t045 CODE f1( i )

TEST t046 WITH c := dtos( date() ) ;
          INFO f2( c[1...8] ) ;
          CODE f2( c )

TEST t047 WITH c := repl( dtos( date() ), 5000 ) ;
          INFO f2( c[1...40000] ) ;
          CODE f2( c )

TEST t048 WITH c := repl( dtos( date() ), 5000 ) ;
          INFO f2( @c[1...40000] ) ;
          CODE f2( c )

TEST t049 WITH c := repl( dtos( date() ),5000 ), c2 ;
          INFO "f2( @c[1...40000] ), c2 := c" ;
          CODE f2( @c ); c2 := c

TEST t050 WITH a := {}, a2 := { 1, 2, 3 }, bc := { |x| f1(x) }, ;
               s := dtos( date() ), s2 := "static text", n := 1.23 ;
          CODE f3( a, a2, s, i, s2, bc, i, n, x )

TEST t051 WITH a := { 1, 2, 3 } CODE f2( a )

TEST t052 CODE x := f4()

TEST t053 CODE x := f5()


proc test( par1, par2 )
local nLoopOverHead, nTimes, nSeconds, cExclude, cNum, lMT, x, i, aThreads:={}

create_db()

lMt := .f.
if !empty( par1 ) .and. lower( par1 ) = "--exclude="
   cExclude := substr( par1, 11 )
   par1 := par2
elseif !empty( par2 ) .and. lower( par2 ) = "--exclude="
   cExclude := substr( par2, 11 )
else
   cExclude := ""
endif
if lower( cExclude ) == "mem"
   cExclude := "029.030.023.025.027.040.041.043.052.053.019.022.031.032"
endif

#ifdef __HARBOUR__
#include "hbmemory.ch"
if MEMORY( HB_MEM_USEDMAX ) != 0
   ? "Warning !!! Memory statistic enabled."
   ?
endif
#endif

//? "Startup loop to increase CPU clock..."
//x := seconds() + 5; while x > seconds(); enddo

? date(), time(), os()
#ifdef __HARBOUR__
   lMT := !empty( par1 ) .and. hb_mtvm()
   ? version() + iif( hb_mtvm(), " (MT)" + iif( lMT, "+", "" ), "" ), ;
     hb_compiler()
#else
   ? version()
#endif
? "N_LOOPS =", N_LOOPS

x :=t000()
? dsp_result( x, 0 )
nLoopOverHead := x[2]

? replicate("=",68)

nSeconds := seconds()
nTimes := secondsCPU()

#ifdef __HARBOUR__
   if lMT
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

? replicate("=",68)
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
HB_SYMBOL_UNUSED( x )
return nil

function f3(a,b,c,d,e,f,g,h,i)
HB_SYMBOL_UNUSED( a )
HB_SYMBOL_UNUSED( b )
HB_SYMBOL_UNUSED( c )
HB_SYMBOL_UNUSED( d )
HB_SYMBOL_UNUSED( e )
HB_SYMBOL_UNUSED( f )
HB_SYMBOL_UNUSED( g )
HB_SYMBOL_UNUSED( h )
HB_SYMBOL_UNUSED( i )
return nil

function f4()
return space(4000)

function f5()
return space(5)


static func dsp_result( aResult, nLoopOverHead )
   return padr( "[ " + left( aResult[1], 56 ) + " ]", 60, "." ) + ;
          strtran( str( max( aResult[2] - nLoopOverHead, 0 ), 8, 2 ), " ", "." )


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
