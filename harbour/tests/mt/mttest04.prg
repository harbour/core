/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for modifying simple variable by different
 *    threads with and without protection and also complex variables
 *    like array without protection. Because each thread will access
 *    different item in this array then it should be safe and our HVM
 *    should make necessary internal protections automatically.
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#ifdef __XHARBOUR__
   #xtranslate hb_threadStart( <x,...> ) => StartThread( <x> )
   #xtranslate hb_threadJoin( <x,...> ) => JoinThread( <x> )
#endif

#define N_THREADS 5

static s_nVar1 := 0
static s_nVar2 := 0
static s_aCounters
static s_hMutex

proc main()
   local aThreads, i, lEnd, nSum
   ? Version()
   ? "Main start"
   s_aCounters := array( N_THREADS )
   aFill( s_aCounters, 0 )
   aThreads := {}
   s_hMutex := hb_mutexCreate()
   lEnd := .f.
   ? "Starting threads: "
   for i := 1 to N_THREADS
      aadd( aThreads, hb_threadStart( @thFunc(), i, @lEnd ) )
      ?? "<" + ltrim( str( i ) ) + ">"
   next
   ? "Wait 5 seconds or hit any key..."
   inkey( 5 )
   lEnd := .t.
   ? "Waiting for threads..."
   aEval( aThreads, {| x | hb_threadJoin( x ) } )
   ? "Threads joined"
   nSum := 0
   aEval( s_aCounters, {| x | nSum += x } )
   ? "Sum of thread local counters:", nSum
   ? "Protected item result.......:", s_nVar2, ;
     iif( nSum == s_nVar2, "OK", "ERROR" )
   ? "Unprotected item result.....:", s_nVar1, "*"
   ? " * - can be different then local sum on real multi-CPU systems"
   ? "End of main"
return

proc thFunc( nThread, lEnd )
   while ! lEnd
      s_nVar1++
      hb_mutexLock( s_hMutex )
      s_nVar2++
      hb_mutexUnLock( s_hMutex )
      s_aCounters[ nThread ]++
   enddo
return
