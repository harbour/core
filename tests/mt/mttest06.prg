/*
 * Harbour Project source code:
 *    demonstration/test code for using memvars in threads
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#define N_THREADS 5
#define N_COUNT   1000000
#define N_INIT    100

memvar m_var

proc main()
   local aThreads := {}, i, nSum

   ? Version()
   private m_var := N_INIT * 25
   ? "Starting threads:", ""
   for i :=1 to N_THREADS
      aadd( aThreads, hb_threadStart( @thFunc() ) )
      ?? "<" + hb_ntos( i ) + ">"
   next
   ? "Waiting for threads..."
   nSum := 0
   aEval( aThreads, {| x | hb_threadJoin( x, @i ), nSum += i } )
   ? "Threads joined"
   ? "Sum of results:", nSum
   ? "     should be:", N_THREADS * ( N_INIT + N_COUNT ), ;
      iif( nSum == N_THREADS * ( N_INIT + N_COUNT ), "OK", "ERROR" )
return

static func thFunc()
   private m_var := N_INIT - 10
return do()

static func do()
   local i
   private m_var := m_var + 10
   for i := 1 to N_COUNT
      ++m_var
   next
return m_var
