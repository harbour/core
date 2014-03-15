/*
 * Harbour Project source code:
 *    demonstration/test code for using mutexes to send/receive
 *    messages between threads to synchronize divided jobs between
 *    threads.
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#define N_THREADS 5
#define N_JOBS    10000

static s_aCounters
static s_mtxJobs
static s_mtxResults

procedure main()
   local aThreads, aResults, i, nDigit, nSum, nExpected

   ? Version()
   ? "Main start"

   s_aCounters := Array( N_THREADS )
   AFill( s_aCounters, 0 )
   aThreads := {}
   aResults := {}
   s_mtxJobs := hb_mutexCreate()
   s_mtxResults := hb_mutexCreate()

   ? "Starting threads:", ""
   for i := 1 to N_THREADS
      AAdd( aThreads, hb_threadStart( @thFunc() ) )
      ?? "<" + hb_ntos( i ) + ">"
   next

   ? "Sending jobs...", ""
   nDigit := 10
   for i := 1 to N_JOBS
      hb_mutexNotify( s_mtxJobs, nDigit )
      //?? "<" + hb_ntos( i ) + ">"
      nDigit++
   next

   ? "Sending terminate values..."
   for i := 1 to N_THREADS
      hb_mutexNotify( s_mtxJobs, NIL )
      ?? "<" + hb_ntos( i ) + ">"
   next

   ? "Collecting results...", ""
   for i := 1 to N_JOBS
      hb_mutexSubscribe( s_mtxResults,, @nDigit )
      //?? "<" + hb_ntos( i ) + ">"
      AAdd( aResults, nDigit )
   next

   ? "Waiting for threads..."
   AEval( aThreads, {| x | hb_threadJoin( x ) } )
   ? "Threads joined"

   nSum := 0
   for each nDigit in aResults
      nSum += nDigit
   next

   nSum := Round( nSum, 2 )
   nExpected := Round( ( 10 + 10 + N_JOBS - 1 ) / 2 / 3 * N_JOBS, 2 )

   if Round( nSum - nExpected, 2 ) == 0
      ? "OK, final sum:", hb_ntos( nSum )
   else
      ? "ERROR, final sum:", hb_ntos( nSum ), ;
        "expected:", hb_ntos( nExpected )
   endif
   ? "End of main"
   return

static procedure thFunc()
   local xJob, xResult
   while .T.
      hb_mutexSubscribe( s_mtxJobs,, @xJob )
      if xJob == NIL
         exit
      endif
      xResult := xJob / 3
      hb_mutexNotify( s_mtxResults, xResult )
   enddo
   return
