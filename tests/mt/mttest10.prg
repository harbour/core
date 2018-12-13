/*
 * demonstration/test code for using independent console window in
 *    different thread. It needs GT driver which supports such functionality.
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
 */

#include "inkey.ch"

#ifdef __PLATFORM__WINDOWS
   REQUEST HB_GT_WVT_DEFAULT
   #define THREAD_GT hb_gtVersion()
#else
   REQUEST HB_GT_STD_DEFAULT
   #define THREAD_GT "XWC"
#endif

proc main( cGT )
   local i, aThreads

   if ! hb_mtvm()
      ? "This program needs HVM with MT support"
      quit
   endif

   if empty( cGT )
      cGT := THREAD_GT
   endif

   if  cGT == "QTC" .and. ! cGT == hb_gtVersion()
      /* QTC have to be initialized in main thread */
      hb_gtReload( cGT )
   endif

   ? "Starting threads..."
   aThreads := {}
   for i := 1 to 3
      aadd( aThreads, hb_threadStart( @thFunc(), cGT ) )
      ? i, "=>", atail( aThreads )
   next

   ? "Waiting for threads"
   while inkey() != K_ESC
      if hb_threadWait( aThreads, 0.1, .T. ) == len( aThreads )
         wait
         exit
      endif
      ?? "."
   enddo
return

proc thFunc( cGT )
   /* allocate own GT driver */
   hb_gtReload( cGT )
   if ! dbExists( "test" ) .and. dbExists( "../test" )
      use ../test shared
   else
      use test shared
   endif
   browse()
return
