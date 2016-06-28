/*
 * Demonstration/test code for using independent console window in
 * different thread. It needs GT driver which supports such functionality.
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
 */

#include "inkey.ch"

#ifdef __PLATFORM__WINDOWS
   #if ! defined( __HBSCRIPT__HBSHELL )
      request HB_GT_WVT_DEFAULT
   #endif
   #define THREAD_GT  hb_gtVersion()
#else
   request HB_GT_STD_DEFAULT
   #define THREAD_GT  "XWC"
#endif

procedure Main( cGT )

   local i, aThreads

   if ! hb_mtvm()
      ? "This program needs HVM with MT support"
      quit
   endif

   if Empty( cGT )
      cGT := THREAD_GT
   endif

   if cGT == "QTC" .and. ! cGT == hb_gtVersion()
      /* QTC have to be initialized in main thread */
      hb_gtReload( cGT )
   endif

   ? "Starting threads..."
   aThreads := {}
   for i := 1 to 3
      AAdd( aThreads, hb_threadStart( @thFunc(), cGT ) )
      ? i, "=>", ATail( aThreads )
   next

   ? "Waiting for threads"
   while hb_keyStd( Inkey() ) != K_ESC
      if hb_threadWait( aThreads, 0.1, .T. ) == Len( aThreads )
         wait
         exit
      endif
      ?? "."
   enddo

   return

static procedure thFunc( cGT )

   hb_gtReload( cGT )  /* allocate own GT driver */

   if ! dbExists( "test.dbf" ) .and. dbExists( hb_DirSepToOS( "../test.dbf" ) )
      use ( hb_DirSepToOS( "../test.dbf" ) ) shared
   else
      use test.dbf shared
   endif

   Browse()

   return
