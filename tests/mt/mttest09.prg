/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for using the same work area in different
 *    threads. Please note that this program also works when compiled
 *    without thread support.
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

STATIC s_mainThreadID

proc main()
   field F1
   local thID, bResult

   s_mainThreadID := hb_threadSelf()
   /* create table */
   dbCreate("_tst", { { "F1", "C", 1, 0 } } )
   use _tst
   while lastRec() < 10000
      dbAppend()
      F1 := chr( recno() )
   enddo

   ? "main thread ID:", s_mainThreadID
   thID := hb_threadStart( @thFunc() )
   ? "current thread ID:", thID
   ? "work area in use, used() =>", used(), alias()
   WAIT "Press a key to detach work area"
   hb_dbDetach( , {|| countRecords( {|| F1 == "A" } ) } )
   ? "work area detached, used() =>", used(), alias()
   ? "we will make some other things now..."
   hb_idleSleep( 1 )
   ? "let's check the result"
   ? "request for work area"
   hb_dbRequest( , , @bResult, .T. )
   ? "work area atached, used() =>", used(), alias()
   ? "query result:", eval( bResult )
   close
   dbDrop("_tst")
   return

proc thFunc()
   local bQuery, xResult

   if hb_dbRequest( , , @bQuery, .T. )
      xResult := Eval( bQuery )
      hb_dbDetach( , {|| xResult } )
   endif
   return

static func countRecords( bFor )
   local nCount := 0
   dbGoTop()
   while ! eof()
      if eval( bFor )
         nCount ++
      endif
      dbSkip()
   enddo
   ? "!!! JOB DONE !!!" + iif( hb_threadSelf() == s_mainThreadID, ;
                               " (by main thread)", " (by child thread)" )
   return nCount
