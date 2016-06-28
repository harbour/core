/*
 * Demonstration/test code for using the same work area in different
 * threads. Please note that this program also works when compiled
 * without thread support.
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
 */

static s_mainThreadID

procedure Main()

   field F1

   local thID, bResult

   s_mainThreadID := hb_threadSelf()
   /* create table */
   dbCreate( "_tst.dbf", { { "F1", "C", 1, 0 } } )
   use _tst.dbf
   while LastRec() < 10000
      dbAppend()
      F1 := Chr( RecNo() )
   enddo

   ? "main thread ID:", s_mainThreadID
   thID := hb_threadStart( @thFunc() )
   ? "current thread ID:", thID
   ? "work area in use, Used() =>", Used(), Alias()
   WAIT "Press a key to detach work area"
   hb_dbDetach( , {|| countRecords( {|| F1 == "A" } ) } )
   ? "work area detached, Used() =>", Used(), Alias()
   ? "we will make some other things now..."
   hb_idleSleep( 1 )
   ? "let's check the result"
   ? "request for work area"
   hb_dbRequest( , , @bResult, .T. )
   ? "work area atached, Used() =>", Used(), Alias()
   ? "query result:", Eval( bResult )
   close
   hb_dbDrop( "_tst.dbf" )

   return

static procedure thFunc()

   local bQuery, xResult

   if hb_dbRequest( , , @bQuery, .T. )
      xResult := Eval( bQuery )
      hb_dbDetach( , {|| xResult } )
   endif

   return

static function countRecords( bFor )

   local nCount := 0

   dbGoTop()
   while ! Eof()
      if Eval( bFor )
         nCount ++
      endif
      dbSkip()
   enddo

   ? "!!! JOB DONE !!!", iif( hb_threadSelf() == s_mainThreadID, ;
                              "(by main thread)", "(by child thread)" )

   return nCount
