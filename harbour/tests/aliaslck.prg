/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for file lock synchronization between
 *    aliased work areas
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

proc main()
   local cFile := "_tst"

   if !dbExists( cFile )
      dbCreate( cFile, { { "F", "C", 10, 0 } } )
      use _tst exclusive
      while lastRec() < 100
         dbAppend()
      enddo
      close
   endif
   ? "Test exclusive access"
   select 1
   use _tst exclusive alias tst1
   ? "neterr->", netErr(), "used->", used(), "alias->", alias()
   select 2
   use _tst exclusive alias tst2
   ? "neterr->", netErr(), "used->", used(), "alias->", alias()
   if used()
      ? "Error, exclusive mode in aliased work areas does not work"
   else
      ? "OK"
   endif
   dbCloseAll()
   wait
   ?
   ? "Test shared access"
   select 1
   use _tst shared alias tst1
   ? "neterr->", netErr(), "used->", used(), "alias->", alias()
   setLock(2)
   setLock(4)
   setLock(6)
   setLock(7)
   dspLock()
   select 2
   use _tst shared alias tst2
   ? "neterr->", netErr(), "used->", used(), "alias->", alias()
   setLock(1)
   setLock(2)
   setLock(3)
   setLock(4)
   setLock(5)
   setLock(6)
   setLock(7)
   setLock(8)
   setLock(9)
   dspLock()
   wait
   ?
   select 1
   ? "unlock all in", alias()
   dbUnLock()
   dspLock()
   select 2
   ? "lock 10 records in", alias()
   setLock(1)
   setLock(2)
   setLock(3)
   setLock(4)
   setLock(5)
   setLock(6)
   setLock(7)
   setLock(8)
   setLock(9)
   dspLock()
   wait
   ?
   ? "unlock 3, 5, 9 in", alias()
   dbRUnLock(3)
   dbRUnLock(5)
   dbRUnLock(9)
   dspLock()
   select 1
   ? "lock 10 records in", alias()
   setLock(1)
   setLock(2)
   setLock(3)
   setLock(4)
   setLock(5)
   setLock(6)
   setLock(7)
   setLock(8)
   setLock(9)
   dspLock()
   wait
   dbCloseAll()
   hb_dbDrop( cFile )
return

proc setLock( n )
   ? "locking record: " + hb_ntos( n ) + " ->", dbRLock( n )
return

proc dspLock()
   local n
   ? alias(), "active locks:"
   for each n in dbRLockList()
      ?? "", hb_ntos( n )
   next
return
