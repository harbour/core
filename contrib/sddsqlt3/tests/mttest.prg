/*
 * mttest.prg
 *
 * "stress" test for the THREAD:/GLOBAL: connection-scope prefixes added to
 * the SQLBASE SDD RDD layer (rddsql), exercised against the SQLite3 SDD
 * driver (sddsqlt3).
 *
 * Build in MT-mode:
 *    hbmk2 -mt mttest.prg
 *
 * TESTS
 *   A. THREAD: scope   -- many workers, each its own private connection
 *                         and private SQLite file (no SQLite-level file
 *                         contention -- isolated connection-table)
 *                         Half the workers deliberately skip RDDI_DISCONNECT
 *                         to exercise the thread cleanup path.
 *
 *   B. GLOBAL: scope   -- many workers sharing ONE connection, hammering
 *                         it concurrently. Each worker inserts a row
 *                         naming itself, reads RDDI_INSERTID back, and
 *                         verifies the row at that id is really its own
 *                         -- catching a mixed-up rowid/changes() read
 *                         across threads on a shared connection.
 *
 *   C. Hammer          -- EXECUTE hammering on a shared GLOBAL:
 *                         connection, then ALL workers race to call
 *                         RDDI_DISCONNECT on the SAME handle at once.
 *                         Not a realistic usage pattern, targets the
 *                         "two threads both end up with stale pConn
 *                         just before disconnect" race-condition
 */

#require "rddsql"
#require "sddsqlt3"

#include "simpleio.ch"
#include "dbinfo.ch"
#include "rddsys.ch"

REQUEST SDDSQLITE3, SQLMIX

/* workers setup */

#define THREAD_SCOPE_WORKERS     12
#define THREAD_SCOPE_ITERATIONS  150

#define GLOBAL_SCOPE_WORKERS     12
#define GLOBAL_SCOPE_ITERATIONS  150

#define HAMMER_WORKERS           8
#define HAMMER_ITERATIONS        100

STATIC s_lMemory   := .F.

/* shared state start  */

STATIC s_mtxLog
STATIC s_nFailures := 0
STATIC s_nChecks   := 0

PROCEDURE Main( mem )

   LOCAL nStart, cDbDir

#if defined( __HBSCRIPT__HBSHELL )
   rddRegister( "SQLBASE" )
   rddRegister( "SQLMIX" )
   hb_SDDSQLITE3_Register()
#endif

   ? Version(), "  MT:", iif( hb_mtvm(), "yes", "no" )
   IF ! hb_mtvm()
      ? "This build has no multithreading support -- nothing to test. Aborting."
      RETURN
   ENDIF

   IF ! Empty( mem )
      s_lMemory := .T.
      ? "SQLite in-memory table test"
   ENDIF

   s_mtxLog := hb_mutexCreate()
   cDbDir   := hb_DirBase()

//   rddSetDefault( "SQLMIX" )
   rddSetDefault( "SQLBASE" )

   ? "Registered RDDs:"
   AEval( rddList(), {| x | QOut( "  " + x ) } )

   nStart := hb_MilliSeconds()

   ? " * Test A: THREAD: scope, private connections"
   TestThreadScope( cDbDir )

   ? " * Test B: GLOBAL: scope, shared connection, concurrency validation"
   TestGlobalScope( cDbDir )

   ? " * Test C: GLOBAL: scope, EXECUTE hammer + simultaneous DISCONNECT mess"
   TestHammer( cDbDir )

   ? "=== Done in", hb_MilliSeconds() - nStart, "ms ==="
   ? "Checks:", s_nChecks, "  Failures:", s_nFailures
   ? iif( s_nFailures == 0, "PASS", "FAIL" )

   RETURN

/*
 * Helpers
 */

PROCEDURE Logger( cMsg )
   hb_mutexLock( s_mtxLog )
   ? "[T" + hb_ntos( hb_threadId() ) + "] " + cMsg
   hb_mutexUnlock( s_mtxLog )
   RETURN

PROCEDURE Check( lOk, cWhat )
   hb_mutexLock( s_mtxLog )
   s_nChecks++
   IF ! lOk
      s_nFailures++
      ? "[T" + hb_ntos( hb_threadId() ) + "] *** FAIL: " + cWhat
   ENDIF
   hb_mutexUnlock( s_mtxLog )
   RETURN

/* trivial polling barrier - hopefully widens the chance to overlap */
PROCEDURE Barrier( nCount, pMtx, aState )
   hb_mutexLock( pMtx )
   aState[ 1 ]++
   hb_mutexUnlock( pMtx )
   DO WHILE aState[ 1 ] < nCount
      hb_idleSleep( 0.002 )
   ENDDO
   RETURN

/*
 * Test A -- THREAD: scope
 */

PROCEDURE TestThreadScope( cDbDir )

   LOCAL aThreads := {}
   LOCAL n

   FOR n := 1 TO THREAD_SCOPE_WORKERS
      AAdd( aThreads, hb_threadStart( @ThreadScopeWorker(), n, cDbDir ) )
   NEXT

   AEval( aThreads, {| p | hb_threadJoin( p ) } )

   RETURN

PROCEDURE ThreadScopeWorker( nWorker, cDbDir )

   LOCAL cFile := cDbDir + "stress_thread_" + hb_ntos( nWorker ) + ".sqlite3"
   LOCAL cAlias := "chkt" + hb_ntos( nWorker )
   LOCAL hConn, i, cVal, xId
   
   IF s_lMemory
      cFile := ":memory:"
   ELSEIF hb_FileExists( cFile )
      FErase( cFile )
   ENDIF

   hConn := rddInfo( RDDI_CONNECT, { "THREAD:SQLITE3", cFile } )
   Check( hConn != 0, "worker " + hb_ntos( nWorker ) + " THREAD: connect" )
   IF hConn == 0
      Logger( "connect failed: " + hb_ValToExp( rddInfo( RDDI_ERROR ) ) )
      RETURN
   ENDIF

   rddInfo( RDDI_EXECUTE, "CREATE TABLE IF NOT EXISTS t1 ( val TEXT )" )

   FOR i := 1 TO THREAD_SCOPE_ITERATIONS

      cVal := "W" + hb_ntos( nWorker ) + "-" + hb_ntos( i )

      rddInfo( RDDI_EXECUTE, "INSERT INTO t1 ( val ) VALUES ( '" + cVal + "' )" )
      xId := rddInfo( RDDI_INSERTID )

      IF dbUseArea( .T., , "select val from t1 where rowid = " + hb_ntos( xId ), cAlias )
         Check( FIELD->val == cVal, ;
                "worker " + hb_ntos( nWorker ) + " iter " + hb_ntos( i ) + " round-trip mismatch" )
         dbCloseArea()
      ELSE
         Check( .F., "worker " + hb_ntos( nWorker ) + " iter " + hb_ntos( i ) + " verification query failed" )
      ENDIF

   NEXT

   /* Half of the workers disconnect cleanly; the other half left for
      end of the thread WITHOUT disconnecting, to exercise the
      threadcleanup path in sddConnDataRelease(). */
   IF nWorker % 2 == 0
      rddInfo( RDDI_DISCONNECT, hConn )
   ENDIF

   RETURN

/*
 * Test B -- GLOBAL: scope, concurrency validation
 */

PROCEDURE TestGlobalScope( cDbDir )

   LOCAL cFile := cDbDir + "stress_global.sqlite3"
   LOCAL hConn, aThreads := {}, n

   IF s_lMemory
      cFile := ":memory:"
   ELSEIF hb_FileExists( cFile )
      FErase( cFile )
   ENDIF

   hConn := rddInfo( RDDI_CONNECT, { "GLOBAL:SQLITE3", cFile } )
   Check( hConn != 0, "GLOBAL: connect" )
   IF hConn == 0
      Logger( "connect failed: " + hb_ValToExp( rddInfo( RDDI_ERROR ) ) )
      RETURN
   ENDIF

   rddInfo( RDDI_CONNECTION, hConn )
   rddInfo( RDDI_EXECUTE, "CREATE TABLE IF NOT EXISTS t2 ( val TEXT )" )

   FOR n := 1 TO GLOBAL_SCOPE_WORKERS
      AAdd( aThreads, hb_threadStart( @GlobalScopeWorker(), n, hConn ) )
   NEXT

   AEval( aThreads, {| p | hb_threadJoin( p ) } )

   rddInfo( RDDI_CONNECTION, hConn )
   rddInfo( RDDI_DISCONNECT, hConn )

   RETURN

PROCEDURE GlobalScopeWorker( nWorker, hConn )

   LOCAL cAlias := "chkg" + hb_ntos( nWorker )
   LOCAL i, cVal, xId

   rddInfo( RDDI_CONNECTION, hConn )

   FOR i := 1 TO GLOBAL_SCOPE_ITERATIONS

      cVal := "G" + hb_ntos( nWorker ) + "-" + hb_ntos( i )

      rddInfo( RDDI_EXECUTE, "INSERT INTO t2 ( val ) VALUES ( '" + cVal + "' )" )
      xId := rddInfo( RDDI_INSERTID )

      IF dbUseArea( .T., , "select val from t2 where rowid = " + hb_ntos( xId ), cAlias )
         Check( FIELD->val == cVal, ;
                "worker " + hb_ntos( nWorker ) + " iter " + hb_ntos( i ) + ;
                " got " + hb_ValToExp( FIELD->val ) + " expected " + hb_ValToExp( cVal ) + ;
                " (cross-thread rowid mixup?)" )
         dbCloseArea()
      ELSE
         Check( .F., "worker " + hb_ntos( nWorker ) + " iter " + hb_ntos( i ) + " verification query failed" )
      ENDIF

   NEXT

   RETURN

/*
 * Test C -- EXECUTE hammer + simultaneous multi-thread DISCONNECT
 */

PROCEDURE TestHammer( cDbDir )

   LOCAL cFile := cDbDir + "stress_hammer.sqlite3"
   LOCAL hConn, aThreads := {}, n
   LOCAL pBarrierMtx := hb_mutexCreate()
   LOCAL aBarrierState := { 0 }

   IF s_lMemory
      cFile := ":memory:"
   ELSEIF hb_FileExists( cFile )
      FErase( cFile )
   ENDIF

   hConn := rddInfo( RDDI_CONNECT, { "GLOBAL:SQLITE3", cFile } )
   Check( hConn != 0, "hammer GLOBAL: connect" )
   IF hConn == 0
      Logger( "connect failed: " + hb_ValToExp( rddInfo( RDDI_ERROR ) ) )
      RETURN
   ENDIF

   rddInfo( RDDI_CONNECTION, hConn )
   rddInfo( RDDI_EXECUTE, "CREATE TABLE IF NOT EXISTS t3 ( val TEXT )" )

   FOR n := 1 TO HAMMER_WORKERS
      AAdd( aThreads, hb_threadStart( @HammerWorker(), n, hConn, HAMMER_WORKERS, pBarrierMtx, aBarrierState ) )
   NEXT

   AEval( aThreads, {| p | hb_threadJoin( p ) } )

   /* By now every worker has tried to disconnect; excercise old handle */
   rddInfo( RDDI_CONNECTION, hConn )
   rddInfo( RDDI_DISCONNECT, hConn )

   RETURN

PROCEDURE HammerWorker( nWorker, hConn, nWorkers, pBarrierMtx, aBarrierState )

   LOCAL i

   rddInfo( RDDI_CONNECTION, hConn )

   FOR i := 1 TO HAMMER_ITERATIONS
      rddInfo( RDDI_EXECUTE, "INSERT INTO t3 ( val ) VALUES ( 'C" + hb_ntos( nWorker ) + "-" + hb_ntos( i ) + "' )" )
   NEXT

   /* line everyone up, then all fire RDDI_DISCONNECT together */
   Barrier( nWorkers, pBarrierMtx, aBarrierState )

   rddInfo( RDDI_CONNECTION, hConn )
   Logger( "disconnect attempt -> " + hb_ValToExp( rddInfo( RDDI_DISCONNECT, hConn ) ) )

   RETURN
