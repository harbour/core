/*
 * $Id$
 */

REQUEST RLCDX

PROCEDURE Main()

   dbCreate( "_tst", { { "F1", "C", 10, 0 } }, "RLCDX" )
   USE _tst VIA "RLCDX" SHARED
   ? "Table: ", Alias(), " open VIA: ", rddName()
   ? "APPEND"
   dbAppend()
   ? "Current record locks:"
   AEval( dbRLockList(), {| nRecNo | QQOut( nRecNo ) } )
   ? "APPEND"
   dbAppend()
   ? "Current record locks:"
   AEval( dbRLockList(), {| nRecNo | QQOut( nRecNo ) } )
   ? "UNLOCK"
   dbUnlock()
   ? "Current record locks:"
   AEval( dbRLockList(), {| nRecNo | QQOut( nRecNo ) } )

   ? "Locking record 1", dbRLock( 1 )
   ? "Locking record 1", dbRLock( 1 )
   ? "Locking record 1", dbRLock( 1 )
   ? "Locking record 2", dbRLock( 2 )
   ? "Current record locks:"
   AEval( dbRLockList(), {| nRecNo | QQOut( nRecNo ) } )
   ? "UnLocking record 1..."
   dbRUnlock( 1 )
   ? "Current record locks:"
   AEval( dbRLockList(), {| nRecNo | QQOut( nRecNo ) } )
   ? "UnLocking record 2..."
   dbRUnlock( 2 )
   ? "Current record locks:"
   AEval( dbRLockList(), {| nRecNo | QQOut( nRecNo ) } )
   ? "UnLocking record 1..."
   dbRUnlock( 1 )
   ? "Current record locks:"
   AEval( dbRLockList(), {| nRecNo | QQOut( nRecNo ) } )
   ? "UnLocking record 1..."
   dbRUnlock( 1 )
   ? "Current record locks:"
   AEval( dbRLockList(), {| nRecNo | QQOut( nRecNo ) } )

   CLOSE

   RETURN
