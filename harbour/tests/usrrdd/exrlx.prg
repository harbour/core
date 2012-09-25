/*
 * $Id$
 */

REQUEST RLCDX

PROCEDURE Main()

   DBCREATE( "_tst", { { "F1", "C", 10, 0 } }, "RLCDX" )
   USE _tst VIA "RLCDX" SHARED
   ? "Table: ", ALIAS(), " open VIA: ", RDDNAME()
   ? "APPEND"
   DBAPPEND()
   ? "Current record locks:"
   AEVAL( DBRLOCKLIST(), {| nRecNo | qqout( nRecNo ) } )
   ? "APPEND"
   DBAPPEND()
   ? "Current record locks:"
   AEVAL( DBRLOCKLIST(), {| nRecNo | qqout( nRecNo ) } )
   ? "UNLOCK"
   DBUNLOCK()
   ? "Current record locks:"
   AEVAL( DBRLOCKLIST(), {| nRecNo | qqout( nRecNo ) } )

   ? "Locking record 1", DBRLOCK( 1 )
   ? "Locking record 1", DBRLOCK( 1 )
   ? "Locking record 1", DBRLOCK( 1 )
   ? "Locking record 2", DBRLOCK( 2 )
   ? "Current record locks:"
   AEVAL( DBRLOCKLIST(), {| nRecNo | qqout( nRecNo ) } )
   ? "UnLocking record 1..."
   DBRUNLOCK(1)
   ? "Current record locks:"
   AEVAL( DBRLOCKLIST(), {| nRecNo | qqout( nRecNo ) } )
   ? "UnLocking record 2..."
   DBRUNLOCK(2)
   ? "Current record locks:"
   AEVAL( DBRLOCKLIST(), {| nRecNo | qqout( nRecNo ) } )
   ? "UnLocking record 1..."
   DBRUNLOCK(1)
   ? "Current record locks:"
   AEVAL( DBRLOCKLIST(), {| nRecNo | qqout( nRecNo ) } )
   ? "UnLocking record 1..."
   DBRUNLOCK(1)
   ? "Current record locks:"
   AEVAL( DBRLOCKLIST(), {| nRecNo | qqout( nRecNo ) } )

   CLOSE

   RETURN
