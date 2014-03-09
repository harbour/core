#include "dbinfo.ch"
#include "hbusrrdd.ch"

// Request for LOGRDD rdd driver
REQUEST LOGRDD

// Here put Request for RDD you want to inherit then add
// function hb_LogRddInherit() (see at bottom)
REQUEST DBFCDX

PROCEDURE Main()

   hb_FCopy( hb_DirSepToOS( "../test.dbf" ), "test.dbf" )

   // Set LOGRDD as default RDD otherwise I have to set explicitly use
   // with DRIVER option
   rddSetDefault( "LOGRDD" )
   // Adding Memofile Info
   rddInfo( RDDI_MEMOVERSION, DB_MEMOVER_CLIP, "LOGRDD" )

   // Define Log File Name and position
   hb_LogRddLogFileName( "changes.log" )
   // Define Tag to add for each line logged
   hb_LogRddTag( NetName() + "\" + hb_UserName() )
   // Activate Logging, it can be stopped/started at any moment
   hb_LogRddActive( .T. )

   // Uncomment next command to change logged string that I have to return to standard LOGRDD file
#if 0
   hb_LogRddMsgLogBlock( {| cTag, cRDDName, cCmd, nWA, xPar1, xPar2, xPar3 | MyToString( cTag, cRDDName, cCmd, nWA, xPar1, xPar2, xPar3 ) } )
#endif

   // Uncomment next command to change standard destination of my logged string
#if 0
   hb_LogRddUserLogBlock( {| cTag, cRDDName, cCmd, nWA, xPar1, xPar2, xPar3 | hb_ToOutDebug( MyToString( cTag, cRDDName, cCmd, nWA, xPar1, xPar2, xPar3 ) + "\n\r" ) } )
#endif

   // Start program logic

   // Open a table with logging (default RDD is LOGRDD)
   USE test.dbf
   field->first := "Francesco"
   dbCloseArea()

   // Open a table without logging

   USE test.dbf VIA "DBFCDX"
   dbAppend()
   field->first := "Francesco"

   RETURN

STATIC FUNCTION MyToString( cCmd, nWA, xPar1, xPar2, xPar3 )

   SWITCH cCmd
   CASE "CREATE"
      // Parameters received: xPar1: aOpenInfo
      RETURN xPar1[ UR_OI_NAME ]
   CASE "CREATEFIELDS"
      // Parameters received: xPar1: aStruct
      RETURN hb_ValToExp( xPar1 )
   CASE "OPEN"
      // Parameters received: xPar1: aOpenInfo
      // RETURN 'Table: "' + xPar1[ UR_OI_NAME ] + '", Alias: "' + Alias() + '", WorkArea: ' + hb_ntos( nWA )
      // In this example I don't want to log Open Command
   CASE "CLOSE"
      // Parameters received: xPar1: cTableName, xPar2: cAlias
      // RETURN 'Table: "' + xPar1 + '", Alias: "' + xPar2 + '", WorkArea: ' + hb_ntos( nWA )
      // In this example I don't want to log Close Command
   CASE "APPEND"
      // Parameters received: xPar1: lUnlockAll
      RETURN Alias() + "->RecNo() == " + hb_ntos( RecNo() )
   CASE "DELETE"
      // Parameters received: none
      RETURN Alias() + "->RecNo() == " + hb_ntos( RecNo() )
   CASE "RECALL"
      // Parameters received: none
      RETURN Alias() + "->RecNo() == " + hb_ntos( RecNo() )
   CASE "PUTVALUE"
      // Parameters received: xPar1: nField, xPar2: xValue, xPar3: xOldValue
      HB_SYMBOL_UNUSED( xPar3 ) // Here don't log previous value
      RETURN Alias() + "(" + hb_ntos( RecNo() ) + ")->" + PadR( FieldName( xPar1 ), 10 ) + " := " + hb_LogRddValueToText( xPar2 )
   CASE "ZAP"
      // Parameters received: none
      RETURN 'Alias: "' + Alias() + ' Table: "' + dbInfo( DBI_FULLPATH ) + '"'
   ENDSWITCH

   RETURN NIL

FUNCTION hb_LogRddInherit()
   RETURN "DBFCDX"
