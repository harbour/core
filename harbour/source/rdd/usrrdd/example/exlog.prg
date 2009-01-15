/*
 * $Id: exfcm.prg 9551 2008-10-05 18:13:15Z vszakats $
 */

#include "dbinfo.ch"

// Request for LOGRDD rdd driver
REQUEST LOGRDD

// Here put Request for RDD you want to inherit then add
// function hb_LogRddInherit() (see at bottom)
REQUEST DBFCDX

PROCEDURE Main()

   // Set LOGRDD as default RDD otherwise I have to set explicitly use
   // with DRIVER option
   RDDSetDefault( "LOGRDD" )
   // Adding Memofile Info
   rddInfo( RDDI_MEMOVERSION, DB_MEMOVER_CLIP, "LOGRDD" )

   // Define Log File Name and position
   hb_LogRddLogFileName( "logs\changes.log" )
   // Define Tag to add for each line logged
   hb_LogRddTag( NETNAME() + "\" + hb_USERNAME() )
   // Activate Logging, it can be stopped/started at any moment
   hb_LogRddActive( .T. )

   // Uncomment next command to change logged string to standard LOGRDD file
   // hb_LogRddMsgLogBlock( {|cTag, cRDDName, cCmd, nWA, cMsg| DToS( Date() ) + " / " + Time() + " " + cTag + ": " + PadR( cRDDName + "_" + cCmd, 20 ) + " - " + cMsg } )
   // Uncomment next command to change standard destination logged string
   // hb_LogRddUserLogBlock( {|cTag, cRDDName, cCmd, nWA, cMsg| hb_toOutDebug( DToS( Date() ) + " : " + Time() + " " + cTag + ": " + PadR( cRDDName + "_" + cCmd, 20 ) + " - " + cMsg + "\n\r" ) } )

   // Start program logic

   // Open a table with logging (default RDD is LOGRDD)
   USE test
   field->name := "Francesco"
   CLOSE

   // Open a table without logging
   USE test DRIVER "DBFCDX"
   APPEND BLANK
   field->name := "Francesco"

RETURN

FUNCTION hb_LogRddInherit()
RETURN "DBFCDX"
