/*
 * $Id: rlcdx.prg 9754 2008-10-27 22:40:04Z vszakats $
 */

/*
 * Harbour Project source code:
 *    LOGRDD
 *
 * Copyright 2009 Francesco Saverio Giudice <info / at / fsgiudice.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
 * A simple RDD which introduce logging to file. It inheriths from
 * any existent RDD but if you write / replace / delete something
 * on tables it writes changes in a log file.
 * An example is avalaible at bottom of this file.
 */

#include "rddsys.ch"
#ifdef __XHARBOUR__
  #include "usrrdd.ch"
  #xtranslate hb_valtoexp(  => cStr(
#else
  #include "hbusrrdd.ch"
#endif
#include "common.ch"
#include "fileio.ch"
#include "dbinfo.ch"

#define ARRAY_FILENAME    1
#define ARRAY_FHANDLE     2
#define ARRAY_TAG         3
#define ARRAY_ACTIVE      4
#define ARRAY_RDDNAME     5

ANNOUNCE LOGRDD
REQUEST  HB_LOGRDDINHERIT  /* To be defined at user level */

STATIC FUNCTION LOGRDD_INIT( nRDD )
   LOCAL lActive, cFileName, cTag, cRDDName

   /* Defaults */
   cFileName := "changes.log"
   lActive   := .F.
   #ifdef __XHARBOUR__
      cTag := NETNAME() + "\" + NETNAME( 1 )
   #else
      cTag := NETNAME() + "\" + hb_USERNAME()
   #endif
   cRDDName := hb_LogRddInherit()

   /* Log File will be open later so user can change parameters */

   /* Store data in RDD cargo */
                        /* cFileName, nHandle, cTag, lActive, cRDDName */
   USRRDD_RDDDATA( nRDD, { cFileName, NIL, cTag, lActive, cRDDName } )

RETURN SUCCESS

STATIC FUNCTION LOGRDD_EXIT( nRDD )
   LOCAL aRDDData  := USRRDD_RDDDATA( nRDD )

   /* Closing log file */
   IF aRDDData[ ARRAY_FHANDLE ] != NIL
      FClose( aRDDData[ ARRAY_FHANDLE ] )
      aRDDData[ ARRAY_FHANDLE ] := NIL
   ENDIF

RETURN SUCCESS

// Create database from current WA fields definition
STATIC FUNCTION LOGRDD_CREATE( nWA, aOpenInfo )
   LOCAL nResult := UR_SUPER_CREATE( nWA, aOpenInfo )
   IF nResult == SUCCESS
      ToLog( "CREATE", nWA, aOpenInfo[ UR_OI_NAME ] )
   ENDIF
   RETURN nResult

// Creating fields for new DBF - dbCreate() in current workarea
STATIC FUNCTION LOGRDD_CREATEFIELDS( nWA, aStruct )
   LOCAL nResult := UR_SUPER_CREATEFIELDS( nWA, aStruct )
   IF nResult == SUCCESS
      ToLog( "CREATEFIELDS", nWA, hb_ValToExp( aStruct ) )
   ENDIF
   RETURN nResult

// Open workarea
STATIC FUNCTION LOGRDD_OPEN( nWA, aOpenInfo )
   LOCAL nResult := UR_SUPER_OPEN( nWA, aOpenInfo )
   IF nResult == SUCCESS
      ToLog( "OPEN", nWA, 'Table : "' + aOpenInfo[ UR_OI_NAME ] + '", Alias : "' + Alias() + '", WorkArea : ' + LTrim( Str( nWA ) ) )
   ENDIF
   RETURN nResult

// Close workarea
STATIC FUNCTION LOGRDD_CLOSE( nWA )
   LOCAL cDbf    := dbInfo( DBI_FULLPATH )
   LOCAL cAlias  := Alias()
   LOCAL nResult := UR_SUPER_CLOSE( nWA )
   IF nResult == SUCCESS
      ToLog( "CLOSE", nWA, 'Table : "' + cDbf + '", Alias : "' + cAlias + '", WorkArea : ' + LTrim( Str( nWA ) ) )
   ENDIF
   RETURN nResult

STATIC FUNCTION LOGRDD_APPEND( nWA, lUnlockAll )
   LOCAL nResult := UR_SUPER_APPEND( nWA, lUnlockAll )
   IF nResult == SUCCESS
      ToLog( "APPEND", nWA, Alias() + "->RecNo() = " + LTrim( Str( RecNo() ) ) )
   ENDIF
   RETURN nResult

STATIC FUNCTION LOGRDD_DELETE( nWA )
   LOCAL nResult := UR_SUPER_DELETE( nWA )
   IF nResult == SUCCESS
      ToLog( "DELETE", nWA, Alias() + "->RecNo() = " + LTrim( Str( RecNo() ) ) )
   ENDIF
   RETURN nResult

STATIC FUNCTION LOGRDD_RECALL( nWA )
   LOCAL nResult := UR_SUPER_RECALL( nWA )
   IF nResult == SUCCESS
      ToLog( "RECALL", nWA, Alias() + "->RecNo() = " + LTrim( Str( RecNo() ) ) )
   ENDIF
   RETURN nResult

STATIC FUNCTION LOGRDD_PUTVALUE( nWA, nField, xValue )
   LOCAL xOldValue := FieldGet( nField )
   LOCAL nResult   := UR_SUPER_PUTVALUE( nWA, nField, xValue )

   //Log Only Changes
   IF !( xOldValue == xValue )
      ToLog( "PUTVALUE", nWA, Alias() + "(" + LTrim( Str( RecNo() ) ) + ")->" + PadR( FieldName( nField ), 10 ) + " := " + ValToText( xValue ) )
   ENDIF
   RETURN nResult

STATIC FUNCTION LOGRDD_ZAP( nWA )
   LOCAL cDbf   := dbInfo( DBI_FULLPATH )
   LOCAL cAlias := Alias()
   LOCAL nResult := UR_SUPER_ZAP( nWA )
   IF nResult == SUCCESS
      ToLog( "ZAP", nWA, 'Alias : "' + Alias() + ' Table : "' + cDbf + '"'  )
   ENDIF
   RETURN nResult

/* Force linking DBFCDX from which our RDD inherits */
REQUEST DBFCDX

/*
 * This function have to exist in all RDD and then name have to be in
 * format: <RDDNAME>_GETFUNCTABLE
 */
FUNCTION LOGRDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID )
   LOCAL cSuperRDD := hb_LogRddInherit() /* We are inheriting from a User Defined RDD */
   LOCAL aMyFunc[ UR_METHODCOUNT ]

   aMyFunc[ UR_INIT         ] := ( @LOGRDD_INIT()         )
   aMyFunc[ UR_EXIT         ] := ( @LOGRDD_EXIT()         )
   aMyFunc[ UR_CREATE       ] := ( @LOGRDD_CREATE()       )
   aMyFunc[ UR_CREATEFIELDS ] := ( @LOGRDD_CREATEFIELDS() )
   aMyFunc[ UR_OPEN         ] := ( @LOGRDD_OPEN()         )
   aMyFunc[ UR_CLOSE        ] := ( @LOGRDD_CLOSE()        )
   aMyFunc[ UR_APPEND       ] := ( @LOGRDD_APPEND()       )
   aMyFunc[ UR_DELETE       ] := ( @LOGRDD_DELETE()       )
   aMyFunc[ UR_RECALL       ] := ( @LOGRDD_RECALL()       )
   aMyFunc[ UR_PUTVALUE     ] := ( @LOGRDD_PUTVALUE()     )
   aMyFunc[ UR_ZAP          ] := ( @LOGRDD_ZAP()          )

RETURN USRRDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID, ;
                            cSuperRDD, aMyFunc )

INIT PROCEDURE _LOGRDD_INIT()
   rddRegister( "LOGRDD", RDT_FULL )
   RETURN

/* -------------------------------------------------- */
/*           UTILITY FUNCTIONS                        */
/* -------------------------------------------------- */

FUNCTION hb_LogRddLogFileName( cFileName )
   LOCAL nRDD, aRDDList
   LOCAL aRDDData
   LOCAL cOldFileName

   aRDDList := RDDLIST( RDT_FULL )
   nRDD     := AScan( aRDDList, "LOGRDD" )

   IF nRDD > 0

      nRDD -- // HACK: Possibly an error of nRDD value in UR_INIT() ? - TODO

      aRDDData := USRRDD_RDDDATA( nRDD )

      cOldFileName := aRDDData[ ARRAY_FILENAME ]

      IF HB_ISSTRING( cFileName )
         aRDDData[ ARRAY_FILENAME ] := cFileName
      ENDIF

   ENDIF
   RETURN cOldFileName

FUNCTION hb_LogRddTag( cTag )
   LOCAL nRDD, aRDDList
   LOCAL aRDDData
   LOCAL cOldTag

   aRDDList := RDDLIST( RDT_FULL )
   nRDD     := AScan( aRDDList, "LOGRDD" )

   IF nRDD > 0

      nRDD -- // HACK: Possibly an error of nRDD value in UR_INIT() ? - TODO

      aRDDData := USRRDD_RDDDATA( nRDD )

      cOldTag := aRDDData[ ARRAY_TAG ]

      IF HB_ISSTRING( cTag )
         aRDDData[ ARRAY_TAG ] := cTag
      ENDIF

   ENDIF
   RETURN cOldTag

FUNCTION hb_LogRddActive( lActive )
   LOCAL nRDD, aRDDList
   LOCAL aRDDData
   LOCAL lOldActive

   aRDDList := RDDLIST( RDT_FULL )
   nRDD     := AScan( aRDDList, "LOGRDD" )

   IF nRDD > 0

      nRDD -- // HACK: Possibly an error of nRDD value in UR_INIT() ? - TODO

      aRDDData := USRRDD_RDDDATA( nRDD )

      lOldActive := aRDDData[ ARRAY_ACTIVE ]

      IF HB_ISLOGICAL( lActive )
         aRDDData[ ARRAY_ACTIVE ] := lActive
      ENDIF

   ENDIF
   RETURN lOldActive

STATIC PROCEDURE OpenLogFile( nWA )
   LOCAL aRDDData  := USRRDD_RDDDATA( USRRDD_ID( nWA ) )
   LOCAL cFileName := aRDDData[ ARRAY_FILENAME ]
   LOCAL nHandle   := aRDDData[ ARRAY_FHANDLE  ]
   LOCAL lActive   := aRDDData[ ARRAY_ACTIVE   ]

   //TraceLog( "nHandle " + cStr( nHandle ) )

   IF lActive .AND. nHandle == NIL

      /* Open Access Log File */
      IF File( cFileName )
         nHandle := FOpen( cFileName, FO_READWRITE + FO_SHARED )
      ELSE
         nHandle := FCreate( cFileName )
         /* Close and reopen in shared mode */
         IF FError() == 0 .AND. nHandle > 0
            FClose( nHandle )
            nHandle := FOpen( cFileName, FO_READWRITE + FO_SHARED )
         ENDIF
      ENDIF
      IF FError() == 0 .AND. nHandle > 0
         /* Move to end of file */
         FSeek( nHandle, 0, FS_END )
      ELSE
         nHandle := NIL
      ENDIF

      aRDDData[ ARRAY_FHANDLE  ] := nHandle

   ENDIF
   RETURN

STATIC PROCEDURE ToLog( cCmd, nWA, cMsg )
   LOCAL aRDDData := USRRDD_RDDDATA( USRRDD_ID( nWA ) )
   LOCAL nHandle  := aRDDData[ ARRAY_FHANDLE ]
   LOCAL cTag     := aRDDData[ ARRAY_TAG     ]
   LOCAL lActive  := aRDDData[ ARRAY_ACTIVE  ]
   LOCAL cRDDName := aRDDData[ ARRAY_RDDNAME  ]

   //TraceLog( "nHandle " + cStr( nHandle ) + " cUser " + cUser )
   IF lActive

      IF nHandle == NIL
         OpenLogFile( nWA )
      ENDIF

      IF nHandle != NIL
         FWrite( nHandle, DToS( Date() ) + " " + Time() + " " + cTag + ": " + PadR( cRDDName + "_" + cCmd, 20 ) + " - " + cMsg + hb_OSNewLine() )
      ENDIF

   ENDIF
   RETURN

#ifdef __XHARBOUR__
STATIC FUNCTION ValToText( uValue )

   LOCAL cType := ValType( uValue )
   LOCAL cText := ValToPrg( uValue )

   RETURN "[" + cType + "]>>>" + cText + "<<<"
#else
STATIC FUNCTION ValToText( uValue )

   LOCAL cType := ValType( uValue )
   LOCAL cText

   DO CASE
   CASE cType == "C"
      cText := hb_StrToExp( uValue )

   CASE cType == "N"
      cText := hb_NToS( uValue )

   CASE cType == "D"
      cText := DToS( uValue )
      cText := "0d" + iif( Empty( cText ), "00000000", cText )

   OTHERWISE
      cText := hb_ValToStr( uValue )
   ENDCASE

   RETURN "[" + cType + "]>>>" + cText + "<<<"
#endif

/*****************************************************************
  EXAMPLE:

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

   // Start program logic

   // Open a table with logging (default RDD is LOGRDD)
   USE test
   field->name := "Francesco"
   CLOSE

   // Open a table without logging
   USE test DRIVER "DBFCDX"
   ....

RETURN

FUNCTION hb_LogRddInherit()
RETURN "DBFCDX"

*******************************************************************/
