/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    PostgreSQL RDD
 *
 * Copyright 2006 Lorenzo Fiorini <lorenzo_fiorini / at / teamwork / dot / it>
 * www - http://www.harbour-project.org
 * www - http://www.xharbour.org
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
 * This is an experimental RDD for contrib/pgsql interface.
 * It has been created to test the possibilities of usrrdd.
 * It doesn't support many functions and commands and many things could be optimized.
 */

#include "rddsys.ch"
#include "hbusrrdd.ch"
#include "fileio.ch"
#include "error.ch"
#include "dbstruct.ch"
#include "common.ch"

#define AREA_QUERY    1
#define AREA_ROW      2
#define AREA_APPEND   3

#define AREA_LEN      3

ANNOUNCE PGRDD

THREAD STATIC t_aConnections := {}

FUNCTION DBPGCONNECTION( cConnString )

   LOCAL aParams
   LOCAL oServer
   LOCAL nConn

   aParams := hb_ATokens( cConnString, ";" )

   ASize( aParams, 6 )

   oServer := TPQServer():New( aParams[1], aParams[2], aParams[3], aParams[4], aParams[5], aParams[6] )

   IF oServer:NetErr()
      RETURN HB_FAILURE
   ELSE
      AAdd( t_aConnections, oServer )
      nConn := Len( t_aConnections )
   ENDIF

   RETURN nConn

FUNCTION DBPGCLEARCONNECTION( nConn )

   LOCAL oServer := t_aConnections[ nConn ]

   oServer:Close()

   t_aConnections[ nConn ] := NIL

   RETURN HB_SUCCESS

/*
 * non work area methods receive RDD ID as first parameter
 * Methods INIT and EXIT does not have to execute SUPER methods - these is
 * always done by low level USRRDD code
 */
STATIC FUNCTION PG_INIT( nRDD )

   USRRDD_RDDDATA( nRDD )

   RETURN HB_SUCCESS

/*
 * methods: NEW and RELEASE receive pointer to work area structure
 * not work area number. It's necessary because the can be executed
 * before work area is allocated
 * these methods does not have to execute SUPER methods - these is
 * always done by low level USRRDD code
 */
STATIC FUNCTION PG_NEW( pWA )

   USRRDD_AREADATA( pWA, Array( AREA_LEN ) )

   RETURN HB_SUCCESS

STATIC FUNCTION PG_OPEN( nWA, aOpenInfo )
   LOCAL aField, oError, lError, cError, nResult
   LOCAL oServer, oQuery, aStruct, aFieldStruct

   LOCAL aWAData := USRRDD_AREADATA( nWA )

   IF ! Empty( aOpenInfo[ UR_OI_CONNECT ] ) .AND. aOpenInfo[ UR_OI_CONNECT ] <= Len( t_aConnections )
      oServer := t_aConnections[ aOpenInfo[ UR_OI_CONNECT ] ]
   ENDIF

   IF ! Empty( oServer )
      oServer:lAllCols := .F.
      oQuery := oServer:Query( aOpenInfo[ UR_OI_NAME ] )
      lError := oQuery:NetErr()
      cError := oQuery:ErrorMsg()
   ELSE
      lError := .T.
      cError := "Invalid connection handle"
   ENDIF

   IF lError
      oError := ErrorNew()
      oError:GenCode     := EG_OPEN
      oError:SubCode     := 1000
      oError:Description := hb_langErrMsg( EG_OPEN ) + ", " + cError
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      oError:CanDefault  := .T.
      UR_SUPER_ERROR( nWA, oError )
      RETURN HB_FAILURE
   ELSE
      aWAData[ AREA_QUERY ] := oQuery
   ENDIF

   UR_SUPER_SETFIELDEXTENT( nWA, oQuery:nFields )

   aStruct := oQuery:Struct()

   FOR EACH aFieldStruct IN aStruct

       aField := Array( UR_FI_SIZE )
       aField[ UR_FI_NAME ]    := aFieldStruct[ DBS_NAME ]
       aField[ UR_FI_TYPE ]    := aFieldStruct[ DBS_TYPE ]
       aField[ UR_FI_TYPEEXT ] := 0
       aField[ UR_FI_LEN ]     := aFieldStruct[ DBS_LEN ]
       aField[ UR_FI_DEC ]     := aFieldStruct[ DBS_DEC ]
       UR_SUPER_ADDFIELD( nWA, aField )

   NEXT

   /* Call SUPER OPEN to finish allocating work area (f.e.: alias settings) */
   nResult := UR_SUPER_OPEN( nWA, aOpenInfo )

   RETURN nResult

STATIC FUNCTION PG_CLOSE( nWA )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   aWAData[ AREA_QUERY ]:Close()

   RETURN UR_SUPER_CLOSE( nWA )

STATIC FUNCTION PG_GETVALUE( nWA, nField, xValue )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   IF ! Empty( aWAData[ AREA_ROW ] )
      xValue := aWAData[ AREA_ROW ]:FieldGet( nField )
   ELSE
      xValue := aWAData[ AREA_QUERY ]:FieldGet( nField )
   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION PG_PUTVALUE( nWA, nField, xValue )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   IF Empty( aWAData[ AREA_ROW ] )
      aWAData[ AREA_ROW ] := aWAData[ AREA_QUERY ]:GetRow()
   ENDIF

   aWAData[ AREA_ROW ]:FieldPut( nField, xValue )

   RETURN HB_SUCCESS

STATIC FUNCTION PG_SKIP( nWA, nRecords )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   IF ! Empty( aWAData[ AREA_ROW ] )
      PG_FLUSH( nWA )
   ENDIF

   aWAData[ AREA_QUERY ]:Skip( nRecords )

   RETURN HB_SUCCESS

STATIC FUNCTION PG_GOTOP( nWA )
   RETURN PG_GOTO( nWA, 1 )

STATIC FUNCTION PG_GOBOTTOM( nWA )
   RETURN PG_GOTO( nWA, -1 )

STATIC FUNCTION PG_GOTOID( nWA, nRecord )
   RETURN PG_GOTO( nWA, nRecord )

STATIC FUNCTION PG_GOTO( nWA, nRecord )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   IF ! Empty( aWAData[ AREA_ROW ] )
      PG_FLUSH( nWA )
   ENDIF

   IF nRecord < 0
      nRecord := aWAData[ AREA_QUERY ]:nLastRec
   ELSEIF nRecord == 0
      nRecord := aWAData[ AREA_QUERY ]:nRecno
   ENDIF

   aWAData[ AREA_QUERY ]:Goto( nRecord )

   RETURN HB_SUCCESS

STATIC FUNCTION PG_RECCOUNT( nWA, nRecords )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   nRecords := aWAData[ AREA_QUERY ]:nLastRec

   RETURN HB_SUCCESS

STATIC FUNCTION PG_BOF( nWA, lBof )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   lBof := aWAData[ AREA_QUERY ]:lBof

   RETURN HB_SUCCESS

STATIC FUNCTION PG_EOF( nWA, lEof )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   lEof := aWAData[ AREA_QUERY ]:lEof

   RETURN HB_SUCCESS

STATIC FUNCTION PG_RECID( nWA, nRecNo )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   nRecno := aWAData[ AREA_QUERY ]:nRecNo

   RETURN HB_SUCCESS

STATIC FUNCTION PG_DELETED( nWA, lDeleted )
   HB_SYMBOL_UNUSED( nWA )
   lDeleted := .F.
   RETURN HB_SUCCESS

STATIC FUNCTION PG_FLUSH( nWA )
   LOCAL oError
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL nRecno

   IF aWAData[ AREA_ROW ] != NIL
      IF ! Empty( aWAData[ AREA_APPEND ] )
         aWAData[ AREA_QUERY ]:Append( aWAData[ AREA_ROW ] )
      ELSE
         nRecno := aWAData[ AREA_QUERY ]:nRecNo
         aWAData[ AREA_QUERY ]:Update( aWAData[ AREA_ROW ] )
      ENDIF

      IF aWAData[ AREA_QUERY ]:lError
         oError := ErrorNew()
         oError:GenCode     := EG_DATATYPE
         oError:SubCode     := 3000
         oError:Description := HB_LANGERRMSG( EG_DATATYPE ) + ", " + aWAData[ AREA_QUERY ]:ErrorMsg()
         UR_SUPER_ERROR( nWA, oError )
         RETURN HB_FAILURE
      ENDIF

/*
 * The :Refresh() below costs a lot in term of performance.
 * It redo the select to include inserts and updates.
 * It is the only solution I've found so far to simulate dbf behaviour
 */
      aWAData[ AREA_QUERY ]:Refresh( .T., .F. )

      IF ! Empty( aWAData[ AREA_APPEND ] )
         aWAData[ AREA_APPEND ] := .F.
         nRecno := aWAData[ AREA_QUERY ]:nLastRec
      ENDIF

      aWAData[ AREA_ROW ] := NIL

      PG_GOTO( nWA, nRecno )

   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION PG_APPEND( nWA, nRecords )
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   HB_SYMBOL_UNUSED( nRecords )

   aWAData[ AREA_ROW ] := aWAData[ AREA_QUERY ]:GetBlankRow()

   aWAData[ AREA_APPEND ] := .T.

   RETURN HB_SUCCESS

STATIC FUNCTION PG_DELETE( nWA )
   LOCAL oError
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   aWAData[ AREA_ROW ] := aWAData[ AREA_QUERY ]:GetRow()

   aWAData[ AREA_QUERY ]:Delete( aWAData[ AREA_ROW ] )

   IF aWAData[ AREA_QUERY ]:lError
      oError := ErrorNew()
      oError:GenCode     := EG_DATATYPE
      oError:SubCode     := 2000
      oError:Description := HB_LANGERRMSG( EG_DATATYPE ) + ", " + aWAData[ AREA_QUERY ]:ErrorMsg()
      UR_SUPER_ERROR( nWA, oError )
      RETURN HB_FAILURE
   ENDIF

   aWAData[ AREA_ROW ] := NIL

   RETURN HB_SUCCESS

/*
 * This function have to exist in all RDD and then name have to be in
 * format: <RDDNAME>_GETFUNCTABLE
 */
FUNCTION PGRDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID )
   LOCAL cSuperRDD := NIL     /* NO SUPER RDD */
   LOCAL aMyFunc[ UR_METHODCOUNT ]

   aMyFunc[ UR_INIT         ] := ( @PG_INIT()         )
   aMyFunc[ UR_NEW          ] := ( @PG_NEW()          )
   aMyFunc[ UR_OPEN         ] := ( @PG_OPEN()         )
   aMyFunc[ UR_GETVALUE     ] := ( @PG_GETVALUE()     )
   aMyFunc[ UR_PUTVALUE     ] := ( @PG_PUTVALUE()     )
   aMyFunc[ UR_SKIP         ] := ( @PG_SKIP()         )
   aMyFunc[ UR_GOTO         ] := ( @PG_GOTO()         )
   aMyFunc[ UR_GOTOID       ] := ( @PG_GOTOID()       )
   aMyFunc[ UR_GOTOP        ] := ( @PG_GOTOP()        )
   aMyFunc[ UR_GOBOTTOM     ] := ( @PG_GOBOTTOM()     )
   aMyFunc[ UR_RECCOUNT     ] := ( @PG_RECCOUNT()     )
   aMyFunc[ UR_RECID        ] := ( @PG_RECID()        )
   aMyFunc[ UR_BOF          ] := ( @PG_BOF()          )
   aMyFunc[ UR_EOF          ] := ( @PG_EOF()          )
   aMyFunc[ UR_DELETED      ] := ( @PG_DELETED()      )
   aMyFunc[ UR_FLUSH        ] := ( @PG_FLUSH()        )
   aMyFunc[ UR_APPEND       ] := ( @PG_APPEND()       )
   aMyFunc[ UR_DELETE       ] := ( @PG_DELETE()       )
   aMyFunc[ UR_CLOSE        ] := ( @PG_CLOSE()        )

   RETURN USRRDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID, ;
                               cSuperRDD, aMyFunc )

INIT PROC PG_INIT()
   rddRegister( "PGRDD", RDT_FULL )
   RETURN
