/*
 * RDDADO - RDD to automatically manage Microsoft ADO
 *
 * Copyright 2007 Fernando Mancera <fmancera@viaopen.com>
 * Copyright 2007 Antonio Linares <alinares@fivetechsoft.com>
 * Copyright 2007-2008 Miguel Angel Marchuet <miguelangel@marchuet.net>
 *  ADO_GOTOID( nWA, nRecord )
 *  ADO_GOTO( nWA, nRecord )
 *  ADO_OPEN( nWA, aOpenInfo ) some modifications
 *     Open: Excel files
 *           Paradox files
 *           Access with password
 *           Firebird
 *  ADO_CLOSE( nWA )
 *  ADO_ZAP( nWA )
 *  ADO_ORDINFO( nWA, nIndex, aOrderInfo ) some modifications
 *  ADO_RECINFO( nWA, nRecord, nInfoType, uInfo )
 *  ADO_FIELDINFO( nWA, nField, nInfoType, uInfo )
 *  ADO_FIELDNAME( nWA, nField )
 *  ADO_FORCEREL( nWA )
 *  ADO_RELEVAL( nWA, aRelInfo )
 *  ADO_EXISTS( nRdd, cTable, cIndex, ulConnect )
 *  ADO_DROP(  nRdd, cTable, cIndex, ulConnect )
 *  ADO_LOCATE( nWA, lContinue )
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "rddsys.ch"
#include "fileio.ch"
#include "error.ch"
#include "dbstruct.ch"
#include "dbinfo.ch"

#include "rddado.ch"

#include "hbusrrdd.ch"

#define WA_RECORDSET    1
#define WA_BOF          2
#define WA_EOF          3
#define WA_CONNECTION   4
#define WA_CATALOG      5
#define WA_TABLENAME    6
#define WA_ENGINE       7
#define WA_SERVER       8
#define WA_USERNAME     9
#define WA_PASSWORD     10
#define WA_QUERY        11
#define WA_LOCATEFOR    12
#define WA_SCOPEINFO    13
#define WA_SQLSTRUCT    14
#define WA_CONNOPEN     15
#define WA_PENDINGREL   16
#define WA_FOUND        17

#define WA_SIZE         17

#define RDD_CONNECTION  1
#define RDD_CATALOG     2

#define RDD_SIZE        2

THREAD STATIC t_cTableName := ""
THREAD STATIC t_cEngine := ""
THREAD STATIC t_cServer := ""
THREAD STATIC t_cUserName := ""
THREAD STATIC t_cPassword := ""
THREAD STATIC t_cQuery := ""

PROCEDURE ADO()  /* ANNOUNCE */
   RETURN

PROCEDURE ADORDD()  /* ANNOUNCE */  /* DEPRECATED */
   RETURN

STATIC FUNCTION ADO_INIT( nRDD )

   LOCAL aRData := Array( RDD_SIZE )

   USRRDD_RDDDATA( nRDD, aRData )

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_NEW( nWA )

   LOCAL aWAData := Array( WA_SIZE )

   aWAData[ WA_BOF ] := .F.
   aWAData[ WA_EOF ] := .F.

   USRRDD_AREADATA( nWA, aWAData )

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_CREATE( nWA, aOpenInfo )

   LOCAL cDataBase  := hb_tokenGet( aOpenInfo[ UR_OI_NAME ], 1, ";" )
   LOCAL cTableName := hb_tokenGet( aOpenInfo[ UR_OI_NAME ], 2, ";" )
   LOCAL cDbEngine  := hb_tokenGet( aOpenInfo[ UR_OI_NAME ], 3, ";" )
   LOCAL cServer    := hb_tokenGet( aOpenInfo[ UR_OI_NAME ], 4, ";" )
   LOCAL cUserName  := hb_tokenGet( aOpenInfo[ UR_OI_NAME ], 5, ";" )
   LOCAL cPassword  := hb_tokenGet( aOpenInfo[ UR_OI_NAME ], 6, ";" )

   LOCAL oConnection := win_oleCreateObject( "ADODB.Connection" )
   LOCAL oCatalog := win_oleCreateObject( "ADOX.Catalog" )
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL oError, n

   LOCAL cParam

   DO CASE
   CASE Lower( hb_FNameExt( cDatabase ) ) == ".mdb"
      cParam := ;
         "Provider=Microsoft.Jet.OLEDB.4.0" + ;
         ";Data Source=" + cDataBase
      IF ! hb_vfExists( cDataBase )
         oCatalog:Create( cParam )
      ENDIF
      oConnection:Open( cParam )

   CASE Lower( hb_FNameExt( cDatabase ) ) == ".xls"
      cParam := ;
         "Provider=Microsoft.Jet.OLEDB.4.0" + ;
         ";Data Source=" + cDataBase + ;
         ";Extended Properties='Excel 8.0;HDR=YES'" + ;
         ";Persist Security Info=False"
      IF ! hb_vfExists( cDataBase )
         oCatalog:Create( cParam )
      ENDIF
      oConnection:Open( cParam )

   CASE Lower( hb_FNameExt( cDatabase ) ) == ".db"
      cParam := ;
         "Provider=Microsoft.Jet.OLEDB.4.0" + ;
         ";Data Source=" + cDataBase + ;
         ";Extended Properties='Paradox 3.x';"
      IF ! hb_vfExists( cDataBase )
         oCatalog:Create( cParam )
      ENDIF
      oConnection:Open( cParam )

   CASE Lower( hb_FNameExt( cDatabase ) ) == ".fdb"
      cParam := ;
         "Driver=Firebird/InterBase(r) driver" + ;
         ";UID=" + cUserName + ;
         ";PWD=" + cPassword + ;
         ";DbName=" + cDataBase + ";"
      IF ! hb_vfExists( cDataBase )
         oCatalog:Create( cParam )
      ENDIF
      oConnection:Open( cParam )
      oConnection:CursorLocation := adUseClient

   CASE Upper( cDbEngine ) == "MARIADB"  /* https://mariadb.com/kb/en/about-the-mariadb-odbc-driver/ */
      oConnection:Open( ;
         "Driver={MariaDB ODBC 1.0 Driver}" + ;
         ";Server=" + cServer + ;
         ";Database=" + cDataBase + ;
         ";UID=" + cUserName + ;
         ";PWD=" + cPassword )

   CASE Upper( cDbEngine ) == "MYSQL"
      oConnection:Open( ;
         "Driver={MySQL ODBC 3.51 Driver}" + ;
         ";Server=" + cServer + ;
         ";Database=" + cDataBase + ;
         ";UID=" + cUserName + ;
         ";PWD=" + cPassword )

   ENDCASE

   BEGIN SEQUENCE WITH __BreakBlock()
      oConnection:Execute( "DROP TABLE " + cTableName )
   RECOVER
   END SEQUENCE

   BEGIN SEQUENCE WITH __BreakBlock()
      IF Lower( hb_FNameExt( cDataBase ) ) == ".fdb"
         oConnection:Execute( "CREATE TABLE " + cTableName + " (" + hb_StrReplace( aWAData[ WA_SQLSTRUCT ], "[]", '""' ) + ")" )
      ELSE
         oConnection:Execute( "CREATE TABLE [" + cTableName + "] (" + aWAData[ WA_SQLSTRUCT ] + ")" )
      ENDIF
   RECOVER
      oError := ErrorNew()
      oError:GenCode := EG_CREATE
      oError:SubCode := 1004
      oError:Description := hb_langErrMsg( EG_CREATE ) + " (" + ;
         hb_langErrMsg( EG_UNSUPPORTED ) + ")"
      oError:FileName := aOpenInfo[ UR_OI_NAME ]
      oError:CanDefault := .T.

      FOR n := 0 TO oConnection:Errors:Count - 1
         oError:Description += oConnection:Errors( n ):Description
      NEXT

      UR_SUPER_ERROR( nWA, oError )
   END SEQUENCE

   oConnection:Close()

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_CREATEFIELDS( nWA, aStruct )

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL field

   aWAData[ WA_SQLSTRUCT ] := ""

   FOR EACH field IN aStruct

      IF ! field:__enumIsFirst()
         aWAData[ WA_SQLSTRUCT ] += ", "
      ENDIF

      aWAData[ WA_SQLSTRUCT ] += "[" + field[ DBS_NAME ] + "]"

      SWITCH Left( field[ DBS_TYPE ], 1 )
      CASE "C"
         aWAData[ WA_SQLSTRUCT ] += " CHAR(" + hb_ntos( field[ DBS_LEN ] ) + ") NULL"
         EXIT
      CASE "V"
         aWAData[ WA_SQLSTRUCT ] += " VARCHAR(" + hb_ntos( field[ DBS_LEN ] ) + ") NULL"
         EXIT
      CASE "B"
         aWAData[ WA_SQLSTRUCT ] += " DOUBLE NULL"
         EXIT
      CASE "Y"
         aWAData[ WA_SQLSTRUCT ] += " SMALLINT NULL"
         EXIT
      CASE "I"
         aWAData[ WA_SQLSTRUCT ] += " MEDIUMINT NULL"
         EXIT
      CASE "D"
         aWAData[ WA_SQLSTRUCT ] += " DATE NULL"
         EXIT
      CASE "T"
         aWAData[ WA_SQLSTRUCT ] += " DATETIME NULL"
         EXIT
      CASE "@"
         aWAData[ WA_SQLSTRUCT ] += " TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP"
         EXIT
      CASE "M"
         aWAData[ WA_SQLSTRUCT ] += " TEXT NULL"
         EXIT
      CASE "N"
         aWAData[ WA_SQLSTRUCT ] += " NUMERIC(" + hb_ntos( field[ DBS_LEN ] ) + ")"
         EXIT
      CASE "L"
         aWAData[ WA_SQLSTRUCT ] += " LOGICAL"
      ENDSWITCH
   NEXT

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_OPEN( nWA, aOpenInfo )

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL aField, oError, nResult
   LOCAL oRecordSet, nTotalFields, n

   /* When there is no ALIAS we will create new one using file name */
   IF Empty( aOpenInfo[ UR_OI_ALIAS ] )
      aOpenInfo[ UR_OI_ALIAS ] := hb_FNameName( aOpenInfo[ UR_OI_NAME ] )
   ENDIF

   IF Empty( aOpenInfo[ UR_OI_CONNECT ] )
      aWAData[ WA_CONNECTION ] := win_oleCreateObject( "ADODB.Connection" )
      aWAData[ WA_TABLENAME ] := t_cTableName
      aWAData[ WA_QUERY ] := t_cQuery
      aWAData[ WA_USERNAME ] := t_cUserName
      aWAData[ WA_PASSWORD ] := t_cPassword
      aWAData[ WA_SERVER ] := t_cServer
      aWAData[ WA_ENGINE ] := t_cEngine
      aWAData[ WA_CONNOPEN ] := .T.

      DO CASE
      CASE Lower( hb_FNameExt( aOpenInfo[ UR_OI_NAME ] ) ) == ".mdb"
         aWAData[ WA_CONNECTION ]:Open( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + aOpenInfo[ UR_OI_NAME ] + ;
            iif( Empty( aWAData[ WA_PASSWORD ] ), "", ";Jet OLEDB:Database Password=" + AllTrim( aWAData[ WA_PASSWORD ] ) ) )

      CASE Lower( hb_FNameExt( aOpenInfo[ UR_OI_NAME ] ) ) == ".xls"
         aWAData[ WA_CONNECTION ]:Open( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + aOpenInfo[ UR_OI_NAME ] + ";Extended Properties='Excel 8.0;HDR=YES';Persist Security Info=False" )

      CASE Lower( hb_FNameExt( aOpenInfo[ UR_OI_NAME ] ) ) == ".dbf"
         aWAData[ WA_CONNECTION ]:Open( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + aOpenInfo[ UR_OI_NAME ] + ";Extended Properties=dBASE IV;User ID=Admin;Password=;" )

      CASE Lower( hb_FNameExt( aOpenInfo[ UR_OI_NAME ] ) ) == ".db"
         aWAData[ WA_CONNECTION ]:Open( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + aOpenInfo[ UR_OI_NAME ] + ";Extended Properties='Paradox 3.x';" )

      CASE aWAData[ WA_ENGINE ] == "MARIADB"
         aWAData[ WA_CONNECTION ]:Open( ;
            "Driver={MariaDB ODBC 1.0 Driver}" + ;
            ";Server=" + aWAData[ WA_SERVER ] + ;
            ";Database=" + aOpenInfo[ UR_OI_NAME ] + ;
            ";UID=" + aWAData[ WA_USERNAME ] + ;
            ";PWD=" + aWAData[ WA_PASSWORD ] )

      CASE aWAData[ WA_ENGINE ] == "MYSQL"
         aWAData[ WA_CONNECTION ]:Open( ;
            "Driver={MySQL ODBC 3.51 Driver}" + ;
            ";Server=" + aWAData[ WA_SERVER ] + ;
            ";Database=" + aOpenInfo[ UR_OI_NAME ] + ;
            ";UID=" + aWAData[ WA_USERNAME ] + ;
            ";PWD=" + aWAData[ WA_PASSWORD ] )

      CASE aWAData[ WA_ENGINE ] == "SQL"
         aWAData[ WA_CONNECTION ]:Open( ;
            "Provider=SQLOLEDB" + ;
            ";Server=" + aWAData[ WA_SERVER ] + ;
            ";Database=" + aOpenInfo[ UR_OI_NAME ] + ;
            ";UID=" + aWAData[ WA_USERNAME ] + ;
            ";PWD=" + aWAData[ WA_PASSWORD ] )

      CASE aWAData[ WA_ENGINE ] == "ORACLE"
         aWAData[ WA_CONNECTION ]:Open( ;
            "Provider=MSDAORA.1" + ;
            ";Persist Security Info=False" + ;
            iif( Empty( aWAData[ WA_SERVER ] ), "", ";Data source=" + aWAData[ WA_SERVER ] ) + ;
            ";User ID=" + aWAData[ WA_USERNAME ] + ;
            ";Password=" + aWAData[ WA_PASSWORD ] )

      CASE aWAData[ WA_ENGINE ] == "FIREBIRD"
         aWAData[ WA_CONNECTION ]:Open( ;
            "Driver=Firebird/InterBase(r) driver" + ;
            ";Persist Security Info=False" + ;
            ";UID=" + aWAData[ WA_USERNAME ] + ;
            ";PWD=" + aWAData[ WA_PASSWORD ] + ;
            ";DbName=" + aOpenInfo[ UR_OI_NAME ] )
      ENDCASE
   ELSE
      aWAData[ WA_CONNECTION ] := win_oleAuto()
      aWAData[ WA_CONNECTION ]:__hObj := aOpenInfo[ UR_OI_CONNECT ] /* "ADODB.Connection" */
      aWAData[ WA_TABLENAME ] := t_cTableName
      aWAData[ WA_QUERY ] := t_cQuery
      aWAData[ WA_USERNAME ] := t_cUserName
      aWAData[ WA_PASSWORD ] := t_cPassword
      aWAData[ WA_SERVER ] := t_cServer
      aWAData[ WA_ENGINE ] := t_cEngine
      aWAData[ WA_CONNOPEN ] := .F.
   ENDIF

   /* will be initilized */
   t_cQuery := ""

   IF Empty( aWAData[ WA_QUERY ] )
      aWAData[ WA_QUERY ] := "SELECT * FROM "
   ENDIF

   oRecordSet := win_oleCreateObject( "ADODB.Recordset" )

   IF oRecordSet == NIL
      oError := ErrorNew()
      oError:GenCode := EG_OPEN
      oError:SubCode := 1001
      oError:Description := hb_langErrMsg( EG_OPEN )
      oError:FileName := aOpenInfo[ UR_OI_NAME ]
      oError:OsCode := 0 /* TODO */
      oError:CanDefault := .T.

      UR_SUPER_ERROR( nWA, oError )
      RETURN HB_FAILURE
   ENDIF
   oRecordSet:CursorType := adOpenDynamic
   oRecordSet:CursorLocation := adUseClient
   oRecordSet:LockType := adLockPessimistic
   IF aWAData[ WA_QUERY ] == "SELECT * FROM "
      oRecordSet:Open( aWAData[ WA_QUERY ] + aWAData[ WA_TABLENAME ], aWAData[ WA_CONNECTION ] )
   ELSE
      oRecordSet:Open( aWAData[ WA_QUERY ], aWAData[ WA_CONNECTION ] )
   ENDIF

   BEGIN SEQUENCE WITH __BreakBlock()
      aWAData[ WA_CATALOG ] := win_oleCreateObject( "ADOX.Catalog" )
      aWAData[ WA_CATALOG ]:ActiveConnection := aWAData[ WA_CONNECTION ]
   RECOVER
   END SEQUENCE

   IF Empty( aWAData[ WA_CATALOG ] )
      BEGIN SEQUENCE WITH __BreakBlock()
         aWAData[ WA_CATALOG ] := aWAData[ WA_CONNECTION ]:OpenSchema( adSchemaIndexes )
      RECOVER
      END SEQUENCE
   ENDIF

   aWAData[ WA_RECORDSET ] := oRecordSet
   aWAData[ WA_BOF ] := aWAData[ WA_EOF ] := .F.

   UR_SUPER_SETFIELDEXTENT( nWA, nTotalFields := oRecordSet:Fields:Count )

   FOR n := 1 TO nTotalFields
      aField := Array( UR_FI_SIZE )
      aField[ UR_FI_NAME ] := oRecordSet:Fields( n - 1 ):Name
      aField[ UR_FI_TYPE ] := ADO_GETFIELDTYPE( oRecordSet:Fields( n - 1 ):Type )
      aField[ UR_FI_TYPEEXT ] := 0
      aField[ UR_FI_LEN ] := ADO_GETFIELDSIZE( aField[ UR_FI_TYPE ], oRecordSet:Fields( n - 1 ):DefinedSize )
      aField[ UR_FI_DEC ] := 0
#ifdef UR_FI_FLAGS
      aField[ UR_FI_FLAGS ] := 0
#endif
#ifdef UR_FI_STEP
      aField[ UR_FI_STEP ] := 0
#endif
      UR_SUPER_ADDFIELD( nWA, aField )
   NEXT

   nResult := UR_SUPER_OPEN( nWA, aOpenInfo )

   IF nResult == HB_SUCCESS
      ADO_GOTOP( nWA )
   ENDIF

   RETURN nResult

STATIC FUNCTION ADO_CLOSE( nWA )

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   BEGIN SEQUENCE WITH __BreakBlock()
      oRecordSet:Close()
      IF ! Empty( aWAData[ WA_CONNOPEN ] )
         IF aWAData[ WA_CONNECTION ]:State != adStateClosed
            IF aWAData[ WA_CONNECTION ]:State != adStateOpen
               aWAData[ WA_CONNECTION ]:Cancel()
            ELSE
               aWAData[ WA_CONNECTION ]:Close()
            ENDIF
         ENDIF
      ENDIF
   RECOVER
   END SEQUENCE

   RETURN UR_SUPER_CLOSE( nWA )

STATIC FUNCTION ADO_GETVALUE( nWA, nField, /* @ */ xValue )

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL rs := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   IF aWAData[ WA_EOF ] .OR. rs:EOF .OR. rs:BOF
      IF ADO_GETFIELDTYPE( rs:Fields( nField - 1 ):Type ) == HB_FT_STRING
         xValue := Space( rs:Fields( nField - 1 ):DefinedSize )
      ELSE
         xValue := NIL
      ENDIF
   ELSE
      xValue := rs:Fields( nField - 1 ):Value

      SWITCH ADO_GETFIELDTYPE( rs:Fields( nField - 1 ):Type )
      CASE HB_FT_STRING
         IF ValType( xValue ) == "U"
            xValue := Space( rs:Fields( nField - 1 ):DefinedSize )
         ELSE
            xValue := PadR( xValue, rs:Fields( nField - 1 ):DefinedSize )
         ENDIF
         EXIT
      CASE HB_FT_DATE
         /* Null values */
         IF ValType( xValue ) == "U"
            xValue := hb_SToD()
         ENDIF
         EXIT
      CASE HB_FT_TIMESTAMP
         /* Null values */
         IF ValType( xValue ) == "U"
            xValue := hb_SToD()
         ENDIF
         EXIT
      ENDSWITCH
   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_GOTO( nWA, nRecord )

   LOCAL nRecNo
   LOCAL rs := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   IF rs:RecordCount > 0
      rs:MoveFirst()
      rs:Move( nRecord - 1, 0 )
   ENDIF
   ADO_RECID( nWA, @nRecNo )

   RETURN iif( nRecord == nRecNo, HB_SUCCESS, HB_FAILURE )

STATIC FUNCTION ADO_GOTOID( nWA, nRecord )

   LOCAL nRecNo
   LOCAL rs := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   IF rs:RecordCount > 0
      rs:MoveFirst()
      rs:Move( nRecord - 1, 0 )
   ENDIF
   ADO_RECID( nWA, @nRecNo )

   RETURN iif( nRecord == nRecNo, HB_SUCCESS, HB_FAILURE )

STATIC FUNCTION ADO_GOTOP( nWA )

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL oRecordSet := aWAData[ WA_RECORDSET ]

   IF oRecordSet:RecordCount() != 0
      oRecordSet:MoveFirst()
   ENDIF

   aWAData[ WA_BOF ] := .F.
   aWAData[ WA_EOF ] := .F.

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_GOBOTTOM( nWA )

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL oRecordSet := aWAData[ WA_RECORDSET ]

   oRecordSet:MoveLast()

   aWAData[ WA_BOF ] := .F.
   aWAData[ WA_EOF ] := .F.

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_SKIPRAW( nWA, nToSkip )

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL oRecordSet := aWAData[ WA_RECORDSET ]
   LOCAL nResult := HB_SUCCESS

   IF ! Empty( aWAData[ WA_PENDINGREL ] )
      IF ADO_FORCEREL( nWA ) != HB_SUCCESS
         RETURN HB_FAILURE
      ENDIF
   ENDIF

   IF nToSkip != 0
      IF aWAData[ WA_EOF ]
         IF nToSkip > 0
            RETURN HB_SUCCESS
         ENDIF
         ADO_GOBOTTOM( nWA )
         ++nToSkip
      ENDIF
      BEGIN SEQUENCE WITH __BreakBlock()
         IF aWAData[ WA_CONNECTION ]:State != adStateClosed
            IF nToSkip < 0 .AND. oRecordSet:AbsolutePosition <= - nToSkip
               oRecordSet:MoveFirst()
               aWAData[ WA_BOF ] := .T.
               aWAData[ WA_EOF ] := oRecordSet:EOF
            ELSEIF nToSkip != 0
               oRecordSet:Move( nToSkip )
               aWAData[ WA_BOF ] := .F.
               aWAData[ WA_EOF ] := oRecordSet:EOF
            ENDIF
         ELSE
            nResult := HB_FAILURE
         ENDIF
      RECOVER
         nResult := HB_FAILURE
      END SEQUENCE
   ENDIF

   RETURN nResult

STATIC FUNCTION ADO_BOF( nWA, /* @ */ lBof )

   LOCAL aWAData := USRRDD_AREADATA( nWA )

   lBof := aWAData[ WA_BOF ]

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_EOF( nWA, /* @ */ lEof )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]
   LOCAL nResult := HB_SUCCESS

   BEGIN SEQUENCE WITH __BreakBlock()
      IF USRRDD_AREADATA( nWA )[ WA_CONNECTION ]:State != adStateClosed
         lEof := ( oRecordSet:AbsolutePosition == -3 )
      ENDIF
   RECOVER
      nResult := HB_FAILURE
   END SEQUENCE

   RETURN nResult

STATIC FUNCTION ADO_DELETED( nWA, /* @ */ lDeleted )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   BEGIN SEQUENCE WITH __BreakBlock()
      lDeleted := ( oRecordSet:Status == adRecDeleted )
   RECOVER
      lDeleted := .F.
   END SEQUENCE

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_DELETE( nWA )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   oRecordSet:Delete()

   ADO_SKIPRAW( nWA, 1 )

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_RECNO( nWA, /* @ */ nRecNo )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]
   LOCAL nResult := HB_SUCCESS

   BEGIN SEQUENCE WITH __BreakBlock()
      IF USRRDD_AREADATA( nWA )[ WA_CONNECTION ]:State != adStateClosed
         nRecno := iif( oRecordSet:AbsolutePosition == -3, oRecordSet:RecordCount() + 1, oRecordSet:AbsolutePosition )
      ELSE
         nRecno := 0
         nResult := HB_FAILURE
      ENDIF
   RECOVER
      nRecNo := 0
      nResult := HB_FAILURE
   END SEQUENCE

   RETURN nResult

STATIC FUNCTION ADO_RECID( nWA, /* @ */ nRecNo )
   RETURN ADO_RECNO( nWA, @nRecNo )

STATIC FUNCTION ADO_RECCOUNT( nWA, /* @ */ nRecords )

   nRecords := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]:RecordCount()

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_PUTVALUE( nWA, nField, xValue )

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL oRecordSet := aWAData[ WA_RECORDSET ]

   IF ! aWAData[ WA_EOF ] .AND. !( oRecordSet:Fields( nField - 1 ):Value == xValue )
      oRecordSet:Fields( nField - 1 ):Value := xValue
      BEGIN SEQUENCE WITH __BreakBlock()
         oRecordSet:Update()
      RECOVER
      END SEQUENCE
   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_APPEND( nWA, lUnLockAll )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   HB_SYMBOL_UNUSED( lUnLockAll )

   oRecordSet:AddNew()

   BEGIN SEQUENCE WITH __BreakBlock()
      oRecordSet:Update()
   RECOVER
   END SEQUENCE

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_FLUSH( nWA )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   BEGIN SEQUENCE WITH __BreakBlock()
      oRecordSet:Update()
   RECOVER
   END SEQUENCE

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_ORDINFO( nWA, nIndex, aOrderInfo )

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL oRecordSet := aWAData[ WA_RECORDSET ]
   LOCAL nResult := HB_SUCCESS

   DO CASE
   CASE nIndex == DBOI_EXPRESSION
      IF ! Empty( aWAData[ WA_CATALOG ] ) .AND. ! Empty( aOrderInfo[ UR_ORI_TAG ] ) .AND. ;
            aOrderInfo[ UR_ORI_TAG ] < aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes:Count
         aOrderInfo[ UR_ORI_RESULT ] := aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes( aOrderInfo[ UR_ORI_TAG ] ):Name
      ELSE
         aOrderInfo[ UR_ORI_RESULT ] := ""
      ENDIF
   CASE nIndex == DBOI_NAME
      IF ! Empty( aWAData[ WA_CATALOG ] ) .AND. ! Empty( aOrderInfo[ UR_ORI_TAG ] ) .AND. ;
            aOrderInfo[ UR_ORI_TAG ] < aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes:Count
         aOrderInfo[ UR_ORI_RESULT ] := aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes( aOrderInfo[ UR_ORI_TAG ] ):Name
      ELSE
         aOrderInfo[ UR_ORI_RESULT ] := ""
      ENDIF
   CASE nIndex == DBOI_NUMBER
      aOrderInfo[ UR_ORI_RESULT ] := aOrderInfo[ UR_ORI_TAG ]
   CASE nIndex == DBOI_BAGNAME
      aOrderInfo[ UR_ORI_RESULT ] := ""
   CASE nIndex == DBOI_BAGEXT
      aOrderInfo[ UR_ORI_RESULT ] := ""
   CASE nIndex == DBOI_ORDERCOUNT
      IF Empty( aWAData[ WA_CATALOG ] )
         aOrderInfo[ UR_ORI_RESULT ] := 0
      ELSE
         aOrderInfo[ UR_ORI_RESULT ] := aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes:Count
      ENDIF
   CASE nIndex == DBOI_FILEHANDLE
      aOrderInfo[ UR_ORI_RESULT ] := 0
   CASE nIndex == DBOI_ISCOND
      aOrderInfo[ UR_ORI_RESULT ] := .F.
   CASE nIndex == DBOI_ISDESC
      aOrderInfo[ UR_ORI_RESULT ] := .F.
   CASE nIndex == DBOI_UNIQUE
      aOrderInfo[ UR_ORI_RESULT ] := .F.
   CASE nIndex == DBOI_POSITION
      IF aWAData[ WA_CONNECTION ]:State != adStateClosed
         ADO_RECID( nWA, @aOrderInfo[ UR_ORI_RESULT ] )
      ELSE
         aOrderInfo[ UR_ORI_RESULT ] := 0
         nResult := HB_FAILURE
      ENDIF
   CASE nIndex == DBOI_RECNO
      IF aWAData[ WA_CONNECTION ]:State != adStateClosed
         ADO_RECID( nWA, @aOrderInfo[ UR_ORI_RESULT ] )
      ELSE
         aOrderInfo[ UR_ORI_RESULT ] := 0
         nResult := HB_FAILURE
      ENDIF
   CASE nIndex == DBOI_KEYCOUNT
      IF aWAData[ WA_CONNECTION ]:State != adStateClosed
         aOrderInfo[ UR_ORI_RESULT ] := oRecordSet:RecordCount
      ELSE
         aOrderInfo[ UR_ORI_RESULT ] := 0
         nResult := HB_FAILURE
      ENDIF
   ENDCASE

   RETURN nResult

STATIC FUNCTION ADO_RECINFO( nWA, nRecord, nInfoType, /* @ */ uInfo )

   LOCAL nResult := HB_SUCCESS

#if 0
   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]
#endif

   HB_SYMBOL_UNUSED( nWA )

#ifdef UR_DBRI_DELETED
   DO CASE
   CASE nInfoType == UR_DBRI_DELETED
      uInfo := .F.
   CASE nInfoType == UR_DBRI_LOCKED
      uInfo := .T.
   CASE nInfoType == UR_DBRI_RECSIZE
   CASE nInfoType == UR_DBRI_RECNO
      nResult := ADO_RECID( nWA, @nRecord )
   CASE nInfoType == UR_DBRI_UPDATED
      uInfo := .F.
   CASE nInfoType == UR_DBRI_ENCRYPTED
      uInfo := .F.
   CASE nInfoType == UR_DBRI_RAWRECORD
      uInfo := ""
   CASE nInfoType == UR_DBRI_RAWMEMOS
      uInfo := ""
   CASE nInfoType == UR_DBRI_RAWDATA
      nResult := ADO_GOTO( nWA, nRecord )
      uInfo := ""
   ENDCASE
#else
   HB_SYMBOL_UNUSED( nRecord )
   HB_SYMBOL_UNUSED( nInfoType )
   HB_SYMBOL_UNUSED( uInfo )
#endif

   RETURN nResult

STATIC FUNCTION ADO_FIELDNAME( nWA, nField, /* @ */ cFieldName )

   LOCAL nResult := HB_SUCCESS
   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   BEGIN SEQUENCE WITH __BreakBlock()
      cFieldName := oRecordSet:Fields( nField - 1 ):Name
   RECOVER
      cFieldName := ""
      nResult := HB_FAILURE
   END SEQUENCE

   RETURN nResult

STATIC FUNCTION ADO_FIELDINFO( nWA, nField, nInfoType, /* @ */ uInfo )

   LOCAL nType, nLen
   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   DO CASE
   CASE nInfoType == DBS_NAME
      uInfo := oRecordSet:Fields( nField - 1 ):Name

   CASE nInfoType == DBS_TYPE
      nType := ADO_GETFIELDTYPE( oRecordSet:Fields( nField - 1 ):Type )
      DO CASE
      CASE nType == HB_FT_STRING
         uInfo := "C"
      CASE nType == HB_FT_LOGICAL
         uInfo := "L"
      CASE nType == HB_FT_MEMO
         uInfo := "M"
      CASE nType == HB_FT_OLE
         uInfo := "G"
#ifdef HB_FT_PICTURE
      CASE nType == HB_FT_PICTURE
         uInfo := "P"
#endif
      CASE nType == HB_FT_ANY
         uInfo := "V"
      CASE nType == HB_FT_DATE
         uInfo := "D"
#ifdef HB_FT_DATETIME
      CASE nType == HB_FT_DATETIME
         uInfo := "T"
#endif
      CASE nType == HB_FT_TIMESTAMP
         uInfo := "@"
      CASE nType == HB_FT_LONG
         uInfo := "N"
      CASE nType == HB_FT_INTEGER
         uInfo := "I"
      CASE nType == HB_FT_DOUBLE
         uInfo := "B"
      OTHERWISE
         uInfo := "U"
      ENDCASE

   CASE nInfoType == DBS_LEN
      ADO_FIELDINFO( nWA, nField, DBS_TYPE, @nType )
      IF nType == "N"
         nLen := oRecordSet:Fields( nField - 1 ):Precision
      ELSE
         nLen := oRecordSet:Fields( nField - 1 ):DefinedSize
      ENDIF
      /* Un campo mayor de 1024 lo consideramos un campo memo */
      uInfo := iif( nLen > 1024, 10, nLen )

   CASE nInfoType == DBS_DEC
      ADO_FIELDINFO( nWA, nField, DBS_LEN, @nLen )
      ADO_FIELDINFO( nWA, nField, DBS_TYPE, @nType )
      IF oRecordSet:Fields( nField - 1 ):Type == adInteger
         uInfo := 0
      ELSEIF nType == "N"
         uInfo := Min( Max( 0, nLen - 1 - oRecordSet:Fields( nField - 1 ):DefinedSize ), 15 )
      ELSE
         uInfo := 0
      ENDIF
#ifdef DBS_FLAG
   CASE nInfoType == DBS_FLAG
      uInfo := 0
#endif
#ifdef DBS_STEP
   CASE nInfoType == DBS_STEP
      uInfo := 0
#endif
   OTHERWISE
      RETURN HB_FAILURE
   ENDCASE

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_ORDLSTFOCUS( nWA, aOrderInfo )

   HB_SYMBOL_UNUSED( nWA )
   HB_SYMBOL_UNUSED( aOrderInfo )

   /* TODO */
#if 0
   LOCAL nRecNo
   LOCAL aWAData    := USRRDD_AREADATA( nWA )
   LOCAL oRecordSet := aWAData[ WA_RECORDSET ]

   ADO_RECID( nWA, @nRecNo )

   oRecordSet:Close()
   IF aOrderInfo[ UR_ORI_TAG ] == 0
       oRecordSet:Open( "SELECT * FROM " + s_aTableNames[ nWA ] , hb_QWith(), adOpenDynamic, adLockPessimistic )
   ELSE
    // oRecordSet:Open( "SELECT * FROM " + ::oTabla:cTabla + " ORDER BY " + ::OrdKey( uTag ), QWith(), adOpenDynamic, adLockPessimistic, adCmdUnspecified )
       oRecordSet:Open( "SELECT * FROM " + s_aTableNames[ nWA ], hb_QWith(), adOpenDynamic, adLockPessimistic )
   ENDIF
   aOrderInfo[ UR_ORI_RESULT ] := aOrderInfo[ UR_ORI_TAG ]

   ADO_GOTOP( nWA )
   ADO_GOTO( nWA, nRecNo )
#endif

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_PACK( nWA )

#if 0
   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]
#endif

   HB_SYMBOL_UNUSED( nWA )

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_RAWLOCK( nWA, nAction, nRecNo )

#if 0
   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]
#endif

   /* TODO */

   HB_SYMBOL_UNUSED( nRecNo )
   HB_SYMBOL_UNUSED( nWA )
   HB_SYMBOL_UNUSED( nAction )

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_LOCK( nWA, aLockInfo )

#if 0
   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]
#endif

   HB_SYMBOL_UNUSED( nWA )

   aLockInfo[ UR_LI_METHOD ] := DBLM_MULTIPLE
   aLockInfo[ UR_LI_RECORD ] := RecNo()
   aLockInfo[ UR_LI_RESULT ] := .T.

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_UNLOCK( nWA, xRecID )

#if 0
   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]
#endif

   /* TODO */

   HB_SYMBOL_UNUSED( xRecId )
   HB_SYMBOL_UNUSED( nWA )

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_SETFILTER( nWA, aFilterInfo )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   oRecordSet:Filter := SQLTranslate( aFilterInfo[ UR_FRI_CEXPR ] )

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_CLEARFILTER( nWA )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   BEGIN SEQUENCE WITH __BreakBlock()
      oRecordSet:Filter := ""
   RECOVER
   END SEQUENCE

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_ZAP( nWA )

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL oRecordSet := aWAData[ WA_RECORDSET ]

   IF aWAData[ WA_CONNECTION ] != NIL .AND. aWAData[ WA_TABLENAME ] != NIL
      BEGIN SEQUENCE WITH __BreakBlock()
         aWAData[ WA_CONNECTION ]:Execute( "TRUNCATE TABLE " + aWAData[ WA_TABLENAME ] )
      RECOVER
         aWAData[ WA_CONNECTION ]:Execute( "DELETE * FROM " + aWAData[ WA_TABLENAME ] )
      END SEQUENCE
      oRecordSet:Requery()
   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_SETLOCATE( nWA, aScopeInfo )

   LOCAL aWAData := USRRDD_AREADATA( nWA )

   aScopeInfo[ UR_SI_CFOR ] := SQLTranslate( aWAData[ WA_LOCATEFOR ] )

   aWAData[ WA_SCOPEINFO ] := aScopeInfo

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_LOCATE( nWA, lContinue )

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL oRecordSet := aWAData[ WA_RECORDSET ]

   oRecordSet:Find( aWAData[ WA_SCOPEINFO ][ UR_SI_CFOR ], iif( lContinue, 1, 0 ) )
   aWAData[ WA_FOUND ] := ! oRecordSet:EOF
   aWAData[ WA_EOF ] := oRecordSet:EOF

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_CLEARREL( nWA )

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL nKeys := 0, cKeyName

   BEGIN SEQUENCE WITH __BreakBlock()
      nKeys := aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys:Count
   RECOVER
   END SEQUENCE

   IF nKeys > 0
      cKeyName := aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys( nKeys - 1 ):Name
      IF !( Upper( cKeyName ) == "PRIMARYKEY" )
         aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys:Delete( cKeyName )
      ENDIF
   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_RELAREA( nWA, nRelNo, /* @ */ nRelArea )

   LOCAL aWAData := USRRDD_AREADATA( nWA )

   BEGIN SEQUENCE WITH __BreakBlock()
      IF nRelNo <= aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys:Count()
         nRelArea := Select( aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys( nRelNo - 1 ):RelatedTable )
      ENDIF
   RECOVER
      nRelArea := 0
   END SEQUENCE

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_RELTEXT( nWA, nRelNo, /* @ */ cExpr )

   LOCAL aWAData := USRRDD_AREADATA( nWA )

   BEGIN SEQUENCE WITH __BreakBlock()
      IF nRelNo <= aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys:Count()
         cExpr := aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys( nRelNo - 1 ):Columns( 0 ):RelatedColumn
      ENDIF
   RECOVER
      cExpr := ""
   END SEQUENCE

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_SETREL( nWA, aRelInfo )

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL cParent := Alias( aRelInfo[ UR_RI_PARENT ] )
   LOCAL cChild := Alias( aRelInfo[ UR_RI_CHILD ] )
   LOCAL cKeyName := cParent + "_" + cChild

   BEGIN SEQUENCE WITH __BreakBlock()
      aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys:Append( cKeyName, adKeyForeign, ;
         aRelInfo[ UR_RI_CEXPR ], cChild, aRelInfo[ UR_RI_CEXPR ] )
   RECOVER
      /* raise error for cannot create relation */
   END SEQUENCE

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_FORCEREL( nWA )

   LOCAL aPendingRel
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   IF ! Empty( aWAData[ WA_PENDINGREL ] )

      aPendingRel := aWAData[ WA_PENDINGREL ]
      aWAData[ WA_PENDINGREL ] := NIL

      RETURN ADO_RELEVAL( nWA, aPendingRel )
   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_RELEVAL( nWA, aRelInfo )

   LOCAL aInfo, nReturn, nOrder, uResult

   nReturn := ADO_EVALBLOCK( aRelInfo[ UR_RI_PARENT ], aRelInfo[ UR_RI_BEXPR ], @uResult )

   IF nReturn == HB_SUCCESS
      /* Check the current order */
      aInfo := Array( UR_ORI_SIZE )
      nReturn := ADO_ORDINFO( nWA, DBOI_NUMBER, aInfo )

      IF nReturn == HB_SUCCESS
         nOrder := aInfo[ UR_ORI_RESULT ]
         IF nOrder != 0
            IF aRelInfo[ UR_RI_SCOPED ]
               aInfo[ UR_ORI_NEWVAL ] := uResult
               nReturn := ADO_ORDINFO( nWA, DBOI_SCOPETOP, aInfo )
               IF nReturn == HB_SUCCESS
                  nReturn := ADO_ORDINFO( nWA, DBOI_SCOPEBOTTOM, aInfo )
               ENDIF
            ENDIF
            IF nReturn == HB_SUCCESS
               nReturn := ADO_SEEK( nWA, .F., uResult, .F. )
            ENDIF
         ELSE
            nReturn := ADO_GOTO( nWA, uResult )
         ENDIF
      ENDIF
   ENDIF

   RETURN nReturn

STATIC FUNCTION ADO_ORDLSTADD( nWA, aOrderInfo )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   BEGIN SEQUENCE WITH __BreakBlock()
      oRecordSet:Index := aOrderInfo[ UR_ORI_BAG ]
   RECOVER
   END SEQUENCE

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_ORDLSTCLEAR( nWA )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   BEGIN SEQUENCE WITH __BreakBlock()
      oRecordSet:Index := ""
   RECOVER
   END SEQUENCE

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_ORDCREATE( nWA, aOrderCreateInfo )

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL oIndex, oError, n, lFound := .F.

   IF aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes != NIL
      FOR n := 1 TO aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes:Count
         oIndex := aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes( n - 1 )
         IF oIndex:Name == iif( Empty( aOrderCreateInfo[ UR_ORCR_TAGNAME ] ), aOrderCreateInfo[ UR_ORCR_CKEY ], aOrderCreateInfo[ UR_ORCR_TAGNAME ] )
            lFound := .T.
            EXIT
         ENDIF
      NEXT
   ENDIF

   BEGIN SEQUENCE WITH __BreakBlock()
      IF aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes == NIL .OR. ! lFound
         oIndex := win_oleCreateObject( "ADOX.Index" )
         oIndex:Name := iif( Empty( aOrderCreateInfo[ UR_ORCR_TAGNAME ] ), aOrderCreateInfo[ UR_ORCR_CKEY ], aOrderCreateInfo[ UR_ORCR_TAGNAME ] )
         oIndex:PrimaryKey := .F.
         oIndex:Unique := aOrderCreateInfo[ UR_ORCR_UNIQUE ]
         oIndex:Columns:Append( aOrderCreateInfo[ UR_ORCR_CKEY ] )
         aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes:Append( oIndex )
      ENDIF
   RECOVER
      oError := ErrorNew()
      oError:GenCode := EG_CREATE
      oError:SubCode := 1004
      oError:Description := hb_langErrMsg( EG_CREATE ) + " (" + ;
         hb_langErrMsg( EG_UNSUPPORTED ) + ")"
      oError:FileName := aOrderCreateInfo[ UR_ORCR_BAGNAME ]
      oError:CanDefault := .T.
      UR_SUPER_ERROR( nWA, oError )
   END SEQUENCE

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_ORDDESTROY( nWA, aOrderInfo )

   LOCAL aWAData := USRRDD_AREADATA( nWA ), n, oIndex

   IF aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes != NIL
      FOR n := 1 TO aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes:Count
         oIndex := aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes( n - 1 )
         IF oIndex:Name == aOrderInfo[ UR_ORI_TAG ]
            aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes:Delete( oIndex:Name )
         ENDIF
      NEXT
   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_EVALBLOCK( nArea, bBlock, /* @ */ uResult )

   LOCAL nCurrArea := Select()

   IF nCurrArea != nArea
      dbSelectArea( nArea )
   ELSE
      nCurrArea := 0
   ENDIF

   uResult := Eval( bBlock )

   IF nCurrArea > 0
      dbSelectArea( nCurrArea )
   ENDIF

   RETURN HB_SUCCESS

STATIC FUNCTION ADO_EXISTS( nRdd, cTable, cIndex, ulConnect )

#if 0
   LOCAL n
#endif
   LOCAL nRet := HB_FAILURE
   LOCAL aRData := USRRDD_RDDDATA( nRDD )

   HB_SYMBOL_UNUSED( ulConnect )

   IF ! Empty( cTable ) .AND. ! Empty( aRData[ WA_CATALOG ] )
      BEGIN SEQUENCE WITH __BreakBlock()
#if 0
         n := aRData[ WA_CATALOG ]:Tables( cTable )
#endif
         nRet := HB_SUCCESS
      RECOVER
      END SEQUENCE
      IF ! Empty( cIndex )
         BEGIN SEQUENCE WITH __BreakBlock()
#if 0
            n := aRData[ WA_CATALOG ]:Tables( cTable ):Indexes( cIndex )
#endif
            nRet := HB_SUCCESS
         RECOVER
         END SEQUENCE
      ENDIF
   ENDIF

   RETURN nRet

STATIC FUNCTION ADO_DROP( nRdd, cTable, cIndex, ulConnect )

#if 0
   LOCAL n
#endif
   LOCAL nRet := HB_FAILURE
   LOCAL aRData := USRRDD_RDDDATA( nRDD )

   HB_SYMBOL_UNUSED( ulConnect )

   IF ! Empty( cTable ) .AND. ! Empty( aRData[ WA_CATALOG ] )
      BEGIN SEQUENCE WITH __BreakBlock()
#if 0
         n := aRData[ WA_CATALOG ]:Tables:Delete( cTable )
#endif
         nRet := HB_SUCCESS
      RECOVER
      END SEQUENCE
      IF ! Empty( cIndex )
         BEGIN SEQUENCE WITH __BreakBlock()
#if 0
            n := aRData[ WA_CATALOG ]:Tables( cTable ):Indexes:Delete( cIndex )
#endif
            nRet := HB_SUCCESS
         RECOVER
         END SEQUENCE
      ENDIF
   ENDIF

   RETURN nRet

STATIC FUNCTION ADO_SEEK( nWA, lSoftSeek, cKey, lFindLast )

   HB_SYMBOL_UNUSED( nWA )
   HB_SYMBOL_UNUSED( lSoftSeek )
   HB_SYMBOL_UNUSED( cKey )
   HB_SYMBOL_UNUSED( lFindLast )

   /* TODO */

   RETURN HB_FAILURE

STATIC FUNCTION ADO_FOUND( nWA, /* @ */ lFound )

   LOCAL aWAData := USRRDD_AREADATA( nWA )

   lFound := aWAData[ WA_FOUND ]

   RETURN HB_SUCCESS

FUNCTION ADORDD_GETFUNCTABLE( ... )  /* DEPRECATED */
   RETURN ADO_GETFUNCTABLE( ... )

FUNCTION ADO_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID )

   LOCAL aADOFunc[ UR_METHODCOUNT ]

   aADOFunc[ UR_INIT ]         := @ADO_INIT()
   aADOFunc[ UR_NEW ]          := @ADO_NEW()
   aADOFunc[ UR_CREATE ]       := @ADO_CREATE()
   aADOFunc[ UR_CREATEFIELDS ] := @ADO_CREATEFIELDS()
   aADOFunc[ UR_OPEN ]         := @ADO_OPEN()
   aADOFunc[ UR_CLOSE ]        := @ADO_CLOSE()
   aADOFunc[ UR_BOF  ]         := @ADO_BOF()
   aADOFunc[ UR_EOF  ]         := @ADO_EOF()
   aADOFunc[ UR_DELETED ]      := @ADO_DELETED()
   aADOFunc[ UR_SKIPRAW ]      := @ADO_SKIPRAW()
   aADOFunc[ UR_GOTO ]         := @ADO_GOTO()
   aADOFunc[ UR_GOTOID ]       := @ADO_GOTOID()
   aADOFunc[ UR_GOTOP ]        := @ADO_GOTOP()
   aADOFunc[ UR_GOBOTTOM ]     := @ADO_GOBOTTOM()
   aADOFunc[ UR_RECNO ]        := @ADO_RECNO()
   aADOFunc[ UR_RECID ]        := @ADO_RECID()
   aADOFunc[ UR_RECCOUNT ]     := @ADO_RECCOUNT()
   aADOFunc[ UR_GETVALUE ]     := @ADO_GETVALUE()
   aADOFunc[ UR_PUTVALUE ]     := @ADO_PUTVALUE()
   aADOFunc[ UR_DELETE ]       := @ADO_DELETE()
   aADOFunc[ UR_APPEND ]       := @ADO_APPEND()
   aADOFunc[ UR_FLUSH ]        := @ADO_FLUSH()
   aADOFunc[ UR_ORDINFO ]      := @ADO_ORDINFO()
   aADOFunc[ UR_RECINFO ]      := @ADO_RECINFO()
   aADOFunc[ UR_FIELDINFO ]    := @ADO_FIELDINFO()
   aADOFunc[ UR_FIELDNAME ]    := @ADO_FIELDNAME()
   aADOFunc[ UR_ORDLSTFOCUS ]  := @ADO_ORDLSTFOCUS()
   aADOFunc[ UR_PACK ]         := @ADO_PACK()
   aADOFunc[ UR_RAWLOCK ]      := @ADO_RAWLOCK()
   aADOFunc[ UR_LOCK ]         := @ADO_LOCK()
   aADOFunc[ UR_UNLOCK ]       := @ADO_UNLOCK()
   aADOFunc[ UR_SETFILTER ]    := @ADO_SETFILTER()
   aADOFunc[ UR_CLEARFILTER ]  := @ADO_CLEARFILTER()
   aADOFunc[ UR_ZAP ]          := @ADO_ZAP()
   aADOFunc[ UR_SETLOCATE ]    := @ADO_SETLOCATE()
   aADOFunc[ UR_LOCATE ]       := @ADO_LOCATE()
   aADOFunc[ UR_FOUND ]        := @ADO_FOUND()
   aADOFunc[ UR_FORCEREL ]     := @ADO_FORCEREL()
   aADOFunc[ UR_RELEVAL ]      := @ADO_RELEVAL()
   aADOFunc[ UR_CLEARREL ]     := @ADO_CLEARREL()
   aADOFunc[ UR_RELAREA ]      := @ADO_RELAREA()
   aADOFunc[ UR_RELTEXT ]      := @ADO_RELTEXT()
   aADOFunc[ UR_SETREL ]       := @ADO_SETREL()
   aADOFunc[ UR_ORDCREATE ]    := @ADO_ORDCREATE()
   aADOFunc[ UR_ORDDESTROY ]   := @ADO_ORDDESTROY()
   aADOFunc[ UR_ORDLSTADD ]    := @ADO_ORDLSTADD()
   aADOFunc[ UR_ORDLSTCLEAR ]  := @ADO_ORDLSTCLEAR()
   aADOFunc[ UR_EVALBLOCK ]    := @ADO_EVALBLOCK()
   aADOFunc[ UR_SEEK ]         := @ADO_SEEK()
   aADOFunc[ UR_EXISTS ]       := @ADO_EXISTS()
   aADOFunc[ UR_DROP ]         := @ADO_DROP()

   RETURN USRRDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID, ;
      /* NO SUPER RDD */, aADOFunc )

INIT PROCEDURE ADO_INIT()

   rddRegister( "ADO", RDT_FULL )
   rddRegister( "ADORDD", RDT_FULL )  /* DEPRECATED */

   RETURN

STATIC FUNCTION ADO_GETFIELDSIZE( nDBFFieldType, nADOFieldSize )

   IF HB_ISNUMERIC( nDBFFieldType )

      SWITCH nDBFFieldType
      CASE HB_FT_STRING
      CASE HB_FT_INTEGER
      CASE HB_FT_DOUBLE
         RETURN nADOFieldSize
#ifdef HB_FT_DATETIME
      CASE HB_FT_DATETIME
#endif
      CASE HB_FT_DATE
      CASE HB_FT_TIMESTAMP
         RETURN 8
#ifdef HB_FT_PICTURE
      CASE HB_FT_PICTURE
#endif
      CASE HB_FT_OLE
      CASE HB_FT_MEMO
         RETURN 10
      CASE HB_FT_LOGICAL
         RETURN 1
      ENDSWITCH
   ENDIF

   RETURN 0

STATIC FUNCTION ADO_GETFIELDTYPE( nADOFieldType )

   IF HB_ISNUMERIC( nADOFieldType )

      SWITCH nADOFieldType
      CASE adTinyInt
      CASE adSmallInt
      CASE adInteger
      CASE adBigInt
      CASE adCurrency
         RETURN HB_FT_INTEGER
      CASE adDouble
         RETURN HB_FT_DOUBLE
      CASE adDecimal
      CASE adNumeric
         RETURN HB_FT_LONG
      CASE adVariant
         RETURN HB_FT_ANY
#ifdef HB_FT_DATETIME
      CASE adDate
      CASE adDBDate
      CASE adFileTime
         RETURN HB_FT_DATETIME
#else
      CASE adFileTime
         EXIT
      CASE adDate
      CASE adDBDate
         RETURN HB_FT_DATE
#endif
      CASE adDBTimeStamp
         RETURN HB_FT_TIMESTAMP
      CASE adGUID
      CASE adBSTR
      CASE adChar
      CASE adVarChar
      CASE adLongVarChar
      CASE adWChar
      CASE adVarWChar
         RETURN HB_FT_STRING
      CASE adBinary
      CASE adVarBinary
      CASE adLongVarBinary
         RETURN HB_FT_OLE
      CASE adBoolean
         RETURN HB_FT_LOGICAL
      CASE adLongVarWChar
      CASE adPropVariant
         RETURN HB_FT_MEMO

      CASE adEmpty
      CASE adUnsignedTinyInt
      CASE adUnsignedSmallInt
      CASE adUnsignedInt
      CASE adUnsignedBigInt
      CASE adSingle
      CASE adError
      CASE adUserDefined
      CASE adIDispatch
      CASE adIUnknown
      CASE adDBTime
      CASE adChapter
      CASE adVarNumeric
#if 0
      CASE adArray
#endif
      ENDSWITCH
   ENDIF

   RETURN 0

PROCEDURE hb_adoSetTable( cTableName )

   t_cTableName := hb_defaultValue( cTableName, "" )

   RETURN

PROCEDURE hb_adoSetEngine( cEngine )

   t_cEngine := hb_defaultValue( cEngine, "" )

   RETURN

PROCEDURE hb_adoSetServer( cServer )

   t_cServer := hb_defaultValue( cServer, "" )

   RETURN

PROCEDURE hb_adoSetUser( cUser )

   t_cUserName := hb_defaultValue( cUser, "" )

   RETURN

PROCEDURE hb_adoSetPassword( cPassword )

   t_cPassword := hb_defaultValue( cPassword, "" )

   RETURN

PROCEDURE hb_adoSetQuery( cQuery )

   t_cQuery := hb_defaultValue( cQuery, "SELECT * FROM " )

   RETURN

PROCEDURE hb_adoSetLocateFor( cLocateFor )

   USRRDD_AREADATA( Select() )[ WA_LOCATEFOR ] := cLocateFor

   RETURN

STATIC FUNCTION SQLTranslate( cExpr )

   IF Left( cExpr, 1 ) == '"' .AND. Right( cExpr, 1 ) == '"'
      cExpr := SubStr( cExpr, 2, Len( cExpr ) - 2 )
   ENDIF

   RETURN hb_StrReplace( cExpr, { ;
      '""'    => ""    , ;
      '"'     => "'"   , ;
      "''"    => "'"   , ;
      "=="    => "="   , ;
      ".and." => "AND" , ;
      ".or."  => "OR"  , ;
      ".AND." => "AND" , ;
      ".OR."  => "OR"  } )

FUNCTION hb_adoRddGetConnection( nWA )

   IF ! HB_ISNUMERIC( nWA )
      nWA := Select()
   ENDIF

   RETURN USRRDD_AREADATA( nWA )[ WA_CONNECTION ]

FUNCTION hb_adoRddGetCatalog( nWA )

   IF ! HB_ISNUMERIC( nWA )
      nWA := Select()
   ENDIF

   RETURN USRRDD_AREADATA( nWA )[ WA_CATALOG ]

FUNCTION hb_adoRddGetRecordSet( nWA )

   LOCAL aWAData

   IF ! HB_ISNUMERIC( nWA )
      nWA := Select()
   ENDIF

   aWAData := USRRDD_AREADATA( nWA )

   RETURN iif( aWAData != NIL, aWAData[ WA_RECORDSET ], NIL )
