/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ADORDD - RDD to automatically manage Microsoft ADO
 *
 * Copyright 2007 Fernando Mancera <fmancera@viaopen.com> and
 * Antonio Linares <alinares@fivetechsoft.com>
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

#include "rddsys.ch"
#include "hbusrrdd.ch"
#include "fileio.ch"
#include "error.ch"
#include "adordd.ch"
#include "common.ch"

ANNOUNCE ADORDD

static s_cTableName, s_cEngine, s_cServer, s_cUserName, s_cPassword
static s_cQuery := "SELECT * FROM "
static s_aConnections[ 255 ], s_aCatalogs[ 255 ]
static s_aTableNames[ 255 ]

STATIC FUNCTION ADO_INIT( nRDD )

   LOCAL aRData := ARRAY( 10 )

   AFILL( aRData, -1 )
   USRRDD_RDDDATA( nRDD, aRData )

RETURN SUCCESS

STATIC FUNCTION ADO_NEW( pWA )

   LOCAL aWData := { -1, .F., .F. } 

   USRRDD_AREADATA( pWA, aWData )
	
RETURN SUCCESS

STATIC FUNCTION ADO_CREATE( nWA, aOpenInfo )

	/*
   LOCAL oError := ErrorNew()

   oError:GenCode     := EG_CREATE
   oError:SubCode     := 1004
   oError:Description := HB_LANGERRMSG( EG_CREATE ) + " (" + ;
                         HB_LANGERRMSG( EG_UNSUPPORTED ) + ")"
   oError:FileName    := aOpenInfo[ UR_OI_NAME ]
   oError:CanDefault  := .T.
   UR_SUPER_ERROR( nWA, oError )
   */
   
RETURN FAILURE

STATIC FUNCTION ADO_OPEN( nWA, aOpenInfo )

   LOCAL cName, nMode, nSlot, nHandle, aRData, aWData, aField, oError, nResult
   LOCAL oADO, nTotalFields := 0, i := 1

   // When there is no ALIAS we will create new one using file name
   IF aOpenInfo[ UR_OI_ALIAS ] == NIL
      HB_FNAMESPLIT( aOpenInfo[ UR_OI_NAME ], , @cName )
      aOpenInfo[ UR_OI_ALIAS ] := cName
   ENDIF
   
   nMode := IIF( aOpenInfo[ UR_OI_SHARED ], FO_SHARED , FO_EXCLUSIVE ) + ;
            IIF( aOpenInfo[ UR_OI_READONLY ], FO_READ, FO_READWRITE )

   aRData := USRRDD_RDDDATA( USRRDD_ID( nWA ) )
   aWData := USRRDD_AREADATA( nWA )
   nSlot := ASCAN( aRData, -1 )

   IF nSlot == 0
      oError := ErrorNew()
      oError:GenCode     := EG_OPEN
      oError:SubCode     := 1000
      oError:Description := HB_LANGERRMSG( EG_OPEN ) + ", no free slots"
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      oError:CanDefault  := .T.
      UR_SUPER_ERROR( nWA, oError )
      RETURN FAILURE
   ENDIF

   s_aConnections[ nWA ] = TOleAuto():New( "ADODB.Connection" )
   
   do case
       case Lower( Right( aOpenInfo[ UR_OI_NAME ], 4 ) ) == ".mdb"
            s_aConnections[ nWA ]:Open( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + aOpenInfo[ UR_OI_NAME ] )
               
       case s_cEngine == "MYSQL"
            s_aConnections[ nWA ]:Open( "DRIVER={MySQL ODBC 3.51 Driver};" + ;
                                        "server=" + s_cServer + ;
                                        ";database=" + aOpenInfo[ UR_OI_NAME ] + ;
                                        ";uid=" + s_cUserName + ;
                                        ";pwd=" + s_cPassword )
                                
       case s_cEngine == "SQL" 
            s_aConnections[ nWA ]:Open( "Provider=SQLOLEDB;" + ; 
                                        "server=" + s_cServer + ; 
                                        ";database=" + aOpenInfo[ UR_OI_NAME ] + ; 
                                        ";uid=" + s_cUserName + ; 
                                        ";pwd=" + s_cPassword )
                                
       case s_cEngine == "ORACLE"
            s_aConnections[ nWA ]:Open( "Provider=MSDAORA.1;" + ;
                                        "Persist Security Info=False" + ;
                                        If( s_cServer == NIL .OR. s_cServer == "",; 
                                            "", ";Data source=" + s_cServer ) + ;
                                        ";User ID=" + s_cUserName + ;
                                        + ";Password=" + s_cPassword )                                                                
       
   endcase                               

   oADO := TOleAuto():New( "ADODB.Recordset" )
   oAdo:CursorType     = adOpenDynamic
   oAdo:CursorLocation = adUseClient
   oAdo:LockType       = adLockPessimistic
   oAdo:Open( s_cQuery + s_cTableName, s_aConnections[ nWA ] )

   s_aCatalogs[ nWA ] = TOleAuto():New( "ADOX.Catalog" )
   s_aCatalogs[ nWA ]:ActiveConnection = s_aConnections[ nWA ]
   
   s_aTableNames[ nWA ] = s_cTableName
   
   IF oADO == NIL
      oError := ErrorNew()
      oError:GenCode     := EG_OPEN
      oError:SubCode     := 1001
      oError:Description := HB_LANGERRMSG( EG_OPEN )
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      oError:OsCode      := fError()
      oError:CanDefault  := .T.

      UR_SUPER_ERROR( nWA, oError )
      RETURN FAILURE
   ENDIF
   
   aRData[ nSlot ] := oADO
   aWData[ 1 ] := oADO
   aWData[ 2 ] := aWData[ 3 ] := .F.

    nTotalFields := oADO:Fields:Count
    
    UR_SUPER_SETFIELDEXTENT( nWA, oADO:Fields:Count )   
    	 
    FOR i = 1 TO nTotalFields
   		aField := ARRAY( UR_FI_SIZE )
   		aField[ UR_FI_NAME ]    := oADO:Fields( i - 1 ):Name
   		aField[ UR_FI_TYPE ]    := ADO_GETFIELDTYPE( oADO:Fields( i - 1 ):Type )
   		aField[ UR_FI_TYPEEXT ] := 0
   		aField[ UR_FI_LEN ]     := ADO_GETFIELDSIZE( aField[ UR_FI_TYPE ], oADO:Fields( i - 1 ):DefinedSize  )// 80   // set any arbitrary length - the real size will be differ
   		aField[ UR_FI_DEC ]     := 0
   		UR_SUPER_ADDFIELD( nWA, aField )
    NEXT

   nResult := UR_SUPER_OPEN( nWA, aOpenInfo )

   IF nResult == SUCCESS
      ADO_GOTOP( nWA )
   ENDIF
	
RETURN nResult

STATIC FUNCTION ADO_CLOSE( nWA )

   LOCAL aRData, oADO := USRRDD_AREADATA( nWA )[ 1 ]
			
   oADO:Close()   
   
   aRData := USRRDD_RDDDATA( USRRDD_ID( nWA ) )

RETURN UR_SUPER_CLOSE( nWA )

STATIC FUNCTION ADO_GETVALUE( nWA, nField, xValue )

   LOCAL aWData := USRRDD_AREADATA( nWA )
   LOCAL oADO := USRRDD_AREADATA( nWA )[ 1 ]

   IF aWData[ 3 ]
      xValue := ""
   ELSE
      xValue := oADO:Fields( nField - 1 ):Value
   ENDIF

RETURN SUCCESS

STATIC FUNCTION ADO_GOTO( nWA, nRecord )

/*
   LOCAL aWData := USRRDD_AREADATA( nWA )
   LOCAL oADO := aWData[ 1 ]
   
   IF nRecord <= 0
      aWData[ 2 ] := aWData[ 3 ] := .T.
   ELSEIF nRecord == 1
      oADO:MoveFirst()
      aWData[ 2 ] := aWData[ 3 ] := HB_FEOF()
   ELSE
      //HB_FSKIP(0) // Clear the EOF flag inside HB_F* engin
                  //   - it's not done automatically in HB_FGOBOTTOM() :-( 
      //HB_FGOTO( nRecord )
      oADO:Move( nRecord )
      aWData[ 2 ] := HB_FRECNO() == 0
      aWData[ 3 ] := HB_FEOF()
   ENDIF
  */ 
/*
   LOCAL aWData := USRRDD_AREADATA( nWA )
   HB_FSELECT( aWData[ 1 ] )
   IF nRecord <= 0
      aWData[ 2 ] := aWData[ 3 ] := .T.
   ELSEIF nRecord == 1
      HB_FGOTOP()
      aWData[ 2 ] := aWData[ 3 ] := HB_FEOF()
   ELSE
      HB_FSKIP(0) // Clear the EOF flag inside HB_F* engin
                  //   - it's not done automatically in HB_FGOBOTTOM() :-( 
      HB_FGOTO( nRecord )
      aWData[ 2 ] := HB_FRECNO() == 0
      aWData[ 3 ] := HB_FEOF()
   ENDIF

*/

RETURN SUCCESS

STATIC FUNCTION ADO_GOTOID( nWA, nRecord )

RETURN SUCCESS // ADO_GOTO( nWA, nRecord )

STATIC FUNCTION ADO_GOTOP( nWA )

   local oADO := USRRDD_AREADATA( nWA )[ 1 ]

   if oADO:RecordCount != 0
      oADO:MoveFirst()
   endif   
   USRRDD_AREADATA( nWA )[ 2 ] = .f.
   USRRDD_AREADATA( nWA )[ 3 ] = .f.

RETURN SUCCESS

STATIC FUNCTION ADO_GOBOTTOM( nWA )

   local oADO := USRRDD_AREADATA( nWA )[ 1 ]

   oADO:MoveLast()
   USRRDD_AREADATA( nWA )[ 2 ] = .f.
   USRRDD_AREADATA( nWA )[ 3 ] = .f.
 
RETURN SUCCESS

STATIC FUNCTION ADO_SKIPRAW( nWA, nRecords )

   LOCAL aWData, oADO

   IF nRecords != 0
      aWData := USRRDD_AREADATA( nWA )
      oADO := aWData[ 1 ]
      IF aWData[ 3 ]
         IF nRecords > 0
            RETURN SUCCESS
         ENDIF
         ADO_GOBOTTOM( nWA )
         ++nRecords
       ENDIF
       IF nRecords < 0 .AND. oADO:AbsolutePosition <= -nRecords
          oADO:MoveFirst()
          aWData[ 2 ] := .T.
          aWData[ 3 ] := oADO:EOF
       ELSEIF nRecords != 0
          oADO:Move( nRecords )
          aWData[ 2 ] := .F.
          aWData[ 3 ] := oADO:EOF
       ENDIF
   ENDIF

RETURN SUCCESS

STATIC FUNCTION ADO_BOF( nWA, lBof )
   
   LOCAL aWData := USRRDD_AREADATA( nWA )

   lBof := aWData[ 2 ]
   
RETURN SUCCESS

STATIC FUNCTION ADO_EOF( nWA, lEof )

   local oADO := USRRDD_AREADATA( nWA )[ 1 ]

   lEof := ( oADO:AbsolutePosition == -3 ) // lEof := aWData[ 3 ]  

RETURN SUCCESS

STATIC FUNCTION ADO_DELETED( nWA, lDeleted )

	local oADO := USRRDD_AREADATA( nWA )[ 1 ]

	IF oADO:Status == adRecDeleted // To be checked, ACCESS does not uses it
	   lDeleted := .T.
	ELSE
	   lDeleted := .F.
	ENDIF

RETURN SUCCESS

STATIC FUNCTION ADO_DELETE( nWA )

	local oADO := USRRDD_AREADATA( nWA )[ 1 ]

   oADO:Delete()
   
   ADO_SKIPRAW( nWA, 1 )

RETURN SUCCESS

STATIC FUNCTION ADO_RECID( nWA, nRecNo )

   local oADO := USRRDD_AREADATA( nWA )[ 1 ]
	
	 nRecno := If( oADO:AbsolutePosition == -3, oAdo:RecordCount + 1, oAdo:AbsolutePosition )
	 
RETURN SUCCESS

STATIC FUNCTION ADO_RECCOUNT( nWA, nRecords )

   local oADO := USRRDD_AREADATA( nWA )[ 1 ]

   nRecords := oADO:RecordCount

RETURN SUCCESS

STATIC FUNCTION ADO_PUTVALUE( nWA, nField, xValue )

   LOCAL aWData := USRRDD_AREADATA( nWA )
   LOCAL oADO := USRRDD_AREADATA( nWA )[ 1 ]

   IF aWData[ 3 ]
       xValue := ""
   ELSE
		oADO:Fields( nField - 1 ):Value := xValue
		oADO:Update()			
   ENDIF

RETURN SUCCESS

STATIC FUNCTION ADO_LOCATE( nWA, lContinue )

   local oADO := USRRDD_AREADATA( nWA )[ 1 ]

   // not implemented yet

RETURN SUCCESS

STATIC FUNCTION ADO_SETLOCATE( nWA, aDBScopeInfo )

   local oADO := USRRDD_AREADATA( nWA )[ 1 ]

   // not implemented yet
 
RETURN SUCCESS
 
STATIC FUNCTION ADO_APPEND( nWA, lUnLockAll )

	local oADO := USRRDD_AREADATA( nWA )[ 1 ]
	
	oADO:AddNew()
	oADO:Update() // keep it here, or there is an ADO error
	
RETURN SUCCESS

STATIC FUNCTION ADO_FLUSH( nWA )

	local oADO := USRRDD_AREADATA( nWA )[ 1 ]
	
	oADO:Update()

RETURN SUCCESS

STATIC FUNCTION ADO_ORDINFO( nWA, iIndex, aOrderInfo )

	local oADO := USRRDD_AREADATA( nWA )[ 1 ]

RETURN SUCCESS // aOrderInfo[ iIndex ]

STATIC FUNCTION ADO_PACK( nWA )

	local oADO := USRRDD_AREADATA( nWA )[ 1 ]
	
RETURN SUCCESS

STATIC FUNCTION ADO_RAWLOCK( nWA, nAction, nRecNo )

	local oADO := USRRDD_AREADATA( nWA )[ 1 ]
	
RETURN SUCCESS

STATIC FUNCTION ADO_LOCK( nWA, aLockInfo  )

	local oADO := USRRDD_AREADATA( nWA )[ 1 ]
	
	aLockInfo[ UR_LI_METHOD ] := DBLM_MULTIPLE
  aLockInfo[ UR_LI_RECORD ] := RECNO()
  aLockInfo[ UR_LI_RESULT ] := .T.
  
RETURN SUCCESS

STATIC FUNCTION ADO_UNLOCK( nWA, xRecID )

	local oADO := USRRDD_AREADATA( nWA )[ 1 ]

RETURN SUCCESS

STATIC FUNCTION ADO_SETFILTER( nWA, aInfo )

	local oADO := USRRDD_AREADATA( nWA )[ 1 ]
  local cFilter := aInfo[ 2 ]
     
  if Left( cFilter, 1 ) == '"' .and. Right( cFilter, 1 ) == '"'
     cFilter = SubStr( cFilter, 2, Len( cFilter ) - 2 )
  endif      
     
  cFilter = StrTran( cFilter, '""', "" )
  cFilter = StrTran( cFilter, '"', "'" )
  cFilter = StrTran( cFilter, "''", "'" )
  cFilter = StrTran( cFilter, "==", "=" )
  cFilter = StrTran( cFilter, ".and.", "AND" )
  cFilter = StrTran( cFilter, ".or.", "OR" )
  cFilter = StrTran( cFilter, ".AND.", "AND" )
  cFilter = StrTran( cFilter, ".OR.", "OR" )

	oADO:Filter = cFilter

RETURN SUCCESS

STATIC FUNCTION ADO_CLEARFILTER( nWA )

	local oADO := USRRDD_AREADATA( nWA )[ 1 ]
     
	oADO:Filter = ""

RETURN SUCCESS

STATIC FUNCTION ADO_ZAP( nWA )

	local oADO := USRRDD_AREADATA( nWA )[ 1 ]
     
  if s_aConnections[ nWA ] != NIL .and. s_aTableNames[ nWA ] != nil
     s_aConnections[ nWA ]:Execute( "DELETE * FROM " + s_aTableNames[ nWA ] )
  endif      

RETURN SUCCESS

FUNCTION ADORDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID )

   LOCAL cSuperRDD := NIL     /* NO SUPER RDD */
   LOCAL aMyFunc[ UR_METHODCOUNT ]

   aMyFunc[ UR_INIT ]      := ( @ADO_INIT() )
   aMyFunc[ UR_NEW ]       := ( @ADO_NEW() )
   aMyFunc[ UR_CREATE ]    := ( @ADO_CREATE() )
   aMyFunc[ UR_OPEN ]      := ( @ADO_OPEN() )
   aMyFunc[ UR_CLOSE ]     := ( @ADO_CLOSE() )
   aMyFunc[ UR_BOF  ]      := ( @ADO_BOF() )
   aMyFunc[ UR_EOF  ]      := ( @ADO_EOF() )
   aMyFunc[ UR_DELETED ]   := ( @ADO_DELETED() )
   aMyFunc[ UR_SKIPRAW ]   := ( @ADO_SKIPRAW() )
   aMyFunc[ UR_GOTO ]      := ( @ADO_GOTO() )
   aMyFunc[ UR_GOTOID ]    := ( @ADO_GOTOID() )
   aMyFunc[ UR_GOTOP ]     := ( @ADO_GOTOP() )
   aMyFunc[ UR_GOBOTTOM ]  := ( @ADO_GOBOTTOM() )
   aMyFunc[ UR_RECID ]     := ( @ADO_RECID() )
   aMyFunc[ UR_RECCOUNT ]  := ( @ADO_RECCOUNT() )
   aMyFunc[ UR_GETVALUE ]  := ( @ADO_GETVALUE() )
   aMyFunc[ UR_PUTVALUE ]  := ( @ADO_PUTVALUE() )
   aMyFunc[ UR_DELETE ]  	 := ( @ADO_DELETE() )
   aMyFunc[ UR_LOCATE ]  	 := ( @ADO_LOCATE() )
   aMyFunc[ UR_SETLOCATE ] := ( @ADO_SETLOCATE() )
   aMyFunc[ UR_APPEND ]    := ( @ADO_APPEND() )
	 aMyFunc[ UR_FLUSH ]     := ( @ADO_FLUSH() )
	 aMyFunc[ UR_ORDINFO ]   := ( @ADO_ORDINFO() )
	 aMyFunc[ UR_PACK ]      := ( @ADO_PACK() )
	 aMyFunc[ UR_RAWLOCK ]   := ( @ADO_RAWLOCK() )
	 aMyFunc[ UR_LOCK ]      := ( @ADO_LOCK() )
	 aMyFunc[ UR_UNLOCK ]    := ( @ADO_UNLOCK() )
	 aMyFunc[ UR_SETFILTER ] := ( @ADO_SETFILTER() )
	 aMyFunc[ UR_CLEARFILTER ] := ( @ADO_CLEARFILTER() )
	 aMyFunc[ UR_ZAP ]       := ( @ADO_ZAP() )

RETURN USRRDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID, ;
                                                    cSuperRDD, aMyFunc )

INIT PROC ADORDD_INIT()
   rddRegister( "ADORDD", RDT_FULL )
RETURN

STATIC FUNCTION ADO_GETFIELDSIZE( nDBFTypeField, nADOFielSize )

	LOCAL nDBFFieldSize := 0
	
   DO CASE
	
			CASE nDBFTypeField == HB_FT_STRING
           nDBFFieldSize := nADOFielSize

			CASE nDBFTypeField == HB_FT_INTEGER
           nDBFFieldSize := nADOFielSize
				
			CASE nDBFTypeField == HB_FT_DATE
           nDBFFieldSize := 8
			
			CASE nDBFTypeField == HB_FT_LOGICAL
           nDBFFieldSize := 1
			
   ENDCASE
	
RETURN nDBFFieldSize

STATIC FUNCTION ADO_GETFIELDTYPE( nADOFielfType )

	LOCAL nDBFTypeField := 0

	DO CASE

		CASE nADOFielfType == adEmpty						// 0
		CASE nADOFielfType == adTinyInt 					// 16
		CASE nADOFielfType == adSmallInt 					// 2
		CASE nADOFielfType == adInteger 					// 3
         nDBFTypeField := HB_FT_INTEGER
		
		CASE nADOFielfType == adBigInt 						// 20
		CASE nADOFielfType == adUnsignedTinyInt 	    // 17
		CASE nADOFielfType == adUnsignedSmallInt 	// 18
		CASE nADOFielfType == adUnsignedInt 			// 19
		CASE nADOFielfType == adUnsignedBigInt 		// 21
		CASE nADOFielfType == adSingle 						// 4
		CASE nADOFielfType == adDouble 						// 5
		CASE nADOFielfType == adCurrency 					// 6
		CASE nADOFielfType == adDecimal 					// 14
		CASE nADOFielfType == adNumeric 					// 131
		CASE nADOFielfType == adBoolean 					// 11
         nDBFTypeField := HB_FT_LOGICAL
		
		CASE nADOFielfType == adError 						// 10
		CASE nADOFielfType == adUserDefined 			    // 132
		CASE nADOFielfType == adVariant 					// 12
		CASE nADOFielfType == adIDispatch 				    // 9
		CASE nADOFielfType == adIUnknown 				// 13
		CASE nADOFielfType == adGUID 						// 72
		CASE nADOFielfType == adDate 						// 7
         nDBFTypeField := HB_FT_DATE
		
		CASE nADOFielfType == adDBDate 					// 133
		CASE nADOFielfType == adDBTime 					// 134
		CASE nADOFielfType == adDBTimeStamp 			// 135
		CASE nADOFielfType == adBSTR 						// 8
		CASE nADOFielfType == adChar 						// 129
	 	     // nDBFTypeField := HB_FT_STRING
					
		CASE nADOFielfType == adVarChar 					// 200
		     // nDBFTypeField := HB_FT_STRING

		CASE nADOFielfType == adLongVarChar 			// 201
	        //	 nDBFTypeField := HB_FT_STRING

		CASE nADOFielfType == adWChar 						// 130
            // nDBFTypeField := HB_FT_STRING

		CASE nADOFielfType == adVarWChar 				// 202
         nDBFTypeField := HB_FT_STRING

		CASE nADOFielfType == adLongVarWChar 			// 203

		CASE nADOFielfType == adBinary 						// 128
		CASE nADOFielfType == adVarBinary 				// 204
		CASE nADOFielfType == adLongVarBinary 		    // 205
		CASE nADOFielfType == adChapter 					// 136
		CASE nADOFielfType == adFileTime 					// 64
		CASE nADOFielfType == adPropVariant 			    // 138
		CASE nADOFielfType == adVarNumeric 				// 139
		// CASE nADOFielfType == adArray &H2000

   ENDCASE

RETURN nDBFTypeField

function HB_AdoSetTable( cTableName )

   s_cTableName = cTableName

return nil

function HB_AdoSetEngine( cEngine )

   s_cEngine = cEngine
   
return nil   

function HB_AdoSetServer( cServer )

   s_cServer = cServer
  
return nil

function HB_AdoSetUser( cUser )

   s_cUserName = cUser
   
return nil

function HB_AdoSetPassword( cPassword )

   s_cPassword = cPassword
   
return nil      

function HB_AdoSetQuery( cQuery ) 

   DEFAULT cQuery TO "SELECT * FROM " 

   s_cQuery = cQuery 

return nil
