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
 * along with this software; see the file COPYING.  if not, write to
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
 * Project under the name Harbour.  if you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * if you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * if you do not wish that, delete this exception notice.
 *
 */

#include "rddsys.ch"
#include "fileio.ch"
#include "error.ch"
#include "adordd.ch"
#include "common.ch"
#include "dbstruct.ch"

#ifndef __XHARBOUR__
   #include "hbusrrdd.ch"
   #xcommand TRY              => bError := errorBlock( {|oErr| break( oErr ) } ) ;;
                                 BEGIN SEQUENCE
   #xcommand CATCH [<!oErr!>] => errorBlock( bError ) ;;
                                 RECOVER [USING <oErr>] <-oErr-> ;;
                                 errorBlock( bError )
#else   
   #include "usrrdd.ch"
#endif

#define WA_RECORDSET  1
#define WA_BOF        2
#define WA_EOF        3
#define WA_CONNECTION 4
#define WA_CATALOG    5
#define WA_TABLENAME  6
#define WA_ENGINE     7
#define WA_SERVER     8
#define WA_USERNAME   9
#define WA_PASSWORD  10
#define WA_QUERY     11
#define WA_LOCATEFOR 12
#define WA_SCOPEINFO 13
#define WA_SQLTRUCT  14

#define WA_SIZE      14

ANNOUNCE ADORDD

static bError, s_cTableName, s_cEngine, s_cServer, s_cUserName, s_cPassword, s_cQuery := ""

#ifdef __XHARBOUR__

static function HB_TokenGet( cText, nPos, cSep )

   local aTokens := HB_ATokens( cText, cSep )

return If( nPos <= Len( aTokens ), aTokens[ nPos ], "" )

#endif

static function ADO_INIT( nRDD )

   local aRData

   USRRDD_RDDDATA( nRDD, aRData )

return SUCCESS

static function ADO_NEW( nWA )

   local aWAData := Array( WA_SIZE )

   aWAData[ WA_BOF ] = .F.
   aWAData[ WA_EOF ] = .F.

   USRRDD_AREADATA( nWA, aWAData )
  
return SUCCESS

static function ADO_CREATE( nWA, aOpenInfo )

   local cDataBase   := HB_TokenGet( aOpenInfo[ UR_OI_NAME ], 1, ";" )
   local cTableName  := HB_TokenGet( aOpenInfo[ UR_OI_NAME ], 2, ";" )
   local cDbEngine   := HB_TokenGet( aOpenInfo[ UR_OI_NAME ], 3, ";" )
   local cServer     := HB_TokenGet( aOpenInfo[ UR_OI_NAME ], 4, ";" )
   local cUserName   := HB_TokenGet( aOpenInfo[ UR_OI_NAME ], 5, ";" )
   local cPassword   := HB_TokenGet( aOpenInfo[ UR_OI_NAME ], 6, ";" )
   local oConnection := TOleAuto():New( "ADODB.Connection" )
   local oCatalog    := TOleAuto():New( "ADOX.Catalog" )
   local aWAData     := USRRDD_AREADATA( nWA )
   local oError

   do case
      case Upper( Right( cDataBase, 4 ) ) == ".MDB"
           if ! File( cDataBase )
              oCatalog:Create( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + cDataBase )
           endif   
           oConnection:Open( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + cDataBase )
           
      case Upper( cDbEngine ) == "MYSQL"     
           oConnection:Open( "DRIVER={MySQL ODBC 3.51 Driver};" + ;
                             "server=" + cServer + ;
                             ";database=" + cDataBase + ;
                             ";uid=" + cUserName + ;
                             ";pwd=" + cPassword )
           
   endcase        

   TRY
      oConnection:Execute( "DROP TABLE " + cTableName )
   CATCH   
   END

   TRY
      oConnection:Execute( "CREATE TABLE [" + cTableName + "] (" + aWAData[ WA_SQLTRUCT ] + ")" )
   CATCH
      oError := ErrorNew()
      oError:GenCode     := EG_CREATE
      oError:SubCode     := 1004
      oError:Description := HB_LANGERRMSG( EG_CREATE ) + " (" + ;
                            HB_LANGERRMSG( EG_UNSUPPORTED ) + ")"
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      oError:CanDefault  := .T.
      UR_SUPER_ERROR( nWA, oError )
   END
      
   oConnection:Close()
   
return SUCCESS

static function ADO_CREATEFIELDS( nWA, aStruct )

   local aWAData := USRRDD_AREADATA( nWA )
   local n

   aWAData[ WA_SQLTRUCT ] = ""

  for n = 1 to Len( aStruct )
     if n > 1
        aWAData[ WA_SQLTRUCT ] += ", "
     endif   
     aWAData[ WA_SQLTRUCT ] += "[" + aStruct[ n ][ DBS_NAME ] + "]"
     do case
        case aStruct[ n ][ DBS_TYPE ] $ "C,Character"
             aWAData[ WA_SQLTRUCT ] += " CHAR(" + AllTrim( Str( aStruct[ n ][ DBS_LEN ] ) ) + ") NULL" 

        case aStruct[ n ][ DBS_TYPE ] == "N"
             aWAData[ WA_SQLTRUCT ] += " NUMERIC(" + AllTrim( Str( aStruct[ n ][ DBS_LEN ] ) ) + ")"

        case aStruct[ n ][ DBS_TYPE ] == "L"
             aWAData[ WA_SQLTRUCT ] += " LOGICAL"
     endcase     
  next      

return SUCCESS

static function ADO_OPEN( nWA, aOpenInfo )

   local aWAData := USRRDD_AREADATA( nWA )
   local cName, aField, oError, nResult
   local oRecordSet, nTotalFields, n

   // When there is no ALIAS we will create new one using file name
   if aOpenInfo[ UR_OI_ALIAS ] == nil
      HB_FNAMESPLIT( aOpenInfo[ UR_OI_NAME ], , @cName )
      aOpenInfo[ UR_OI_ALIAS ] := cName
   endif
   
   aWAData[ WA_CONNECTION ] = TOleAuto():New( "ADODB.Connection" )
   aWAData[ WA_TABLENAME ] = s_cTableName
   aWAData[ WA_QUERY ]    = s_cQuery
   aWAData[ WA_USERNAME ] = s_cUserName
   aWAData[ WA_PASSWORD ] = s_cPassword
   aWAData[ WA_SERVER ] = s_cServer
   aWAData[ WA_ENGINE ] = s_cEngine
   
   do case
      case Lower( Right( aOpenInfo[ UR_OI_NAME ], 4 ) ) == ".mdb"
           if Empty( aWAData[ WA_PASSWORD ] )
              aWAData[ WA_CONNECTION ]:Open( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + aOpenInfo[ UR_OI_NAME ] )
           else
              aWAData[ WA_CONNECTION ]:Open( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + aOpenInfo[ UR_OI_NAME ] + ";Jet OLEDB:Database Password=" + AllTrim( aWAData[ WA_PASSWORD ] ) )
           endif

      case Lower( Right( aOpenInfo[ UR_OI_NAME ], 4 ) ) == ".xls"
           aWAData[ WA_CONNECTION ]:Open( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + aOpenInfo[ UR_OI_NAME ] + ";Extended Properties='Excel 8.0;HDR=YES';Persist Security Info=False" )

      case Lower( Right( aOpenInfo[ UR_OI_NAME ], 4 ) ) == ".dbf"
           aWAData[ WA_CONNECTION ]:Open( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + aOpenInfo[ UR_OI_NAME ] + ";Extended Properties=dBASE IV;User ID=Admin;Password=;" )

      case Lower( Right( aOpenInfo[ UR_OI_NAME ], 3 ) ) == ".db"
           aWAData[ WA_CONNECTION ]:Open( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + aOpenInfo[ UR_OI_NAME ] + ";Extended Properties='Paradox 3.x';" )
               
      case aWAData[ WA_ENGINE ] == "MYSQL"
           aWAData[ WA_CONNECTION ]:Open( "DRIVER={MySQL ODBC 3.51 Driver};" + ;
                                          "server=" + aWAData[ WA_SERVER ] + ;
                                          ";database=" + aOpenInfo[ UR_OI_NAME ] + ;
                                          ";uid=" + aWAData[ WA_USERNAME ] + ;
                                          ";pwd=" + aWAData[ WA_PASSWORD ] )
                                
      case aWAData[ WA_ENGINE ] == "SQL" 
           aWAData[ WA_CONNECTION ]:Open( "Provider=SQLOLEDB;" + ; 
                                          "server=" + aWAData[ WA_SERVER ] + ; 
                                          ";database=" + aOpenInfo[ UR_OI_NAME ] + ; 
                                          ";uid=" + aWAData[ WA_USERNAME ] + ; 
                                          ";pwd=" + aWAData[ WA_PASSWORD ] )
                                
      case aWAData[ WA_ENGINE ] == "ORACLE"
           aWAData[ WA_CONNECTION ]:Open( "Provider=MSDAORA.1;" + ;
                                          "Persist Security Info=False" + ;
                                          If( Empty( aWAData[ WA_SERVER ] ),; 
                                          "", ";Data source=" + aWAData[ WA_SERVER ] ) + ;
                                          ";User ID=" + aWAData[ WA_USERNAME ] + ;
                                          ";Password=" + aWAData[ WA_PASSWORD ] )                                                                
       
   endcase                               

   oRecordSet := TOleAuto():New( "ADODB.Recordset" )
   oRecordSet:CursorType     = adOpenDynamic
   oRecordSet:CursorLocation = adUseClient
   oRecordSet:LockType       = adLockPessimistic
   oRecordSet:Open( aWAData[ WA_QUERY ] + aWAData[ WA_TABLENAME ], aWAData[ WA_CONNECTION ] )

   aWAData[ WA_CATALOG ] = TOleAuto():New( "ADOX.Catalog" )
   aWAData[ WA_CATALOG ]:ActiveConnection = aWAData[ WA_CONNECTION ]
   
   if oRecordSet == NIL
      oError := ErrorNew()
      oError:GenCode     := EG_OPEN
      oError:SubCode     := 1001
      oError:Description := HB_LANGERRMSG( EG_OPEN )
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      oError:OsCode      := 0 // To be implemented
      oError:CanDefault  := .T.

      UR_SUPER_ERROR( nWA, oError )
      return FAILURE
   endif
   
   aWAData[ WA_RECORDSET ] := oRecordSet
   aWAData[ WA_BOF ] := aWAData[ WA_EOF ] := .F.

   UR_SUPER_SETFIELDEXTENT( nWA, nTotalFields := oRecordSet:Fields:Count )   
       
   FOR n = 1 TO nTotalFields
      aField := ARRAY( UR_FI_SIZE )
      aField[ UR_FI_NAME ]    := oRecordSet:Fields( n - 1 ):Name
      aField[ UR_FI_TYPE ]    := ADO_GETFIELDTYPE( oRecordSet:Fields( n - 1 ):Type )
      aField[ UR_FI_TYPEEXT ] := 0
      aField[ UR_FI_LEN ]     := ADO_GETFIELDSIZE( aField[ UR_FI_TYPE ], oRecordSet:Fields( n - 1 ):DefinedSize )
      aField[ UR_FI_DEC ]     := 0
      UR_SUPER_ADDFIELD( nWA, aField )
    NEXT

   nResult := UR_SUPER_OPEN( nWA, aOpenInfo )

   if nResult == SUCCESS
      ADO_GOTOP( nWA )
   endif
  
return nResult

static function ADO_CLOSE( nWA )

   local oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]
      
   TRY 
      // oRecordSet:Close()   
   CATCH
   END   
   
return UR_SUPER_CLOSE( nWA )

static function ADO_GETVALUE( nWA, nField, xValue )

   local aWAData := USRRDD_AREADATA( nWA )
   local oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   if aWAData[ WA_EOF ] .or. oRecordSet:RecordCount() == 0
      xValue := nil
   else
      xValue := oRecordSet:Fields( nField - 1 ):Value
   endif

return SUCCESS

static function ADO_GOTOID( nWA, nRecord )

   local oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ], nRecNo

   if oRecordSet:RecordCount() > 0
      oRecordSet:MoveFirst()
      oRecordSet:Move( nRecord - 1, 0 )
    endif
    ADO_RECID( nWA, @nRecNo )

RETURN If( nRecord == nRecNo, SUCCESS, FAILURE )

static function ADO_GOTOP( nWA )

   local aWAData    := USRRDD_AREADATA( nWA ) 
   local oRecordSet := aWAData[ WA_RECORDSET ]

   if oRecordSet:RecordCount() != 0
      oRecordSet:MoveFirst()
   endif   
   
   aWAData[ WA_BOF ] = .F.
   aWAData[ WA_EOF ] = .F.

return SUCCESS

static function ADO_GOBOTTOM( nWA )

   local aWAData    := USRRDD_AREADATA( nWA ) 
   local oRecordSet := aWAData[ WA_RECORDSET ]

   oRecordSet:MoveLast()

   aWAData[ WA_BOF ] = .F.
   aWAData[ WA_EOF ] = .F.
 
return SUCCESS

static function ADO_SKIPRAW( nWA, nRecords )

   local aWAData    := USRRDD_AREADATA( nWA ) 
   local oRecordSet := aWAData[ WA_RECORDSET ]

   if nRecords != 0
      if aWAData[ WA_EOF ]
         if nRecords > 0
            return SUCCESS
         endif
         ADO_GOBOTTOM( nWA )
         ++nRecords
       endif
       if nRecords < 0 .AND. oRecordSet:AbsolutePosition <= -nRecords
          oRecordSet:MoveFirst()
          aWAData[ WA_BOF ] := .T.
          aWAData[ WA_EOF ] := oRecordSet:EOF
       elseif nRecords != 0
          oRecordSet:Move( nRecords )
          aWAData[ WA_BOF ] := .F.
          aWAData[ WA_EOF ] := oRecordSet:EOF
       endif
   endif

return SUCCESS

static function ADO_BOF( nWA, lBof )
   
   local aWAData := USRRDD_AREADATA( nWA )

   lBof := aWAData[ WA_BOF ]
   
return SUCCESS

static function ADO_EOF( nWA, lEof )

   local oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   lEof := ( oRecordSet:AbsolutePosition == -3 )

return SUCCESS

static function ADO_DELETED( nWA, lDeleted )

  local oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

  TRY   
     if oRecordSet:Status == adRecDeleted
        lDeleted := .T.
     else
        lDeleted := .F.
     endif
  CATCH
     lDeleted := .f.
  END   

return SUCCESS

static function ADO_DELETE( nWA )

  local oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   oRecordSet:Delete()
   
   ADO_SKIPRAW( nWA, 1 )

return SUCCESS

static function ADO_RECID( nWA, nRecNo )

   local oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]
  
   nRecno := If( oRecordSet:AbsolutePosition == -3, oRecordSet:RecordCount() + 1, oRecordSet:AbsolutePosition )
   
return SUCCESS

static function ADO_RECCOUNT( nWA, nRecords )

   local oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   nRecords := oRecordSet:RecordCount()
   
return SUCCESS

static function ADO_PUTVALUE( nWA, nField, xValue )

   local aWAData := USRRDD_AREADATA( nWA )
   local oRecordSet := aWAData[ WA_RECORDSET ]

   if ! aWAData[ WA_EOF ]
      oRecordSet:Fields( nField - 1 ):Value := xValue
      TRY
         oRecordSet:Update()     
      CATCH
      END   
   endif

return SUCCESS

static function ADO_APPEND( nWA, lUnLockAll )

  local oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]
  
  oRecordSet:AddNew()
  
  TRY
     oRecordSet:Update()
  CATCH
  END   
  
return SUCCESS

static function ADO_FLUSH( nWA )

   local oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]
  
   oRecordSet:Update()

return SUCCESS

static function ADO_ORDINFO( nWA, nIndex, aOrderInfo )

   local aWAData    := USRRDD_AREADATA( nWA )
   local oRecordSet := aWAData[ WA_RECORDSET ]
  
   do case
      case nIndex == UR_ORI_TAG
           if aOrderInfo[ UR_ORI_TAG ] < aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes:Count
              aOrderInfo[ UR_ORI_RESULT ] = aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes( aOrderInfo[ UR_ORI_TAG ] ):Name
           else   
              aOrderInfo[ UR_ORI_RESULT ] = ""
           endif   
   endcase   

return SUCCESS

static function ADO_PACK( nWA )

   local oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]
  
return SUCCESS

static function ADO_RAWLOCK( nWA, nAction, nRecNo )

   local oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]
  
return SUCCESS

static function ADO_LOCK( nWA, aLockInfo  )

   local oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]
  
   aLockInfo[ UR_LI_METHOD ] := DBLM_MULTIPLE
   aLockInfo[ UR_LI_RECORD ] := RECNO()
   aLockInfo[ UR_LI_RESULT ] := .T.
  
return SUCCESS

static function ADO_UNLOCK( nWA, xRecID )

   local oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

return SUCCESS

static function ADO_SETFILTER( nWA, aFilterInfo )

   local oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]
     
   oRecordSet:Filter = SQLTranslate( aFilterInfo[ UR_FRI_CEXPR ] )

return SUCCESS

static function ADO_CLEARFILTER( nWA )

   local oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]
     
   TRY 
      oRecordSet:Filter = ""
   CATCH
   END   

return SUCCESS

static function ADO_ZAP( nWA )

   local aWAData    := USRRDD_AREADATA( nWA )
   local oRecordSet := aWAData[ WA_RECORDSET ]
     
   if aWAData[ WA_CONNECTION ] != nil .and. aWAData[ WA_TABLENAME ] != nil
      aWAData[ WA_CONNECTION ]:Execute( "DELETE * FROM " + aWAData[ WA_TABLENAME ] )
      oRecordSet:Requery()
   endif      

return SUCCESS

static function ADO_SETLOCATE( nWA, aScopeInfo )

   local aWAData := USRRDD_AREADATA( nWA )
   
   aScopeInfo[ UR_SI_CFOR ] = SQLTranslate( aWAData[ WA_LOCATEFOR ] )
   
   aWAData[ WA_SCOPEINFO ] = aScopeInfo

return SUCCESS

static function ADO_LOCATE( nWA, lContinue )

   local aWAData    := USRRDD_AREADATA( nWA )
   local oRecordSet := aWAData[ WA_RECORDSET ]

   oRecordSet:Find( aWAData[ WA_SCOPEINFO ][ UR_SI_CFOR ], If( lContinue, 1, 0 ) )
  
return SUCCESS

static function ADO_CLEARREL( nWA )

   local aWAData := USRRDD_AREADATA( nWA )
   local nKeys := 0, cKeyName
   
   if aWAData[ WA_CATALOG ] != nil .and. aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys != nil
      TRY
         nKeys = aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys:Count
      CATCH
      END   
   endif   

   if nKeys > 0 
      cKeyName = aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys( nKeys - 1 ):Name
      if Upper( cKeyName ) != "PRIMARYKEY"
         aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys:Delete( cKeyName )
      endif   
   endif   

return SUCCESS

static function ADO_RELAREA( nWA, nRelNo, nRelArea )

   local aWAData := USRRDD_AREADATA( nWA )

   if nRelNo <= aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys:Count() 
      nRelArea = Select( aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys( nRelNo - 1 ):RelatedTable )
   endif  

return SUCCESS

static function ADO_RELTEXT( nWA, nRelNo, cExpr )

   local aWAData := USRRDD_AREADATA( nWA )

   if nRelNo <= aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys:Count() 
      cExpr = aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys( nRelNo - 1 ):Columns( 0 ):RelatedColumn
   endif  

return SUCCESS

static function ADO_SETREL( nWA, aRelInfo )

   local aWAData := USRRDD_AREADATA( nWA )
   local cParent := Alias( aRelInfo[ UR_RI_PARENT ] )
   local cChild  := Alias( aRelInfo[ UR_RI_CHILD ] )
   local cKeyName := cParent + "_" + cChild

   TRY
      aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys:Append( cKeyName, adKeyForeign,;
                                    aRelInfo[ UR_RI_CEXPR ], cChild, aRelInfo[ UR_RI_CEXPR ] )
   CATCH
      // raise error for can't create relation
   END                                    

return SUCCESS

static function ADO_ORDLSTADD( nWA, aOrderInfo )

   local oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]
  
   TRY 
      oRecordSet:Index = aOrderInfo[ UR_ORI_BAG ]
   CATCH
   END   
  
return SUCCESS

static function ADO_ORDLSTCLEAR( nWA )

   local oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   TRY 
      oRecordSet:Index = ""
   CATCH
   END   
  
return SUCCESS

static function ADO_ORDCREATE( nWA, aOrderCreateInfo )

   local aWAData := USRRDD_AREADATA( nWA )
   local oIndex
   
   TRY
      if aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes( aOrderCreateInfo[ UR_ORCR_BAGNAME ] ) == nil
         oIndex = TOleAuto():New( "ADOX.Index" )
         oIndex:Name = aOrderCreateInfo[ UR_ORCR_BAGNAME ]
         oIndex:PrimaryKey = .F.
         oIndex:Unique = aOrderCreateInfo[ UR_ORCR_UNIQUE ]
         oIndex:Columns:Append( aOrderCreateInfo[ UR_ORCR_CKEY ] )
         aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes:Append( oIndex )
      endif   
   CATCH
      // raise error for can't create index
   END   

return SUCCESS

function ADORDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID )

   local cSuperRDD   /* NO SUPER RDD */
   local aMyFunc[ UR_METHODCOUNT ]

   aMyFunc[ UR_INIT ]      := ( @ADO_INIT() )
   aMyFunc[ UR_NEW ]       := ( @ADO_NEW() )
   aMyFunc[ UR_CREATE ]    := ( @ADO_CREATE() )
   aMyFunc[ UR_CREATEFIELDS ] := ( @ADO_CREATEFIELDS() )
   aMyFunc[ UR_OPEN ]      := ( @ADO_OPEN() )
   aMyFunc[ UR_CLOSE ]     := ( @ADO_CLOSE() )
   aMyFunc[ UR_BOF  ]      := ( @ADO_BOF() )
   aMyFunc[ UR_EOF  ]      := ( @ADO_EOF() )
   aMyFunc[ UR_DELETED ]   := ( @ADO_DELETED() )
   aMyFunc[ UR_SKIPRAW ]   := ( @ADO_SKIPRAW() )
   aMyFunc[ UR_GOTO ]      := ( @ADO_GOTOID() )
   aMyFunc[ UR_GOTOID ]    := ( @ADO_GOTOID() )
   aMyFunc[ UR_GOTOP ]     := ( @ADO_GOTOP() )
   aMyFunc[ UR_GOBOTTOM ]  := ( @ADO_GOBOTTOM() )
   aMyFunc[ UR_RECID ]     := ( @ADO_RECID() )
   aMyFunc[ UR_RECCOUNT ]  := ( @ADO_RECCOUNT() )
   aMyFunc[ UR_GETVALUE ]  := ( @ADO_GETVALUE() )
   aMyFunc[ UR_PUTVALUE ]  := ( @ADO_PUTVALUE() )
   aMyFunc[ UR_DELETE ]    := ( @ADO_DELETE() )
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
   aMyFunc[ UR_SETLOCATE ] := ( @ADO_SETLOCATE() )
   aMyFunc[ UR_LOCATE ]    := ( @ADO_LOCATE() )
   aMyFunc[ UR_CLEARREL ]  := ( @ADO_CLEARREL() )
   aMyFunc[ UR_RELAREA ]   := ( @ADO_RELAREA() )
   aMyFunc[ UR_RELTEXT ]   := ( @ADO_RELTEXT() )
   aMyFunc[ UR_SETREL ]    := ( @ADO_SETREL() )
   aMyFunc[ UR_ORDCREATE ] := ( @ADO_ORDCREATE() )
   aMyFunc[ UR_ORDLSTADD ] := ( @ADO_ORDLSTADD() )
   aMyFunc[ UR_ORDLSTCLEAR ] := ( @ADO_ORDLSTCLEAR() )

return USRRDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID, cSuperRDD,;
                            aMyFunc )

init procedure ADORDD_INIT()
   rddRegister( "ADORDD", RDT_FULL )
return

static function ADO_GETFIELDSIZE( nDBFTypeField, nADOFieldSize )

   local nDBFFieldSize := 0
  
   do case
  
      case nDBFTypeField == HB_FT_STRING
           nDBFFieldSize := nADOFieldSize

      case nDBFTypeField == HB_FT_INTEGER
           nDBFFieldSize := nADOFieldSize
        
      case nDBFTypeField == HB_FT_DATE
           nDBFFieldSize := 8
      
      case nDBFTypeField == HB_FT_LOGICAL
           nDBFFieldSize := 1
      
   endcase
  
return nDBFFieldSize

static function ADO_GETFIELDTYPE( nADOFielfType )

   local nDBFTypeField := 0

   do case

      case nADOFielfType == adEmpty
      case nADOFielfType == adTinyInt
      case nADOFielfType == adSmallInt
      case nADOFielfType == adInteger
           nDBFTypeField := HB_FT_INTEGER
    
      case nADOFielfType == adBigInt            
      case nADOFielfType == adUnsignedTinyInt   
      case nADOFielfType == adUnsignedSmallInt  
      case nADOFielfType == adUnsignedInt       
      case nADOFielfType == adUnsignedBigInt    
      case nADOFielfType == adSingle            
      case nADOFielfType == adDouble            
      case nADOFielfType == adCurrency          
      case nADOFielfType == adDecimal           
      case nADOFielfType == adNumeric           
      case nADOFielfType == adBoolean           
           nDBFTypeField := HB_FT_LOGICAL
    
      case nADOFielfType == adError             
      case nADOFielfType == adUserDefined       
      case nADOFielfType == adVariant           
      case nADOFielfType == adIDispatch         
      case nADOFielfType == adIUnknown        
      case nADOFielfType == adGUID            
      case nADOFielfType == adDate            
           nDBFTypeField := HB_FT_DATE
    
      case nADOFielfType == adDBDate          
      case nADOFielfType == adDBTime          
      case nADOFielfType == adDBTimeStamp       
      case nADOFielfType == adBSTR            
      case nADOFielfType == adChar            
           // nDBFTypeField := HB_FT_STRING
          
      case nADOFielfType == adVarChar           
           // nDBFTypeField := HB_FT_STRING

      case nADOFielfType == adLongVarChar       
           //   nDBFTypeField := HB_FT_STRING

      case nADOFielfType == adWChar             
           // nDBFTypeField := HB_FT_STRING

      case nADOFielfType == adVarWChar        
           nDBFTypeField := HB_FT_STRING

      case nADOFielfType == adLongVarWChar      

      case nADOFielfType == adBinary            
      case nADOFielfType == adVarBinary         
      case nADOFielfType == adLongVarBinary     
      case nADOFielfType == adChapter           
      case nADOFielfType == adFileTime          
      case nADOFielfType == adPropVariant       
      case nADOFielfType == adVarNumeric        
      // case nADOFielfType == adArray 

   endcase

return nDBFTypeField

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

function HB_AdoSetLocateFor( cLocateFor )

   USRRDD_AREADATA( Select() )[ WA_LOCATEFOR ] = cLocateFor
   
return nil   

static function SQLTranslate( cExpr )

  if Left( cExpr, 1 ) == '"' .and. Right( cExpr, 1 ) == '"'
     cExpr = SubStr( cExpr, 2, Len( cExpr ) - 2 )
  endif      
     
  cExpr = StrTran( cExpr, '""', "" )
  cExpr = StrTran( cExpr, '"', "'" )
  cExpr = StrTran( cExpr, "''", "'" )
  cExpr = StrTran( cExpr, "==", "=" )
  cExpr = StrTran( cExpr, ".and.", "AND" )
  cExpr = StrTran( cExpr, ".or.", "OR" )
  cExpr = StrTran( cExpr, ".AND.", "AND" )
  cExpr = StrTran( cExpr, ".OR.", "OR" )

return cExpr   

function HB_AdoRddGetConnection( nWA )

   DEFAULT nWA TO Select()
   
return USRRDD_AREADATA( nWA )[ WA_CONNECTION ]   

function HB_AdoRddGetCatalog( nWA )

   DEFAULT nWA TO Select()
   
return USRRDD_AREADATA( nWA )[ WA_CATALOG ]   

function HB_AdoRddGetRecordSet( nWA )

   local aWAData

   DEFAULT nWA TO Select()
   
   aWAData = USRRDD_AREADATA( nWA )
   
return If( aWAData != nil, aWAData[ WA_RECORDSET ], nil )