/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    SIX compatible functions:
 *          sxChar()
 *          sxNum()
 *          sxDate()
 *          sxLog()
 *          Sx_Compress()
 *          Sx_Decompress()
 *          Sx_TagInfo()
 *          Sx_TagCount()
 *          Sx_Tags()
 *          Sx_SetTag()
 *          Sx_KillTag()
 *          Sx_FileOrder()
 *          Sx_SetFileOrd()
 *          RDD_Count()
 *          RDD_Name()
 *          RDD_Info()
 *          Sx_IsDBT()
 *          Sx_AutoOpen()
 *          Sx_AutoShare()
 *          Sx_BLOB2File()
 *          Sx_File2BLOB()
 *          Sx_dbCreate()
 *          Sx_VSigLen()
 *          Sx_MemoExt()
 *          Sx_MemoBlk()
 *          Sx_SetMemoBlock()
 *          Sx_StrxCheck()
 *          Sx_LockRetry()
 *          Sx_IsLocked()
 *          Sx_SetTrigger()
 *          Sx_VFGet()
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
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

#include "dbinfo.ch"
#include "ord.ch"
#include "hbsxdef.ch"

FUNCTION sxChar( nLen, xKeyVal )

   SWITCH ValType( xKeyVal )
   CASE "C"
   CASE "M"
      EXIT
   CASE "N"
      xKeyVal := Str( xKeyVal )
      EXIT
   CASE "D"
      xKeyVal := DToS( xKeyVal )
      EXIT
   CASE "L"
      xKeyVal := iif( xKeyVal, "T", "F" )
      EXIT
   OTHERWISE
      xKeyVal := iif( HB_ISNUMERIC( nLen ), "", Space( 10 ) )
      EXIT
   ENDSWITCH

   RETURN iif( HB_ISNUMERIC( nLen ), PadR( LTrim( xKeyVal ), nLen ), xKeyVal )

FUNCTION sxNum( xKeyVal )

   SWITCH ValType( xKeyVal )
   CASE "N"
      EXIT
   CASE "C"
   CASE "M"
      xKeyVal := Val( xKeyVal )
      EXIT
   CASE "D"
      xKeyVal := xKeyVal - hb_SToD()
      EXIT
   CASE "L"
      xKeyVal := iif( xKeyVal, 1, 0 )
      EXIT
   OTHERWISE
      xKeyVal := 0.00
      EXIT
   ENDSWITCH

   RETURN xKeyVal

FUNCTION sxDate( xKeyVal )

   SWITCH ValType( xKeyVal )
   CASE "D"
      EXIT
   CASE "C"
   CASE "M"
      xKeyVal := CToD( xKeyVal )
      EXIT
   CASE "N"
      xKeyVal := hb_SToD() + xKeyVal
      EXIT
   OTHERWISE
      xKeyVal := hb_SToD()
      EXIT
   ENDSWITCH

   RETURN xKeyVal

FUNCTION sxLog( xKeyVal )

   SWITCH ValType( xKeyVal )
   CASE "L"
      EXIT
   CASE "C"
   CASE "M"
      SWITCH xKeyVal
      CASE  "T";  CASE  "t";  CASE  "Y";  CASE  "y"
      CASE ".T."; CASE ".t."; CASE ".Y."; CASE ".y."
         xKeyVal := .T.
         EXIT
      OTHERWISE
         xKeyVal := .F.
         EXIT
      ENDSWITCH
      EXIT
   CASE "N"
      xKeyVal := xKeyVal != 0
      EXIT
   OTHERWISE
      xKeyVal := .F.
      EXIT
   ENDSWITCH

   RETURN xKeyVal

FUNCTION sx_Compress( xVal )

   LOCAL cType := ValType( xVal ), xRetVal

   IF cType $ "CM"
      xRetVal := _sx_StrCompress( xVal )
   ELSEIF cType == "A"
      xRetVal := Array( Len( xVal ) )
      AEval( xVal, {| x | xRetVal := sx_Compress( x ) } )
   ELSE
      xRetVal := xVal
   ENDIF

   RETURN xRetVal

FUNCTION sx_Decompress( xVal )

   LOCAL cType := ValType( xVal ), xRetVal

   IF cType $ "CM"
      xRetVal := _sx_StrDecompress( xVal )
   ELSEIF cType == "A"
      xRetVal := Array( Len( xVal ) )
      AEval( xVal, {| x | xRetVal := sx_Decompress( x ) } )
   ELSE
      xRetVal := xVal
   ENDIF

   RETURN xRetVal

FUNCTION sx_TagInfo( cIndex )

   LOCAL aInfo, nOrds, nFirst, i

   IF Used() .AND. ( nOrds := ordCount( cIndex ) ) > 0
      aInfo := Array( nOrds, 6 )
      IF HB_ISSTRING( cIndex )
         nFirst := dbOrderInfo( DBOI_BAGORDER, cIndex )
         nOrds += nFirst - 1
      ELSE
         nFirst := 1
      ENDIF
      FOR i := nFirst TO nOrds
         aInfo[ i, 1 ] := ordName( i )
         aInfo[ i, 2 ] := ordKey( i )
         aInfo[ i, 3 ] := ordFor( i )
         aInfo[ i, 4 ] := ordIsUnique( i )
         aInfo[ i, 5 ] := ordDescend( i )
         aInfo[ i, 6 ] := ordCustom( i )
      NEXT
   ELSE
      aInfo := {}
   ENDIF

   RETURN aInfo

FUNCTION sx_TagCount( xIndex )

   LOCAL nTags := 0, cIndex, nOrder

   IF Used()
      IF HB_ISNUMERIC( xIndex )
         nOrder := sx_TagOrder( 1, xIndex )
         IF nOrder != 0
            cIndex := dbOrderInfo( DBOI_FULLPATH,, nOrder )
         ENDIF
      ELSEIF HB_ISSTRING( xIndex ) .AND. !Empty( xIndex )
         cIndex := xIndex
      ELSE
         cIndex := dbOrderInfo( DBOI_FULLPATH )
      ENDIF
      IF !Empty( cIndex )
         nTags := ordCount( cIndex )
      ENDIF
   ENDIF

   RETURN nTags

FUNCTION sx_Tags( xIndex )

   LOCAL aTagNames := {}, nOrder, nTags

   IF Used()
      IF HB_ISNUMERIC( xIndex )
         nOrder := sx_TagOrder( 1, xIndex )
      ELSEIF HB_ISSTRING( xIndex ) .AND. !Empty( xIndex )
         nOrder := dbOrderInfo( DBOI_BAGORDER, xIndex )
      ELSE
         nOrder := ordNumber()
      ENDIF
      IF nOrder != 0
         nTags := ordCount( dbOrderInfo( DBOI_FULLPATH,, nOrder ) )
         DO WHILE --nTags >= 0
            AAdd( aTagNames, ordName( nOrder++ ) )
         ENDDO
      ENDIF
   ENDIF

   RETURN aTagNames

FUNCTION sx_SetTag( xTag, xIndex )

   LOCAL lRet := .F., nOrder := 0, nOldOrd, cIndex

   IF Used() .AND. ValType( xTag ) $ "CN"
      IF HB_ISNUMERIC( xTag )
         IF Empty( xIndex ) .OR. !ValType( xIndex ) $ "CN"
            nOrder := xTag
         ELSEIF HB_ISSTRING( xIndex )
            IF xTag >= 1 .AND. xTag <= ordCount( xIndex )
               nOrder := dbOrderInfo( DBOI_BAGORDER, xIndex ) + xTag - 1
            ENDIF
         ELSE
            nOrder := sx_TagOrder( xTag, xIndex )
         ENDIF
      ELSE
         IF Empty( xIndex ) .OR. !ValType( xIndex ) $ "CN"
            nOrder := ordNumber( xTag )
         ELSEIF HB_ISSTRING( xIndex )
            nOrder := sx_TagOrder( xTag, xIndex )
         ELSE
            nOrder := sx_TagOrder( 1, xIndex )
            IF nOrder != 0
               cIndex := dbOrderInfo( DBOI_FULLPATH,, nOrder )
               IF Empty( cIndex )
                  nOrder := 0
               ELSE
                  nOrder := sx_TagOrder( xTag, cIndex )
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF nOrder != 0
         nOldOrd := ordNumber()
         ordSetFocus( nOrder )
         lRet := nOrder == ordNumber()
         IF ! lRet
            ordSetFocus( nOldOrd )
         ENDIF
      ELSEIF Empty( xTag )
         ordSetFocus( 0 )
         lRet := .T.
      ENDIF
   ENDIF

   RETURN lRet

FUNCTION sx_KillTag( xTag, xIndex )

   LOCAL lRet := .F., nOrder, cIndex

   IF HB_ISLOGICAL( xTag )
      IF xTag
         IF Empty( xIndex )
            cIndex := sx_IndexName()
         ELSEIF HB_ISNUMERIC( xIndex )
            cIndex := sx_IndexName( 1, xIndex )
         ELSEIF HB_ISSTRING( xIndex )
            nOrder := dbOrderInfo( DBOI_BAGORDER, xIndex )
            IF nOrder != 0
               cIndex := dbOrderInfo( DBOI_FULLPATH,, nOrder )
            ENDIF
         ENDIF
         IF !Empty( cIndex )
            IF ordBagClear( cIndex )
               lRet := FErase( cIndex ) != -1
            ENDIF
         ENDIF
      ENDIF
   ELSE
      IF HB_ISNUMERIC( xTag )
         IF Empty( xIndex ) .OR. !ValType( xIndex ) $ "CN"
            nOrder := xTag
         ELSEIF HB_ISSTRING( xIndex )
            IF xTag >= 1 .AND. xTag <= ordCount( xIndex )
               nOrder := dbOrderInfo( DBOI_BAGORDER, xIndex ) + xTag - 1
            ELSE
               nOrder := 0
            ENDIF
         ELSE
            nOrder := sx_TagOrder( xTag, xIndex )
         ENDIF
      ELSE
         IF Empty( xIndex ) .OR. !ValType( xIndex ) $ "CN"
            nOrder := ordNumber( xTag )
         ELSEIF HB_ISSTRING( xIndex )
            nOrder := sx_TagOrder( xTag, xIndex )
         ELSE
            nOrder := sx_TagOrder( 1, xIndex )
            IF nOrder != 0
               cIndex := dbOrderInfo( DBOI_FULLPATH,, nOrder )
               IF Empty( cIndex )
                  nOrder := 0
               ELSE
                  nOrder := sx_TagOrder( xTag, cIndex )
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF nOrder != 0
         lRet := ordDestroy( nOrder )
      ENDIF
   ENDIF

   RETURN lRet

FUNCTION sx_FileOrder()
   RETURN dbOrderInfo( DBOI_BAGNUMBER )

FUNCTION sx_SetFileOrd( nIndex )
   RETURN iif( HB_ISNUMERIC( nIndex ), ;
      ordSetFocus( sx_TagOrder( 1, nIndex ) ), ;
      ordSetFocus() )

FUNCTION rdd_Count()
   RETURN Len( rddList() )

FUNCTION rdd_Name( nRDD )

   LOCAL aRDD

   IF HB_ISNUMERIC( nRDD ) .AND. nRDD >= 1
      aRDD := rddList()
      IF nRDD <= Len( aRDD )
         RETURN aRDD[ nRDD ]
      ENDIF
   ENDIF

   RETURN ""

FUNCTION rdd_Info( xID )

   LOCAL aInfo, cRDD

   IF HB_ISNUMERIC( xID )
      IF !Empty( Alias( xID ) )
         ( xID )->( rddName() )
      ENDIF
   ELSEIF HB_ISSTRING( xID )
      cRDD := Upper( AllTrim( xID ) )
      IF AScan( rddList(), {| x | Upper( x ) == cRDD } ) == 0
         cRDD := NIL
      ENDIF
   ELSEIF xID == NIL
      cRDD := rddSetDefault()
   ENDIF

   IF Empty( cRDD )
      aInfo := {}
   ELSE
      aInfo := Array( 6 )
      aInfo[ 1 ] := cRDD
      aInfo[ 2 ] := .T.
      aInfo[ 3 ] := hb_rddInfo( RDDI_TABLEEXT, NIL, cRDD )
      aInfo[ 4 ] := hb_rddInfo( RDDI_ORDBAGEXT, NIL, cRDD )
      aInfo[ 5 ] := hb_rddInfo( RDDI_ORDEREXT, NIL, cRDD )
      aInfo[ 6 ] := hb_rddInfo( RDDI_MEMOEXT, NIL, cRDD )
   ENDIF

   RETURN aInfo

FUNCTION sx_IsDBT( cRDD )
   RETURN hb_rddInfo( RDDI_MEMOTYPE, NIL, cRDD ) == DB_MEMO_DBT

FUNCTION sx_MemoExt( cNewExt, cRDD )
   RETURN hb_rddInfo( RDDI_MEMOEXT, cNewExt, cRDD )

FUNCTION sx_MemoBlk( nNewBlock, cRDD )
   RETURN hb_rddInfo( RDDI_MEMOBLOCKSIZE, nNewBlock, cRDD )

FUNCTION sx_SetMemoBlock( nNewBlock, cRDD )
   RETURN hb_rddInfo( RDDI_MEMOBLOCKSIZE, nNewBlock, cRDD )

FUNCTION sx_StrXCheck( lStrict, cRDD )
   RETURN hb_rddInfo( RDDI_STRICTSTRUCT, lStrict, cRDD )

FUNCTION sx_LockRetry( nRetry, cRDD )
   RETURN hb_rddInfo( RDDI_LOCKRETRY, nRetry, cRDD )

FUNCTION sx_AutoOpen( lAuto, cRDD )
   RETURN hb_rddInfo( RDDI_AUTOOPEN, lAuto, cRDD )

FUNCTION sx_AutoShare( lAuto, cRDD )
   RETURN hb_rddInfo( RDDI_AUTOSHARE, lAuto, cRDD )

FUNCTION sx_Blob2File( cFileName, cFldName )
   RETURN dbFileGet( cFldName, cFileName, FILEGET_OVERWRITE )

FUNCTION sx_File2Blob( cFileName, cFldName, nActionCode )

   LOCAL nAction := 0

   IF hb_bitAnd( nActionCode, BLOB_FILECOMPRESS ) != 0
      nAction := hb_bitOr( nAction, FILEPUT_COMPRESS )
   ENDIF
   IF hb_bitAnd( nActionCode, BLOB_FILEENCRYPT ) != 0
      nAction := hb_bitOr( nAction, FILEPUT_ENCRYPT )
   ENDIF

   RETURN dbFilePut( cFldName, cFileName, nAction )

FUNCTION sx_dbCreate( cFileName, aStruct, cRDD )

   LOCAL aField, aDbStruct

   aDbStruct := AClone( aStruct )
   FOR EACH aField IN aDbStruct
      SWITCH aField[ 2 ]
      CASE "V"
         aField[ 3 ] += 6
         EXIT
      CASE "D"
         IF aField[ 3 ] == 3
            aField[ 2 ] := "V"
         ENDIF
         EXIT
      CASE "I"
         IF aField[ 3 ] == 4
            aField[ 2 ] := "V"
         ENDIF
         EXIT
      ENDSWITCH
   NEXT

   RETURN dbCreate( cFileName, aDbStruct, cRDD )

FUNCTION sx_VSigLen( xField )

   LOCAL nResult := 0, nField := 0

   IF Used()
      IF HB_ISSTRING( xField )
         nField := FieldPos( xField )
      ELSEIF HB_ISNUMERIC( xField )
         nField := xField
      ENDIF
      IF nField >= 1 .AND. nField <= FCount()
         nResult := FieldLen( nField )
         IF FieldType( nField ) == "V" .AND. nResult >= 6
            nResult -= 6
         ENDIF
      ENDIF
   ENDIF

   RETURN nResult

FUNCTION sx_VFGet( cExpr, nLen )

   /* Our RDDs does not use any internal flags to cut V-Fields so
    * we can simply evaluate given expression */

   IF Used() .AND. PCount() == 2
      RETURN PadR( &cExpr, nLen )
   ENDIF

   RETURN NIL

FUNCTION sx_IsLocked( xRec )

   LOCAL lResult := .F., xRecord

   IF Used()
      xRecord := iif( xRec == NIL, RecNo(), xRec )
      /*
       * Don't be confused by function name.
       * Even if it looks strange and results are not very usable due
       * to possible race condition then this is what SIX3 exactly does.
       */
      IF sx_Rlock( xRecord )
         sx_Unlock( xRecord )
      ELSE
         lResult := .T.
      ENDIF
   ENDIF

   RETURN lResult

FUNCTION sx_SetTrigger( nAction, cTriggerName, cRDD /* Harbour extensions */ )

   LOCAL cPrevTrigger := ""

   IF HB_ISNUMERIC( nAction )
      IF nAction == TRIGGER_PENDING
         IF HB_ISSTRING( cTriggerName )
            hb_rddInfo( RDDI_PENDINGTRIGGER, cTriggerName, cRDD )
         ENDIF
      ELSEIF Used()
         cPrevTrigger := dbInfo( DBI_TRIGGER )
         SWITCH nAction
         CASE TRIGGER_ENABLE
            dbInfo( DBI_TRIGGER, .T. )
            EXIT
         CASE TRIGGER_DISABLE
            dbInfo( DBI_TRIGGER, .F. )
            EXIT
         CASE TRIGGER_REMOVE
            dbInfo( DBI_TRIGGER, "" )
            EXIT
         CASE TRIGGER_INSTALL
            IF HB_ISSTRING( cTriggerName )
               dbInfo( DBI_TRIGGER, cTriggerName )
            ENDIF
            EXIT
         ENDSWITCH
      ENDIF
   ENDIF

   RETURN cPrevTrigger
