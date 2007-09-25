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
 *          Sx_SetPass()
 *          Sx_VFGet()
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "common.ch"
#include "dbinfo.ch"
#include "ord.ch"
#include "hbsxdef.ch"

function sxChar( nLen, xKeyVal )

   switch valType( xKeyVal )
      case "C"
      case "M"
         exit
      case "N"
         xKeyVal := str( xKeyVal )
         exit
      case "D"
         xKeyVal := dtos( xKeyVal )
         exit
      case "L"
         xKeyVal := iif( xKeyVal, "T", "F" )
         exit
      otherwise
         xKeyVal := iif( valType( nLen ) == "N", "", space( 10 ) )
         exit
   endswitch

return iif( valType( nLen ) == "N", padr( ltrim( xKeyVal ), nLen ), xKeyVal )

function sxNum( xKeyVal )

   switch valType( xKeyVal )
      case "N"
         exit
      case "C"
      case "M"
         xKeyVal := val( xKeyVal )
         exit
      case "D"
         xKeyVal := xKeyVal - ctod( "" )
         exit
      case "L"
         xKeyVal := iif( xKeyVal, 1, 0 )
         exit
      otherwise
         xKeyVal := 0.00
         exit
   endswitch

return xKeyVal


function sxDate( xKeyVal )

   switch valType( xKeyVal )
      case "D"
         exit
      case "C"
      case "M"
         xKeyVal := ctod( xKeyVal )
         exit
      case "N"
         xKeyVal := ctod( "" ) + xKeyVal
         exit
      otherwise
         xKeyVal := ctod( "" )
         exit
   endswitch

return xKeyVal

function sxLog( xKeyVal )

   switch valType( xKeyVal )
      case "L"
         exit
      case "C"
      case "M"
         switch xKeyVal
            case  "T";  case  "t";  case  "Y";  case  "y"
            case ".T."; case ".t."; case ".Y."; case ".y."
               xKeyVal := .t.
               exit
            otherwise
               xKeyVal := .f.
               exit
         endswitch
         exit
      case "N"
         xKeyVal := xKeyVal != 0
         exit
      otherwise
         xKeyVal := .f.
         exit
   endswitch

return xKeyVal

function Sx_Compress( xVal )
   local cType := valType( xVal ), xRetVal
   if cType $ "CM"
      xRetVal := _sx_strCompress( xVal )
   elseif cType == "A"
      xRetVal := array( len( xVal ) )
      aEval( xVal, { |x| xRetVal := Sx_Compress( x ) } )
   else
      xRetVal := xVal
   endif
return xRetVal

function Sx_Decompress( xVal )
   local cType := valType( xVal ), xRetVal
   if cType $ "CM"
      xRetVal := _sx_strDecompress( xVal )
   elseif cType == "A"
      xRetVal := array( len( xVal ) )
      aEval( xVal, { |x| xRetVal := Sx_Decompress( x ) } )
   else
      xRetVal := xVal
   endif
return xRetVal

function Sx_TagInfo( cIndex )
   local aInfo, nOrds, nFirst, i

   if Used() && ( nOrds := OrdCount( cIndex ) ) > 0
      aInfo := array( nOrds, 6 )
      if valType( cIndex ) == "C"
         nFirst := dbOrderInfo( DBOI_BAGORDER, cIndex )
         nOrds += nFirst - 1
      else
         nFirst := 1
      endif
      for i := nFirst to nOrds
         aInfo[ i, 1 ] := ordName( i )
         aInfo[ i, 2 ] := ordKey( i )
         aInfo[ i, 3 ] := ordFor( i )
         aInfo[ i, 4 ] := ordIsUnique( i )
         aInfo[ i, 5 ] := ordDescend( i )
         aInfo[ i, 6 ] := ordCustom( i )
      next
   else
      aInfo := {}
   endif
return aInfo

function Sx_TagCount( xIndex )
   local nTags := 0, cIndex, nOrder
   if Used()
      if valtype( xIndex ) == "N"
         nOrder := Sx_TagOrder( 1, xIndex )
         if nOrder != 0
            cIndex := dbOrderInfo( DBOI_FULLPATH,, nOrder )
         endif
      elseif valtype( xIndex ) == "C" .and. !Empty( xIndex )
         cIndex := xIndex
      else
         cIndex := dbOrderInfo( DBOI_FULLPATH )
      endif
      if !Empty( cIndex )
         nTags := ordCount( cIndex )
      endif
   endif
return nTags

function Sx_Tags( xIndex )
   local aTagNames := {}, nOrder, nTags
   if Used()
      if valtype( xIndex ) == "N"
         nOrder := Sx_TagOrder( 1, xIndex )
      elseif valtype( xIndex ) == "C" .and. !Empty( xIndex )
         nOrder := dbOrderInfo( DBOI_BAGORDER, xIndex )
      else
         nOrder := OrdNumber()
      endif
      if nOrder != 0
         nTags := ordCount( dbOrderInfo( DBOI_FULLPATH,, nOrder ) )
         while --nTags >= 0
            aadd( aTagNames, ordName( nOrder++ ) )
         enddo
      endif
   endif
return aTagNames

function Sx_SetTag( xTag, xIndex )
   local lRet := .f., nOrder := 0, nOldOrd, cIndex
   if Used() .and. valtype( xTag ) $ "CN"
      if valtype( xTag ) == "N"
         if empty( xIndex ) .or. !valtype( xIndex ) $ "CN"
            nOrder := xTag
         elseif valtype( xIndex ) == "C"
            if xTag >= 1 .and. xTag <= ordCount( xIndex )
               nOrder := dbOrderInfo( DBOI_BAGORDER, xIndex ) + xTag - 1
            endif
         else
            nOrder := Sx_TagOrder( xTag, xIndex )
         endif
      else
         if empty( xIndex ) .or. !valtype( xIndex ) $ "CN"
            nOrder := OrdNumber( xTag )
         elseif valtype( xIndex ) == "C"
            nOrder := Sx_TagOrder( xTag, xIndex )
         else
            nOrder := Sx_TagOrder( 1, xIndex )
            if nOrder != 0
               cIndex := dbOrderInfo( DBOI_FULLPATH,, nOrder )
               if empty( cIndex )
                  nOrder := 0
               else
                  nOrder := Sx_TagOrder( xTag, cIndex )
               endif
            endif
         endif
      endif
      if nOrder != 0
         nOldOrd := OrdNumber()
         OrdSetFocus( nOrder )
         lRet := nOrder == OrdSetFocus()
         if ! lRet
            OrdSetFocus( nOldOrd )
         endif
      elseif empty( xTag )
         OrdSetFocus( 0 )
         lRet := .t.
      endif
   endif
return lRet

function Sx_KillTag( xTag, xIndex )
   local lRet := .f., nOrder, cIndex
   if valtype( xTag ) == "L"
      if xTag
         if empty( xIndex )
            cIndex := Sx_IndexName()
         elseif valtype( xIndex ) == "N"
            cIndex := Sx_IndexName( 1, xIndex )
         elseif valtype( xIndex ) == "C"
            nOrder := dbOrderInfo( DBOI_BAGORDER, xIndex )
            if nOrder != 0
               cIndex := dbOrderInfo( DBOI_FULLPATH,, nOrder )
            endif
         endif
         if !empty( cIndex )
            if ordBagClear( cIndex )
               lRet := ferase( cIndex ) != -1
            endif
         endif
      endif
   else
      if valtype( xTag ) == "N"
         if empty( xIndex ) .or. !valtype( xIndex ) $ "CN"
            nOrder := xTag
         elseif valtype( xIndex ) == "C"
            if xTag >= 1 .and. xTag <= ordCount( xIndex )
               nOrder := dbOrderInfo( DBOI_BAGORDER, xIndex ) + xTag - 1
            else
               nOrder := 0
            endif
         else
            nOrder := Sx_TagOrder( xTag, xIndex )
         endif
      else
         if empty( xIndex ) .or. !valtype( xIndex ) $ "CN"
            nOrder := OrdNumber( xTag )
         elseif valtype( xIndex ) == "C"
            nOrder := Sx_TagOrder( xTag, xIndex )
         else
            nOrder := Sx_TagOrder( 1, xIndex )
            if nOrder != 0
               cIndex := dbOrderInfo( DBOI_FULLPATH,, nOrder )
               if empty( cIndex )
                  nOrder := 0
               else
                  nOrder := Sx_TagOrder( xTag, cIndex )
               endif
            endif
         endif
      endif
      if nOrder != 0
         lRet := ordDestroy( nOrder )
      endif
   endif
return lRet

function Sx_FileOrder()
return dbOrderInfo( DBOI_BAGNUMBER )

function Sx_SetFileOrd( nIndex )
return iif( valtype( nIndex ) == "N", ;
            OrdSetFocus( Sx_TagOrder( 1, nIndex ) ), ;
            OrdSetFocus() )

function RDD_Count()
return len( RDDList() )

function RDD_Name( nRDD )
   local aRDD

   if valType( nRDD ) == "N" .and. nRDD >= 1
      aRDD := RDDList()
      if nRDD <= len( aRDD )
         return aRDD[ nRDD ]
      endif
   endif
return ""

function RDD_Info( xID )
   local aInfo, cRDD

   if valType( xID ) == "N"
      if !empty( alias( xID ) )
         ( xID )->( RDDName() )
      endif
   elseif valType( xID ) == "C"
      cRDD := upper( alltrim( xID ) )
      if ascan( RDDList(), {|x| upper( x ) == cRDD } ) == 0
         cRDD := NIL
      endif
   elseif xID == NIL
      cRDD := rddSetDefault()
   endif

   if empty( cRDD )
      aInfo := {}
   else
      aInfo := array( 6 )
      aInfo[ 1 ] := cRDD
      aInfo[ 2 ] := .t.
      aInfo[ 3 ] := rddInfo( RDDI_TABLEEXT, NIL, cRDD )
      aInfo[ 4 ] := rddInfo( RDDI_ORDBAGEXT, NIL, cRDD )
      aInfo[ 5 ] := rddInfo( RDDI_ORDEREXT, NIL, cRDD )
      aInfo[ 6 ] := rddInfo( RDDI_MEMOEXT, NIL, cRDD )
   endif
return aInfo

function Sx_IsDBT( cRDD )
return rddInfo( RDDI_MEMOTYPE, NIL, cRDD ) == DB_MEMO_DBT

function Sx_MemoExt( cNewExt, cRDD )
return rddInfo( RDDI_MEMOEXT, cNewExt, cRDD )

function Sx_MemoBlk( nNewBlock, cRDD )
return rddInfo( RDDI_MEMOBLOCKSIZE, nNewBlock, cRDD )

function Sx_SetMemoBlock( nNewBlock, cRDD )
return rddInfo( RDDI_MEMOBLOCKSIZE, nNewBlock, cRDD )

function Sx_StrxCheck( lStrict, cRDD )
return rddInfo( RDDI_STRICTSTRUCT, lStrict, cRDD )

function Sx_LockRetry( nRetry, cRDD )
return rddInfo( RDDI_LOCKRETRY, nRetry, cRDD )

function Sx_AutoOpen( lAuto, cRDD )
return rddInfo( RDDI_AUTOOPEN, lAuto, cRDD )

function Sx_AutoShare( lAuto, cRDD )
return rddInfo( RDDI_AUTOSHARE, lAuto, cRDD )

function Sx_BLOB2File( cFileName, cFldName )
return dbFileGet( cFldName, cFileName, FILEGET_OVERWRITE )

function Sx_File2BLOB( cFileName, cFldName, nActionCode )
   local nAction := 0
   if HB_BITAND( nActionCode, BLOB_FILECOMPRESS ) != 0
      nAction := HB_BITOR( nAction, FILEPUT_COMPRESS )
   endif
   if HB_BITAND( nActionCode, BLOB_FILEENCRYPT ) != 0
      nAction := HB_BITOR( nAction, FILEPUT_ENCRYPT )
   endif
return dbFileGet( cFldName, cFileName, nAction )

function Sx_dbCreate( cFileName, aStruct, cRDD )
   local aField, aDbStruct

   aDbStruct := AClone( aStruct )
   for each aField in aDbStruct
      switch aField[ 2 ]
         case "V"
            aField[ 3 ] += 6
            exit
         case "D"
            if aField[ 3 ] == 3
               aField[ 2 ] := "V"
            endif
            exit
         case "I"
            if aField[ 3 ] == 4
               aField[ 2 ] := "V"
            endif
            exit
      end
   next

return dbCreate( cFileName, aDbStruct, cRDD )

function Sx_VSigLen( xField )
   local nResult := 0, nField := 0

   if Used()
      if valtype( xField ) == "C"
         nField := FieldPos( xField )
      elseif valtype( xField ) == "N"
         nField := xField
      endif
      if nField >= 1 .and. nField <= FCount()
         nResult := FieldLen( nField )
         if FieldType( nField ) == "V" .and. nResult >= 6
            nResult -= 6
         endif
      endif
   endif

return nResult

function Sx_VFGet( cExpr, nLen )

   /* Our RDDs does not use any internal flags to cut V-Fields so
    * we can simply evaluate given expression */
    */
   if Used() .and. PCount() == 2
      return padr( &cExpr, nLen )
   endif

return NIL

function Sx_IsLocked( xRec )
   local lResult := .f., xRecord

   if Used()
      xRecord := IIF( xRec == NIL, RecNo(), xRec )
      /*
       * Don't be confused by function name.
       * Even if it looks strange and results are not very usable due
       * to possible race condition then this is what SIX3 exactly does.
       */
      if Sx_RLock( xRecord )
         Sx_UnLock( xRecord )
      else
         lResult := .t.
      endif
   endif

return lResult

function Sx_SetTrigger( nAction, cTriggerName, cRDD /* Harbour extensions */ )
   local cPrevTrigger := ""

   if valtype( nAction ) == "N"
      if nAction == TRIGGER_PENDING
         if valtype( cTriggerName ) == "C"
            rddInfo( RDDI_PENDINGTRIGGER, cTriggerName, cRDD )
         endif
      elseif Used()
         cPrevTrigger := dbInfo( DBI_TRIGGER )
         switch nAction
            case TRIGGER_ENABLE
               dbInfo( DBI_TRIGGER, .T. )
               exit
            case TRIGGER_DISABLE
               dbInfo( DBI_TRIGGER, .F. )
               exit
            case TRIGGER_REMOVE
               dbInfo( DBI_TRIGGER, "" )
               exit
            case TRIGGER_INSTALL
               if valtype( cTriggerName ) == "C"
                  dbInfo( DBI_TRIGGER, cTriggerName )
               endif
               exit
         end
      endif
   endif

return cPrevTrigger

function Sx_SetPass( cPass, nMode, cRdd /* Harbour extensions */ )

   HB_SYMBOL_UNUSED( nMode )

   if valtype( cPass ) == "C"
      rddInfo( RDDI_PENDINGPASSWORD, cPass, cRdd )
   endif

return nil
