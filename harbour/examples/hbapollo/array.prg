/*
 * $Id$
 */

/*
 * SixAPI Project source code:
 *
 * Copyright 2010 Andi Jahja <xharbour@telkom.net.id>
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
 */

/*
  Part of this program was taken from Nanfor Libs
*/

FUNCTION sx_ReplaceArray( cFieldName, aArray, xAlias )

   LOCAL cFileName  := "sxarray.dat"
   LOCAL nErrorCode := 0

   IF ValType( aArray ) == "A"
      IF FT_SAVEARR( aArray, cFileName, @nErrorCode )
         sx_ReplaceBLOB( cFieldName, cFileName, xAlias )
         FErase( cFileName )
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

FUNCTION sx_GetValueArray ( cFieldName, xAlias )

   LOCAL aArray := {}
   LOCAL cFileName := "sxarray.dat"
   LOCAL nErrorCode := 0

   IF sx_BlobToFile( cFieldName, cFileName, xAlias )
      aArray := FT_RESTARR( cFileName, @nErrorCode )
      FErase( cFileName )
   ENDIF

   RETURN aArray

STATIC FUNCTION FT_SAVEARR( aArray, cFileName, nErrorCode )

   LOCAL nHandle, lRet

   nHandle := FCreate( cFileName )
   nErrorCode := FError()
   IF nErrorCode == 0
      lRet := _ftsavesub( aArray, nHandle, @nErrorCode )
      FClose( nHandle )
      IF lRet .AND. FError() != 0
         nErrorCode := FError()
         lRet = .F.
      ENDIF
   ELSE
      lRet = .F.
   ENDIF

   RETURN lRet

STATIC FUNCTION _ftsavesub( xMemVar, nHandle, nErrorCode )

   LOCAL cValType, nLen, cString
   LOCAL lRet := .T.

   cValType := ValType( xMemVar )
   FWrite( nHandle, cValType, 1 )
   IF FError() = 0
      DO CASE
      CASE cValType = "A"
         nLen := Len( xMemVar )
         FWrite( nHandle, L2Bin( nLen ), 4 )
         IF FError() = 0
            AEval( xMemVar, {| xMemVar1 | lRet := _ftsavesub( xMemVar1, nHandle ) } )
         ELSE
            lRet = .F.
         ENDIF
      CASE cValType = "B"
         lRet := .F.
      CASE cValType = "C"
         nLen := Len( xMemVar )
         FWrite( nHandle, L2Bin( nLen ), 4 )
         FWrite( nHandle, xMemVar )
      CASE cValType = "D"
         nLen := 8
         FWrite( nHandle, L2Bin( nLen ), 4 )
         FWrite( nHandle, DToC( xMemVar ) )
      CASE cValType = "L"
         nLen := 1
         FWrite( nHandle, L2Bin( nLen ), 4 )
         FWrite( nHandle, iif( xMemVar, "T", "F" ) )
      CASE cValType = "N"
         cString := Str( xMemVar )
         nLen := Len( cString )
         FWrite( nHandle, L2Bin( nLen ), 4 )
         FWrite( nHandle, cString )
      ENDCASE
   ELSE
      lRet = .F.
   ENDIF
   nErrorCode = FError()

   RETURN lRet

STATIC FUNCTION FT_RESTARR( cFileName, nErrorCode )

   LOCAL nHandle, aArray

   nHandle := FOpen( cFileName )
   nErrorCode := FError()
   IF nErrorCode = 0
      aArray := _ftrestsub( nHandle, @nErrorCode )
      FClose( nHandle )
   ELSE
      aArray := {}
   ENDIF

   RETURN aArray

STATIC FUNCTION _ftrestsub( nHandle, nErrorCode )

   LOCAL cValType, nLen, cLenStr, xMemVar, cMemVar, nk

   cValType := " "
   FRead( nHandle, @cValType, 1 )
   cLenStr := Space( 4 )
   FRead( nHandle, @cLenStr, 4 )
   nLen = Bin2L( cLenStr )
   nErrorCode = FError()
   IF nErrorCode == 0
      DO CASE
      CASE cValType = "A"
         xMemVar := {}
         FOR nk := 1 TO nLen
            AAdd( xMemVar, _ftrestsub( nHandle ) )      // Recursive call
         NEXT
      CASE cValType = "C"
         xMemVar := Space( nLen )
         FRead( nHandle, @xMemVar, nLen )
      CASE cValType = "D"
         cMemVar = Space( 8 )
         FRead( nHandle, @cMemVar, 8 )
         xMemVar := CToD( cMemVar )
      CASE cValType = "L"
         cMemVar := " "
         FRead( nHandle, @cMemVar, 1 )
         xMemVar := ( cMemVar =  "T" )
      CASE cValType = "N"
         cMemVar := Space( nLen )
         FRead( nHandle, @cMemVar, nLen )
         xMemVar = Val( cMemVar )
      ENDCASE
      nErrorCode := FError()
   ENDIF

   RETURN xMemVar
