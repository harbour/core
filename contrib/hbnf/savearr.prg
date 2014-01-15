/*
 * Author....: David Barrett
 * CIS ID....: 72037,105
 *
 * This is an original work by David Barrett and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 22:04:18   GLENN
 * A few users have reported that these functions do not support
 * multi-dimensional arrays.  Until the bugs are verified and
 * workarounds or re-writes devised, a warning has been placed in the
 * documentation.
 *
 *    Rev 1.2   15 Aug 1991 23:06:06   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:54   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   07 Jun 1991 23:39:38   GLENN
 * Initial revision.
 *
 *
 */

/* Set <lDropCompatibility> to .T. if you don't mind, if the created
   file cannot be read back by Cl*pper applications using the original
   NFLIB, if they have any date value in them. This mode fixes the Y2K
   and date format dependence bugs in original NFLIB implementation.
   [vszakats] */

FUNCTION ft_SaveArr( aArray, cFileName, /* @ */ nErrorCode, lDropCompatibility /* HB_EXTENSION */ )

   LOCAL nHandle, lRet

   nHandle := FCreate( cFileName )
   nErrorCode := FError()
   IF nErrorCode == 0
      lRet := _ftsavesub( aArray, nHandle, @nErrorCode, ;
         hb_defaultValue( lDropCompatibility, .F. ) )
      FClose( nHandle )
      IF lRet .AND. FError() != 0
         nErrorCode := FError()
         lRet := .F.
      ENDIF
   ELSE
      lRet := .F.
   ENDIF

   RETURN lRet

STATIC FUNCTION _ftsavesub( xMemVar, nHandle, /* @ */ nErrorCode, lDropCompatibility )

   LOCAL cValType, nLen, cString

   LOCAL lRet := .T.

   cValType := ValType( xMemVar )
   FWrite( nHandle, cValType, 1 )
   IF FError() == 0
      SWITCH cValType
      CASE "A"
         nLen := Len( xMemVar )
         FWrite( nHandle, L2Bin( nLen ), 4 )
         IF FError() == 0
            AEval( xMemVar, {| xMemVar1 | ;
               lRet := _ftsavesub( xMemVar1, nHandle,, lDropCompatibility ) } )
         ELSE
            lRet := .F.
         ENDIF
         EXIT
      CASE "B"
         lRet := .F.
         EXIT
      CASE "C"
         nLen := Len( xMemVar )
         FWrite( nHandle, L2Bin( nLen ), 4 )
         FWrite( nHandle, xMemVar )
         EXIT
      CASE "D"
         nLen := 8
         FWrite( nHandle, L2Bin( nLen ), 4 )
         FWrite( nHandle, iif( lDropCompatibility, DToS( xMemVar ), DToC( xMemVar ) ), 8 )
         EXIT
      CASE "L"
         nLen := 1
         FWrite( nHandle, L2Bin( nLen ), 4 )
         FWrite( nHandle, iif( xMemVar, "T", "F" ) )
         EXIT
      CASE "N"
         cString := Str( xMemVar )
         nLen := Len( cString )
         FWrite( nHandle, L2Bin( nLen ), 4 )
         FWrite( nHandle, cString )
         EXIT
      ENDSWITCH
   ELSE
      lRet := .F.
   ENDIF
   nErrorCode := FError()

   RETURN lRet

FUNCTION ft_RestArr( cFileName, /* @ */ nErrorCode )

   LOCAL nHandle, aArray

   nHandle := FOpen( cFileName )
   nErrorCode := FError()
   IF nErrorCode == 0
      aArray := _ftrestsub( nHandle, @nErrorCode )
      FClose( nHandle )
   ELSE
      aArray := {}
   ENDIF

   RETURN aArray

STATIC FUNCTION _ftrestsub( nHandle, /* @ */ nErrorCode )

   LOCAL cValType, nLen, cLenStr, xMemVar, cMemVar, nk

   cValType := Space( 1 )
   FRead( nHandle, @cValType, 1 )
   cLenStr := Space( 4 )
   FRead( nHandle, @cLenStr, 4 )
   nLen := Bin2L( cLenStr )
   nErrorCode := FError()
   IF nErrorCode == 0
      SWITCH cValType
      CASE "A"
         xMemVar := {}
         FOR nk := 1 TO nLen
            AAdd( xMemVar, _ftrestsub( nHandle ) )  /* Recursive call */
         NEXT
         EXIT
      CASE "C"
         xMemVar := Space( nLen )
         FRead( nHandle, @xMemVar, nLen )
         EXIT
      CASE "D"
         cMemVar := Space( 8 )
         FRead( nHandle, @cMemVar, 8 )
         IF Empty( hb_StrReplace( cMemVar, "0123456789", "" ) )
            xMemVar := hb_SToD( cMemVar )
         ELSE
            /* Original Cl*pper NFLIB format:
               not Y2K compatible, and it needs same _SET_DATEFORMAT on save and load */
            xMemVar := CToD( cMemVar )
         ENDIF
         EXIT
      CASE "L"
         cMemVar := Space( 1 )
         FRead( nHandle, @cMemVar, 1 )
         xMemVar := ( cMemVar == "T" )
         EXIT
      CASE "N"
         cMemVar := Space( nLen )
         FRead( nHandle, @cMemVar, nLen )
         xMemVar := Val( cMemVar )
         EXIT
      ENDSWITCH
      nErrorCode := FError()
   ENDIF

   RETURN xMemVar
