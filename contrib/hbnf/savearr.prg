/* This is an original work by David Barrett and is placed in the public domain.

      Rev 1.3   28 Sep 1992 22:04:18   GLENN
   A few users have reported that these functions do not support
   multi-dimensional arrays.  Until the bugs are verified and
   workarounds or re-writes devised, a warning has been placed in the
   documentation.

      Rev 1.2   15 Aug 1991 23:06:06   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   14 Jun 1991 19:52:54   GLENN
   Minor edit to file header

      Rev 1.0   07 Jun 1991 23:39:38   GLENN
   Initial revision.
 */

/* Set <lDropCompatibility> to .T. if you don't mind, if the created
   file cannot be read back by Cl*pper applications using the original
   NFLIB, if they have any date value in them. This mode fixes the Y2K
   and date format dependence bugs in original NFLIB implementation.
   [vszakats] */

FUNCTION ft_SaveArr( aArray, cFileName, /* @ */ nErrorCode, lDropCompatibility /* HB_EXTENSION */ )

   LOCAL nHandle, lRet

   nHandle := FCreate( cFileName )
   IF ( nErrorCode := FError() ) == 0
      lRet := _ftsavesub( aArray, nHandle, @nErrorCode, hb_defaultValue( lDropCompatibility, .F. ) )
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

   LOCAL lRet := .T.
   LOCAL cValType := ValType( xMemVar )

   IF FWrite( nHandle, cValType ) == hb_BLen( cValType )
      SWITCH cValType
      CASE "A"
         IF FWrite( nHandle, L2Bin( Len( xMemVar ) ) ) == 4
            AEval( xMemVar, {| xMemVar1 | lRet := _ftsavesub( xMemVar1, nHandle,, lDropCompatibility ) },, 0xFFFFFFFF )
            EXIT
         ENDIF
         // fall through
      CASE "B"
         lRet := .F.
         EXIT
      CASE "N"
         xMemVar := Str( xMemVar )
         // fall through
      CASE "C" ; FWrite( nHandle, L2Bin( hb_BLen( xMemVar ) ) + hb_BLeft( xMemVar, 0xFFFFFFFF ) ) ; EXIT
      CASE "D" ; FWrite( nHandle, L2Bin( 8 ) + iif( lDropCompatibility, DToS( xMemVar ), hb_BLeft( DToC( xMemVar ), 8 ) ) ) ; EXIT
      CASE "L" ; FWrite( nHandle, L2Bin( 1 ) + iif( xMemVar, "T", "F" ) ) ; EXIT
      ENDSWITCH
   ELSE
      lRet := .F.
   ENDIF
   nErrorCode := FError()

   RETURN lRet

FUNCTION ft_RestArr( cFileName, /* @ */ nErrorCode )

   LOCAL aArray
   LOCAL nHandle := FOpen( cFileName )

   IF ( nErrorCode := FError() ) == 0
      aArray := _ftrestsub( nHandle, @nErrorCode )
      FClose( nHandle )
   ELSE
      aArray := {}
   ENDIF

   RETURN aArray

STATIC FUNCTION _ftrestsub( nHandle, /* @ */ nErrorCode )

   LOCAL cValType := hb_FReadLen( nHandle, 1 ), xMemVar, tmp
   LOCAL nLen := Bin2L( hb_FReadLen( nHandle, 4 ) )

   IF ( nErrorCode := FError() ) == 0
      SWITCH cValType
      CASE "C" ; xMemVar := hb_FReadLen( nHandle, nLen ) ; EXIT
      CASE "N" ; xMemVar := Val( hb_FReadLen( nHandle, nLen ) ) ; EXIT
      CASE "L" ; xMemVar := ( hb_FReadLen( nHandle, 1 ) == "T" ) ; EXIT
      CASE "D"
         xMemVar := hb_FReadLen( nHandle, 8 )
         // Fall back to CToD() to handle original Cl*pper NFLIB format:
         // not Y2K compatible, and it needs same _SET_DATEFORMAT on save and load
         xMemVar := iif( Empty( hb_StrReplace( xMemVar, "0123456789" ) ), hb_SToD( xMemVar ), CToD( xMemVar ) )
         EXIT
      CASE "A"
         xMemVar := {}
         FOR tmp := 1 TO nLen
            AAdd( xMemVar, _ftrestsub( nHandle ) )  // Recursive call
         NEXT
         EXIT
      ENDSWITCH
      nErrorCode := FError()
   ENDIF

   RETURN xMemVar
