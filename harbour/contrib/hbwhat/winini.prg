/*
 * $Id$
 */

//----------------------------------------------------------------------//
// hbwhat
* Useful .ini Interface enhancements
//----------------------------------------------------------------------//
// Get from private Ini
//----------------------------------------------------------------------//
FUNCTION WHT_GetPrivateProfileLog( cSection, cEntry, lDefault, cFile )

  LOCAL cDefault:=iif(lDefault,"YES","NO")
  LOCAL cRet := VWN_GetPrivateProfileString(cSection, cEntry, cDefault, cFile )

  RETURN(UPPER(cRet) $ "YESON1")
//----------------------------------------------------------------------//
FUNCTION WHT_GetPrivateProfileFloat(cSection, cEntry, nDefault, cFile )

  LOCAL cDefault:=STR(nDefault)
  LOCAL cRet:=VWN_GetPrivateProfileString(cSection, cEntry, cDefault, cFile )

  RETURN(VAL(cRet))
//----------------------------------------------------------------------//
FUNCTION WHT_GetPrivateProfileDate( cSection, cEntry, dDefault, cFile )

  LOCAL cDefault:=DTOS( dDefault )
  LOCAL cRet:=VWN_GetPrivateProfileString(cSection, cEntry, cDefault, cFile )

  RETURN(STOD( cRet ))
//----------------------------------------------------------------------//
// Get from win.ini
//----------------------------------------------------------------------//
FUNCTION WHT_GetProfileLog( cSection, cEntry, lDefault )

  LOCAL cDefault:=iif(lDefault,"YES","NO")
  LOCAL cRet:=VWN_GetProfileString(cSection, cEntry, cDefault )

  RETURN(UPPER(cRet) $ "YESON1")
//----------------------------------------------------------------------//
FUNCTION WHT_GetProfileFloat(cSection, cEntry, nDefault )

  LOCAL cDefault := STR(nDefault)
  LOCAL cRet := VWN_GetProfileString(cSection, cEntry, cDefault )

  RETURN(VAL(cRet))
//----------------------------------------------------------------------//
FUNCTION WHT_GetProfileDate( cSection, cEntry, dDefault )

  LOCAL cDefault:=DTOS( dDefault )
  LOCAL cRet:=VWN_GetProfileString(cSection, cEntry, cDefault )

  RETURN(STOD( cRet ))
//----------------------------------------------------------------------//
// Write to Private Ini
//----------------------------------------------------------------------//
FUNCTION WHT_WritePrivateProfileInt( cSection, cEntry, nData, cFile )

  RETURN( VWN_WritePrivateProfileString( cSection, cEntry, STR( INT(nData) ), cFile ) )
//----------------------------------------------------------------------//
FUNCTION WHT_WritePrivateProfileLog( cSection, cEntry, lData, cFile )

  RETURN( VWN_WritePrivateProfileString( cSection, cEntry, iif(lData,"Yes","No") , cFile ) )
//----------------------------------------------------------------------//
FUNCTION WHT_WritePrivateProfileFloat( cSection, cEntry, nData, cFile )

  RETURN( VWN_WritePrivateProfileString( cSection, cEntry, STR( nData ) , cFile ) )
//----------------------------------------------------------------------//
FUNCTION WHT_WritePrivateProfileDate( cSection, cEntry, dData, cFile )

  RETURN( VWN_WritePrivateProfileString( cSection, cEntry, DTOS( dData ) , cFile ) )
//----------------------------------------------------------------------//
// Write to win.ini
//----------------------------------------------------------------------//
FUNCTION WHT_WriteProfileLog( cSection, cEntry, lData )

  RETURN( VWN_WriteProfileString( cSection, cEntry, iif(lData,"Yes","No") ) )
//----------------------------------------------------------------------//
FUNCTION WHT_WriteProfileFloat( cSection, cEntry, nData )

  RETURN( VWN_WriteProfileString( cSection, cEntry, STR( nData ) ) )
//----------------------------------------------------------------------//
FUNCTION WHT_WriteProfileDate( cSection, cEntry, dData )

  RETURN( VWN_WriteProfileString( cSection, cEntry, DTOS( dData ) ) )
//----------------------------------------------------------------------//
FUNCTION WHT_WriteProfileInt( cSection, cEntry, nData )

  RETURN( VWN_WriteProfileString( cSection, cEntry, STR( INT(nData) ) ) )
//----------------------------------------------------------------------//
