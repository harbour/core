/*
 * $Id$
 */

// hbwhat

* Useful .ini Interface enhancements


// Get from private Ini

*-----------------------------------------------------------------------------*

FUNCTION GetPrivateProfileLog( cSection, cEntry, lDefault, cFile )

  LOCAL cDefault:=iif(lDefault,"YES","NO")
  LOCAL cRet:=GetPrivateProfileString(cSection, cEntry, cDefault, cFile )

  RETURN(UPPER(cRet) $ "YESON1")

*-----------------------------------------------------------------------------*

FUNCTION GetPrivateProfileFloat(cSection, cEntry, nDefault, cFile )

  LOCAL cDefault:=STR(nDefault)
  LOCAL cRet:=GetPrivateProfileString(cSection, cEntry, cDefault, cFile )

  RETURN(VAL(cRet))

*-----------------------------------------------------------------------------*

FUNCTION GetPrivateProfileDate( cSection, cEntry, dDefault, cFile )

  LOCAL cDefault:=DTOS( dDefault )
  LOCAL cRet:=GetPrivateProfileString(cSection, cEntry, cDefault, cFile )

  RETURN(STOD( cRet ))


// Get from win.ini

*-----------------------------------------------------------------------------*

FUNCTION GetProfileLog( cSection, cEntry, lDefault )

  LOCAL cDefault:=iif(lDefault,"YES","NO")
  LOCAL cRet:=GetProfileString(cSection, cEntry, cDefault )

  RETURN(UPPER(cRet) $ "YESON1")

*-----------------------------------------------------------------------------*

FUNCTION GetProfileFloat(cSection, cEntry, nDefault )

  LOCAL cDefault:=STR(nDefault)
  LOCAL cRet:=GetProfileString(cSection, cEntry, cDefault )

  RETURN(VAL(cRet))

*-----------------------------------------------------------------------------*

FUNCTION GetProfileDate( cSection, cEntry, dDefault )

  LOCAL cDefault:=DTOS( dDefault )
  LOCAL cRet:=GetProfileString(cSection, cEntry, cDefault )

  RETURN(STOD( cRet ))


// Write to Private Ini

*-----------------------------------------------------------------------------*

FUNCTION WritePrivateProfileInt( cSection, cEntry, nData, cFile )

  RETURN( WritePrivateProfileString( cSection, cEntry, STR( INT(nData) ), cFile ) )

*-----------------------------------------------------------------------------*

FUNCTION WritePrivateProfileLog( cSection, cEntry, lData, cFile )

  RETURN( WritePrivateProfileString( cSection, cEntry, iif(lData,"Yes","No") , cFile ) )

*-----------------------------------------------------------------------------*

FUNCTION WritePrivateProfileFloat( cSection, cEntry, nData, cFile )

  RETURN( WritePrivateProfileString( cSection, cEntry, STR( nData ) , cFile ) )

*-----------------------------------------------------------------------------*

FUNCTION WritePrivateProfileDate( cSection, cEntry, dData, cFile )

  RETURN( WritePrivateProfileString( cSection, cEntry, DTOS( dData ) , cFile ) )



// Write to win.ini

*-----------------------------------------------------------------------------*

FUNCTION WriteProfileLog( cSection, cEntry, lData )

  RETURN( WriteProfileString( cSection, cEntry, iif(lData,"Yes","No") ) )


*-----------------------------------------------------------------------------*

FUNCTION WriteProfileFloat( cSection, cEntry, nData )

  RETURN( WriteProfileString( cSection, cEntry, STR( nData ) ) )

*-----------------------------------------------------------------------------*

FUNCTION WriteProfileDate( cSection, cEntry, dData )

  RETURN( WriteProfileString( cSection, cEntry, DTOS( dData ) ) )

*-----------------------------------------------------------------------------*


FUNCTION WriteProfileInt( cSection, cEntry, nData )

  RETURN( WriteProfileString( cSection, cEntry, STR( INT(nData) ) ) )
