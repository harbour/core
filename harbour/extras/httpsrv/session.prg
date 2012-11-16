/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    uHTTPD (Micro HTTP server) session functions
 *
 * Copyright 2009 Francesco Saverio Giudice <info / at / fsgiudice.com>
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

#include "hbclass.ch"
#include "fileio.ch"
#include "directry.ch"

#xtranslate SetNewValueReturnOld( <p>, <v> ) => LOCAL xOld, xOld := <p>, iif( <v> != NIL, <p> := <v>,  ), xOld
#xtranslate Default( <p>, <v> ) => ( <p> := iif( <p> == NIL, <v>, <p> ) )


MEMVAR _COOKIE, _SESSION, _REQUEST, _SERVER

#define MY_CRCKEY "UhTTpK3y76"

FUNCTION uhttpd_SessionNew( cSessionName, cSessionPath )
   RETURN uhttpd_Session():New( cSessionName, cSessionPath )

CLASS uhttpd_Session

   METHOD New()

   DESTRUCTOR DestroyObject()

   METHOD Start()
   METHOD IsRegistered()
   METHOD CacheExpire( nTimeInMinutes ) INLINE SetNewValueReturnOld( ::nCache_Expire, nTimeInMinutes )
   METHOD CacheLimiter()
   METHOD GetCookieParams()     INLINE { ::nCookie_LifeTime, ::cCookie_Path, ::cCookie_Domain, ::lCookie_Secure  }
   METHOD SetCookieParams()
   METHOD ID( cID )             INLINE SetNewValueReturnOld( ::cSID, cID )
   METHOD Name( cName )         INLINE SetNewValueReturnOld( ::cName, cName )
   METHOD RegenerateID()
   METHOD SavePath( cPath )     INLINE SetNewValueReturnOld( ::cSavePath, cPath )
   METHOD IsStarted()           INLINE ( ::nActiveSessions > 0 )
   METHOD UseOnlyCookies()      INLINE ::lUse_Only_Cookies
   METHOD UseTransSID()         INLINE ::lUse_Only_Cookies

   METHOD SaveCookie()
   METHOD GetSessionVars()
   METHOD GetVar( cVar )        INLINE uhttpd_HGetValue( _SESSION, cVar )
   METHOD SetVar( cVar, xValue ) INLINE _SESSION[ cVar ] := xValue

   METHOD SetSaveHandler()
   METHOD Open( cPath, cName )
   METHOD Close()
   METHOD Read( cID )
   METHOD Write( cID, cData )
   METHOD Destroy( cID )
   METHOD GC( nMaxLifeTime )

   METHOD SessionContainer( hHash )  INLINE SetNewValueReturnOld( _SESSION, hHash )
   METHOD Encode()              // INLINE hb_Serialize( _SESSION )
   METHOD Decode()

   HIDDEN:

   VAR oCookie
   VAR cSID
   VAR cSavePath               INIT "/tmp"
   VAR cName                   // INIT "SESSIONID"
   VAR lAuto_Start             INIT .F.       // .F. = no autostart
   VAR nGc_Probability         INIT 33        // Every 1/3 of checks i'll lunch Session GC
   VAR nGc_MaxLifeTime         INIT 1440      // seconds - Number of seconds after gc can delete a session
   // VAR cSerialize_Handler      INIT "HBHTMLLIB"
   VAR nCookie_LifeTime        INIT 3600 // 0         // Number of seconds to keep cookie, 0 = until browser is closed
   VAR cCookie_Path            INIT "/"
   VAR cCookie_Domain
   VAR lCookie_Secure          INIT .F.
   VAR lUse_Cookies            INIT .T.       // .T. = Use cookies to store session id on client side
   VAR lUse_Only_Cookies       INIT .F.
   VAR cReferrer_Check                        // If is set check if referrer is equal to, if it isn't block
   // VAR cEntropy_File
   // VAR nEntropy_Lenght
   VAR cCache_Limiter          INIT "nocache" // Possible values are: none, nocache, private, private_no_expire, public
   VAR nCache_Expire           INIT 180       // in minutes, not checked if cCache_Limiter == none or nocache
   VAR lUse_Trans_SID          INIT .F.       // .F. = no SID appended to URL

   // Session Storage code blocks
   VAR bOpen                   // INIT {| cPath, cName | ::SessionOpen( cPath, cName ) }
   VAR bClose                  // INIT {|| ::SessionClose() }
   VAR bRead                   // INIT {| cID | ::SessionRead( cID ) }
   VAR bWrite                  // INIT {| cID, cData | ::SessionWrite( cID, cData ) }
   VAR bDestroy                // INIT {| cID | ::SessionDestroy( cID ) }
   VAR bGC                     // INIT {| nMaxLifeTime | ::SessionGC( nMaxLifeTime ) }
   VAR nFileRetry              INIT 10        // How many time try to open / write / delete file in case of error
   VAR nFileWait               INIT 500       // How many milliseconds have to wait before retry

   VAR nActiveSessions         INIT 0

   VAR lSessionActive          INIT .F.

   METHOD GenerateSID()
   METHOD CheckSID()
   METHOD SessionOpen()
   METHOD SessionClose()
   METHOD SessionRead()
   METHOD SessionWrite()
   METHOD SessionDestroy()
   METHOD SessionGC()

   METHOD SendCacheLimiter()

ENDCLASS

// ------------------------------

METHOD New( cSessionName, cSessionPath ) CLASS uhttpd_Session

   // hb_ToOutDebug( "cSessionName = %s, cSessionPath = %s\n\r", cSessionName, cSessionPath )

   __defaultNIL( @cSessionName, "SESSION" )
   __defaultNIL( @cSessionPath, ::cSavePath )

   // ::cSID := ::GenerateSID()

   // As default we will use FILES - this is FILE version
   ::bOpen     := {| cPath, cName | ::SessionOpen( cPath, cName ) }
   ::bClose    := {|| ::SessionClose() }
   ::bRead     := {| cID | ::SessionRead( cID ) }
   ::bWrite    := {| cID, cData | ::SessionWrite( cID, cData ) }
   ::bDestroy  := {| cID | ::SessionDestroy( cID ) }
   ::bGC       := {| nMaxLifeTime | ::SessionGC( nMaxLifeTime ) }

#if 0
   // DBF version - we will store in a DBF - this only an example
   ::bOpen     := {| cPath, cName | DBF_Session_Open( cPath, cName ) }
   ::bClose    := {|| DBF_Session_Close() }
   ::bRead     := {| cID | DBF_Session_Read( cID ) }
   ::bWrite    := {| cID, cData | DBF_Session_Write( cID, cData ) }
   ::bDestroy  := {| cID | DBF_Session_Destroy( cID ) }
   ::bGC       := {| nMaxLifeTime | DBF_Session_GC( nMaxLifeTime ) }
#endif

   ::cName     := cSessionName + "ID"
   ::cReferrer_Check := _SERVER[ "HTTP_REFERER" ]

   ::cSavePath := cSessionPath

   ::oCookie   := uhttpd_CookieNew( ::cCookie_Domain, ::cCookie_Path )

   RETURN Self

METHOD Start( cSID ) CLASS uhttpd_Session

   LOCAL lSendCookie := .T.
   LOCAL lDefine_SID := .T.
   LOCAL xVal, nRand, nPos
   LOCAL hUrl

   IF cSID != NIL
      ::cSID := cSID
   ENDIF

   // hb_ToOutDebug( "cSID = %s, ::cSID = %s\n\r", cSID, ::cSID )

   // TraceLog( "Active Sessions : " + hb_CStr( ::nActiveSessions ) )

   IF ::nActiveSessions != 0
      RETURN .F.
   ENDIF

   // Start checking ID from global vars
   IF ( nPos := hb_HPos( _REQUEST, ::cName ) ) > 0
      // ::cSID := ::oCGI:h_Request[ ::cName ]
      ::cSID := hb_HValueAt( _REQUEST, nPos )
      IF HB_ISARRAY( ::cSID )
         ::cSID := ::cSID[ 1 ] // Get Only 1-st
      ENDIF
      lSendCookie := .F.
      lDefine_SID := .F.
      // ::oCGI:ToLogFile( "::cSID = " + hb_CStr( ::cSID ), "/pointtoit/tmp/log.txt" )
   ENDIF

   IF ! Empty( ::cSID ) .AND. ! ::CheckSID()
      // Check if the SID is NOT valid, someone altered it
      // ::oCGI:ToLogFile( "::cSID = " + hb_CStr( ::cSID ) + " SID is NOT valid, someone altered it", "/pointtoit/tmp/log.txt" )
      ::cSID      := NIL   // invalidate current SID, i'll generate a new one
      lSendCookie := .T.
      lDefine_SID := .T.
   ENDIF

   IF ! Empty( ::cSID ) .AND. ! Empty( ::cReferrer_Check )
      // TODO: fix

      // oUrl := TUrl():New( ::cReferrer_Check )
      hUrl := uhttpd_SplitUrl( ::cReferrer_Check )

      // hb_ToOutDebug( "hUrl = %s\n\r", hb_ValToExp( hUrl ) )

      // IF !( oUrl:cServer == _SERVER[ "SERVER_NAME" ] )
      IF !( hUrl[ "HOST" ] == _SERVER[ "SERVER_NAME" ] )
         ::cSID      := NIL   // invalidate current SID, i'll generate a new one
         lSendCookie := .T.
         lDefine_SID := .T.
      ENDIF

      // // Check whether the current request was referred to by
      // // an external site which invalidates the previously found ID
      // $url = parse_url($GLOBALS['HTTP_REFERER']);
      // IF !(Trim($url['host']) == $GLOBALS['SERVER_NAME'])
      //    unset(session->id)
      //    send_cookie := .T.
      //    define_sid := .T.
   ENDIF

   // Do we have an existing session ID?
   IF Empty( ::cSID )
      // Create new session ID
      ::cSID := ::GenerateSID()
   ENDIF

   // Is use_cookies set to false?
   IF ! ::lUse_Cookies .AND. lSendCookie
      lDefine_SID := .T.
      lSendCookie := .F.
   ENDIF

   // Should we send a cookie?
   IF lSendCookie
      ::oCookie:SetCookie( ::cName, ::cSID, ::cCookie_Domain, ::cCookie_Path, uhttpd_DateToGMT(,,, ::nCookie_LifeTime ), ::lCookie_Secure )
   ENDIF

   // Should we define the SID?
   IF lDefine_SID
      cSID := ::cName + "=" + ::cSID
      _REQUEST[ ::cName ] := ::cSID
   ENDIF

   ::nActiveSessions++

   // Send caching headers

   // Start session
   IF ! ::Open( ::cSavePath, ::cName )
      uhttpd_Die( "ERROR: Failed to open session file" )
   ENDIF

   // Read session data
   IF !( ( xVal := ::Read( ::cSID  ) ) == NIL )
      // TraceLog( "Read session data - xVal", xVal )
      // ::oCGI:ToLogFile( "xval = " + hb_CStr( xVal ), "/pointtoit/tmp/log.txt" )
      // Decode session data
      ::Decode( xVal )
      // ::oCGI:ToLogFile( "decoded", "/pointtoit/tmp/log.txt" )
   ENDIF

   // Send HTTP cache headers
   ::SendCacheLimiter()

   // Check if we should clean up (call the garbage collection routines)
   // TraceLog( "::nGc_probability = " + hb_CStr( ::nGc_probability ) )
   IF ::nGc_probability > 0
      nRand := hb_RandomInt( 1, 100 )
      // TraceLog( "::nGc_probability - nRand = " + hb_CStr( nRand ) )
      IF nRand <= ::nGc_Probability
         ::GC( ::nGc_MaxLifeTime )
      ENDIF
   ENDIF

   RETURN .T.

METHOD Destroy() CLASS uhttpd_Session

   IF ::nActiveSessions == 0
      RETURN .F.
   ENDIF

   // Destroy session
   IF ! Eval( ::bDestroy, ::cSID )
      RETURN .F.
   ENDIF

   RETURN .T.

METHOD Close() CLASS uhttpd_Session

   LOCAL cVal

   // TraceLog( "Session Close() - oCGI:h_Session", DumpValue( oCGI:h_Session ) )

   IF ::nActiveSessions == 0
      RETURN .F.
   ENDIF

   // Encode session
   cVal := ::Encode()

   // Save session
   IF ! ::Write( ::cSID, cVal )
      uhttpd_Die( "Session could not be saved." )
   ENDIF
   // Close session
   IF ! Eval( ::bClose )
      uhttpd_Die( "Session could not be closed." )
   ENDIF
   ::nActiveSessions--

   RETURN .T.

METHOD Open( cPath, cName ) CLASS uhttpd_Session
   RETURN Eval( ::bOpen, cPath, cName  )

METHOD Read( cID ) CLASS uhttpd_Session
   RETURN Eval( ::bRead, cID  )

METHOD Write( cID, cData ) CLASS uhttpd_Session
   RETURN Eval( ::bWrite, cID, cData )

METHOD GC( nMaxLifeTime ) CLASS uhttpd_Session
   RETURN Eval( ::bGC, nMaxLifeTime )


METHOD IsRegistered() CLASS uhttpd_Session

   LOCAL lRegistered := .F.

   RETURN lRegistered

METHOD CacheLimiter( cNewLimiter ) CLASS uhttpd_Session

   LOCAL cOldLimiter := ::cCache_Limiter

   IF cNewLimiter != NIL
      IF cNewLimiter $ "none/nocache/private/private_no_expire/public"
         ::cCache_Limiter := cNewLimiter
      ELSE
         uhttpd_Die( "ERROR: uhttpd_Session:CacheLimiter() - New Limiter is incorrect" )
      ENDIF
   ENDIF

   RETURN cOldLimiter

METHOD SetCookieParams( nLifeTime, cPath, cDomain, lSecure  ) CLASS uhttpd_Session

   IF nLifeTime != NIL
      ::nCookie_LifeTime := nLifeTime
   ENDIF
   IF cPath     != NIL
      ::cCookie_Path     := cPath
   ENDIF
   IF cDomain   != NIL
      ::cCookie_Domain   := cDomain
   ENDIF
   IF lSecure   != NIL
      ::lCookie_Secure   := lSecure
   ENDIF

   RETURN NIL

METHOD RegenerateID() CLASS uhttpd_Session

   ::cSID := ::GenerateSID()
   IF ::lUse_Cookies
      ::oCookie:SetCookie( ::cName, ::cSID, ::cCookie_Domain, ::cCookie_Path, uhttpd_DateToGMT(,,, ::nCookie_LifeTime ), ::lCookie_Secure )
   ENDIF

   RETURN ::cSID

METHOD SaveCookie() CLASS uhttpd_Session

   LOCAL cExpires := uhttpd_DateToGMT( Date(), Time(),, ::nCookie_LifeTime )
   LOCAL cKey

   // oCGI:SetCookie( ::cName, ::cSID, ::cCookie_Domain, ::cCookie_Path, cExpires, ::lCookie_Secure )
   FOR EACH cKey IN _SESSION:Keys
      ::oCookie:SetCookie( ::cName + "_" + cKey, _SESSION[ cKey ], ::cCookie_Domain, ::cCookie_Path, cExpires, ::lCookie_Secure )
   NEXT

   RETURN NIL

#if 0
METHOD ReadCookie()

   oCGI:SetCookie( ::cName, ::cSID, ::cCookie_Domain, ::cCookie_Path, cExpires, ::lCookie_Secure )

   RETURN NIL
#endif

METHOD GetSessionVars( aHashVars, cFields, cSeparator ) CLASS uhttpd_Session

   LOCAL aNotSessionFlds := {}
   LOCAL aField, cField, aFields
   LOCAL cName, xValue
   LOCAL cSessPrefix := ::cName + "_"
   LOCAL cFieldsNotInSession := ""
   LOCAL cSessVarName

   __defaultNIL( @cSeparator, "&" )

   aFields := hb_regexSplit( cSeparator, cFields )

   FOR EACH cField in aFields
      aField := hb_regexSplit( "=", cField, 2 )
      IF Len( aField ) != 2
         LOOP
      ENDIF

      cSessVarName := LTrim( aField[ 1 ] )
      // cName  := "_" + LTrim( aField[ 1 ] )   // ERROR ON VAR NAME WITH LEN 1. X
      // cName  := LTrim( aField[ 1 ] )   // ERROR ON VAR NAME WITH LEN 1. X

      // TraceLog( "SESSION: cSessVarName, cSessPrefix, Left( cSessVarName, Len( cSessPrefix ) )", ;
      //                    cSessVarName, cSessPrefix, Left( cSessVarName, Len( cSessPrefix ) ) )

      IF Left( cSessVarName, Len( cSessPrefix ) ) == cSessPrefix // IF Left part of var is equal to session prefixname i.e. "SESSION"

         cName  := SubStr( cSessVarName, Len( cSessPrefix ) + 1 )
         xValue := uhttpd_UrlDecode( aField[ 2 ] )
         // TraceLog( "SESSION: cName, xValue", cName, xValue )

         // TraceLog( "cName, xValue", cName, xValue )

         // is it an array entry?
         IF SubStr( cName, Len( cName ) - 1 ) == "[]"
            cName := SubStr( cName, 1, Len( cName ) - 2 )
            // aHashVars[ cName ] := { xValue }

            aHashVars[ cName ] := { xValue }

            // aHashVars:Keys( cName )
            // __objSendMsg( aHashVars, "_" + cName, { xValue } )  // variant from Ron to handle 1 lenght name

         ELSE
            // aHashVars[ cName ] := xValue

            aHashVars[ cName ] := xValue

            // aHashVars:Keys( cName )
            // __objSendMsg( aHashVars, "_" + cName, xValue )  // variant from Ron to handle 1 lenght name
         ENDIF
         // TraceLog( "aHashVars, cName, xValue", DumpValue( aHashVars ), cName, xValue )
      ELSE
         AAdd( aNotSessionFlds, aField )
      ENDIF
   NEXT
   IF ! Empty( aNotSessionFlds )
      FOR EACH aField IN aNotSessionFlds
         cFieldsNotInSession += aField[ 1 ] + "=" + aField[ 2 ] + "&"
      NEXT
      // Delete last & char
      cFieldsNotInSession := Left( cFieldsNotInSession, Len( cFieldsNotInSession ) - 1 )
   ENDIF

   // TraceLog( "SESSION: cFieldsNotInSession", cFieldsNotInSession )

   RETURN cFieldsNotInSession



/*
 * SID = 25 random chars + 5 CRC chars
 */

METHOD GenerateSID( cCRCKey ) CLASS uhttpd_Session

   LOCAL cSID, nSIDCRC, cSIDCRC, n, cTemp
   LOCAL nLenSID     := 25
   LOCAL cBaseKeys   := "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
   LOCAL nLenKeys    := Len( cBaseKeys )
   LOCAL cRet
   LOCAL nRand, nKey := 0
   LOCAL nLenTemp

   // LOCAL a := 0

   // Max Lenght must to be 10
   // __defaultNIL( @cCRCKey, "3InFoW4lL5" )
   __defaultNIL( @cCRCKey, MY_CRCKEY )

   /* Let's generate the sequence */
   // cSID := Space( nLenSID )
   cSID := ""
   FOR n := 1 TO nLenSID - 5 // 5 = CRC Length
      nRand     := hb_RandomInt( 1, nLenKeys )
      // cSID[ n ] := cBaseKeys[ nRand ]
      cSID += SubStr( cBaseKeys, nRand, 1 )
      nKey += nRand
   NEXT

   nSIDCRC  := nKey * 51 // Max Value is 99603. a 5 chars number
   cTemp    := StrZero( nSIDCRC, 5 )
   cSIDCRC  := ""
   nLenTemp := Len( cTemp )
   FOR n := 1 TO nLenTemp
      // cSIDCRC += cCRCKey[ Val( cTemp[ n ] ) + 1 ]
      cSIDCRC += SubStr( cCRCKey, Val( SubStr( cTemp, n, 1 ) ) + 1, 1 )
      // ::oCGI:ToLogFile( "cCRCKey = " + hb_CStr( SubStr( cCRCKey, Val( SubStr( cTemp, n, 1 ) ) + 1, 1 ) ), "/pointtoit/tmp/log.txt" )
   NEXT

   cRet := cSID + cSIDCRC
   // ::oCGI:ToLogFile( "::GenerateSID() = " + hb_CStr( cSID ) + " " + hb_CStr( cSIDCRC ), "/pointtoit/tmp/log.txt" )

   // TraceLog( "Generate SID: cRet, cSID, nSIDCRC, cTemp, cSIDCRC, nKey, a", cRet, cSID, nSIDCRC, cTemp, cSIDCRC, nKey, a )

   RETURN cRet

METHOD CheckSID( cSID, cCRCKey ) CLASS uhttpd_Session

   LOCAL nSIDCRC, cSIDCRC, n, cTemp
   LOCAL nLenSID     := 25
   LOCAL cBaseKeys   := "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
   LOCAL nRand, nKey := 0
   LOCAL nLenTemp
   LOCAL lOk

   // LOCAL a := 0

   __defaultNIL( @::cSID, ::RegenerateID() )
   __defaultNIL( @cSID, ::cSID )
   // Max Lenght must to be 10
   __defaultNIL( @cCRCKey, MY_CRCKEY )

   // hb_ToOutDebug( "cSID = %s, ::cSID = %s\n\r", hb_ValToExp( cSID ), hb_ValToExp( ::cSID ) )

   IF ! Empty( cSID )

      /* Calculate the key */
      FOR n := 1 TO nLenSID - 5 // 5 = CRC Length
         // nRand     := At( cSID[ n ], cBaseKeys )
         nRand     := At( SubStr( cSID, n, 1 ), cBaseKeys )
         nKey += nRand
      NEXT

      // Recalculate the CRC
      nSIDCRC  := nKey * 51 // Max Value is 99603. a 5 chars number
      cTemp    := StrZero( nSIDCRC, 5 )
      cSIDCRC  := ""
      nLenTemp := Len( cTemp )
      FOR n := 1 TO nLenTemp
         // cSIDCRC += cCRCKey[ Val( cTemp[ n ] ) + 1 ]
         cSIDCRC += SubStr( cCRCKey, Val( SubStr( cTemp, n, 1 ) ) + 1, 1 )
      NEXT

      lOk := ( Right( cSID, 5 ) == cSIDCRC )

      // TraceLog( "Check SID: cRet, cSID, nSIDCRC, cTemp, cSIDCRC, nKey, a", cRet, cSID, nSIDCRC, cTemp, cSIDCRC, nKey, a )
      // ::oCGI:ToLogFile( "::CheckSID() = " + hb_CStr( cSID ) + " " + hb_CStr( cSIDCRC ), "/pointtoit/tmp/log.txt" )
   ENDIF

   RETURN lOk

// ------------------------------

METHOD SetSaveHandler( bOpen, bClose, bRead, bWrite, bDestroy, bGC ) CLASS uhttpd_Session

   IF bOpen != NIL
      ::bOpen := bOpen
   ENDIF
   IF bClose != NIL
      ::bClose := bClose
   ENDIF
   IF bRead != NIL
      ::bRead := bRead
   ENDIF
   IF bWrite != NIL
      ::bWrite := bWrite
   ENDIF
   IF bDestroy != NIL
      ::bDestroy := bDestroy
   ENDIF
   IF bGC != NIL
      ::bGC := bGC
   ENDIF

   RETURN NIL

METHOD SessionOpen( cPath, cName ) CLASS uhttpd_Session

   // TraceLog( "SessionOpen() - cName", cName )
   IF cPath != NIL
      ::cSavePath := cPath
   ENDIF
   IF cName != NIL
      ::cName := cName
   ENDIF

   RETURN .T.

METHOD SessionClose() CLASS uhttpd_Session

   // TraceLog( "SessionClose()" )
   // Nothing to do

   RETURN .T.

METHOD SessionRead( cID ) CLASS uhttpd_Session

   LOCAL nH
   LOCAL cFile
   LOCAL nFileSize
   LOCAL cBuffer
   LOCAL nRetry  := 0

   __defaultNIL( @cID, ::cSID )
   cFile := ::cSavePath + hb_ps() + ::cName + "_" + cID
   // TraceLog( "SessionRead: cFile", cFile )
   IF hb_FileExists( cFile )
      DO WHILE nRetry++ <= ::nFileRetry
         IF ( nH := FOpen( cFile, FO_READ + FO_DENYWRITE ) ) != F_ERROR

            nRetry := 0
            DO WHILE nRetry++ <= ::nFileRetry
               nFileSize := FSeek( nH, 0, FS_END )
               FSeek( nH, 0, FS_SET )
               cBuffer := Space( nFileSize )
               IF FRead( nH, @cBuffer,  nFileSize ) != nFileSize
                  // uhttpd_Die( "ERROR: On reading session file : " + cFile + ", File error : " + hb_CStr( FError() ) )
                  hb_idleSleep( ::nFileWait / 1000 )
                  LOOP
               ENDIF
               FClose( nH )
               EXIT
            ENDDO

         ELSE
            // uhttpd_Die( "ERROR: On opening session file : " + cFile + ", File error : " + hb_CStr( FError() ) )
            hb_idleSleep( ::nFileWait / 1000 )
            LOOP
         ENDIF
         EXIT
      ENDDO
   ENDIF
   // TraceLog( "SessionRead() - cID, cFile, nFileSize, cBuffer", cID, cFile, nFileSize, cBuffer )

   RETURN cBuffer

METHOD SessionWrite( cID, cData ) CLASS uhttpd_Session

   LOCAL nH
   LOCAL cFile
   LOCAL nFileSize
   LOCAL lOk := .F.
   LOCAL nRetry  := 0

   // TraceLog( "SessionWrite() - cID, cData", cID, cData )
   __defaultNIL( @cID, ::cSID )
   __defaultNIL( @cData, "" )

   nFileSize := Len( cData )

   cFile := ::cSavePath + hb_ps() + ::cName + "_" + cID
   // TraceLog( "SessionWrite() - cFile", cFile )
   IF nFileSize > 0
      DO WHILE nRetry++ <= ::nFileRetry
         IF ( nH := hb_FCreate( cFile, FC_NORMAL, FO_READWRITE + FO_DENYWRITE ) ) != F_ERROR
            IF FWrite( nH, @cData,  nFileSize ) != nFileSize
               uhttpd_Die( "ERROR: On writing session file : " + cFile + ", File error : " + hb_CStr( FError() ) )
            ELSE
               lOk := .T.
            ENDIF
            FClose( nH )
         ELSE
            // uhttpd_Die( "ERROR: On WRITING session file. I can not create session file : " + cFile + ", File error : " + hb_CStr( FError() ) )
            hb_idleSleep( ::nFileWait / 1000 )
            LOOP
         ENDIF
         EXIT
      ENDDO
   ELSE
      // If session data is empty, I will delete the file if exist
      // IF hb_FileExists( cFile )
      //   FErase( cFile )
      // ENDIF
      // Return that all is ok
      lOk := .T.
   ENDIF

   RETURN lOk

METHOD SessionDestroy( cID ) CLASS uhttpd_Session

   LOCAL cFile
   LOCAL lOk
   LOCAL nRetry  := 0

   // TraceLog( "SessionDestroy() - cID", cID )
   __defaultNIL( @cID, ::cSID )

   _SESSION := { => }
   ::oCookie:DeleteCookie( ::cName )

   // TraceLog( "SessionDestroy() - cID, oCGI:h_Session", cID, DumpValue( oCGI:h_Session ) )
   cFile := ::cSavePath + hb_ps() + ::cName + "_" + cID

   lOk := .F.
   DO WHILE nRetry++ <= ::nFileRetry
      IF ( lOk := ( FErase( cFile ) == 0 ) )
         EXIT
      ELSE
         hb_idleSleep( ::nFileWait / 1000 )
         LOOP
      ENDIF
   ENDDO

#if 0
   IF !( lOk := ( FErase( cFile ) == 0 ) )
      uhttpd_Die( "ERROR: On deleting session file : " + cFile + ", File error : " + hb_CStr( FError() ) )
   ELSE
#endif

   IF lOk
      // TraceLog( "SessionDestroy() - Sessione Eliminata - File " + cFile )
      // Genero un nuovo SID
      ::RegenerateID()
   ENDIF

   RETURN lOk

METHOD SessionGC( nMaxLifeTime ) CLASS uhttpd_Session

   // TraceLog( "SessionGC() - nMaxLifeTime", nMaxLifeTime )
   // STATIC s_nStartTime
   LOCAL nSecs
   LOCAL aDir, aFile

   __defaultNIL( @nMaxLifeTime, ::nGc_MaxLifeTime )
   aDir := Directory( ::cSavePath + hb_ps() + ::cName + "_*.*" )

   FOR EACH aFile IN aDir
      nSecs := TimeDiffAsSeconds( aFile[ F_DATE ], Date(), aFile[ F_TIME ], Time() )
      // TraceLog( "GC: aFile[ F_NAME ], aFile[ F_DATE ], Date(), aFile[ F_TIME ], Time(), nSecs, nMaxLifeTime", ;
      //                aFile[ F_NAME ], aFile[ F_DATE ], Date(), aFile[ F_TIME ], Time(), nSecs, nMaxLifeTime )
      IF nSecs > nMaxLifeTime
         // No error checking here, because if I cannot delete file now I will find it again on next loop
         FErase( ::cSavePath + hb_ps() + aFile[ F_NAME ] )
      ENDIF
   NEXT

   RETURN .T.

STATIC FUNCTION TimeDiffAsSeconds( dDateStart, dDateEnd, cTimeStart, cTimeEnd )

   LOCAL aRetVal

   __defaultNIL( @dDateEnd, Date() )
   __defaultNIL( @cTimeEnd, Time() )

   aRetVal := ft_Elapsed( dDateStart, dDateEnd, cTimeStart, cTimeEnd )

   RETURN aRetVal[ 4, 2 ]

// ------------------------------

METHOD Encode() CLASS uhttpd_Session

   LOCAL aSerial := {}
   LOCAL cKey, xVal

   IF Type( "_SESSION" ) == "H"

      FOR EACH cKey IN _SESSION:Keys
         xVal := _SESSION[ cKey ]
         IF xVal != NIL
            AAdd( aSerial, { cKey, xVal } )
         ENDIF
      NEXT

   ENDIF

   RETURN iif( ! Empty( aSerial ), hb_Serialize( aSerial ), NIL )

METHOD Decode( cData ) CLASS uhttpd_Session

   LOCAL lOk := .T.
   LOCAL cSerial := cData
   LOCAL xVal, aElem

   // LOCAL cKey

   // TraceLog( "Decode - cSerial", cSerial )
   // ::oCGI:ToLogFile( "Decode - cSerial = " + hb_CStr( cSerial ), "/pointtoit/tmp/log.txt" )

   DO WHILE ( xVal := hb_Deserialize( @cSerial ) ) != NIL
      // TraceLog( "Decode - xVal", DumpValue( xVal ) )
      // ::oCGI:ToLogFile( "Decode - xVal = " + hb_CStr( xVal ) + ", ValType( xVal ) = " + ValType( xVal ), "/pointtoit/tmp/log.txt" )

      SWITCH ValType( xVal )
#if 0
      CASE "O"
         // TraceLog( "Decode - xVal - Object", xVal )
         IF xVal:classname == "TASSOCIATIVEARRAY"
            // TraceLog( "Decode - xVal - Object - TAssociativeArray - Keys", xVal:Keys )
            FOR EACH cKey IN xVal:Keys
               // TraceLog( "Decode TassociativeArray - cKey, xVal:SendKey( cKey )", cKey, xVal:SendKey( cKey ) )
               _SESSION:SendKey( cKey, xVal:SendKey( cKey ) )
            NEXT
         ENDIF
         EXIT
#endif

      CASE "A"  // Le variabili sono conservate come array { VarName, Value }
         // TraceLog( "Decode - xVal - Array", xVal )
         // ::oCGI:ToLogFile( "Decode - xVal - Array = " + hb_CStr( xVal ) + ", Len = " + hb_CStr( Len( xVal ) ), "/pointtoit/tmp/log.txt" )
         FOR EACH aElem IN xVal
            // ::oCGI:ToLogFile( "Decode - aElem = " + hb_CStr( hb_ValToExp( aElem ) ), "/pointtoit/tmp/log.txt" )
            _SESSION[ aElem[ 1 ] ] := aElem[ 2 ]
         NEXT
         EXIT

      OTHERWISE
         uhttpd_Die( "ERROR: On deserializing session data" )
         lOk := .F.
         EXIT
      ENDSWITCH
   ENDDO

   RETURN lOk

METHOD SendCacheLimiter() CLASS uhttpd_Session

   LOCAL dDate

   DO CASE
   CASE ::cCache_Limiter == "nocache"
      // uhttpd_SetHeader( "Expires", "Thu, 19 Nov 1981 08:52:00 GMT" )
      uhttpd_SetHeader( "Expires", uhttpd_DateToGMT( ,, -1, ) )
      uhttpd_SetHeader( "Cache-Control", "no-cache" )
      // uhttpd_SetHeader( "Cache-Control", "no-store, no-cache, must-revalidate" )  // HTTP/1.1
      // uhttpd_SetHeader( "Cache-Control", "post-check=0, pre-check=0", .F. )
      uhttpd_SetHeader( "Pragma", "no-cache" )
   CASE ::cCache_Limiter == "private"
      uhttpd_SetHeader( "Expires", "Thu, 19 Nov 1981 08:52:00 GMT" )
      uhttpd_SetHeader( "Cache-Control", "private, max-age=" + hb_ntos( ::nCache_Expire * 60 ) )
      IF hb_FGetDateTime( hb_argv( 0 ), @dDate )
         uhttpd_SetHeader( "Last-Modified", uhttpd_DateToGMT( dDate ) )
      ENDIF
   CASE ::cCache_Limiter == "public"
      uhttpd_SetHeader( "Expires", uhttpd_DateToGMT( ,,, ::nCache_Expire * 60 ) )
      uhttpd_SetHeader( "Cache-Control", "public, max-age=" + hb_ntos( ::nCache_Expire * 60 ) )
      IF hb_FGetDateTime( hb_argv( 0 ), @dDate )
         uhttpd_SetHeader( "Last-Modified", uhttpd_DateToGMT( dDate ) )
      ENDIF
   OTHERWISE
      uhttpd_Die( "ERROR: Caching method " + ::cCache_Limiter + " not implemented." )
   ENDCASE
   // __OutDebug( "Header cache '" + ::cCache_Limiter + "' inviato" )

   RETURN NIL

PROCEDURE DestroyObject() CLASS uhttpd_Session

   ::Close()
   // ::oCGI:ToLogFile( "Session destroyed" )
   // ::oCGI := NIL

   RETURN
