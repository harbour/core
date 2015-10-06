/*
 * uHTTPD (Micro HTTP server) session functions
 *
 * Copyright 2009 Francesco Saverio Giudice <info / at / fsgiudice.com>
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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


MEMVAR _COOKIE, _SESSION, _REQUEST, _SERVER

#define MY_CRCKEY "UhTTpK3y76"

FUNCTION uhttpd_SessionNew( cSessionName, cSessionPath )
   RETURN uhttpd_Session():New( cSessionName, cSessionPath )

CLASS uhttpd_Session

   METHOD New( cSessionName, cSessionPath )

   DESTRUCTOR DestroyObject()

   METHOD Start( cSID )
   METHOD IsRegistered()
   METHOD CacheExpire( nTimeInMinutes ) INLINE SetNewValueReturnOld( ::nCache_Expire, nTimeInMinutes )
   METHOD CacheLimiter( cNewLimiter )
   METHOD GetCookieParams()     INLINE { ::nCookie_LifeTime, ::cCookie_Path, ::cCookie_Domain, ::lCookie_Secure  }
   METHOD SetCookieParams( nLifeTime, cPath, cDomain, lSecure  )
   METHOD ID( cID )             INLINE SetNewValueReturnOld( ::cSID, cID )
   METHOD Name( cName )         INLINE SetNewValueReturnOld( ::cName, cName )
   METHOD RegenerateID()
   METHOD SavePath( cPath )     INLINE SetNewValueReturnOld( ::cSavePath, cPath )
   METHOD IsStarted()           INLINE ( ::nActiveSessions > 0 )
   METHOD UseOnlyCookies()      INLINE ::lUse_Only_Cookies
   METHOD UseTransSID()         INLINE ::lUse_Only_Cookies

   METHOD SaveCookie()
   METHOD GetSessionVars( aHashVars, cFields, cSeparator )
   METHOD GetVar( cVar )        INLINE uhttpd_HGetValue( _SESSION, cVar )
   METHOD SetVar( cVar, xValue ) INLINE _SESSION[ cVar ] := xValue

   METHOD SetSaveHandler( bOpen, bClose, bRead, bWrite, bDestroy, bGC )
   METHOD Open( cPath, cName )
   METHOD Close()
   METHOD Read( cID )
   METHOD Write( cID, cData )
   METHOD Destroy()
   METHOD GC( nMaxLifeTime )

   METHOD SessionContainer( hHash )  INLINE SetNewValueReturnOld( _SESSION, hHash )
   METHOD Encode()
   METHOD Decode( cData )

   HIDDEN:

   VAR oCookie
   VAR cSID
   VAR cSavePath               INIT "/tmp"
   VAR cName                   // INIT "SESSIONID"
   VAR lAuto_Start             INIT .F.       // .F.: no autostart
   VAR nGc_Probability         INIT 33        // Every 1/3 of checks i'll lunch Session GC
   VAR nGc_MaxLifeTime         INIT 1440      // seconds - Number of seconds after gc can delete a session
   // VAR cSerialize_Handler      INIT "HBHTMLLIB"
   VAR nCookie_LifeTime        INIT 3600 // 0         // Number of seconds to keep cookie, 0: until browser is closed
   VAR cCookie_Path            INIT "/"
   VAR cCookie_Domain
   VAR lCookie_Secure          INIT .F.
   VAR lUse_Cookies            INIT .T.       // .T.: Use cookies to store session id on client side
   VAR lUse_Only_Cookies       INIT .F.
   VAR cReferrer_Check                        // If is set check if referrer is equal to, if it isn't block
   // VAR cEntropy_File
   // VAR nEntropy_Length
   VAR cCache_Limiter          INIT "nocache" // Possible values are: none, nocache, private, private_no_expire, public
   VAR nCache_Expire           INIT 180       // in minutes, not checked if cCache_Limiter == none or nocache
   VAR lUse_Trans_SID          INIT .F.       // .F.: no SID appended to URL

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

   METHOD GenerateSID( cCRCKey )
   METHOD CheckSID( cSID, cCRCKey )
   METHOD SessionOpen( cPath, cName )
   METHOD SessionClose()
   METHOD SessionRead( cID )
   METHOD SessionWrite( cID, cData )
   METHOD SessionDestroy( cID )
   METHOD SessionGC( nMaxLifeTime )

   METHOD SendCacheLimiter()

ENDCLASS

// ---

METHOD New( cSessionName, cSessionPath ) CLASS uhttpd_Session

   // hb_ToOutDebug( "cSessionName: %s, cSessionPath: %s\n\r", cSessionName, cSessionPath )

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

   ::cName     := hb_defaultValue( cSessionName, "SESSION" ) + "ID"
   ::cReferrer_Check := _SERVER[ "HTTP_REFERER" ]

   ::cSavePath := hb_defaultValue( cSessionPath, ::cSavePath )

   ::oCookie   := uhttpd_CookieNew( ::cCookie_Domain, ::cCookie_Path )

   RETURN Self

METHOD Start( cSID ) CLASS uhttpd_Session

   LOCAL lSendCookie := .T.
   LOCAL lDefine_SID := .T.
   LOCAL xVal, nRand
   LOCAL hUrl

   IF HB_ISSTRING( cSID )
      ::cSID := cSID
   ENDIF

   // hb_ToOutDebug( "cSID: %s, ::cSID: %s\n\r", cSID, ::cSID )

   // TraceLog( "Active Sessions: " + hb_CStr( ::nActiveSessions ) )

   IF ::nActiveSessions != 0
      RETURN .F.
   ENDIF

   // Start checking ID from global vars
   IF ::cName $ _REQUEST
      ::cSID := _REQUEST[ ::cName ]
      IF HB_ISARRAY( ::cSID )
         ::cSID := ::cSID[ 1 ] // Get Only 1-st
      ENDIF
      lSendCookie := .F.
      lDefine_SID := .F.
      // ::oCGI:ToLogFile( "::cSID: " + hb_CStr( ::cSID ), "/pointtoit/tmp/log.txt" )
   ENDIF

   IF ! Empty( ::cSID ) .AND. ! ::CheckSID()
      // Check if the SID is NOT valid, someone altered it
      // ::oCGI:ToLogFile( "::cSID: " + hb_CStr( ::cSID ) + " SID is NOT valid, someone altered it", "/pointtoit/tmp/log.txt" )
      ::cSID      := NIL   // invalidate current SID, i'll generate a new one
      lSendCookie := .T.
      lDefine_SID := .T.
   ENDIF

   IF ! Empty( ::cSID ) .AND. ! Empty( ::cReferrer_Check )
      // TODO: fix

      // oUrl := TUrl():New( ::cReferrer_Check )
      hUrl := uhttpd_SplitUrl( ::cReferrer_Check )

      // hb_ToOutDebug( "hUrl: %s\n\r", hb_ValToExp( hUrl ) )

      // Check whether the current request was referred to by
      // an external site which invalidates the previously found ID

      IF !( hUrl[ "HOST" ] == _SERVER[ "SERVER_NAME" ] )
         ::cSID      := NIL   // invalidate current SID, i'll generate a new one
         lSendCookie := .T.
         lDefine_SID := .T.
      ENDIF
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
      ::oCookie:SetCookie( ::cName, ::cSID, ::cCookie_Domain, ::cCookie_Path, uhttpd_DateToGMT( ,, ::nCookie_LifeTime ), ::lCookie_Secure )
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
      // ::oCGI:ToLogFile( "xval: " + hb_CStr( xVal ), "/pointtoit/tmp/log.txt" )
      // Decode session data
      ::Decode( xVal )
      // ::oCGI:ToLogFile( "decoded", "/pointtoit/tmp/log.txt" )
   ENDIF

   // Send HTTP cache headers
   ::SendCacheLimiter()

   // Check if we should clean up (call the garbage collection routines)
   // TraceLog( "::nGc_probability: " + hb_CStr( ::nGc_probability ) )
   IF ::nGc_probability > 0
      nRand := hb_randInt( 100 )
      // TraceLog( "::nGc_probability - nRand: " + hb_CStr( nRand ) )
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

   IF HB_ISSTRING( cNewLimiter )
      IF cNewLimiter $ "none/nocache/private/private_no_expire/public"
         ::cCache_Limiter := cNewLimiter
      ELSE
         uhttpd_Die( "ERROR: uhttpd_Session:CacheLimiter() - New Limiter is incorrect" )
      ENDIF
   ENDIF

   RETURN cOldLimiter

METHOD PROCEDURE SetCookieParams( nLifeTime, cPath, cDomain, lSecure ) CLASS uhttpd_Session

   IF HB_ISNUMERIC( nLifeTime )
      ::nCookie_LifeTime := nLifeTime
   ENDIF
   IF HB_ISSTRING( cPath )
      ::cCookie_Path     := cPath
   ENDIF
   IF HB_ISSTRING( cDomain )
      ::cCookie_Domain   := cDomain
   ENDIF
   IF HB_ISLOGICAL( lSecure )
      ::lCookie_Secure   := lSecure
   ENDIF

   RETURN

METHOD RegenerateID() CLASS uhttpd_Session

   ::cSID := ::GenerateSID()
   IF ::lUse_Cookies
      ::oCookie:SetCookie( ::cName, ::cSID, ::cCookie_Domain, ::cCookie_Path, uhttpd_DateToGMT( ,, ::nCookie_LifeTime ), ::lCookie_Secure )
   ENDIF

   RETURN ::cSID

METHOD PROCEDURE SaveCookie() CLASS uhttpd_Session

   LOCAL cExpires := uhttpd_DateToGMT( ,, ::nCookie_LifeTime )
   LOCAL cKey

   // oCGI:SetCookie( ::cName, ::cSID, ::cCookie_Domain, ::cCookie_Path, cExpires, ::lCookie_Secure )
   FOR EACH cKey IN _SESSION:Keys
      ::oCookie:SetCookie( ::cName + "_" + cKey, _SESSION[ cKey ], ::cCookie_Domain, ::cCookie_Path, cExpires, ::lCookie_Secure )
   NEXT

   RETURN

#if 0
METHOD PROCEDURE ReadCookie() CLASS uhttpd_Session

   oCGI:SetCookie( ::cName, ::cSID, ::cCookie_Domain, ::cCookie_Path, cExpires, ::lCookie_Secure )

   RETURN
#endif

METHOD GetSessionVars( aHashVars, cFields, cSeparator ) CLASS uhttpd_Session

   LOCAL aNotSessionFlds := {}
   LOCAL aField, cField
   LOCAL cName, xValue
   LOCAL cSessPrefix := ::cName + "_"
   LOCAL cFieldsNotInSession := ""
   LOCAL cSessVarName

   FOR EACH cField IN hb_regexSplit( hb_defaultValue( cSeparator, "&" ), cFields )

      IF Len( aField := hb_regexSplit( "=", cField, 2 ) ) != 2
         LOOP
      ENDIF

      cSessVarName := LTrim( aField[ 1 ] )
      // cName  := "_" + LTrim( aField[ 1 ] )   // ERROR ON VAR NAME WITH LEN 1. X
      // cName  := LTrim( aField[ 1 ] )   // ERROR ON VAR NAME WITH LEN 1. X

      // TraceLog( "SESSION: cSessVarName, cSessPrefix, Left( cSessVarName, Len( cSessPrefix ) )", ;
      //                     cSessVarName, cSessPrefix, Left( cSessVarName, Len( cSessPrefix ) ) )

      IF hb_LeftEq( cSessVarName, cSessPrefix )  // If left part of var is equal to session prefixname i.e. "SESSION"

         cName  := SubStr( cSessVarName, Len( cSessPrefix ) + 1 )
         xValue := tip_URLDecode( aField[ 2 ] )
         // TraceLog( "SESSION: cName, xValue", cName, xValue )

         // TraceLog( "cName, xValue", cName, xValue )

         // is it an array entry?
         IF Right( cName, 2 ) == "[]"
            cName := hb_StrShrink( cName, 2 )
            // aHashVars[ cName ] := { xValue }

            aHashVars[ cName ] := { xValue }

            // aHashVars:Keys( cName )
            // __objSendMsg( aHashVars, "_" + cName, { xValue } )  // variant from Ron to handle 1 length name

         ELSE
            // aHashVars[ cName ] := xValue

            aHashVars[ cName ] := xValue

            // aHashVars:Keys( cName )
            // __objSendMsg( aHashVars, "_" + cName, xValue )  // variant from Ron to handle 1 length name
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
      // Delete last '&' char
      cFieldsNotInSession := hb_StrShrink( cFieldsNotInSession )
   ENDIF

   // TraceLog( "SESSION: cFieldsNotInSession", cFieldsNotInSession )

   RETURN cFieldsNotInSession


/* SID == 25 random chars + 5 CRC chars */
METHOD GenerateSID( cCRCKey ) CLASS uhttpd_Session
   RETURN tip_GenerateSID( hb_defaultValue( cCRCKey, MY_CRCKEY ) )

METHOD CheckSID( cSID, cCRCKey ) CLASS uhttpd_Session

   IF ::cSID == NIL
      ::cSID := ::RegenerateID()
   ENDIF

   RETURN tip_CheckSID( hb_defaultValue( cSID, ::cSID ), hb_defaultValue( cCRCKey, MY_CRCKEY ) )

// ---

METHOD PROCEDURE SetSaveHandler( bOpen, bClose, bRead, bWrite, bDestroy, bGC ) CLASS uhttpd_Session

   IF HB_ISEVALITEM( bOpen )
      ::bOpen := bOpen
   ENDIF
   IF HB_ISEVALITEM( bClose )
      ::bClose := bClose
   ENDIF
   IF HB_ISEVALITEM( bRead )
      ::bRead := bRead
   ENDIF
   IF HB_ISEVALITEM( bWrite )
      ::bWrite := bWrite
   ENDIF
   IF HB_ISEVALITEM( bDestroy )
      ::bDestroy := bDestroy
   ENDIF
   IF HB_ISEVALITEM( bGC )
      ::bGC := bGC
   ENDIF

   RETURN

METHOD SessionOpen( cPath, cName ) CLASS uhttpd_Session

   // TraceLog( "SessionOpen() - cName", cName )
   IF HB_ISSTRING( cPath )
      ::cSavePath := cPath
   ENDIF
   IF HB_ISSTRING( cName )
      ::cName := cName
   ENDIF

   RETURN .T.

METHOD SessionClose() CLASS uhttpd_Session

   // TraceLog( "SessionClose()" )
   // Nothing to do

   RETURN .T.

METHOD SessionRead( cID ) CLASS uhttpd_Session

   LOCAL hFile
   LOCAL cFile
   LOCAL nFileSize
   LOCAL cBuffer
   LOCAL nRetry  := 0

   cFile := ::cSavePath + hb_ps() + ::cName + "_" + hb_defaultValue( cID, ::cSID )
   // TraceLog( "SessionRead: cFile", cFile )
   IF hb_vfExists( cFile )
      DO WHILE nRetry++ <= ::nFileRetry
         IF ( hFile := hb_vfOpen( cFile, FO_READ + FO_DENYWRITE ) ) != NIL

            nRetry := 0
            DO WHILE nRetry++ <= ::nFileRetry
               nFileSize := hb_vfSize( hFile )
               hb_vfSeek( hFile, 0, FS_SET )
               cBuffer := Space( nFileSize )
               IF hb_vfRead( hFile, @cBuffer, nFileSize ) != nFileSize
                  // uhttpd_Die( "ERROR: On reading session file: " + cFile + ", File error: " + hb_CStr( FError() ) )
                  hb_idleSleep( ::nFileWait / 1000 )
                  LOOP
               ENDIF
               hb_vfClose( hFile )
               EXIT
            ENDDO
         ELSE
            // uhttpd_Die( "ERROR: On opening session file: " + cFile + ", File error: " + hb_CStr( FError() ) )
            hb_idleSleep( ::nFileWait / 1000 )
            LOOP
         ENDIF
         EXIT
      ENDDO
   ENDIF
   // TraceLog( "SessionRead() - cID, cFile, nFileSize, cBuffer", cID, cFile, nFileSize, cBuffer )

   RETURN cBuffer

METHOD SessionWrite( cID, cData ) CLASS uhttpd_Session

   LOCAL hFile
   LOCAL cFile
   LOCAL lOk := .F.
   LOCAL nRetry := 0

   // TraceLog( "SessionWrite() - cID, cData", cID, cData )
   hb_default( @cData, "" )

   cFile := ::cSavePath + hb_ps() + ::cName + "_" + hb_defaultValue( cID, ::cSID )
   // TraceLog( "SessionWrite() - cFile", cFile )
   IF HB_ISSTRING( cData ) .AND. ! HB_ISNULL( cData )
      DO WHILE nRetry++ <= ::nFileRetry
         IF ( hFile := hb_vfOpen( cFile, FO_CREAT + FO_TRUNC + FO_WRITE + FO_DENYWRITE ) ) != NIL
            IF hb_vfWrite( hFile, cData ) != hb_BLen( cData )
               uhttpd_Die( "ERROR: On writing session file: " + cFile + ", File error: " + hb_CStr( FError() ) )
            ELSE
               lOk := .T.
            ENDIF
            hb_vfClose( hFile )
         ELSE
            // uhttpd_Die( "ERROR: On WRITING session file. I can not create session file: " + cFile + ", File error: " + hb_CStr( FError() ) )
            hb_idleSleep( ::nFileWait / 1000 )
            LOOP
         ENDIF
         EXIT
      ENDDO
   ELSE
      // If session data is empty, I will delete the file if exist
#if 0
      IF hb_vfExists( cFile )
         hb_vfErase( cFile )
      ENDIF
#endif
      // Return that all is ok
      lOk := .T.
   ENDIF

   RETURN lOk

METHOD SessionDestroy( cID ) CLASS uhttpd_Session

   LOCAL cFile
   LOCAL lOk
   LOCAL nRetry := 0

   // TraceLog( "SessionDestroy() - cID", cID )

   _SESSION := { => }
   ::oCookie:DeleteCookie( ::cName )

   // TraceLog( "SessionDestroy() - cID, oCGI:h_Session", cID, DumpValue( oCGI:h_Session ) )
   cFile := ::cSavePath + hb_ps() + ::cName + "_" + hb_defaultValue( cID, ::cSID )

   lOk := .F.
   DO WHILE nRetry++ <= ::nFileRetry
      IF ( lOk := ( hb_vfErase( cFile ) != F_ERROR ) )
         EXIT
      ELSE
         hb_idleSleep( ::nFileWait / 1000 )
         LOOP
      ENDIF
   ENDDO

#if 0
   IF !( lOk := ( hb_vfErase( cFile ) != F_ERROR ) )
      uhttpd_Die( "ERROR: On deleting session file: " + cFile + ", File error: " + hb_CStr( FError() ) )
   ELSE
#endif

   IF lOk
      // TraceLog( "SessionDestroy() - Sessione Eliminata - File " + cFile )
      // Genero un nuovo SID
      ::RegenerateID()
   ENDIF

   RETURN lOk

METHOD SessionGC( nMaxLifeTime ) CLASS uhttpd_Session

   LOCAL aFile

   // STATIC s_nStartTime
   // TraceLog( "SessionGC() - nMaxLifeTime", nMaxLifeTime )

   hb_default( @nMaxLifeTime, ::nGc_MaxLifeTime )

   FOR EACH aFile IN hb_vfDirectory( ::cSavePath + hb_ps() + ::cName + "_*.*" )
      IF ( ( hb_DateTime() - aFile[ F_DATE ] ) * 86400 ) > nMaxLifeTime
         // No error checking here, because if I cannot delete file now I will find it again on next loop
         hb_vfErase( ::cSavePath + hb_ps() + aFile[ F_NAME ] )
      ENDIF
   NEXT

   RETURN .T.

// ---

METHOD Encode() CLASS uhttpd_Session

   LOCAL aSerial := {}
   LOCAL cKey, xVal

   IF Type( "_SESSION" ) == "H"
      FOR EACH cKey IN _SESSION:Keys
         IF ( xVal := _SESSION[ cKey ] ) != NIL
            AAdd( aSerial, { cKey, xVal } )
         ENDIF
      NEXT
   ENDIF

   RETURN iif( Empty( aSerial ),, hb_Serialize( aSerial ) )

METHOD Decode( cData ) CLASS uhttpd_Session

   LOCAL lOk := .T.
   LOCAL cSerial := cData
   LOCAL xVal, aElem

#if 0
   LOCAL cKey
#endif

   // TraceLog( "Decode - cSerial", cSerial )
   // ::oCGI:ToLogFile( "Decode - cSerial: " + hb_CStr( cSerial ), "/pointtoit/tmp/log.txt" )

   DO WHILE ( xVal := hb_Deserialize( @cSerial ) ) != NIL
      // TraceLog( "Decode - xVal", DumpValue( xVal ) )
      // ::oCGI:ToLogFile( "Decode - xVal: " + hb_CStr( xVal ) + ", ValType( xVal ): " + ValType( xVal ), "/pointtoit/tmp/log.txt" )

      SWITCH ValType( xVal )
#if 0
      CASE "O"
         // TraceLog( "Decode - xVal - Object", xVal )
         IF xVal:className() == "TASSOCIATIVEARRAY"
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
         // ::oCGI:ToLogFile( "Decode - xVal - Array: " + hb_CStr( xVal ) + ", Len: " + hb_CStr( Len( xVal ) ), "/pointtoit/tmp/log.txt" )
         FOR EACH aElem IN xVal
            // ::oCGI:ToLogFile( "Decode - aElem: " + hb_CStr( hb_ValToExp( aElem ) ), "/pointtoit/tmp/log.txt" )
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

METHOD PROCEDURE SendCacheLimiter() CLASS uhttpd_Session

   LOCAL dDate

   SWITCH ::cCache_Limiter
   CASE "nocache"
#if 0
      uhttpd_SetHeader( "Expires", "Thu, 19 Nov 1981 08:52:00 GMT" )
#endif
      uhttpd_SetHeader( "Expires", uhttpd_DateToGMT( , -1 ) )
      uhttpd_SetHeader( "Cache-Control", "no-cache" )
#if 0
      uhttpd_SetHeader( "Cache-Control", "no-store, no-cache, must-revalidate" )  // HTTP/1.1
      uhttpd_SetHeader( "Cache-Control", "post-check=0, pre-check=0", .F. )
#endif
      uhttpd_SetHeader( "Pragma", "no-cache" )
      EXIT
   CASE "private"
      uhttpd_SetHeader( "Expires", "Thu, 19 Nov 1981 08:52:00 GMT" )
      uhttpd_SetHeader( "Cache-Control", "private, max-age=" + hb_ntos( ::nCache_Expire * 60 ) )
      IF hb_vfTimeGet( hb_ProgName(), @dDate )
         uhttpd_SetHeader( "Last-Modified", uhttpd_DateToGMT( dDate ) )
      ENDIF
      EXIT
   CASE "public"
      uhttpd_SetHeader( "Expires", uhttpd_DateToGMT( ,, ::nCache_Expire * 60 ) )
      uhttpd_SetHeader( "Cache-Control", "public, max-age=" + hb_ntos( ::nCache_Expire * 60 ) )
      IF hb_vfTimeGet( hb_ProgName(), @dDate )
         uhttpd_SetHeader( "Last-Modified", uhttpd_DateToGMT( dDate ) )
      ENDIF
      EXIT
   OTHERWISE
      uhttpd_Die( "ERROR: Caching method " + ::cCache_Limiter + " not implemented." )
   ENDSWITCH
   // __OutDebug( "Header cache '" + ::cCache_Limiter + "' inviato" )

   RETURN

PROCEDURE DestroyObject() CLASS uhttpd_Session

   ::Close()
#if 0
   ::oCGI:ToLogFile( "Session destroyed" )
   ::oCGI := NIL
#endif

   RETURN
