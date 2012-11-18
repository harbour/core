/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Versatile logging system.
 *
 * Copyright 2003 Giancarlo Niccolai [gian@niccolai.ws]
 * www - http://www.xharbour.org
 *
 * this program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * this program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE.  See the
 * GNU General public License for more details.
 *
 * You should have received a copy of the GNU General public License
 * along with this software; see the file COPYING.  if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, xHarbour license gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * this exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General public License.
 *
 * this exception applies only to the code released with this xHarbour
 * explicit exception.  if you add/copy code from other sources,
 * as the General public License permits, the above exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * if you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * if you do not wish that, delete this exception notice.
 *
 */

#include "hbclass.ch"
#include "hblog.ch"
#include "fileio.ch"


/*****
 * Static standard logger access
 */

#define HB_THREAD_SUPPORT

#ifdef HB_THREAD_SUPPORT
STATIC s_StdLogMutex := hb_mutexCreate()
#endif

STATIC s_StdLogger

PROCEDURE hb_InitStandardLog( ... )

   LOCAL PARAM

   s_StdLogger := HB_Logger():New()

   FOR EACH PARAM in hb_AParams()
#ifdef HB_THREAD_SUPPORT
      hb_mutexLock( s_StdLogMutex )
#endif

      s_StdLogger:AddChannel( Param )

#ifdef HB_THREAD_SUPPORT
      hb_mutexUnlock( s_StdLogMutex )
#endif
   NEXT

#ifdef HB_THREAD_SUPPORT
   hb_mutexLock( s_StdLogMutex )
#endif

   s_StdLogger:SetStyle( HB_LOG_ST_DATE + HB_LOG_ST_ISODATE + HB_LOG_ST_TIME + HB_LOG_ST_LEVEL )

#ifdef HB_THREAD_SUPPORT
   hb_mutexUnlock( s_StdLogMutex )
#endif

   RETURN

PROCEDURE hb_OpenStandardLog()

   s_StdLogger:Open()

   RETURN

PROCEDURE hb_StandardLogAdd( oChannel )

   IF s_StdLogger != NIL
#ifdef HB_THREAD_SUPPORT
      hb_mutexLock( s_StdLogMutex )
#endif

      s_StdLogger:AddChannel( oChannel )

#ifdef HB_THREAD_SUPPORT
      hb_mutexUnlock( s_StdLogMutex )
#endif
   ENDIF

   RETURN

PROCEDURE hb_CloseStandardLog()

   // If the logger is NIL also the mutex is NIL
   IF s_StdLogger != NIL
#ifdef HB_THREAD_SUPPORT
      hb_mutexLock( s_StdLogMutex )
#endif

      s_StdLogger:Close()

#ifdef HB_THREAD_SUPPORT
      hb_mutexUnlock( s_StdLogMutex )
#endif
   ENDIF

   RETURN

PROCEDURE hb_SetStandardLogStyle( nStyle )

   IF s_StdLogger != NIL
#ifdef HB_THREAD_SUPPORT
      hb_mutexLock( s_StdLogMutex )
#endif

      s_StdLogger:SetStyle( nStyle )

#ifdef HB_THREAD_SUPPORT
      hb_mutexUnlock( s_StdLogMutex )
#endif
   ENDIF

   RETURN

PROCEDURE hb_StandardLogName( cName )

#ifdef HB_THREAD_SUPPORT

   hb_mutexLock( s_StdLogMutex )
#endif

   s_StdLogger:cProgName := cName

#ifdef HB_THREAD_SUPPORT
   hb_mutexUnlock( s_StdLogMutex )
#endif

   RETURN

PROCEDURE hb_StandardLog( cMsg, nPrio )

   IF s_StdLogger != NIL
#ifdef HB_THREAD_SUPPORT
      hb_mutexLock( s_StdLogMutex )
#endif

      s_StdLogger:Log( cMsg, nPrio )

#ifdef HB_THREAD_SUPPORT
      hb_mutexUnlock( s_StdLogMutex )
#endif
   ENDIF

   RETURN

FUNCTION hb_BldLogMsg( ... )

   LOCAL xVar
   LOCAL cMsg := ""

   FOR EACH xVar IN hb_AParams()
      IF HB_ISNUMERIC( xVar )
         cMsg += AllTrim( hb_CStr( xVar ) )
      ELSEIF ! HB_ISSTRING( xVar )
         cMsg += hb_CStr( xVar )
      ELSE
         cMsg += xVar
      ENDIF

      IF xVar:__enumIndex() < PCount()
         cMsg += " "
      ENDIF
   NEXT

   RETURN cMsg

FUNCTION hb_LogDateStamp()

   LOCAL dToday := Date()

   RETURN  Str( Year( dToday ), 4 ) + "-" + PadL( Month( dToday ), 2, "0" ) + "-" + PadL( Day( dToday ), 2, "0" )

/*****
 * Logger class
 */

CREATE CLASS HB_Logger

   VAR cProgName
   VAR aLogToChannel                   INIT  {}
   VAR nStyle                          INIT  -1
   VAR nDefaultPriority                INIT  HB_LOG_INFO

   METHOD New()
   METHOD AddChannel( oChannel )       INLINE AAdd( ::aLogToChannel, oChannel )

   METHOD SetStyle( nStyle )           INLINE ::nStyle := nStyle

   METHOD Open()
   METHOD close()

   METHOD Log( cMessage, nPriority )

ENDCLASS

/**
 * Builds a new logger object.
 * Call with ::New( ch1, ch2... chN ) where ch are the channels
 * where to log.
 * Channels can be called at a second time.
 */

METHOD New() CLASS HB_Logger

   LOCAL nCount

   FOR nCount := 1 TO PCount()
      ::AddChannel( hb_PValue( nCount ) )
   NEXT

   RETURN Self

/**
 * Open all the channels calling their ::Open() method
 */

METHOD PROCEDURE Open() CLASS HB_Logger

   LOCAL oChannel

   IF ::cProgName == NIL
      hb_FNameSplit( hb_argv( 0 ), , @::cProgName )
   ENDIF

   FOR EACH oChannel IN ::aLogToChannel
      oChannel:Open( ::cProgName )
   NEXT

   RETURN

/**
 * Close all the channels calling their ::Close() method
 */

METHOD PROCEDURE close() CLASS HB_Logger

   LOCAL oChannel

   IF ::cProgName == NIL
      hb_FNameSplit( hb_argv( 0 ), , @::cProgName )
   ENDIF

   FOR EACH oChannel IN ::aLogToChannel
      oChannel:Close( ::cProgName )
   NEXT

   RETURN

/**
 * Send a log message to all the channels
 */

METHOD PROCEDURE Log( cMessage, nPriority ) CLASS HB_Logger

   LOCAL oChannel

   IF nPriority == NIL
      nPriority := ::nDefaultPriority
   ENDIF

   FOR EACH oChannel IN ::aLogToChannel
      /* Channels may want to have something to say about the format,
         so message formatting is done by the channels */
      oChannel:Log( ::nStyle, cMessage, ::cProgName, nPriority )
   NEXT

   RETURN

/*****
 * Logger Channel class (mostly VIRTUAL)
 */

CREATE CLASS HB_LogChannel

   VAR lOpened                   INIT .F.

   METHOD New( nLevel )          CONSTRUCTOR
   METHOD Open( cName )          VIRTUAL
   METHOD close( cName )         VIRTUAL

   METHOD Log( nStyle, cMessage, cName, nPriority )
   METHOD SetActive( lAct )      INLINE   ::lActive := lAct

   METHOD Format( nStyle, cMessage, cName, nPriority )

   PROTECTED:
   METHOD Send( nStyle, cMessage, cName, nPriority )    VIRTUAL

   HIDDEN:
   VAR nLevel
   VAR lActive                  INIT .T.

ENDCLASS

/**
 *  Creates a new channel. nLeven can be nil ( and will log all ),
 *  cName is the "program name" and must be given
 */

METHOD New( nLevel ) CLASS HB_LogChannel

   IF nLevel == NIL
      // log everything by default
      nLevel := HB_LOG_ALL
   ENDIF

   ::nLevel := nLevel

   RETURN Self

/**
 * Log the message: it send a request to the subclass "send" method
 * if the log level is higher or equal than the channel setting
 */

METHOD PROCEDURE Log( nStyle, cMessage, cName, nPriority ) CLASS HB_LogChannel

   IF nPriority <= ::nLevel .AND. ::lActive
      ::Send( nStyle, cMessage, cName, nPriority )
   ENDIF

   RETURN

/**
 * This is an utility functions for subclasses, used to
 * have a standard formatting for the message. Subclasses
 * may or may not call it.
 */

METHOD Format( nStyle, cMessage, cName, nPriority ) CLASS HB_LogChannel

   LOCAL cPrefix := ""

   IF hb_bitAnd( nStyle, HB_LOG_ST_DATE ) > 0
      IF hb_bitAnd( nStyle, HB_LOG_ST_ISODATE ) > 0
         cPrefix += hb_LogDateStamp()
      ELSE
         cPrefix += DToC( Date() )
      ENDIF
      cPrefix += " "
   ENDIF

   IF hb_bitAnd( nStyle, HB_LOG_ST_NAME ) > 0
      cPrefix += cName + " "
   ENDIF

   IF hb_bitAnd( nStyle, HB_LOG_ST_TIME ) > 0
      cPrefix += Time() + " "
   ENDIF

   IF hb_bitAnd( nStyle, HB_LOG_ST_LEVEL ) > 0
      SWITCH nPriority
      CASE HB_LOG_CRITICAL
         cPrefix += "CRITICAL: "
         EXIT

      CASE HB_LOG_ERROR
         cPrefix += "ERROR: "
         EXIT

      CASE HB_LOG_WARNING
         cPrefix += "WARNING: "
         EXIT

      CASE HB_LOG_INFO
         cPrefix += "INFO: "
         EXIT

      CASE HB_LOG_DEBUG
         cPrefix += "DEBUG: "
         EXIT

      OTHERWISE
         cPrefix += "DEBUG" + hb_ntos( nPriority - HB_LOG_DEBUG ) +  ": "
      ENDSWITCH
   ENDIF

   RETURN cPrefix + cMessage

/*****
 * Console channel
 */

CREATE CLASS HB_LogConsole FROM HB_LogChannel

   METHOD New( nLevel )
   METHOD Open( cName )
   METHOD close( cName )
   METHOD Out( ... )

   METHOD LogOnVt( ldo )      INLINE ::lRealConsole := ldo

   PROTECTED:
   METHOD Send( nStyle, cMessage, cName, nPriority )
   VAR lRealConsole    INIT .T.

ENDCLASS

METHOD New( nLevel ) CLASS HB_LogConsole

   ::Super:New( nLevel )

   RETURN Self

METHOD Open( cName ) CLASS HB_LogConsole

   IF ::lOpened
      RETURN .F.
   ENDIF

   ::Out( hb_LogDateStamp(), Time(), "--", cName, "start --" )
   ::lOpened := .T.

   RETURN .T.

METHOD close( cName ) CLASS HB_LogConsole

   IF ! ::lOpened
      RETURN .F.
   ENDIF

   ::Out( hb_LogDateStamp(), Time(), "--", cName, "end --" )
   ::lOpened := .F.

   RETURN .T.

METHOD PROCEDURE Send( nStyle, cMessage, cName, nPriority ) CLASS HB_LogConsole

   ::Out( ::Format( nStyle, cMessage, cName, nPriority ) )

   RETURN

METHOD PROCEDURE Out( ... ) CLASS HB_LogConsole

   LOCAL cMsg := "", xPar
   LOCAL nLen := Len( hb_AParams() )

   FOR EACH xPar IN hb_AParams()
      cMsg += hb_CStr( xPar )
      IF xPar:__EnumIndex() < nLen
         cMsg += " "
      ENDIF
   NEXT
   IF ::lRealConsole
      OutStd( cMsg, hb_eol() )
   ELSE
      QOut( cMsg )
   ENDIF

   RETURN

/*****
 * Console channel - to file
 */

CREATE CLASS HB_LogFile FROM HB_LogChannel

   VAR cFileName
   VAR nFileHandle
   VAR nFileLimit         INIT -1
   VAR nBackup            INIT 5

   METHOD New( nLevel, cFilename, nMaxSize, nBackup )
   METHOD Open( cProgName )
   METHOD close( cProgName )

   PROTECTED:
   METHOD Send( nStyle, cMessage, cProgName, nPriority )

ENDCLASS

METHOD New( nLevel, cFilename, nMaxSize, nBackup ) CLASS HB_LogFile

   ::Super:New( nLevel )
   ::cFileName := cFileName

   IF nMaxSize != NIL
      ::nFileLimit := nMaxSize
   ENDIF

   IF nBackup != NIL
      ::nBackup := nBackup
   ENDIF

   RETURN Self

METHOD Open( cProgName ) CLASS HB_LogFile

   IF ::lOpened
      RETURN .F.
   ENDIF

   IF hb_FileExists( ::cFileName )
      ::nFileHandle := FOpen( ::cFileName, FO_READWRITE )
      IF ::nFileHandle != F_ERROR
         FSeek( ::nFileHandle, 0, FS_END )
      ENDIF
   ELSE
      ::nFileHandle := hb_FCreate( ::cFileName, FC_NORMAL, FO_READWRITE )
   ENDIF

   IF ::nFileHandle == F_ERROR
      RETURN .F.
   ENDIF

   FWrite( ::nFileHandle, hb_BldLogMsg( hb_LogDateStamp(), Time(), "--", cProgName, "start --", hb_eol() ) )

   hb_FCommit( ::nFileHandle )
   ::lOpened := .T.

   RETURN .T.

METHOD close( cProgName ) CLASS HB_LogFile

   IF ! ::lOpened
      RETURN .F.
   ENDIF

   FWrite( ::nFileHandle, hb_BldLogMsg( hb_LogDateStamp(), Time(), "--", cProgName, "end --", hb_eol() ) )

   FClose( ::nFileHandle )
   ::nFileHandle := F_ERROR

   ::lOpened := .F.

   RETURN .T.

METHOD Send( nStyle, cMessage, cProgName, nPriority ) CLASS HB_LogFile

   LOCAL nCount

   FWrite( ::nFileHandle, ::Format( nStyle, cMessage, cProgName, nPriority ) + hb_eol() )
   hb_FCommit( ::nFileHandle )

   // see file limit and eventually swap file.
   IF ::nFileLimit > 0
      IF FSeek( ::nFileHandle, 0, FS_RELATIVE ) > ::nFileLimit * 1024
         FWrite( ::nFileHandle, hb_BldLogMsg( hb_LogDateStamp(), Time(), "LogFile: Closing file due to size limit breaking", hb_eol() ) )
         FClose( ::nFileHandle )

         IF ::nBackup > 1
            IF hb_FileExists( ::cFileName + "." + PadL( ::nBackup - 1, 3, "0" ) )
               FErase( ::cFileName + "." + PadL( ::nBackup - 1, 3, "0" ) )
            ENDIF
            FOR nCount := ::nBackup - 1 TO 1 STEP -1
               FRename( ::cFileName + "." + PadL( nCount - 1, 3, "0" ), ::cFileName + "." + PadL( nCount, 3, "0" ) )
            NEXT
         ENDIF

         IF FRename( ::cFileName, ::cFileName + ".000" ) == 0
            ::nFileHandle := hb_FCreate( ::cFileName, FC_NORMAL, FO_READWRITE )
            FWrite( ::nFileHandle, hb_BldLogMsg( hb_LogDateStamp(), Time(), "LogFile: Reopening file due to size limit breaking", hb_eol() ) )
         ENDIF
      ENDIF
   ENDIF

   RETURN FError() == 0


/*****
 * Console channel - to dbf
 */

CREATE CLASS HB_LogDbf FROM HB_LogChannel

   VAR cDBFName    INIT "messages.dbf"
   VAR cIndexName  INIT "messages.cdx"
   VAR cDriver     INIT "DBFCDX"
   VAR aStruct     INIT { ;
      { "PRIORITY", "N",   2, 0 }, ;
      { "PROGNAME", "C",  30, 0 }, ;
      { "MESSAGE" , "C", 250, 0 }, ;
      { "DATE"    , "D",   8, 0 }, ;
      { "TIME"    , "C",   8, 0 } }

   METHOD New( nLevel, cDBFName, cIndexName, aStruct, cDriver )
   METHOD Open( cProgName )
   METHOD close( cProgName )

   PROTECTED:
   METHOD Send( nStyle, cMessage, cProgName, nPriority )

ENDCLASS

METHOD New( nLevel, cDBFName, cIndexName, aStruct, cDriver ) CLASS HB_LogDbf

   LOCAL cPath, cName, cExt, cDrive

   ::Super:New( nLevel )
   IF HB_ISSTRING( cDBFName )
      hb_FNameSplit( cDBFName, @cPath, @cName, @cExt, @cDrive )
      IF Empty( cExt )
         cExt := "dbf"
      ENDIF
      ::cDBFName := iif( ! Empty( cDrive ), cDrive + ":\", "" ) + ;
         iif( ! Empty( cPath ), cPath + "\", "" ) + ;
         cName + cExt
      // __OutDebug( "::cDBFName", ::cDBFName )
   ENDIF

   IF HB_ISSTRING( cIndexName )
      hb_FNameSplit( cIndexName, @cPath, @cName, @cExt, @cDrive )
      IF Empty( cExt )
         cExt := "cdx"
      ENDIF
      ::cIndexName := iif( ! Empty( cDrive ), cDrive + ":\", "" ) + ;
         iif( ! Empty( cPath ), cPath + "\", "" ) + ;
         cName + cExt
      // __OutDebug( "::cCDXName", ::cCDXName )
   ENDIF

   IF aStruct != NIL
      ::aStruct := aStruct
   ENDIF

   IF cDriver != NIL
      ::cDriver := cDriver
   ENDIF

   RETURN Self

METHOD Open( cProgName ) CLASS HB_LogDbf

   IF ::lOpened
      RETURN .F.
   ENDIF

   IF ! hb_FileExists( ::cDBFName )
      dbCreate( ::cDBFName, ::aStruct )
      dbUseArea( .T., ::cDriver, ::cDBFName, "LogDbf", .T. )
      INDEX ON DToS( FIELD->date ) + FIELD->time + Str( FIELD->priority, 2 ) + FIELD->MESSAGE TAG "datetime" TO ( ::cIndexName )
      INDEX ON Str( FIELD->priority, 2 ) + DToS( FIELD->date ) + FIELD->time + FIELD->MESSAGE TAG "priority" TO ( ::cIndexName )
      LogDbf->( dbCloseArea() )
   ELSEIF ! hb_FileExists( ::cIndexName )
      dbUseArea( .T., ::cDriver, ::cDBFName, "LogDbf", .T. )
      INDEX ON DToS( FIELD->date ) + FIELD->time + Str( FIELD->priority, 2 ) + FIELD->MESSAGE TAG "datetime" TO ( ::cIndexName )
      INDEX ON Str( FIELD->priority, 2 ) + DToS( FIELD->date ) + FIELD->time + FIELD->MESSAGE TAG "priority" TO ( ::cIndexName )
      LogDbf->( dbCloseArea() )
   ENDIF
   // __OutDebug( "::cDriver, ::cDBFName", ::cDriver, ::cDBFName )
   dbUseArea( .T., ::cDriver, ::cDBFName, "LogDbf", .T. )
   SET INDEX TO ( ::cIndexName )

   LogDbf->( dbAppend() )
   LogDbf->priority := HB_LOG_INFO
   LogDbf->date     := Date()
   LogDbf->time     := Time()
   LogDbf->progname := cProgName
   LogDbf->message  := "-- start --"
   LogDbf->( dbCommit() )

   ::lOpened := .T.

   RETURN .T.

METHOD close( cProgName ) CLASS HB_LogDbf

   IF ! ::lOpened
      RETURN .F.
   ENDIF

   LogDbf->( dbAppend() )
   LogDbf->priority := HB_LOG_INFO
   LogDbf->date     := Date()
   LogDbf->time     := Time()
   LogDbf->progname := cProgName
   LogDbf->message  := "-- end --"
   LogDbf->( dbCommit() )

   ::lOpened := .F.

   RETURN .T.

METHOD Send( nStyle, cMessage, cProgName, nPriority ) CLASS HB_LogDbf

   LogDbf->( dbAppend() )
   LogDbf->priority := nPriority
   LogDbf->date     := Date()
   LogDbf->time     := Time()
   LogDbf->progname := cProgName
   LogDbf->message  := cMessage
   LogDbf->( dbCommit() )

   HB_SYMBOL_UNUSED( nStyle )

   RETURN .T.


/*****
 * Syslog channel - a wrapper for the low level
 * C interface to syslog/ event log system
 */

CREATE CLASS HB_LogSysLog FROM HB_LogChannel

   VAR nId

   METHOD New( nLevel, nId )
   METHOD Open( cName )
   METHOD close( cName )

   PROTECTED:
   METHOD Send( nType, cMessage, cName, nPriority )

ENDCLASS

METHOD New( nLevel, nId ) CLASS HB_LogSysLog

   ::Super:New( nLevel )
   ::nId := nId

   RETURN SELF

METHOD Open( cName ) CLASS HB_LogSysLog

   IF ::lOpened
      RETURN .F.
   ENDIF

   IF hb_SysLogOpen( cName )
      ::lOpened := .T.
      RETURN .T.
   ENDIF

   RETURN .F.

METHOD close( cName ) CLASS HB_LogSysLog

   IF ! ::lOpened
      RETURN .F.
   ENDIF

   IF hb_SysLogClose( cName )
      ::lOpened := .F.
      RETURN .T.
   ENDIF

   RETURN .F.

METHOD Send( nType, cMessage, cName, nPriority ) CLASS HB_LogSysLog

   HB_SYMBOL_UNUSED( nType )
   // Syslog does not need timestamp, nor priority

   RETURN hb_SysLogMessage( ::Format( HB_LOG_ST_LEVEL, cMessage, cName, nPriority ), nPriority, ::nId )


/*****
 * Debug channel
 */

CREATE CLASS HB_LogDebug FROM HB_LogChannel

   VAR nMaxLevel

   METHOD New( nLevel, nMaxLevel )
   // Nothing to do in this version
   METHOD Open()    INLINE .T.
   METHOD close()   INLINE .T.
   PROTECTED:
   METHOD Send( nStyle, cMessage, cName, nPriority )

ENDCLASS

METHOD New( nLevel, nMaxLevel ) CLASS HB_LogDebug

   ::Super:New( nLevel )
   ::nMaxLevel := nMaxLevel

   RETURN Self

METHOD PROCEDURE Send( nStyle, cMessage, cName, nPriority ) CLASS HB_LogDebug

   IF ! Empty( ::nMaxLevel )
      IF nPriority < ::nMaxLevel
         RETURN
      ENDIF
   ENDIF

   hb_OutDebug( ::Format( nStyle, cMessage, cName, nPriority ) )

   RETURN
