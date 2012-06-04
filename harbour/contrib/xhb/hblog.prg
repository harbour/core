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


/*************************************************************
* Static standard logger access
*/

#define HB_THREAD_SUPPORT

#ifdef HB_THREAD_SUPPORT
STATIC s_StdLogMutex := hb_mutexCreate()
#endif

STATIC s_StdLogger

PROCEDURE HB_InitStandardLog( ... )

   LOCAL Param

   s_StdLogger := HB_Logger():New()

   FOR EACH Param in HB_aParams()
      #ifdef HB_THREAD_SUPPORT
         HB_MutexLock( s_StdLogMutex )
      #endif

      s_StdLogger:AddChannel( Param )

      #ifdef HB_THREAD_SUPPORT
         HB_MutexUnlock( s_StdLogMutex )
      #endif
   NEXT

   #ifdef HB_THREAD_SUPPORT
      HB_MutexLock( s_StdLogMutex )
   #endif

   s_StdLogger:SetStyle( HB_LOG_ST_DATE + HB_LOG_ST_ISODATE + HB_LOG_ST_TIME + HB_LOG_ST_LEVEL )

   #ifdef HB_THREAD_SUPPORT
      HB_MutexUnlock( s_StdLogMutex )
   #endif
RETURN

PROCEDURE HB_OpenStandardLog()

   s_StdLogger:Open()

RETURN

PROCEDURE HB_StandardLogAdd( oChannel )

   IF s_StdLogger != NIL
      #ifdef HB_THREAD_SUPPORT
         HB_MutexLock( s_StdLogMutex )
      #endif

      s_StdLogger:AddChannel( oChannel )

      #ifdef HB_THREAD_SUPPORT
         HB_MutexUnlock( s_StdLogMutex )
      #endif
   ENDIF

RETURN

PROCEDURE HB_CloseStandardLog()

   // If the logger is NIL also the mutex is NIL
   IF s_StdLogger != NIL
      #ifdef HB_THREAD_SUPPORT
         HB_MutexLock( s_StdLogMutex )
      #endif

      s_StdLogger:Close()

      #ifdef HB_THREAD_SUPPORT
         HB_MutexUnlock( s_StdLogMutex )
      #endif
   ENDIF

RETURN


PROCEDURE HB_SetStandardLogStyle( nStyle )

   IF s_StdLogger != NIL
      #ifdef HB_THREAD_SUPPORT
         HB_MutexLock( s_StdLogMutex )
      #endif

      s_StdLogger:SetStyle( nStyle )

      #ifdef HB_THREAD_SUPPORT
         HB_MutexUnlock( s_StdLogMutex )
      #endif
   ENDIF

RETURN

PROCEDURE HB_StandardLogName( cName )

   #ifdef HB_THREAD_SUPPORT
      HB_MutexLock( s_StdLogMutex )
   #endif

   s_StdLogger:cProgName := cName

   #ifdef HB_THREAD_SUPPORT
      HB_MutexUnlock( s_StdLogMutex )
   #endif

RETURN

PROCEDURE HB_StandardLog( cMsg, nPrio )

   IF s_StdLogger != NIL
      #ifdef HB_THREAD_SUPPORT
         HB_MutexLock( s_StdLogMutex )
      #endif

      s_StdLogger:Log( cMsg, nPrio )

      #ifdef HB_THREAD_SUPPORT
         HB_MutexUnlock( s_StdLogMutex )
      #endif
   ENDIF

RETURN

FUNCTION HB_BldLogMsg( ... )

   LOCAL xVar
   LOCAL cMsg := ""

   FOR EACH xVar IN HB_aParams()
      IF HB_ISNUMERIC( xVar )
         cMsg += AllTrim( HB_CStr( xVar ) )
      ELSEIF ! HB_ISSTRING( xVar )
         cMsg += HB_CStr( xVar )
      ELSE
         cMsg += xVar
      ENDIF

      IF xVar:__enumIndex() < PCount()
         cMsg += " "
      ENDIF
   NEXT

RETURN cMsg

FUNCTION HB_LogDateStamp()

   LOCAL dToday := Date()

RETURN  Str(Year( dToday ), 4 ) +"-"+ Padl( Month( dToday ) , 2, "0" ) + "-" + Padl( Day( dToday ), 2, "0" )

/**********************************************
* Logger class
***********************************************/

CLASS HB_Logger
   DATA cProgName
   DATA aLogToChannel                  INIT  {}
   DATA nStyle                         INIT  -1
   DATA nDefaultPriority               INIT  HB_LOG_INFO

   METHOD New()
   METHOD AddChannel( oChannel )       INLINE Aadd( ::aLogToChannel, oChannel )

   METHOD SetStyle( nStyle )           INLINE ::nStyle := nStyle

   METHOD Open()
   METHOD Close()

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
      HB_FnameSplit( hb_argv(0),,@::cProgName )
   ENDIF

   FOR EACH oChannel IN ::aLogToChannel
      oChannel:Open( ::cProgName )
   NEXT

RETURN

/**
* Close all the channels calling their ::Close() method
*/
METHOD PROCEDURE Close() CLASS HB_Logger

   LOCAL oChannel

   IF ::cProgName == NIL
      HB_FnameSplit( hb_argv(0),,@::cProgName )
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

/**********************************************
* Logger Channel class (mostly VIRTUAL)
***********************************************/

CLASS HB_LogChannel
   DATA lOpened                  INIT .F.

   METHOD New( nLevel )          CONSTRUCTOR
   METHOD Open( cName )          VIRTUAL
   METHOD Close( cName )         VIRTUAL

   METHOD Log( nStyle, cMessage, cName, nPriority )
   METHOD SetActive( lAct )      INLINE   ::lActive := lAct

   METHOD Format( nStyle, cMessage, cName, nPriority )

PROTECTED:
   METHOD Send( nStyle, cMessage, cName, nPriority )    VIRTUAL

HIDDEN:
   DATA nLevel
   DATA lActive                  INIT .T.

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

   IF nPriority <= ::nLevel .and. ::lActive
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

   IF HB_BitAnd( nStyle, HB_LOG_ST_DATE ) > 0
      IF HB_BitAnd( nStyle, HB_LOG_ST_ISODATE ) > 0
         cPrefix += HB_LogDateStamp()
      ELSE
         cPrefix += DtoC( Date() )
      ENDIF
      cPrefix += " "
   ENDIF

   IF HB_BitAnd( nStyle, HB_LOG_ST_NAME ) > 0
      cPrefix += cName + " "
   ENDIF

   IF HB_BitAnd( nStyle, HB_LOG_ST_TIME ) > 0
      cPrefix += Time() + " "
   ENDIF

   IF HB_BitAnd( nStyle, HB_LOG_ST_LEVEL ) > 0
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
            cPrefix += "DEBUG" + Alltrim( Str(nPriority - HB_LOG_DEBUG) )+  ": "
      END
   ENDIF

RETURN cPrefix + cMessage

/**********************************************
* Console channel
***********************************************/

CLASS HB_LogConsole FROM HB_LogChannel

   METHOD New( nLevel )
   METHOD Open( cName )
   METHOD Close( cName )
   METHOD Out( ... )

   METHOD LogOnVt( ldo )      INLINE ::lRealConsole := ldo

PROTECTED:
   METHOD Send( nStyle, cMessage, cName, nPriority )
   DATA lRealConsole    INIT .T.

ENDCLASS

METHOD New( nLevel ) CLASS HB_LogConsole
   ::Super:New( nLevel )
RETURN Self

METHOD Open( cName ) CLASS HB_LogConsole

   IF ::lOpened
      RETURN .F.
   ENDIF

   ::Out( HB_LogDateStamp(), Time(), "--", cName, "start --" )
   ::lOpened := .T.

RETURN .T.

METHOD Close( cName ) CLASS HB_LogConsole

   IF ! ::lOpened
      RETURN .F.
   ENDIF

   ::Out( HB_LogDateStamp(), Time(), "--", cName, "end --" )
   ::lOpened := .F.

RETURN .T.

METHOD PROCEDURE Send( nStyle, cMessage, cName, nPriority ) CLASS HB_LogConsole

   ::Out( ::Format( nStyle, cMessage, cName, nPriority ) )

RETURN

METHOD PROCEDURE Out( ... ) CLASS HB_LogConsole
   LOCAL cMsg := "", xPar
   LOCAL nLen := Len( hb_aParams() )
   FOR EACH xPar IN hb_aParams()
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

/**********************************************
* Console channel - to file
***********************************************/
CLASS HB_LogFile FROM HB_LogChannel

   DATA cFileName
   DATA nFileHandle
   DATA nFileLimit         INIT -1
   DATA nBackup            INIT 5

   METHOD New( nLevel, cFilename, nMaxSize, nBackup )
   METHOD Open( cProgName )
   METHOD Close( cProgName )

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
      IF ::nFileHandle > 0
         Fseek( ::nFileHandle, 0 ,FS_END )
      END
   ELSE
      ::nFileHandle := hb_FCreate( ::cFileName, FC_NORMAL, FO_READWRITE )
   ENDIF

   IF ::nFileHandle < 0
      RETURN .F.
   ENDIF

   Fwrite( ::nFileHandle, HB_BldLogMsg( HB_LogDateStamp(), Time(), "--", cProgName, "start --", hb_eol() ) )

   HB_Fcommit( ::nFileHandle )
   ::lOpened := .T.

RETURN .T.

METHOD Close( cProgName ) CLASS HB_LogFile

   IF ! ::lOpened
      RETURN .F.
   ENDIF

   Fwrite( ::nFileHandle, HB_BldLogMsg( HB_LogDateStamp(), Time(), "--", cProgName, "end --", hb_eol() ) )

   FClose( ::nFileHandle )
   ::nFileHandle := -1

   ::lOpened := .F.

RETURN .T.

METHOD Send( nStyle, cMessage, cProgName, nPriority ) CLASS HB_LogFile

   LOCAL nCount

   FWrite( ::nFileHandle, ::Format( nStyle, cMessage, cProgName, nPriority ) + hb_eol() )
   HB_FCommit( ::nFileHandle );

   // see file limit and eventually swap file.
   IF ::nFileLimit > 0
      IF FSeek( ::nFileHandle, 0, FS_RELATIVE ) > ::nFileLimit * 1024
         Fwrite( ::nFileHandle, HB_BldLogMsg( HB_LogDateStamp(), Time(), "LogFile: Closing file due to size limit breaking", hb_eol() ) )
         FClose( ::nFileHandle )

         IF ::nBackup > 1
            IF hb_FileExists( ::cFileName +"." + Padl( ::nBackup-1, 3,"0" ) )
               FErase( ::cFileName +"." + Padl( ::nBackup-1, 3,"0" ) )
            ENDIF
            FOR nCount := ::nBackup -1 TO 1 STEP -1
               FRename( ::cFileName +"." + Padl( nCount-1, 3,"0" ), ::cFileName + "." + Padl( nCount, 3,"0" ) )
            NEXT
         ENDIF

         IF FRename( ::cFileName, ::cFileName + ".000" ) == 0
            ::nFileHandle := hb_FCreate( ::cFileName, FC_NORMAL, FO_READWRITE )
            Fwrite( ::nFileHandle, HB_BldLogMsg( HB_LogDateStamp(), Time(), "LogFile: Reopening file due to size limit breaking", hb_eol() ) )
         ENDIF
      ENDIF
   ENDIF

RETURN Ferror() == 0


/**********************************************
* Console channel - to dbf
***********************************************/
CLASS HB_LogDbf FROM HB_LogChannel

   DATA cDBFName    INIT "messages.dbf"
   DATA cIndexName  INIT "messages.cdx"
   DATA cDriver     INIT "DBFCDX"
   DATA aStruct     INIT { ;
                           { "PRIORITY", "N",   2, 0 } ,;
                           { "PROGNAME", "C",  30, 0 } ,;
                           { "MESSAGE" , "C", 250, 0 } ,;
                           { "DATE"    , "D",   8, 0 } ,;
                           { "TIME"    , "C",   8, 0 }  ;
                         }

   METHOD New( nLevel, cDBFName, cIndexName, aStruct, cDriver )
   METHOD Open( cProgName )
   METHOD Close( cProgName )

PROTECTED:
   METHOD Send( nStyle, cMessage, cProgName, nPriority )

ENDCLASS


METHOD New( nLevel, cDBFName, cIndexName, aStruct, cDriver ) CLASS HB_LogDbf
   LOCAL cPath, cName, cExt, cDrive

   ::Super:New( nLevel )
   IF HB_ISSTRING( cDBFName )
      HB_FnameSplit( cDBFName, @cPath, @cName, @cExt, @cDrive )
      IF Empty( cExt )
         cExt := "dbf"
      ENDIF
      ::cDBFName := IIF( !Empty( cDrive ), cDrive + ":\", "" ) + ;
                    IIF( !Empty( cPath ) , cPath + "\", "" ) + ;
                    cName + cExt
      //__OutDebug( "::cDBFName", ::cDBFName )
   ENDIF

   IF HB_ISSTRING( cIndexName )
      HB_FnameSplit( cIndexName, @cPath, @cName, @cExt, @cDrive )
      IF Empty( cExt )
         cExt := "cdx"
      ENDIF
      ::cIndexName := IIF( !Empty( cDrive ), cDrive + ":\", "" ) + ;
                      IIF( !Empty( cPath ) , cPath + "\", "" ) + ;
                      cName + cExt
      //__OutDebug( "::cCDXName", ::cCDXName )
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
      INDEX ON DToS( FIELD->date ) + FIELD->time + Str( FIELD->priority, 2 ) + FIELD->message TAG "datetime" TO (::cIndexName)
      INDEX ON Str( FIELD->priority, 2 ) + DToS( FIELD->date ) + FIELD->time + FIELD->message TAG "priority" TO (::cIndexName)
      LogDbf->( dbCloseArea() )
   ELSEIF ! hb_FileExists( ::cIndexName )
      dbUseArea( .T., ::cDriver, ::cDBFName, "LogDbf", .T. )
      INDEX ON DToS( FIELD->date ) + FIELD->time + Str( FIELD->priority, 2 ) + FIELD->message TAG "datetime" TO (::cIndexName)
      INDEX ON Str( FIELD->priority, 2 ) + DToS( FIELD->date ) + FIELD->time + FIELD->message TAG "priority" TO (::cIndexName)
      LogDbf->( dbCloseArea() )
   ENDIF
   //__OutDebug( "::cDriver, ::cDBFName", ::cDriver, ::cDBFName )
   dbUseArea( .T., ::cDriver, ::cDBFName, "LogDbf", .T. )
   SET INDEX TO (::cIndexName)

   LogDbf->( dbAppend() )
   LogDbf->priority := HB_LOG_INFO
   LogDbf->date     := Date()
   LogDbf->time     := Time()
   LogDbf->progname := cProgName
   LogDbf->message  := "-- start --"
   LogDbf->( dbCommit() )

   ::lOpened := .T.

RETURN .T.

METHOD Close( cProgName ) CLASS HB_LogDbf

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

   (nStyle)
RETURN .T.


/**********************************************
* Syslog channel - a wrapper for the low level
* C interface to syslog/ event log system
***********************************************/

CLASS HB_LogSyslog FROM HB_LogChannel

   DATA nId

   METHOD New( nLevel, nId )
   METHOD Open( cName )
   METHOD Close( cName )

PROTECTED:
   METHOD Send( nType, cMessage, cName, nPriority )

ENDCLASS

METHOD New( nLevel, nId ) CLASS HB_LogSyslog

   ::Super:New( nLevel )
   ::nId := nId

RETURN SELF

METHOD Open( cName ) CLASS HB_LogSyslog

   IF ::lOpened
      RETURN .F.
   ENDIF

   IF HB_SyslogOpen( cName )
      ::lOpened := .T.
      RETURN .T.
   ENDIF

RETURN .F.

METHOD Close( cName ) CLASS HB_LogSyslog

   IF ! ::lOpened
      RETURN .F.
   ENDIF

   IF HB_SyslogClose( cName )
      ::lOpened := .F.
      RETURN .T.
   ENDIF

RETURN .F.

METHOD Send( nType, cMessage, cName, nPriority ) CLASS HB_LogSyslog

   HB_SYMBOL_UNUSED( nType )
   // Syslog does not need timestamp, nor priority
RETURN HB_SyslogMessage( ::Format( HB_LOG_ST_LEVEL, cMessage, cName, nPriority ), nPriority, ::nId )


/**********************************************
* Debug channel
***********************************************/
CLASS HB_LogDebug FROM HB_LogChannel
   DATA nMaxLevel

   METHOD New( nLevel, nMaxLevel )
   // Nothing to do in this version
   METHOD Open()    INLINE .T.
   METHOD Close()   INLINE .T.
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

   HB_OutDebug( ::Format( nStyle, cMessage, cName, nPriority ) )

RETURN
