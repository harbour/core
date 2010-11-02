/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */


#include "hbclass.ch"


REQUEST __HBQTCORE


FUNCTION QProcess( ... )
   RETURN HB_QProcess():new( ... )

FUNCTION QProcessFromPointer( ... )
   RETURN HB_QProcess():fromPointer( ... )


CREATE CLASS QProcess INHERIT HbQtObjectHandler, HB_QIODevice FUNCTION HB_QProcess

   METHOD  new( ... )

   METHOD  close                         // (  )                                               -> NIL
   METHOD  closeReadChannel              // ( nChannel )                                       -> NIL
   METHOD  closeWriteChannel             // (  )                                               -> NIL
   METHOD  environment                   // (  )                                               -> oQStringList
   METHOD  error                         // (  )                                               -> nQProcess_ProcessError
   METHOD  exitCode                      // (  )                                               -> nInt
   METHOD  exitStatus                    // (  )                                               -> nQProcess_ExitStatus
   METHOD  processChannelMode            // (  )                                               -> nProcessChannelMode
   METHOD  readAllStandardError          // (  )                                               -> oQByteArray
   METHOD  readAllStandardOutput         // (  )                                               -> oQByteArray
   METHOD  readChannel                   // (  )                                               -> nProcessChannel
   METHOD  setEnvironment                // ( oQStringList )                                   -> NIL
   METHOD  setProcessChannelMode         // ( nMode )                                          -> NIL
   METHOD  setReadChannel                // ( nChannel )                                       -> NIL
   METHOD  setStandardErrorFile          // ( cFileName, nMode )                               -> NIL
   METHOD  setStandardInputFile          // ( cFileName )                                      -> NIL
   METHOD  setStandardOutputFile         // ( cFileName, nMode )                               -> NIL
   METHOD  setStandardOutputProcess      // ( oQProcess )                                      -> NIL
   METHOD  setWorkingDirectory           // ( cDir )                                           -> NIL
   METHOD  start                         // ( cProgram, oQStringList, nMode )                  -> NIL
                                         // ( cProgram, nMode )                                -> NIL
   METHOD  state                         // (  )                                               -> nQProcess_ProcessState
   METHOD  waitForFinished               // ( nMsecs )                                         -> lBool
   METHOD  waitForStarted                // ( nMsecs )                                         -> lBool
   METHOD  workingDirectory              // (  )                                               -> cQString
   METHOD  execute                       // ( cProgram, oQStringList )                         -> nInt
                                         // ( cProgram )                                       -> nInt
   METHOD  startDetached                 // ( cProgram, oQStringList, cWorkingDirectory, @nPid ) -> lBool
                                         // ( cProgram, oQStringList )                         -> lBool
                                         // ( cProgram )                                       -> lBool
   METHOD  systemEnvironment             // (  )                                               -> oQStringList
   METHOD  kill                          // (  )                                               -> NIL
   METHOD  terminate                     // (  )                                               -> NIL

   ENDCLASS


METHOD QProcess:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QProcess( ... )
   RETURN Self


METHOD QProcess:close( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProcess_close( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:closeReadChannel( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QProcess_closeReadChannel( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:closeWriteChannel( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProcess_closeWriteChannel( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:environment( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QProcess_environment( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:error( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProcess_error( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:exitCode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProcess_exitCode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:exitStatus( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProcess_exitStatus( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:processChannelMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProcess_processChannelMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:readAllStandardError( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QProcess_readAllStandardError( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:readAllStandardOutput( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QProcess_readAllStandardOutput( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:readChannel( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProcess_readChannel( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:setEnvironment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QProcess_setEnvironment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:setProcessChannelMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QProcess_setProcessChannelMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:setReadChannel( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QProcess_setReadChannel( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:setStandardErrorFile( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QProcess_setStandardErrorFile( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QProcess_setStandardErrorFile( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:setStandardInputFile( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QProcess_setStandardInputFile( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:setStandardOutputFile( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QProcess_setStandardOutputFile( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QProcess_setStandardOutputFile( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:setStandardOutputProcess( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QProcess_setStandardOutputProcess( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:setWorkingDirectory( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QProcess_setWorkingDirectory( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:start( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QProcess_start( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QProcess_start_1( ::pPtr, ... )
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QProcess_start( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QProcess_start_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:state( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProcess_state( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:waitForFinished( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QProcess_waitForFinished( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QProcess_waitForFinished( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:waitForStarted( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QProcess_waitForStarted( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QProcess_waitForStarted( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:workingDirectory( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProcess_workingDirectory( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:execute( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QProcess_execute( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QProcess_execute_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:startDetached( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QProcess_startDetached( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QProcess_startDetached( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QProcess_startDetached_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QProcess_startDetached_2( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:systemEnvironment( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QProcess_systemEnvironment( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:kill( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProcess_kill( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QProcess:terminate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QProcess_terminate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

