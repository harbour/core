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


FUNCTION QTimeLine( ... )
   RETURN HB_QTimeLine():new( ... )

FUNCTION QTimeLineFromPointer( ... )
   RETURN HB_QTimeLine():fromPointer( ... )


CREATE CLASS QTimeLine INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QTimeLine

   METHOD  new( ... )

   METHOD  currentFrame                  // (  )                                               -> nInt
   METHOD  currentTime                   // (  )                                               -> nInt
   METHOD  currentValue                  // (  )                                               -> nQreal
   METHOD  curveShape                    // (  )                                               -> nCurveShape
   METHOD  direction                     // (  )                                               -> nDirection
   METHOD  duration                      // (  )                                               -> nInt
   METHOD  endFrame                      // (  )                                               -> nInt
   METHOD  frameForTime                  // ( nMsec )                                          -> nInt
   METHOD  loopCount                     // (  )                                               -> nInt
   METHOD  setCurveShape                 // ( nShape )                                         -> NIL
   METHOD  setDirection                  // ( nDirection )                                     -> NIL
   METHOD  setDuration                   // ( nDuration )                                      -> NIL
   METHOD  setEndFrame                   // ( nFrame )                                         -> NIL
   METHOD  setFrameRange                 // ( nStartFrame, nEndFrame )                         -> NIL
   METHOD  setLoopCount                  // ( nCount )                                         -> NIL
   METHOD  setStartFrame                 // ( nFrame )                                         -> NIL
   METHOD  setUpdateInterval             // ( nInterval )                                      -> NIL
   METHOD  startFrame                    // (  )                                               -> nInt
   METHOD  state                         // (  )                                               -> nState
   METHOD  updateInterval                // (  )                                               -> nInt
   METHOD  valueForTime                  // ( nMsec )                                          -> nQreal
   METHOD  resume                        // (  )                                               -> NIL
   METHOD  setCurrentTime                // ( nMsec )                                          -> NIL
   METHOD  setPaused                     // ( lPaused )                                        -> NIL
   METHOD  start                         // (  )                                               -> NIL
   METHOD  stop                          // (  )                                               -> NIL
   METHOD  toggleDirection               // (  )                                               -> NIL

   ENDCLASS


METHOD QTimeLine:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTimeLine( ... )
   RETURN Self


METHOD QTimeLine:currentFrame( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_currentFrame( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:currentTime( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_currentTime( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:currentValue( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_currentValue( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:curveShape( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_curveShape( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:direction( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_direction( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:duration( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_duration( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:endFrame( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_endFrame( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:frameForTime( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_frameForTime( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:loopCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_loopCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:setCurveShape( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_setCurveShape( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:setDirection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_setDirection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:setDuration( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_setDuration( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:setEndFrame( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_setEndFrame( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:setFrameRange( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTimeLine_setFrameRange( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:setLoopCount( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_setLoopCount( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:setStartFrame( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_setStartFrame( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:setUpdateInterval( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_setUpdateInterval( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:startFrame( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_startFrame( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:state( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_state( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:updateInterval( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_updateInterval( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:valueForTime( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_valueForTime( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:resume( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_resume( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:setCurrentTime( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_setCurrentTime( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:setPaused( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTimeLine_setPaused( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:start( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_start( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:stop( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_stop( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTimeLine:toggleDirection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTimeLine_toggleDirection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

