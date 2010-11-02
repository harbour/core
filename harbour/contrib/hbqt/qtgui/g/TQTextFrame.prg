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


REQUEST __HBQTGUI


FUNCTION QTextFrame( ... )
   RETURN HB_QTextFrame():new( ... )

FUNCTION QTextFrameFromPointer( ... )
   RETURN HB_QTextFrame():fromPointer( ... )


CREATE CLASS QTextFrame INHERIT HbQtObjectHandler, HB_QTextObject FUNCTION HB_QTextFrame

   METHOD  new( ... )

   METHOD  childFrames                   // (  )                                               -> oQList_QTextFrame
   METHOD  firstCursorPosition           // (  )                                               -> oQTextCursor
   METHOD  firstPosition                 // (  )                                               -> nInt
   METHOD  frameFormat                   // (  )                                               -> oQTextFrameFormat
   METHOD  lastCursorPosition            // (  )                                               -> oQTextCursor
   METHOD  lastPosition                  // (  )                                               -> nInt
   METHOD  parentFrame                   // (  )                                               -> oQTextFrame
   METHOD  setFrameFormat                // ( oQTextFrameFormat )                              -> NIL

   ENDCLASS


METHOD QTextFrame:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextFrame( ... )
   RETURN Self


METHOD QTextFrame:childFrames( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QTextFrame_childFrames( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrame:firstCursorPosition( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCursorFromPointer( Qt_QTextFrame_firstCursorPosition( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrame:firstPosition( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFrame_firstPosition( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrame:frameFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextFrameFormatFromPointer( Qt_QTextFrame_frameFormat( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrame:lastCursorPosition( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCursorFromPointer( Qt_QTextFrame_lastCursorPosition( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrame:lastPosition( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFrame_lastPosition( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrame:parentFrame( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextFrameFromPointer( Qt_QTextFrame_parentFrame( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFrame:setFrameFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextFrame_setFrameFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

