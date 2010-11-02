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


FUNCTION QDropEvent( ... )
   RETURN HB_QDropEvent():new( ... )

FUNCTION QDropEventFromPointer( ... )
   RETURN HB_QDropEvent():fromPointer( ... )


CREATE CLASS QDropEvent INHERIT HbQtObjectHandler, HB_QEvent FUNCTION HB_QDropEvent

   METHOD  new( ... )

   METHOD  acceptProposedAction          // (  )                                               -> NIL
   METHOD  dropAction                    // (  )                                               -> nQt_DropAction
   METHOD  keyboardModifiers             // (  )                                               -> nQt_KeyboardModifiers
   METHOD  mimeData                      // (  )                                               -> oQMimeData
   METHOD  mouseButtons                  // (  )                                               -> nQt_MouseButtons
   METHOD  pos                           // (  )                                               -> oQPoint
   METHOD  possibleActions               // (  )                                               -> nQt_DropActions
   METHOD  proposedAction                // (  )                                               -> nQt_DropAction
   METHOD  setDropAction                 // ( nAction )                                        -> NIL
   METHOD  source                        // (  )                                               -> oQWidget

   ENDCLASS


METHOD QDropEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDropEvent( ... )
   RETURN Self


METHOD QDropEvent:acceptProposedAction( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDropEvent_acceptProposedAction( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDropEvent:dropAction( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDropEvent_dropAction( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDropEvent:keyboardModifiers( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDropEvent_keyboardModifiers( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDropEvent:mimeData( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMimeDataFromPointer( Qt_QDropEvent_mimeData( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDropEvent:mouseButtons( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDropEvent_mouseButtons( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDropEvent:pos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QDropEvent_pos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDropEvent:possibleActions( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDropEvent_possibleActions( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDropEvent:proposedAction( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDropEvent_proposedAction( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDropEvent:setDropAction( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDropEvent_setDropAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDropEvent:source( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QDropEvent_source( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

