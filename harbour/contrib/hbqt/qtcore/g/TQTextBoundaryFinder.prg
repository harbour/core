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


FUNCTION QTextBoundaryFinder( ... )
   RETURN HB_QTextBoundaryFinder():new( ... )

FUNCTION QTextBoundaryFinderFromPointer( ... )
   RETURN HB_QTextBoundaryFinder():fromPointer( ... )


CREATE CLASS QTextBoundaryFinder INHERIT HbQtObjectHandler FUNCTION HB_QTextBoundaryFinder

   METHOD  new( ... )

   METHOD  boundaryReasons               // (  )                                               -> nBoundaryReasons
   METHOD  isAtBoundary                  // (  )                                               -> lBool
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  position                      // (  )                                               -> nInt
   METHOD  setPosition                   // ( nPosition )                                      -> NIL
   METHOD  string                        // (  )                                               -> cQString
   METHOD  toEnd                         // (  )                                               -> NIL
   METHOD  toNextBoundary                // (  )                                               -> nInt
   METHOD  toPreviousBoundary            // (  )                                               -> nInt
   METHOD  toStart                       // (  )                                               -> NIL
   METHOD  type                          // (  )                                               -> nBoundaryType

   ENDCLASS


METHOD QTextBoundaryFinder:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextBoundaryFinder( ... )
   RETURN Self


METHOD QTextBoundaryFinder:boundaryReasons( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBoundaryFinder_boundaryReasons( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBoundaryFinder:isAtBoundary( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBoundaryFinder_isAtBoundary( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBoundaryFinder:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBoundaryFinder_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBoundaryFinder:position( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBoundaryFinder_position( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBoundaryFinder:setPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextBoundaryFinder_setPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBoundaryFinder:string( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBoundaryFinder_string( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBoundaryFinder:toEnd( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBoundaryFinder_toEnd( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBoundaryFinder:toNextBoundary( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBoundaryFinder_toNextBoundary( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBoundaryFinder:toPreviousBoundary( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBoundaryFinder_toPreviousBoundary( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBoundaryFinder:toStart( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBoundaryFinder_toStart( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBoundaryFinder:type( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBoundaryFinder_type( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

