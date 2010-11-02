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


FUNCTION QSound( ... )
   RETURN HB_QSound():new( ... )

FUNCTION QSoundFromPointer( ... )
   RETURN HB_QSound():fromPointer( ... )


CREATE CLASS QSound INHERIT HbQtObjectHandler FUNCTION HB_QSound

   METHOD  new( ... )

   METHOD  fileName                      // (  )                                               -> cQString
   METHOD  isFinished                    // (  )                                               -> lBool
   METHOD  loops                         // (  )                                               -> nInt
   METHOD  loopsRemaining                // (  )                                               -> nInt
   METHOD  setLoops                      // ( nNumber )                                        -> NIL
   METHOD  isAvailable                   // (  )                                               -> lBool
   METHOD  play                          // ( cFilename )                                      -> NIL

   ENDCLASS


METHOD QSound:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSound( ... )
   RETURN Self


METHOD QSound:fileName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSound_fileName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSound:isFinished( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSound_isFinished( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSound:loops( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSound_loops( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSound:loopsRemaining( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSound_loopsRemaining( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSound:setLoops( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSound_setLoops( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSound:isAvailable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSound_isAvailable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSound:play( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QSound_play( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

