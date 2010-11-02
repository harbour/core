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


FUNCTION QLCDNumber( ... )
   RETURN HB_QLCDNumber():new( ... )

FUNCTION QLCDNumberFromPointer( ... )
   RETURN HB_QLCDNumber():fromPointer( ... )


CREATE CLASS QLCDNumber INHERIT HbQtObjectHandler, HB_QFrame FUNCTION HB_QLCDNumber

   METHOD  new( ... )

   METHOD  checkOverflow                 // ( nNum )                                           -> lBool
                                         // ( nNum )                                           -> lBool
   METHOD  intValue                      // (  )                                               -> nInt
   METHOD  mode                          // (  )                                               -> nMode
   METHOD  numDigits                     // (  )                                               -> nInt
   METHOD  segmentStyle                  // (  )                                               -> nSegmentStyle
   METHOD  setMode                       // ( nMode )                                          -> NIL
   METHOD  setNumDigits                  // ( nNDigits )                                       -> NIL
   METHOD  setSegmentStyle               // ( nSegmentStyle )                                  -> NIL
   METHOD  smallDecimalPoint             // (  )                                               -> lBool
   METHOD  value                         // (  )                                               -> nDouble
   METHOD  display                       // ( cS )                                             -> NIL
                                         // ( nNum )                                           -> NIL
                                         // ( nNum )                                           -> NIL
   METHOD  setBinMode                    // (  )                                               -> NIL
   METHOD  setDecMode                    // (  )                                               -> NIL
   METHOD  setHexMode                    // (  )                                               -> NIL
   METHOD  setOctMode                    // (  )                                               -> NIL
   METHOD  setSmallDecimalPoint          // ( lBool )                                          -> NIL

   ENDCLASS


METHOD QLCDNumber:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QLCDNumber( ... )
   RETURN Self


METHOD QLCDNumber:checkOverflow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLCDNumber_checkOverflow( ::pPtr, ... )
         // RETURN Qt_QLCDNumber_checkOverflow_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLCDNumber:intValue( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLCDNumber_intValue( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLCDNumber:mode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLCDNumber_mode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLCDNumber:numDigits( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLCDNumber_numDigits( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLCDNumber:segmentStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLCDNumber_segmentStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLCDNumber:setMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLCDNumber_setMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLCDNumber:setNumDigits( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLCDNumber_setNumDigits( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLCDNumber:setSegmentStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLCDNumber_setSegmentStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLCDNumber:smallDecimalPoint( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLCDNumber_smallDecimalPoint( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLCDNumber:value( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLCDNumber_value( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLCDNumber:display( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QLCDNumber_display( ::pPtr, ... )
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLCDNumber_display_1( ::pPtr, ... )
         // RETURN Qt_QLCDNumber_display_2( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLCDNumber:setBinMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLCDNumber_setBinMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLCDNumber:setDecMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLCDNumber_setDecMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLCDNumber:setHexMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLCDNumber_setHexMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLCDNumber:setOctMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLCDNumber_setOctMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLCDNumber:setSmallDecimalPoint( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLCDNumber_setSmallDecimalPoint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

