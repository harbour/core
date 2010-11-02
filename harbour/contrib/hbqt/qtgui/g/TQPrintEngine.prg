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


FUNCTION QPrintEngine( ... )
   RETURN HB_QPrintEngine():new( ... )

FUNCTION QPrintEngineFromPointer( ... )
   RETURN HB_QPrintEngine():fromPointer( ... )


CREATE CLASS QPrintEngine INHERIT HbQtObjectHandler FUNCTION HB_QPrintEngine

   METHOD  new( ... )

   METHOD  abort                         // (  )                                               -> lBool
   METHOD  metric                        // ( nId )                                            -> nInt
   METHOD  newPage                       // (  )                                               -> lBool
   METHOD  printerState                  // (  )                                               -> nQPrinter_PrinterState
   METHOD  property                      // ( nKey )                                           -> oQVariant
   METHOD  setProperty                   // ( nKey, oQVariant )                                -> NIL

   ENDCLASS


METHOD QPrintEngine:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPrintEngine( ... )
   RETURN Self


METHOD QPrintEngine:abort( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrintEngine_abort( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrintEngine:metric( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPrintEngine_metric( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrintEngine:newPage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrintEngine_newPage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrintEngine:printerState( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrintEngine_printerState( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrintEngine:property( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QPrintEngine_property( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrintEngine:setProperty( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QPrintEngine_setProperty( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

