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


FUNCTION QTextOption( ... )
   RETURN HB_QTextOption():new( ... )

FUNCTION QTextOptionFromPointer( ... )
   RETURN HB_QTextOption():fromPointer( ... )


CREATE CLASS QTextOption INHERIT HbQtObjectHandler FUNCTION HB_QTextOption

   METHOD  new( ... )

   METHOD  alignment                     // (  )                                               -> nQt_Alignment
   METHOD  flags                         // (  )                                               -> nFlags
   METHOD  setAlignment                  // ( nAlignment )                                     -> NIL
   METHOD  setFlags                      // ( nFlags )                                         -> NIL
   METHOD  setTabStop                    // ( nTabStop )                                       -> NIL
   METHOD  setTextDirection              // ( nDirection )                                     -> NIL
   METHOD  setUseDesignMetrics           // ( lEnable )                                        -> NIL
   METHOD  setWrapMode                   // ( nMode )                                          -> NIL
   METHOD  tabArray                      // (  )                                               -> oQList_qreal>
   METHOD  tabStop                       // (  )                                               -> nQreal
   METHOD  textDirection                 // (  )                                               -> nQt_LayoutDirection
   METHOD  useDesignMetrics              // (  )                                               -> lBool
   METHOD  wrapMode                      // (  )                                               -> nWrapMode

   ENDCLASS


METHOD QTextOption:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextOption( ... )
   RETURN Self


METHOD QTextOption:alignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextOption_alignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextOption:flags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextOption_flags( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextOption:setAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextOption_setAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextOption:setFlags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextOption_setFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextOption:setTabStop( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextOption_setTabStop( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextOption:setTextDirection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextOption_setTextDirection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextOption:setUseDesignMetrics( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextOption_setUseDesignMetrics( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextOption:setWrapMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextOption_setWrapMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextOption:tabArray( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QTextOption_tabArray( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextOption:tabStop( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextOption_tabStop( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextOption:textDirection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextOption_textDirection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextOption:useDesignMetrics( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextOption_useDesignMetrics( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextOption:wrapMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextOption_wrapMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

