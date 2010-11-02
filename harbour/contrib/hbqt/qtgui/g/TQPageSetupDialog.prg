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


FUNCTION QPageSetupDialog( ... )
   RETURN HB_QPageSetupDialog():new( ... )

FUNCTION QPageSetupDialogFromPointer( ... )
   RETURN HB_QPageSetupDialog():fromPointer( ... )


CREATE CLASS QPageSetupDialog INHERIT HbQtObjectHandler, HB_QDialog FUNCTION HB_QPageSetupDialog

   METHOD  new( ... )

   METHOD  exec                          // (  )                                               -> nInt
   METHOD  open                          // ( oQObject, cMember )                              -> NIL
   METHOD  options                       // (  )                                               -> nPageSetupDialogOptions
   METHOD  printer                       // (  )                                               -> oQPrinter
   METHOD  setOption                     // ( nOption, lOn )                                   -> NIL
   METHOD  setOptions                    // ( nOptions )                                       -> NIL
   METHOD  setVisible                    // ( lVisible )                                       -> NIL
   METHOD  testOption                    // ( nOption )                                        -> lBool

   ENDCLASS


METHOD QPageSetupDialog:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPageSetupDialog( ... )
   RETURN Self


METHOD QPageSetupDialog:exec( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPageSetupDialog_exec( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPageSetupDialog:open( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QPageSetupDialog_open( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPageSetupDialog:options( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPageSetupDialog_options( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPageSetupDialog:printer( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPrinterFromPointer( Qt_QPageSetupDialog_printer( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPageSetupDialog:setOption( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QPageSetupDialog_setOption( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPageSetupDialog_setOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPageSetupDialog:setOptions( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPageSetupDialog_setOptions( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPageSetupDialog:setVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QPageSetupDialog_setVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPageSetupDialog:testOption( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPageSetupDialog_testOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

