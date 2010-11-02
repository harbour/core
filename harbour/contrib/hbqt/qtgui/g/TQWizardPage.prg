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


FUNCTION QWizardPage( ... )
   RETURN HB_QWizardPage():new( ... )

FUNCTION QWizardPageFromPointer( ... )
   RETURN HB_QWizardPage():fromPointer( ... )


CREATE CLASS QWizardPage INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QWizardPage

   METHOD  new( ... )

   METHOD  buttonText                    // ( nWhich )                                         -> cQString
   METHOD  cleanupPage                   // (  )                                               -> NIL
   METHOD  initializePage                // (  )                                               -> NIL
   METHOD  isCommitPage                  // (  )                                               -> lBool
   METHOD  isComplete                    // (  )                                               -> lBool
   METHOD  isFinalPage                   // (  )                                               -> lBool
   METHOD  nextId                        // (  )                                               -> nInt
   METHOD  pixmap                        // ( nWhich )                                         -> oQPixmap
   METHOD  setButtonText                 // ( nWhich, cText )                                  -> NIL
   METHOD  setCommitPage                 // ( lCommitPage )                                    -> NIL
   METHOD  setFinalPage                  // ( lFinalPage )                                     -> NIL
   METHOD  setPixmap                     // ( nWhich, oQPixmap )                               -> NIL
   METHOD  setSubTitle                   // ( cSubTitle )                                      -> NIL
   METHOD  setTitle                      // ( cTitle )                                         -> NIL
   METHOD  subTitle                      // (  )                                               -> cQString
   METHOD  title                         // (  )                                               -> cQString
   METHOD  validatePage                  // (  )                                               -> lBool

   ENDCLASS


METHOD QWizardPage:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWizardPage( ... )
   RETURN Self


METHOD QWizardPage:buttonText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizardPage_buttonText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:cleanupPage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizardPage_cleanupPage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:initializePage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizardPage_initializePage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:isCommitPage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizardPage_isCommitPage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:isComplete( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizardPage_isComplete( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:isFinalPage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizardPage_isFinalPage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:nextId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizardPage_nextId( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:pixmap( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QPixmapFromPointer( Qt_QWizardPage_pixmap( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:setButtonText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QWizardPage_setButtonText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:setCommitPage( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QWizardPage_setCommitPage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:setFinalPage( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QWizardPage_setFinalPage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:setPixmap( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QWizardPage_setPixmap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:setSubTitle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QWizardPage_setSubTitle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:setTitle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QWizardPage_setTitle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:subTitle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizardPage_subTitle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:title( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizardPage_title( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizardPage:validatePage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizardPage_validatePage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

