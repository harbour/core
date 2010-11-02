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


FUNCTION QWizard( ... )
   RETURN HB_QWizard():new( ... )

FUNCTION QWizardFromPointer( ... )
   RETURN HB_QWizard():fromPointer( ... )


CREATE CLASS QWizard INHERIT HbQtObjectHandler, HB_QDialog FUNCTION HB_QWizard

   METHOD  new( ... )

   METHOD  addPage                       // ( oQWizardPage )                                   -> nInt
   METHOD  button                        // ( nWhich )                                         -> oQAbstractButton
   METHOD  buttonText                    // ( nWhich )                                         -> cQString
   METHOD  currentId                     // (  )                                               -> nInt
   METHOD  currentPage                   // (  )                                               -> oQWizardPage
   METHOD  field                         // ( cName )                                          -> oQVariant
   METHOD  hasVisitedPage                // ( nId )                                            -> lBool
   METHOD  nextId                        // (  )                                               -> nInt
   METHOD  options                       // (  )                                               -> nWizardOptions
   METHOD  page                          // ( nId )                                            -> oQWizardPage
   METHOD  pageIds                       // (  )                                               -> oQList_int>
   METHOD  pixmap                        // ( nWhich )                                         -> oQPixmap
   METHOD  removePage                    // ( nId )                                            -> NIL
   METHOD  setButton                     // ( nWhich, oQAbstractButton )                       -> NIL
   METHOD  setButtonText                 // ( nWhich, cText )                                  -> NIL
   METHOD  setDefaultProperty            // ( cClassName, cProperty, cChangedSignal )          -> NIL
   METHOD  setField                      // ( cName, oQVariant )                               -> NIL
   METHOD  setOption                     // ( nOption, lOn )                                   -> NIL
   METHOD  setOptions                    // ( nOptions )                                       -> NIL
   METHOD  setPage                       // ( nId, oQWizardPage )                              -> NIL
   METHOD  setPixmap                     // ( nWhich, oQPixmap )                               -> NIL
   METHOD  setStartId                    // ( nId )                                            -> NIL
   METHOD  setSubTitleFormat             // ( nFormat )                                        -> NIL
   METHOD  setTitleFormat                // ( nFormat )                                        -> NIL
   METHOD  setWizardStyle                // ( nStyle )                                         -> NIL
   METHOD  startId                       // (  )                                               -> nInt
   METHOD  subTitleFormat                // (  )                                               -> nQt_TextFormat
   METHOD  testOption                    // ( nOption )                                        -> lBool
   METHOD  titleFormat                   // (  )                                               -> nQt_TextFormat
   METHOD  validateCurrentPage           // (  )                                               -> lBool
   METHOD  visitedPages                  // (  )                                               -> oQList_int>
   METHOD  wizardStyle                   // (  )                                               -> nWizardStyle
   METHOD  back                          // (  )                                               -> NIL
   METHOD  next                          // (  )                                               -> NIL
   METHOD  restart                       // (  )                                               -> NIL

   ENDCLASS


METHOD QWizard:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWizard( ... )
   RETURN Self


METHOD QWizard:addPage( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_addPage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:button( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QAbstractButtonFromPointer( Qt_QWizard_button( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:buttonText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_buttonText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:currentId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_currentId( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:currentPage( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWizardPageFromPointer( Qt_QWizard_currentPage( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:field( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QWizard_field( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:hasVisitedPage( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_hasVisitedPage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:nextId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_nextId( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:options( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_options( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:page( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QWizardPageFromPointer( Qt_QWizard_page( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:pageIds( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QWizard_pageIds( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:pixmap( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QPixmapFromPointer( Qt_QWizard_pixmap( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:removePage( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_removePage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:setButton( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QWizard_setButton( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:setButtonText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QWizard_setButtonText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:setDefaultProperty( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QWizard_setDefaultProperty( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:setField( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QWizard_setField( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:setOption( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QWizard_setOption( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_setOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:setOptions( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_setOptions( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:setPage( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QWizard_setPage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:setPixmap( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QWizard_setPixmap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:setStartId( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_setStartId( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:setSubTitleFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_setSubTitleFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:setTitleFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_setTitleFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:setWizardStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_setWizardStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:startId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_startId( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:subTitleFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_subTitleFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:testOption( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWizard_testOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:titleFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_titleFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:validateCurrentPage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_validateCurrentPage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:visitedPages( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QWizard_visitedPages( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:wizardStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_wizardStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:back( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_back( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:next( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_next( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWizard:restart( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWizard_restart( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

