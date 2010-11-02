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


FUNCTION QMessageBox( ... )
   RETURN HB_QMessageBox():new( ... )

FUNCTION QMessageBoxFromPointer( ... )
   RETURN HB_QMessageBox():fromPointer( ... )


CREATE CLASS QMessageBox INHERIT HbQtObjectHandler, HB_QDialog FUNCTION HB_QMessageBox

   METHOD  new( ... )

   METHOD  addButton                     // ( oQAbstractButton, nRole )                        -> NIL
                                         // ( cText, nRole )                                   -> oQPushButton
                                         // ( nButton )                                        -> oQPushButton
   METHOD  button                        // ( nWhich )                                         -> oQAbstractButton
   METHOD  buttonRole                    // ( oQAbstractButton )                               -> nButtonRole
   METHOD  buttons                       // (  )                                               -> oQList_QAbstractButton
   METHOD  clickedButton                 // (  )                                               -> oQAbstractButton
   METHOD  defaultButton                 // (  )                                               -> oQPushButton
   METHOD  detailedText                  // (  )                                               -> cQString
   METHOD  escapeButton                  // (  )                                               -> oQAbstractButton
   METHOD  icon                          // (  )                                               -> nIcon
   METHOD  iconPixmap                    // (  )                                               -> oQPixmap
   METHOD  informativeText               // (  )                                               -> cQString
   METHOD  open                          // ( oQObject, cMember )                              -> NIL
   METHOD  removeButton                  // ( oQAbstractButton )                               -> NIL
   METHOD  setDefaultButton              // ( oQPushButton )                                   -> NIL
                                         // ( nButton )                                        -> NIL
   METHOD  setDetailedText               // ( cText )                                          -> NIL
   METHOD  setEscapeButton               // ( oQAbstractButton )                               -> NIL
                                         // ( nButton )                                        -> NIL
   METHOD  setIcon                       // ( nIcon )                                          -> NIL
   METHOD  setIconPixmap                 // ( oQPixmap )                                       -> NIL
   METHOD  setInformativeText            // ( cText )                                          -> NIL
   METHOD  setStandardButtons            // ( nButtons )                                       -> NIL
   METHOD  setText                       // ( cText )                                          -> NIL
   METHOD  setTextFormat                 // ( nFormat )                                        -> NIL
   METHOD  setWindowModality             // ( nWindowModality )                                -> NIL
   METHOD  setWindowTitle                // ( cTitle )                                         -> NIL
   METHOD  standardButton                // ( oQAbstractButton )                               -> nStandardButton
   METHOD  standardButtons               // (  )                                               -> nStandardButtons
   METHOD  text                          // (  )                                               -> cQString
   METHOD  textFormat                    // (  )                                               -> nQt_TextFormat
   METHOD  about                         // ( oQWidget, cTitle, cText )                        -> NIL
   METHOD  aboutQt                       // ( oQWidget, cTitle )                               -> NIL
   METHOD  critical                      // ( oQWidget, cTitle, cText, nButtons, nDefaultButton ) -> nStandardButton
   METHOD  information                   // ( oQWidget, cTitle, cText, nButtons, nDefaultButton ) -> nStandardButton
   METHOD  question                      // ( oQWidget, cTitle, cText, nButtons, nDefaultButton ) -> nStandardButton
   METHOD  warning                       // ( oQWidget, cTitle, cText, nButtons, nDefaultButton ) -> nStandardButton
   METHOD  exec                          // (  )                                               -> nInt

   ENDCLASS


METHOD QMessageBox:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMessageBox( ... )
   RETURN Self


METHOD QMessageBox:addButton( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QPushButtonFromPointer( Qt_QMessageBox_addButton_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QMessageBox_addButton( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QPushButtonFromPointer( Qt_QMessageBox_addButton_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:button( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QAbstractButtonFromPointer( Qt_QMessageBox_button( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:buttonRole( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMessageBox_buttonRole( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:buttons( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QMessageBox_buttons( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:clickedButton( ... )
   SWITCH PCount()
   CASE 0
      RETURN QAbstractButtonFromPointer( Qt_QMessageBox_clickedButton( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:defaultButton( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPushButtonFromPointer( Qt_QMessageBox_defaultButton( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:detailedText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMessageBox_detailedText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:escapeButton( ... )
   SWITCH PCount()
   CASE 0
      RETURN QAbstractButtonFromPointer( Qt_QMessageBox_escapeButton( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:icon( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMessageBox_icon( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:iconPixmap( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPixmapFromPointer( Qt_QMessageBox_iconPixmap( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:informativeText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMessageBox_informativeText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:open( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QMessageBox_open( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:removeButton( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMessageBox_removeButton( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:setDefaultButton( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMessageBox_setDefaultButton_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMessageBox_setDefaultButton( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:setDetailedText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QMessageBox_setDetailedText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:setEscapeButton( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMessageBox_setEscapeButton_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMessageBox_setEscapeButton( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:setIcon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMessageBox_setIcon( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:setIconPixmap( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMessageBox_setIconPixmap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:setInformativeText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QMessageBox_setInformativeText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:setStandardButtons( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMessageBox_setStandardButtons( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:setText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QMessageBox_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:setTextFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMessageBox_setTextFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:setWindowModality( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMessageBox_setWindowModality( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:setWindowTitle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QMessageBox_setWindowTitle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:standardButton( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMessageBox_standardButton( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:standardButtons( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMessageBox_standardButtons( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMessageBox_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:textFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMessageBox_textFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:about( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QMessageBox_about( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:aboutQt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QMessageBox_aboutQt( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMessageBox_aboutQt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:critical( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QMessageBox_critical( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QMessageBox_critical( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QMessageBox_critical( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:information( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QMessageBox_information( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QMessageBox_information( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QMessageBox_information( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:question( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QMessageBox_question( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QMessageBox_question( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QMessageBox_question( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:warning( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QMessageBox_warning( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QMessageBox_warning( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QMessageBox_warning( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMessageBox:exec( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMessageBox_exec( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

