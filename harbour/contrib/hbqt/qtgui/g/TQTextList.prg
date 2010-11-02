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


FUNCTION QTextList( ... )
   RETURN HB_QTextList():new( ... )

FUNCTION QTextListFromPointer( ... )
   RETURN HB_QTextList():fromPointer( ... )


CREATE CLASS QTextList INHERIT HbQtObjectHandler, HB_QTextBlockGroup FUNCTION HB_QTextList

   METHOD  new( ... )

   METHOD  add                           // ( oQTextBlock )                                    -> NIL
   METHOD  count                         // (  )                                               -> nInt
   METHOD  format                        // (  )                                               -> oQTextListFormat
   METHOD  item                          // ( nI )                                             -> oQTextBlock
   METHOD  itemNumber                    // ( oQTextBlock )                                    -> nInt
   METHOD  itemText                      // ( oQTextBlock )                                    -> cQString
   METHOD  remove                        // ( oQTextBlock )                                    -> NIL
   METHOD  removeItem                    // ( nI )                                             -> NIL
   METHOD  setFormat                     // ( oQTextListFormat )                               -> NIL

   ENDCLASS


METHOD QTextList:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextList( ... )
   RETURN Self


METHOD QTextList:add( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextList_add( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextList:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextList_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextList:format( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextListFormatFromPointer( Qt_QTextList_format( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextList:item( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTextBlockFromPointer( Qt_QTextList_item( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextList:itemNumber( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextList_itemNumber( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextList:itemText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextList_itemText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextList:remove( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextList_remove( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextList:removeItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextList_removeItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextList:setFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextList_setFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

