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


FUNCTION QListWidgetItem( ... )
   RETURN HB_QListWidgetItem():new( ... )

FUNCTION QListWidgetItemFromPointer( ... )
   RETURN HB_QListWidgetItem():fromPointer( ... )


CREATE CLASS QListWidgetItem INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QListWidgetItem

   METHOD  new( ... )

   METHOD  background                    // (  )                                               -> oQBrush
   METHOD  checkState                    // (  )                                               -> nQt_CheckState
   METHOD  clone                         // (  )                                               -> oQListWidgetItem
   METHOD  data                          // ( nRole )                                          -> oQVariant
   METHOD  flags                         // (  )                                               -> nQt_ItemFlags
   METHOD  font                          // (  )                                               -> oQFont
   METHOD  foreground                    // (  )                                               -> oQBrush
   METHOD  icon                          // (  )                                               -> oQIcon
   METHOD  isHidden                      // (  )                                               -> lBool
   METHOD  isSelected                    // (  )                                               -> lBool
   METHOD  listWidget                    // (  )                                               -> oQListWidget
   METHOD  read                          // ( oQDataStream )                                   -> NIL
   METHOD  setBackground                 // ( oQBrush )                                        -> NIL
   METHOD  setCheckState                 // ( nState )                                         -> NIL
   METHOD  setData                       // ( nRole, oQVariant )                               -> NIL
   METHOD  setFlags                      // ( nFlags )                                         -> NIL
   METHOD  setFont                       // ( oQFont )                                         -> NIL
   METHOD  setForeground                 // ( oQBrush )                                        -> NIL
   METHOD  setHidden                     // ( lHide )                                          -> NIL
   METHOD  setIcon                       // ( coQIcon )                                        -> NIL
   METHOD  setSelected                   // ( lSelect )                                        -> NIL
   METHOD  setSizeHint                   // ( oQSize )                                         -> NIL
   METHOD  setStatusTip                  // ( cStatusTip )                                     -> NIL
   METHOD  setText                       // ( cText )                                          -> NIL
   METHOD  setTextAlignment              // ( nAlignment )                                     -> NIL
   METHOD  setToolTip                    // ( cToolTip )                                       -> NIL
   METHOD  setWhatsThis                  // ( cWhatsThis )                                     -> NIL
   METHOD  sizeHint                      // (  )                                               -> oQSize
   METHOD  statusTip                     // (  )                                               -> cQString
   METHOD  text                          // (  )                                               -> cQString
   METHOD  textAlignment                 // (  )                                               -> nInt
   METHOD  toolTip                       // (  )                                               -> cQString
   METHOD  type                          // (  )                                               -> nInt
   METHOD  whatsThis                     // (  )                                               -> cQString
   METHOD  write                         // ( oQDataStream )                                   -> NIL

   ENDCLASS


METHOD QListWidgetItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QListWidgetItem( ... )
   RETURN Self


METHOD QListWidgetItem:background( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QListWidgetItem_background( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:checkState( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListWidgetItem_checkState( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:clone( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListWidgetItemFromPointer( Qt_QListWidgetItem_clone( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:data( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QListWidgetItem_data( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:flags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListWidgetItem_flags( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:font( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QListWidgetItem_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:foreground( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QListWidgetItem_foreground( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:icon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QListWidgetItem_icon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:isHidden( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListWidgetItem_isHidden( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:isSelected( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListWidgetItem_isSelected( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:listWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListWidgetFromPointer( Qt_QListWidgetItem_listWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:read( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QListWidgetItem_read( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:setBackground( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QListWidgetItem_setBackground( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:setCheckState( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListWidgetItem_setCheckState( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:setData( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QListWidgetItem_setData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:setFlags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListWidgetItem_setFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:setFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QListWidgetItem_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:setForeground( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QListWidgetItem_setForeground( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:setHidden( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QListWidgetItem_setHidden( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:setIcon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) )
         RETURN Qt_QListWidgetItem_setIcon( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:setSelected( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QListWidgetItem_setSelected( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:setSizeHint( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QListWidgetItem_setSizeHint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:setStatusTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QListWidgetItem_setStatusTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:setText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QListWidgetItem_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:setTextAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListWidgetItem_setTextAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:setToolTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QListWidgetItem_setToolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:setWhatsThis( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QListWidgetItem_setWhatsThis( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:sizeHint( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QListWidgetItem_sizeHint( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:statusTip( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListWidgetItem_statusTip( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListWidgetItem_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:textAlignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListWidgetItem_textAlignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:toolTip( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListWidgetItem_toolTip( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:type( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListWidgetItem_type( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:whatsThis( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListWidgetItem_whatsThis( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListWidgetItem:write( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QListWidgetItem_write( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

