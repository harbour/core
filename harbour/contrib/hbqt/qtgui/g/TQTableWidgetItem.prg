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


FUNCTION QTableWidgetItem( ... )
   RETURN HB_QTableWidgetItem():new( ... )

FUNCTION QTableWidgetItemFromPointer( ... )
   RETURN HB_QTableWidgetItem():fromPointer( ... )


CREATE CLASS QTableWidgetItem INHERIT HbQtObjectHandler FUNCTION HB_QTableWidgetItem

   METHOD  new( ... )

   METHOD  background                    // (  )                                               -> oQBrush
   METHOD  checkState                    // (  )                                               -> nQt_CheckState
   METHOD  clone                         // (  )                                               -> oQTableWidgetItem
   METHOD  column                        // (  )                                               -> nInt
   METHOD  data                          // ( nRole )                                          -> oQVariant
   METHOD  flags                         // (  )                                               -> nQt_ItemFlags
   METHOD  font                          // (  )                                               -> oQFont
   METHOD  foreground                    // (  )                                               -> oQBrush
   METHOD  icon                          // (  )                                               -> oQIcon
   METHOD  isSelected                    // (  )                                               -> lBool
   METHOD  read                          // ( oQDataStream )                                   -> NIL
   METHOD  row                           // (  )                                               -> nInt
   METHOD  setBackground                 // ( oQBrush )                                        -> NIL
   METHOD  setCheckState                 // ( nState )                                         -> NIL
   METHOD  setData                       // ( nRole, oQVariant )                               -> NIL
   METHOD  setFlags                      // ( nFlags )                                         -> NIL
   METHOD  setFont                       // ( oQFont )                                         -> NIL
   METHOD  setForeground                 // ( oQBrush )                                        -> NIL
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
   METHOD  tableWidget                   // (  )                                               -> oQTableWidget
   METHOD  text                          // (  )                                               -> cQString
   METHOD  textAlignment                 // (  )                                               -> nInt
   METHOD  toolTip                       // (  )                                               -> cQString
   METHOD  type                          // (  )                                               -> nInt
   METHOD  whatsThis                     // (  )                                               -> cQString
   METHOD  write                         // ( oQDataStream )                                   -> NIL

   ENDCLASS


METHOD QTableWidgetItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTableWidgetItem( ... )
   RETURN Self


METHOD QTableWidgetItem:background( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QTableWidgetItem_background( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:checkState( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_checkState( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:clone( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTableWidgetItemFromPointer( Qt_QTableWidgetItem_clone( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:column( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_column( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:data( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QTableWidgetItem_data( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:flags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_flags( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:font( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QTableWidgetItem_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:foreground( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QTableWidgetItem_foreground( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:icon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QTableWidgetItem_icon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:isSelected( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_isSelected( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:read( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_read( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:row( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_row( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setBackground( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setBackground( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setCheckState( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setCheckState( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setData( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTableWidgetItem_setData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setFlags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setForeground( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setForeground( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setIcon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) )
         RETURN Qt_QTableWidgetItem_setIcon( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setSelected( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setSelected( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setSizeHint( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setSizeHint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setStatusTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setStatusTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setTextAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setTextAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setToolTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setToolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setWhatsThis( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setWhatsThis( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:sizeHint( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QTableWidgetItem_sizeHint( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:statusTip( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_statusTip( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:tableWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTableWidgetFromPointer( Qt_QTableWidgetItem_tableWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:textAlignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_textAlignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:toolTip( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_toolTip( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:type( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_type( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:whatsThis( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_whatsThis( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:write( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_write( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

