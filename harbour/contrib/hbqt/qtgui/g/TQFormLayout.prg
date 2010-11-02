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


FUNCTION QFormLayout( ... )
   RETURN HB_QFormLayout():new( ... )

FUNCTION QFormLayoutFromPointer( ... )
   RETURN HB_QFormLayout():fromPointer( ... )


CREATE CLASS QFormLayout INHERIT HbQtObjectHandler, HB_QLayout FUNCTION HB_QFormLayout

   METHOD  new( ... )

   METHOD  addRow                        // ( oQWidget, oQWidget )                             -> NIL
                                         // ( oQWidget )                                       -> NIL
                                         // ( cLabelText, oQWidget )                           -> NIL
   METHOD  fieldGrowthPolicy             // (  )                                               -> nFieldGrowthPolicy
   METHOD  formAlignment                 // (  )                                               -> nQt_Alignment
   METHOD  getItemPosition               // ( nIndex, @nRowPtr, @nRolePtr )                    -> NIL
   METHOD  getLayoutPosition             // ( oQLayout, @nRowPtr, @nRolePtr )                  -> NIL
   METHOD  getWidgetPosition             // ( oQWidget, @nRowPtr, @nRolePtr )                  -> NIL
   METHOD  horizontalSpacing             // (  )                                               -> nInt
   METHOD  insertRow                     // ( nRow, oQWidget, oQWidget )                       -> NIL
                                         // ( nRow, oQWidget, oQLayout )                       -> NIL
                                         // ( nRow, oQWidget )                                 -> NIL
                                         // ( nRow, cLabelText, oQWidget )                     -> NIL
                                         // ( nRow, cLabelText, oQLayout )                     -> NIL
                                         // ( nRow, oQLayout )                                 -> NIL
   METHOD  itemAt                        // ( nRow, nRole )                                    -> oQLayoutItem
   METHOD  labelAlignment                // (  )                                               -> nQt_Alignment
   METHOD  labelForField                 // ( oQWidget )                                       -> oQWidget
                                         // ( oQLayout )                                       -> oQWidget
   METHOD  rowCount                      // (  )                                               -> nInt
   METHOD  rowWrapPolicy                 // (  )                                               -> nRowWrapPolicy
   METHOD  setFieldGrowthPolicy          // ( nPolicy )                                        -> NIL
   METHOD  setFormAlignment              // ( nAlignment )                                     -> NIL
   METHOD  setHorizontalSpacing          // ( nSpacing )                                       -> NIL
   METHOD  setItem                       // ( nRow, nRole, oQLayoutItem )                      -> NIL
   METHOD  setLabelAlignment             // ( nAlignment )                                     -> NIL
   METHOD  setLayout                     // ( nRow, nRole, oQLayout )                          -> NIL
   METHOD  setRowWrapPolicy              // ( nPolicy )                                        -> NIL
   METHOD  setSpacing                    // ( nSpacing )                                       -> NIL
   METHOD  setVerticalSpacing            // ( nSpacing )                                       -> NIL
   METHOD  setWidget                     // ( nRow, nRole, oQWidget )                          -> NIL
   METHOD  spacing                       // (  )                                               -> nInt
   METHOD  verticalSpacing               // (  )                                               -> nInt

   ENDCLASS


METHOD QFormLayout:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFormLayout( ... )
   RETURN Self


METHOD QFormLayout:addRow( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QFormLayout_addRow_2( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QFormLayout_addRow( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFormLayout_addRow_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:fieldGrowthPolicy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFormLayout_fieldGrowthPolicy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:formAlignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFormLayout_formAlignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:getItemPosition( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QFormLayout_getItemPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:getLayoutPosition( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QFormLayout_getLayoutPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:getWidgetPosition( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QFormLayout_getWidgetPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:horizontalSpacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFormLayout_horizontalSpacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:insertRow( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 3 ) )
         CASE "QLAYOUT"
            RETURN Qt_QFormLayout_insertRow_4( ::pPtr, ... )
         CASE "QWIDGET"
            RETURN Qt_QFormLayout_insertRow_3( ::pPtr, ... )
         ENDSWITCH
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 2 ) ) + __objGetClsName( hb_pvalue( 3 ) )
         CASE "QWIDGETQLAYOUT"
            RETURN Qt_QFormLayout_insertRow_1( ::pPtr, ... )
         CASE "QWIDGETQWIDGET"
            RETURN Qt_QFormLayout_insertRow( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 2 ) )
         CASE "QWIDGET"
            RETURN Qt_QFormLayout_insertRow_2( ::pPtr, ... )
         CASE "QLAYOUT"
            RETURN Qt_QFormLayout_insertRow_5( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:itemAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QLayoutItemFromPointer( Qt_QFormLayout_itemAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:labelAlignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFormLayout_labelAlignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:labelForField( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QWIDGET"
            RETURN QWidgetFromPointer( Qt_QFormLayout_labelForField( ::pPtr, ... ) )
         CASE "QLAYOUT"
            RETURN QWidgetFromPointer( Qt_QFormLayout_labelForField_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:rowCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFormLayout_rowCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:rowWrapPolicy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFormLayout_rowWrapPolicy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:setFieldGrowthPolicy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFormLayout_setFieldGrowthPolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:setFormAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFormLayout_setFormAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:setHorizontalSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFormLayout_setHorizontalSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:setItem( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QFormLayout_setItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:setLabelAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFormLayout_setLabelAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:setLayout( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QFormLayout_setLayout( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:setRowWrapPolicy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFormLayout_setRowWrapPolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:setSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFormLayout_setSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:setVerticalSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFormLayout_setVerticalSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:setWidget( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QFormLayout_setWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:spacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFormLayout_spacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFormLayout:verticalSpacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFormLayout_verticalSpacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

