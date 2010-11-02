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


FUNCTION HBQGraphicsScene( ... )
   RETURN HB_HBQGraphicsScene():new( ... )

FUNCTION HBQGraphicsSceneFromPointer( ... )
   RETURN HB_HBQGraphicsScene():fromPointer( ... )


CREATE CLASS HBQGraphicsScene INHERIT HbQtObjectHandler, HB_QGraphicsScene FUNCTION HB_HBQGraphicsScene

   METHOD  new( ... )

   METHOD  hbSetBlock                    // ( xBlock )                                         -> NIL
   METHOD  pageSize                      // (  )                                               -> nInt
   METHOD  setPageSize                   // ( nPageSize )                                      -> NIL
   METHOD  paperRect                     // (  )                                               -> oQRectF
   METHOD  setPaperRect                  // ( oQRectF )                                        -> NIL
   METHOD  orientation                   // (  )                                               -> nInt
   METHOD  setOrientation                // ( nOrientation )                                   -> NIL
   METHOD  geometry                      // (  )                                               -> oQRectF
   METHOD  setGeometry                   // ( oQRectF )                                        -> NIL
   METHOD  magnetArea                    // (  )                                               -> nInt
   METHOD  setMagnetArea                 // ( nMagnetArea )                                    -> NIL
   METHOD  showGrid                      // (  )                                               -> lBool
   METHOD  setShowGrid                   // ( lShowGrid )                                      -> NIL
   METHOD  setLeftMagnet                 // ( lMagneted )                                      -> NIL
   METHOD  setRightMagnet                // ( lMagneted )                                      -> NIL
   METHOD  setTopMagnet                  // ( lMagneted )                                      -> NIL
   METHOD  setBottomMagnet               // ( lMagneted )                                      -> NIL
   METHOD  setHorizontalMagnet           // ( lMagneted )                                      -> NIL
   METHOD  setVerticalMagnet             // ( lMagneted )                                      -> NIL

   ENDCLASS


METHOD HBQGraphicsScene:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_HBQGraphicsScene( ... )
   RETURN Self


METHOD HBQGraphicsScene:hbSetBlock( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE (  hb_pvalue( 1 ) != NIL )
         RETURN Qt_HBQGraphicsScene_hbSetBlock( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsScene:pageSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsScene_pageSize( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsScene:setPageSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsScene_setPageSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsScene:paperRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_HBQGraphicsScene_paperRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsScene:setPaperRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsScene_setPaperRect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsScene:orientation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsScene_orientation( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsScene:setOrientation( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsScene_setOrientation( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsScene:geometry( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_HBQGraphicsScene_geometry( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsScene:setGeometry( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsScene_setGeometry( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsScene:magnetArea( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsScene_magnetArea( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsScene:setMagnetArea( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsScene_setMagnetArea( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsScene:showGrid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQGraphicsScene_showGrid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsScene:setShowGrid( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsScene_setShowGrid( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsScene:setLeftMagnet( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsScene_setLeftMagnet( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsScene:setRightMagnet( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsScene_setRightMagnet( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsScene:setTopMagnet( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsScene_setTopMagnet( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsScene:setBottomMagnet( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsScene_setBottomMagnet( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsScene:setHorizontalMagnet( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsScene_setHorizontalMagnet( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQGraphicsScene:setVerticalMagnet( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_HBQGraphicsScene_setVerticalMagnet( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

