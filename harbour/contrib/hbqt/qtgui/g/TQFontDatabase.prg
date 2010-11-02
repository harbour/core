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


FUNCTION QFontDatabase( ... )
   RETURN HB_QFontDatabase():new( ... )

FUNCTION QFontDatabaseFromPointer( ... )
   RETURN HB_QFontDatabase():fromPointer( ... )


CREATE CLASS QFontDatabase INHERIT HbQtObjectHandler FUNCTION HB_QFontDatabase

   METHOD  new( ... )

   METHOD  bold                          // ( cFamily, cStyle )                                -> lBool
   METHOD  families                      // ( nWritingSystem )                                 -> oQStringList
   METHOD  font                          // ( cFamily, cStyle, nPointSize )                    -> oQFont
   METHOD  isBitmapScalable              // ( cFamily, cStyle )                                -> lBool
   METHOD  isFixedPitch                  // ( cFamily, cStyle )                                -> lBool
   METHOD  isScalable                    // ( cFamily, cStyle )                                -> lBool
   METHOD  isSmoothlyScalable            // ( cFamily, cStyle )                                -> lBool
   METHOD  italic                        // ( cFamily, cStyle )                                -> lBool
   METHOD  pointSizes                    // ( cFamily, cStyle )                                -> oQList_int>
   METHOD  smoothSizes                   // ( cFamily, cStyle )                                -> oQList_int>
   METHOD  styleString                   // ( oQFont )                                         -> cQString
                                         // ( oQFontInfo )                                     -> cQString
   METHOD  styles                        // ( cFamily )                                        -> oQStringList
   METHOD  weight                        // ( cFamily, cStyle )                                -> nInt
   METHOD  addApplicationFont            // ( cFileName )                                      -> nInt
   METHOD  addApplicationFontFromData    // ( oQByteArray )                                    -> nInt
   METHOD  applicationFontFamilies       // ( nId )                                            -> oQStringList
   METHOD  removeAllApplicationFonts     // (  )                                               -> lBool
   METHOD  removeApplicationFont         // ( nId )                                            -> lBool
   METHOD  standardSizes                 // (  )                                               -> oQList_int>
   METHOD  supportsThreadedFontRendering // (  )                                               -> lBool
   METHOD  writingSystemName             // ( nWritingSystem )                                 -> cQString
   METHOD  writingSystemSample           // ( nWritingSystem )                                 -> cQString

   ENDCLASS


METHOD QFontDatabase:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFontDatabase( ... )
   RETURN Self


METHOD QFontDatabase:bold( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFontDatabase_bold( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDatabase:families( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QStringListFromPointer( Qt_QFontDatabase_families( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QStringListFromPointer( Qt_QFontDatabase_families( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDatabase:font( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN QFontFromPointer( Qt_QFontDatabase_font( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDatabase:isBitmapScalable( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFontDatabase_isBitmapScalable( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFontDatabase_isBitmapScalable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDatabase:isFixedPitch( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFontDatabase_isFixedPitch( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFontDatabase_isFixedPitch( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDatabase:isScalable( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFontDatabase_isScalable( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFontDatabase_isScalable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDatabase:isSmoothlyScalable( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFontDatabase_isSmoothlyScalable( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFontDatabase_isSmoothlyScalable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDatabase:italic( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFontDatabase_italic( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDatabase:pointSizes( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QListFromPointer( Qt_QFontDatabase_pointSizes( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QListFromPointer( Qt_QFontDatabase_pointSizes( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDatabase:smoothSizes( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QListFromPointer( Qt_QFontDatabase_smoothSizes( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDatabase:styleString( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QFONT"
            RETURN Qt_QFontDatabase_styleString( ::pPtr, ... )
         CASE "QFONTINFO"
            RETURN Qt_QFontDatabase_styleString_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDatabase:styles( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QStringListFromPointer( Qt_QFontDatabase_styles( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDatabase:weight( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFontDatabase_weight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDatabase:addApplicationFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFontDatabase_addApplicationFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDatabase:addApplicationFontFromData( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFontDatabase_addApplicationFontFromData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDatabase:applicationFontFamilies( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QStringListFromPointer( Qt_QFontDatabase_applicationFontFamilies( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDatabase:removeAllApplicationFonts( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontDatabase_removeAllApplicationFonts( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDatabase:removeApplicationFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFontDatabase_removeApplicationFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDatabase:standardSizes( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QFontDatabase_standardSizes( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDatabase:supportsThreadedFontRendering( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontDatabase_supportsThreadedFontRendering( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDatabase:writingSystemName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFontDatabase_writingSystemName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDatabase:writingSystemSample( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFontDatabase_writingSystemSample( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

