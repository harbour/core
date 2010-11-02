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


REQUEST __HBQTCORE


FUNCTION QChar( ... )
   RETURN HB_QChar():new( ... )

FUNCTION QCharFromPointer( ... )
   RETURN HB_QChar():fromPointer( ... )


CREATE CLASS QChar INHERIT HbQtObjectHandler FUNCTION HB_QChar

   METHOD  new( ... )

   METHOD  category                      // (  )                                               -> nCategory
   METHOD  combiningClass                // (  )                                               -> nChar
   METHOD  decomposition                 // (  )                                               -> cQString
   METHOD  decompositionTag              // (  )                                               -> nDecomposition
   METHOD  digitValue                    // (  )                                               -> nInt
   METHOD  direction                     // (  )                                               -> nDirection
   METHOD  hasMirrored                   // (  )                                               -> lBool
   METHOD  isDigit                       // (  )                                               -> lBool
   METHOD  isHighSurrogate               // (  )                                               -> lBool
   METHOD  isLetter                      // (  )                                               -> lBool
   METHOD  isLetterOrNumber              // (  )                                               -> lBool
   METHOD  isLowSurrogate                // (  )                                               -> lBool
   METHOD  isLower                       // (  )                                               -> lBool
   METHOD  isMark                        // (  )                                               -> lBool
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  isNumber                      // (  )                                               -> lBool
   METHOD  isPrint                       // (  )                                               -> lBool
   METHOD  isPunct                       // (  )                                               -> lBool
   METHOD  isSpace                       // (  )                                               -> lBool
   METHOD  isSymbol                      // (  )                                               -> lBool
   METHOD  isTitleCase                   // (  )                                               -> lBool
   METHOD  isUpper                       // (  )                                               -> lBool
   METHOD  joining                       // (  )                                               -> nJoining
   METHOD  mirroredChar                  // (  )                                               -> oQChar
   METHOD  toAscii                       // (  )                                               -> cChar
   METHOD  toCaseFolded                  // (  )                                               -> oQChar
   METHOD  toLatin1                      // (  )                                               -> cChar
   METHOD  toLower                       // (  )                                               -> oQChar
   METHOD  toTitleCase                   // (  )                                               -> oQChar
   METHOD  toUpper                       // (  )                                               -> oQChar
   METHOD  unicode                       // (  )                                               -> nUshort
   METHOD  unicodeVersion                // (  )                                               -> nUnicodeVersion

   ENDCLASS


METHOD QChar:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QChar( ... )
   RETURN Self


METHOD QChar:category( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_category( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:combiningClass( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_combiningClass( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:decomposition( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_decomposition( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:decompositionTag( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_decompositionTag( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:digitValue( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_digitValue( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:direction( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_direction( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:hasMirrored( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_hasMirrored( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isDigit( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isDigit( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isHighSurrogate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isHighSurrogate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isLetter( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isLetter( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isLetterOrNumber( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isLetterOrNumber( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isLowSurrogate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isLowSurrogate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isLower( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isLower( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isMark( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isMark( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isNumber( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isNumber( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isPrint( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isPrint( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isPunct( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isPunct( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isSpace( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isSpace( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isSymbol( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isSymbol( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isTitleCase( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isTitleCase( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isUpper( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isUpper( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:joining( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_joining( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:mirroredChar( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QChar_mirroredChar( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:toAscii( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_toAscii( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:toCaseFolded( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QChar_toCaseFolded( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:toLatin1( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_toLatin1( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:toLower( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QChar_toLower( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:toTitleCase( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QChar_toTitleCase( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:toUpper( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QChar_toUpper( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:unicode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_unicode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:unicodeVersion( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_unicodeVersion( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

