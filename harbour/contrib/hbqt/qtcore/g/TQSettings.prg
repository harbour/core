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


FUNCTION QSettings( ... )
   RETURN HB_QSettings():new( ... )

FUNCTION QSettingsFromPointer( ... )
   RETURN HB_QSettings():fromPointer( ... )


CREATE CLASS QSettings INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QSettings

   METHOD  new( ... )

   METHOD  allKeys                       // (  )                                               -> oQStringList
   METHOD  applicationName               // (  )                                               -> cQString
   METHOD  beginGroup                    // ( cPrefix )                                        -> NIL
   METHOD  beginReadArray                // ( cPrefix )                                        -> nInt
   METHOD  beginWriteArray               // ( cPrefix, nSize )                                 -> NIL
   METHOD  childGroups                   // (  )                                               -> oQStringList
   METHOD  childKeys                     // (  )                                               -> oQStringList
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  contains                      // ( cKey )                                           -> lBool
   METHOD  endArray                      // (  )                                               -> NIL
   METHOD  endGroup                      // (  )                                               -> NIL
   METHOD  fallbacksEnabled              // (  )                                               -> lBool
   METHOD  fileName                      // (  )                                               -> cQString
   METHOD  format                        // (  )                                               -> nFormat
   METHOD  group                         // (  )                                               -> cQString
   METHOD  iniCodec                      // (  )                                               -> oQTextCodec
   METHOD  isWritable                    // (  )                                               -> lBool
   METHOD  organizationName              // (  )                                               -> cQString
   METHOD  remove                        // ( cKey )                                           -> NIL
   METHOD  scope                         // (  )                                               -> nScope
   METHOD  setArrayIndex                 // ( nI )                                             -> NIL
   METHOD  setFallbacksEnabled           // ( lB )                                             -> NIL
   METHOD  setIniCodec                   // ( oQTextCodec )                                    -> NIL
                                         // ( cCodecName )                                     -> NIL
   METHOD  setValue                      // ( cKey, oQVariant )                                -> NIL
   METHOD  status                        // (  )                                               -> nStatus
   METHOD  sync                          // (  )                                               -> NIL
   METHOD  value                         // ( cKey, oQVariant )                                -> oQVariant
   METHOD  defaultFormat                 // (  )                                               -> nFormat
   METHOD  setDefaultFormat              // ( nFormat )                                        -> NIL
   METHOD  setPath                       // ( nFormat, nScope, cPath )                         -> NIL

   ENDCLASS


METHOD QSettings:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSettings( ... )
   RETURN Self


METHOD QSettings:allKeys( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QSettings_allKeys( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:applicationName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSettings_applicationName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:beginGroup( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QSettings_beginGroup( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:beginReadArray( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QSettings_beginReadArray( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:beginWriteArray( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QSettings_beginWriteArray( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QSettings_beginWriteArray( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:childGroups( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QSettings_childGroups( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:childKeys( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QSettings_childKeys( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSettings_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:contains( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QSettings_contains( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:endArray( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSettings_endArray( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:endGroup( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSettings_endGroup( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:fallbacksEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSettings_fallbacksEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:fileName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSettings_fileName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:format( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSettings_format( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:group( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSettings_group( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:iniCodec( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCodecFromPointer( Qt_QSettings_iniCodec( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:isWritable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSettings_isWritable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:organizationName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSettings_organizationName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:remove( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QSettings_remove( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:scope( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSettings_scope( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:setArrayIndex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSettings_setArrayIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:setFallbacksEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QSettings_setFallbacksEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:setIniCodec( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QSettings_setIniCodec_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QSettings_setIniCodec( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:setValue( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QSettings_setValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:status( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSettings_status( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:sync( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSettings_sync( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:value( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QVariantFromPointer( Qt_QSettings_value( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QSettings_value( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:defaultFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSettings_defaultFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:setDefaultFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSettings_setDefaultFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSettings:setPath( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QSettings_setPath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

