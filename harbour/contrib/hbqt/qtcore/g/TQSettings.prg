/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


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

