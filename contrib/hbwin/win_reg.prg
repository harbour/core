/*
 * Harbour Project source code:
 * Windows registry API
 *
 * Copyright 2008-2009 Viktor Szakats (harbour syenar.net)
 * Copyright 2004 Peter Rees <peter@rees.co.nz> Rees Software and Systems Ltd
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "hbwin.ch"

/* Predefined Value Types. from winnt.h */
#define KEY_QUERY_VALUE                1
#define KEY_SET_VALUE                  2
#define KEY_CREATE_SUB_KEY             4
#define KEY_ENUMERATE_SUB_KEYS         8
#define KEY_NOTIFY                     16
#define KEY_CREATE_LINK                32

PROCEDURE win_regPathSplit( cRegPath, /* @ */ nHKEY, /* @ */ cKey, /* @ */ cEntry )

   LOCAL cHKEY
   LOCAL tmp

   nHKEY := WIN_HKEY_CURRENT_USER
   cKey := ""
   cEntry := ""

   tmp := At( "\", cRegPath )
   IF tmp > 0
      cHKEY := Left( cRegPath, tmp - 1 )
      cRegPath := SubStr( cRegPath, tmp + 1 )

      tmp := RAt( "\", cRegPath )
      IF tmp > 0
         cKey := Left( cRegPath, tmp - 1 )
         cEntry := SubStr( cRegPath, tmp + 1 )
      ELSE
         cEntry := cRegPath
      ENDIF

      /* Len( <literal> ) is optimized to a number by Harbour at compile time. */
      DO CASE
      CASE Left( cHKEY, Len( "HKCU"                  ) ) == "HKCU"                  ; nHKEY := WIN_HKEY_CURRENT_USER
      CASE Left( cHKEY, Len( "HKLM"                  ) ) == "HKLM"                  ; nHKEY := WIN_HKEY_LOCAL_MACHINE
      CASE Left( cHKEY, Len( "HKCR"                  ) ) == "HKCR"                  ; nHKEY := WIN_HKEY_CLASSES_ROOT
      CASE Left( cHKEY, Len( "HKU"                   ) ) == "HKU"                   ; nHKEY := WIN_HKEY_USERS
      CASE Left( cHKEY, Len( "HKPD"                  ) ) == "HKPD"                  ; nHKEY := WIN_HKEY_PERFORMANCE_DATA
      CASE Left( cHKEY, Len( "HKCC"                  ) ) == "HKCC"                  ; nHKEY := WIN_HKEY_CURRENT_CONFIG
      CASE Left( cHKEY, Len( "HKDD"                  ) ) == "HKDD"                  ; nHKEY := WIN_HKEY_DYN_DATA
      CASE Left( cHKEY, Len( "HKEY_CURRENT_USER"     ) ) == "HKEY_CURRENT_USER"     ; nHKEY := WIN_HKEY_CURRENT_USER
      CASE Left( cHKEY, Len( "HKEY_LOCAL_MACHINE"    ) ) == "HKEY_LOCAL_MACHINE"    ; nHKEY := WIN_HKEY_LOCAL_MACHINE
      CASE Left( cHKEY, Len( "HKEY_CLASSES_ROOT"     ) ) == "HKEY_CLASSES_ROOT"     ; nHKEY := WIN_HKEY_CLASSES_ROOT
      CASE Left( cHKEY, Len( "HKEY_USERS"            ) ) == "HKEY_USERS"            ; nHKEY := WIN_HKEY_USERS
      CASE Left( cHKEY, Len( "HKEY_PERFORMANCE_DATA" ) ) == "HKEY_PERFORMANCE_DATA" ; nHKEY := WIN_HKEY_PERFORMANCE_DATA
      CASE Left( cHKEY, Len( "HKEY_CURRENT_CONFIG"   ) ) == "HKEY_CURRENT_CONFIG"   ; nHKEY := WIN_HKEY_CURRENT_CONFIG
      CASE Left( cHKEY, Len( "HKEY_DYN_DATA"         ) ) == "HKEY_DYN_DATA"         ; nHKEY := WIN_HKEY_DYN_DATA
      ENDCASE
   ENDIF

   RETURN

FUNCTION win_regRead( cRegPath, xDefault, nRegSam )

   LOCAL nHKEY, cKey, cEntry

   win_regPathSplit( cRegPath, @nHKEY, @cKey, @cEntry )

   RETURN win_regGet( nHKEY, cKey, cEntry, xDefault, nRegSam )

FUNCTION win_regWrite( cRegPath, xValue, nType, nRegSam )

   LOCAL nHKEY, cKey, cEntry

   win_regPathSplit( cRegPath, @nHKEY, @cKey, @cEntry )

   RETURN win_regSet( nHKEY, cKey, cEntry, xValue, nType, nRegSam )

FUNCTION win_regDelete( cRegPath, nRegSam )

   LOCAL nHKEY, cKey, cEntry
   LOCAL lRetVal
   LOCAL pKeyHandle

   win_regPathSplit( cRegPath, @nHKEY, @cKey, @cEntry )

   IF Empty( cEntry )
      lRetVal := win_regDeleteKey( nHKEY, cKey )
   ELSE
      hb_default( @nRegSam, 0 )

      IF win_regOpenKeyEx( nHKEY, cKey, 0, hb_bitOr( KEY_SET_VALUE, nRegSam ), @pKeyHandle )
         lRetVal := win_regDeleteValue( pKeyHandle, cEntry )
         win_regCloseKey( pKeyHandle )
      ELSE
         lRetVal := .F.
      ENDIF
   ENDIF

   RETURN lRetVal

FUNCTION win_regQuery( nHKEY, cKeyName, cEntryName, xValue, lSetIt, nRegSam )

   LOCAL xKey := win_regGet( nHKEY, cKeyName, cEntryName,, nRegSam )

   LOCAL cValType := ValType( xValue )
   LOCAL lRetVal

   hb_default( @lSetIt, .F. )

   IF cValType == "L"
      xValue := iif( xValue, 1, 0 )
      cValType := ValType( xValue )
   ELSEIF cValType == "D"
      xValue := DToS( xValue )
      cValType := ValType( xValue )
   ENDIF

   lRetVal := ( xKey != NIL .AND. xValue != NIL .AND. cValType == ValType( xKey ) .AND. xValue == xKey )
   IF ! lRetVal .AND. lSetIt
      lRetVal := win_regSet( nHKEY, cKeyName, cEntryName, xValue,, nRegSam )
   ENDIF

   RETURN lRetVal

STATIC FUNCTION Bin2U( c )

   LOCAL l := Bin2L( c )

   RETURN iif( l < 0, l + 4294967296, l )

FUNCTION win_regGet( nHKEY, cKeyName, cEntryName, xDefault, nRegSam )

   LOCAL xRetVal
   LOCAL pKeyHandle
   LOCAL nValueType

   hb_default( @nRegSam, 0 )

   IF win_regOpenKeyEx( nHKEY, cKeyName, 0, hb_bitOr( KEY_QUERY_VALUE, nRegSam ), @pKeyHandle )

      /* retrieve the length of the value */

      win_regQueryValueEx( pKeyHandle, cEntryName, 0, @nValueType, @xRetVal )

      IF HB_ISSTRING( xRetVal )
         DO CASE
         CASE nValueType == WIN_REG_DWORD .OR. ;
              nValueType == WIN_REG_DWORD_LITTLE_ENDIAN
            xRetVal := Bin2U( xRetVal )
         CASE nValueType == WIN_REG_DWORD_BIG_ENDIAN
            xRetVal := Bin2U( hb_BRight( xRetVal, 2 ) + hb_BLeft( xRetVal, 2 ) )
         CASE nValueType == WIN_REG_QWORD .OR. ;
              nValueType == WIN_REG_QWORD_LITTLE_ENDIAN
            xRetVal := hb_bitShift( Bin2U( hb_BSubStr( xRetVal, 5, 4 ) ), 32 ) +;
                                    Bin2U( hb_BSubStr( xRetVal, 1, 4 ) )
         OTHERWISE
            /* Strip ending zero byte */
            IF hb_BRight( xRetVal, 1 ) == hb_BChar( 0 )
               xRetVal := hb_BLeft( xRetVal, hb_BLen( xRetVal ) - 1 )
            ENDIF
         ENDCASE
      ELSE
         xRetVal := xDefault
      ENDIF

      win_regCloseKey( pKeyHandle )
   ELSE
      xRetVal := xDefault
   ENDIF

   RETURN xRetVal

FUNCTION win_regSet( nHKEY, cKeyName, cEntryName, xValue, nValueType, nRegSam )

   LOCAL cName
   LOCAL lRetVal := .F.
   LOCAL pKeyHandle

   hb_default( @nRegSam, 0 )

   IF win_regCreateKeyEx( nHKEY, cKeyName, 0, 0, 0, hb_bitOr( KEY_SET_VALUE, nRegSam ), 0, @pKeyHandle )

      /* no support for Arrays, Codeblock ... */
      SWITCH ValType( xValue )
      CASE "L"
         nValueType := WIN_REG_DWORD
         cName := iif( xValue, 1, 0 )
         EXIT
      CASE "D"
         nValueType := WIN_REG_SZ
         cName := DToS( xValue )
         EXIT
      CASE "N"
         IF ! HB_ISNUMERIC( nValueType ) .OR. ;
            !( nValueType == WIN_REG_DWORD .OR. ;
               nValueType == WIN_REG_DWORD_LITTLE_ENDIAN .OR. ;
               nValueType == WIN_REG_DWORD_BIG_ENDIAN .OR. ;
               nValueType == WIN_REG_QWORD .OR. ;
               nValueType == WIN_REG_QWORD_LITTLE_ENDIAN )
            nValueType := WIN_REG_DWORD
         ENDIF
         cName := xValue
         EXIT
      CASE "C"
      CASE "M"
         IF ! HB_ISNUMERIC( nValueType ) .OR. ;
            !( nValueType == WIN_REG_SZ .OR. ;
               nValueType == WIN_REG_EXPAND_SZ .OR. ;
               nValueType == WIN_REG_MULTI_SZ )
            nValueType := WIN_REG_SZ
         ENDIF
         cName := xValue
         EXIT
      ENDSWITCH

      IF cName != NIL
         lRetVal := win_regSetValueEx( pKeyHandle, cEntryName, 0, nValueType, cName )
      ENDIF

      win_regCloseKey( pKeyHandle )
   ENDIF

   RETURN lRetVal
