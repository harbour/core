/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HBDOC reader
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
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

#include "directry.ch"
#include "fileio.ch"

#define _HBDOC_SRC_SUBDIR       "doc"
#define _HBDOC_SRC_EXT          ".txt"

#define _HBDOC_ADD_MSG( a, m )  IF HB_ISARRAY( a ); AAdd( a, m ); ENDIF

FUNCTION __hbdoc_FromSource( cFile, aErrMsg )
   LOCAL aEntry := {}

   IF HB_ISSTRING( cFile )
      __hbdoc__read_stream( aEntry, cFile, "(stream)",, aErrMsg )
   ENDIF

   RETURN aEntry

FUNCTION __hbdoc_LoadDir( cDir, cName, aErrMsg )
   LOCAL hMeta
   LOCAL nCount
   LOCAL aFile
   LOCAL aEntry

   IF HB_ISSTRING( cDir )

      cDir := hb_DirSepAdd( cDir )

      IF hb_DirExists( cDir + _HBDOC_SRC_SUBDIR )

         aEntry := {}
         hMeta := { => }

         IF HB_ISSTRING( cName )
            hMeta[ "_COMPONENT" ] := cName
         ENDIF

         nCount := 0
         FOR EACH aFile IN Directory( cDir + _HBDOC_SRC_SUBDIR + hb_ps() + hb_osFileMask(), "D" )
            IF "D" $ aFile[ F_ATTR ] .AND. ;
               !( aFile[ F_NAME ] == "." ) .AND. ;
               !( aFile[ F_NAME ] == ".." )

               __hbdoc__read_langdir( aEntry, cDir + _HBDOC_SRC_SUBDIR + hb_ps() + aFile[ F_NAME ], hMeta, aErrMsg )
               ++nCount
            ENDIF
         NEXT

         IF nCount == 0
            _HBDOC_ADD_MSG( aErrMsg, hb_StrFormat( "Warning: Component (%1$s) has no language subdirs", cDir ) )
         ENDIF
      ENDIF
   ENDIF

   RETURN aEntry

STATIC PROCEDURE __hbdoc__read_langdir( aEntry, cDir, hMeta, aErrMsg )
   LOCAL aFile
   LOCAL nCount

   nCount := 0
   FOR EACH aFile IN Directory( cDir + hb_ps() + "*" + _HBDOC_SRC_EXT )
      hMeta[ "_LANG" ] := aFile[ F_NAME ]
      __hbdoc__read_file( aEntry, cDir + hb_ps() + aFile[ F_NAME ], hMeta, aErrMsg )
      ++nCount
   NEXT

   IF nCount == 0
      _HBDOC_ADD_MSG( aErrMsg, hb_StrFormat( "Warning: Component (%1$s) has no documentation files", cDir ) )
   ENDIF

   RETURN

STATIC PROCEDURE __hbdoc__read_file( aEntry, cFileName, hMeta, aErrMsg )
   LOCAL aFilenameTemplateMap := {;
      "FUNCTION"   => "func_"  ,;
      "C FUNCTION" => "cfunc_" ,;
      "CLASS"      => "class_" ,;
      "COMMAND"    => "cmd_"   ,;
      "PP"         => "pp_"    }

   LOCAL tmp

   IF "TEMPLATE" $ hMeta
      hb_HDel( hMeta, "TEMPLATE" )
   ENDIF

   /* Preselect the default template based on source filename */
   FOR EACH tmp IN aFilenameTemplateMap
      IF Lower( Left( cFileName, Len( tmp ) ) ) == tmp
         hMeta[ "TEMPLATE" ] := tmp:__enumKey()
      ENDIF
   NEXT

   hMeta[ "_DOCSOURCE" ] := cFileName

   __hbdoc__read_stream( aEntry, MemoRead( cFileName ), cFileName, hMeta, aErrMsg )

   RETURN

STATIC PROCEDURE __hbdoc__read_stream( aEntry, cFile, cFileName, hMeta, aErrMsg )
   LOCAL hEntry := NIL
   LOCAL cLine
   LOCAL cSection
   LOCAL tmp
   LOCAL nLine
   LOCAL nStartCol

   cFile := StrTran( cFile, Chr( 13 ) )
   cFile := StrTran( cFile, Chr( 9 ), " " )

   nLine := 0
   FOR EACH cLine IN hb_ATokens( cFile, Chr( 10 ) )

      cLine := SubStr( cLine, 4 )
      ++nLine

      SWITCH AllTrim( cLine )
      CASE "$DOC$"
         IF hEntry != NIL
            _HBDOC_ADD_MSG( aErrMsg, hb_StrFormat( "Warning: %1$s: %2$d: $DOC$ without $END$", cFileName, nLine ) )
         ELSEIF ! Empty( hEntry )
            AAdd( aEntry, hEntry )
         ENDIF
         hEntry := { => }
         hb_HKeepOrder( hEntry, .T. )
         IF HB_ISHASH( hMeta )
            FOR EACH tmp IN hMeta
               hEntry[ tmp:__enumKey() ] := tmp
            NEXT
         ENDIF
         EXIT
      CASE "$END$"
         IF hEntry == NIL
            _HBDOC_ADD_MSG( aErrMsg, hb_StrFormat( "Warning: %1$s: %2$d: $END$ without $DOC$", cFileName, nLine ) )
         ELSEIF ! Empty( hEntry )
            AAdd( aEntry, hEntry )
         ENDIF
         hEntry := NIL
         EXIT
      OTHERWISE
         IF hEntry == NIL
            /* Ignore line outside entry. Don't warn, this is normal. */
         ELSEIF Left( LTrim( cLine ), 1 ) == "$" .AND. Right( RTrim( cLine ), 1 ) == "$"
            cLine := AllTrim( cLine )
            cSection := SubStr( cLine, 2, Len( cLine ) - 2 )
            IF cSection $ hEntry
               _HBDOC_ADD_MSG( aErrMsg, hb_StrFormat( "Warning: %1$s: %2$d: Duplicate sections inside the same entry", cFileName, nLine ) )
            ELSE
               hEntry[ cSection ] := ""
            ENDIF
         ELSEIF ! Empty( cSection )
            IF ! Empty( hEntry[ cSection ] )
               hEntry[ cSection ] += Chr( 13 ) + Chr( 10 )
            ELSE
               /* some "heuristics" to detect in which column the real content starts,
                  we assume the first line of content is correct, and use this with all
                  consecutive lines. [vszakats] */
               nStartCol := Len( cLine ) - Len( LTrim( cLine ) ) + 1
            ENDIF
            hEntry[ cSection ] += SubStr( cLine, nStartCol )
         ELSEIF ! Empty( cLine )
            _HBDOC_ADD_MSG( aErrMsg, hb_StrFormat( "Warning: %1$s: %2$d: Content outside section", cFileName, nLine ) )
         ENDIF
      ENDSWITCH
   NEXT

   IF hEntry != NIL
      _HBDOC_ADD_MSG( aErrMsg, hb_StrFormat( "Warning: %1$s: %2$d: $DOC$ without $END$", cFileName, nLine ) )
   ENDIF

   RETURN

FUNCTION __hbdoc_ToSource( aEntry )
   LOCAL cSource := ""
   LOCAL hEntry
   LOCAL item
   LOCAL cLine

   IF HB_ISARRAY( aEntry )
      FOR EACH hEntry IN aEntry
         cSource += hb_eol()
         cSource += "/*  $DOC$" + hb_eol()
         FOR EACH item IN hEntry
            IF HB_ISSTRING( item ) .AND. ;
               !( Left( item:__enumKey(), 1 ) == "_" )
               cSource += " *  $" + item:__enumKey() + "$" + hb_eol()
               FOR EACH cLine IN hb_ATokens( StrTran( item, Chr( 13 ) ), Chr( 10 ) )
                  cSource += " * " + iif( Len( cLine ) == 0, "", Space( 4 ) + cLine ) + hb_eol()
               NEXT
            ENDIF
         NEXT
         cSource += " *  $END$" + hb_eol()
         cSource += " */" + hb_eol()
      NEXT
   ENDIF

   RETURN cSource

FUNCTION __hbdoc_FilterOut( cFile )
   LOCAL lEntry := .F.
   LOCAL cLine
   LOCAL cOK := ""
   LOCAL nToSkip := 0
   LOCAL nEmpty := 0

   cFile := StrTran( cFile, Chr( 13 ) )
   cFile := StrTran( cFile, Chr( 9 ), " " )

   FOR EACH cLine IN hb_ATokens( cFile, Chr( 10 ) )

      SWITCH AllTrim( SubStr( cLine, 4 ) )
      CASE "$DOC$"
         lEntry := .T.
         EXIT
      CASE "$END$"
         lEntry := .F.
         nToSkip := 1
         EXIT
      OTHERWISE
         IF ! lEntry
            IF nToSkip > 0
               nToSkip--
            ELSE
               IF Empty( cLine )
                  nEmpty++
               ELSE
                  nEmpty := 0
               ENDIF
               IF nEmpty < 2
                  cOK += cLine
                  IF cLine:__enumIndex() < Len( cLine:__enumBase() )
                     cOK += hb_eol()
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDSWITCH
   NEXT

   RETURN cOK

/*
 * 0xC0, 'H', 'B', 'D' followed two-byte version number in Little Endian order.
 * Corresponding magic(5) rule:
 *
 *    0       string          \xc0HBD         Harbour Documentation
 *    >4      leshort         x               version %d
 *
 * Until such time that the serialized format changes, and handling of
 * previously-saved files is required, only a naive approach of using
 * version 1 is taken.
 */
#define _HBDOC_SIGNATURE ( HB_BCHAR( 0xC0 ) + ;
                           HB_BCHAR( 0x48 ) + ;
                           HB_BCHAR( 0x42 ) + ;
                           HB_BCHAR( 0x44 ) + ;
                           HB_BCHAR( 0x01 ) + ;
                           HB_BCHAR( 0x00 ) )
#define _HBDOC_SIG_LEN   6
#define _HBDOC_EXT       ".hbd"

FUNCTION __hbdoc_SaveHBD( cFileName, aEntry )
   LOCAL fhnd

   IF HB_ISSTRING( cFileName ) .AND. ;
      HB_ISARRAY( aEntry )

      IF Set( _SET_DEFEXTENSIONS )
         cFileName := hb_FNameExtSetDef( cFileName, _HBDOC_EXT )
      ENDIF

      fhnd := hb_FCreate( cFileName, FC_NORMAL, FO_CREAT + FO_TRUNC + FO_READWRITE + FO_EXCLUSIVE )
      IF fhnd != F_ERROR
         FWrite( fhnd, _HBDOC_SIGNATURE )
         FWrite( fhnd, hb_ZCompress( hb_serialize( aEntry ) ) )
         FClose( fhnd )
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

FUNCTION __hbdoc_LoadHBD( cFileName )
   LOCAL fhnd
   LOCAL aEntry := NIL

   LOCAL cBuffer

   IF HB_ISSTRING( cFileName )

      IF Set( _SET_DEFEXTENSIONS )
         cFileName := hb_FNameExtSetDef( cFileName, _HBDOC_EXT )
      ENDIF

      fhnd := FOpen( cFileName, FO_READ )
      IF fhnd != F_ERROR

         cBuffer := Space( _HBDOC_SIG_LEN )
         FRead( fhnd, @cBuffer, hb_BLen( cBuffer ) )
         IF cBuffer == _HBDOC_SIGNATURE

            cBuffer := Space( FSeek( fhnd, 0, FS_END ) - _HBDOC_SIG_LEN )
            FSeek( fhnd, _HBDOC_SIG_LEN, FS_SET )
            FRead( fhnd, @cBuffer, hb_BLen( cBuffer ) )
            FClose( fhnd )

            aEntry := hb_deserialize( hb_ZUncompress( cBuffer ) )
            cBuffer := NIL

            IF ! HB_ISARRAY( aEntry )
               aEntry := NIL
            ENDIF
         ELSE
            FClose( fhnd )
         ENDIF
      ENDIF
   ENDIF

   RETURN aEntry
