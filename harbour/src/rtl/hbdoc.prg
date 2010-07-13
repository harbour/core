/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HBDOC reader
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
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

#include "common.ch"
#include "directry.ch"

#define _HBDOC_SRC_SUBDIR       "doc"
#define _HBDOC_SRC_EXT          ".txt"

#define _HBDOC_ADD_MSG( a, m )  IF ISARRAY( a ); AAdd( a, m ); ENDIF

FUNCTION __hbdoc_LoadDir( cDir, cName, aErrMsg )
   LOCAL hMeta
   LOCAL nCount
   LOCAL aFile
   LOCAL aEntry

   IF ISCHARACTER( cDir )

      cDir := DirAddPathSep( cDir )

      IF hb_DirExists( cDir + _HBDOC_SRC_SUBDIR )

         aEntry := {}
         hMeta := { => }

         IF ISCHARACTER( cName )
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
   LOCAL cFile := MemoRead( cFileName )
   LOCAL hEntry := NIL
   LOCAL cLine
   LOCAL cSection
   LOCAL tmp
   LOCAL nLine

   LOCAL cDefTemplate

   LOCAL aFilenameTemplateMap := {;
      "FUNCTION"   => "func_"  ,;
      "C FUNCTION" => "cfunc_" ,;
      "CLASS"      => "class_" ,;
      "COMMAND"    => "cmd_"   ,;
      "PP"         => "pp_"    }

   /* Preselect the default template based on source filename */
   FOR EACH tmp IN aFilenameTemplateMap
      IF Lower( Left( cFileName, Len( tmp ) ) ) == tmp
         cDefTemplate := tmp:__enumKey()
      ENDIF
   NEXT

   cFile := StrTran( cFile, Chr( 13 ) )
   cFile := StrTran( cFile, Chr( 9 ), " " )

   nLine := 0
   FOR EACH cLine IN hb_ATokens( cFile, Chr( 10 ) )

      cLine := SubStr( cLine, 4 )
      ++nLine

      SWITCH AllTrim( cLine )
      CASE "$DOC$"
         IF hEntry != NIL
            _HBDOC_ADD_MSG( aErrMsg, hb_StrFormat( "Warning: %1$s: %2$s: $DOC$ without $END$", cFileName, hb_ntos( nLine ) ) )
         ELSEIF ! Empty( hEntry )
            AAdd( aEntry, hEntry )
         ENDIF
         hEntry := { => }
         IF hb_isHash( hMeta )
            hMeta[ "_DOCSOURCE" ] := cFileName
            IF cDefTemplate != NIL
               hMeta[ "TEMPLATE" ] := cDefTemplate
            ENDIF
            FOR EACH tmp IN hMeta
               hEntry[ tmp:__enumKey() ] := tmp
            NEXT
         ENDIF
         EXIT
      CASE "$END$"
         IF hEntry == NIL
            _HBDOC_ADD_MSG( aErrMsg, hb_StrFormat( "Warning: %1$s: %2$s: $END$ without $DOC$", cFileName, hb_ntos( nLine ) ) )
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
               _HBDOC_ADD_MSG( aErrMsg, hb_StrFormat( "Warning: %1$s: %2$s: Duplicate sections inside the same entry", cFileName, hb_ntos( nLine ) ) )
            ELSE
               hEntry[ cSection ] := ""
            ENDIF
         ELSEIF ! Empty( cSection )
            IF ! Empty( hEntry[ cSection ] )
              hEntry[ cSection ] += hb_eol()
            ENDIF
            hEntry[ cSection ] += cLine
         ELSEIF ! Empty( cLine )
            _HBDOC_ADD_MSG( aErrMsg, hb_StrFormat( "Warning: %1$s: %2$s: Content outside section", cFileName, hb_ntos( nLine ) ) )
         ENDIF
      ENDSWITCH
   NEXT

   IF hEntry != NIL
      _HBDOC_ADD_MSG( aErrMsg, hb_StrFormat( "Warning: %1$s: %2$s: $DOC$ without $END$", cFileName, hb_ntos( nLine ) ) )
   ENDIF

   RETURN

STATIC FUNCTION DirAddPathSep( cDir )

   IF ! Empty( cDir ) .AND. !( Right( cDir, 1 ) == hb_ps() )
      cDir += hb_ps()
   ENDIF

   RETURN cDir

FUNCTION __hbdoc_ToSource( aEntry )
   LOCAL cSource := ""
   LOCAL hEntry
   LOCAL item
   LOCAL cLine

   IF ISARRAY( aEntry )
      FOR EACH hEntry IN aEntry
         cSource += "/*  $DOC$" + hb_eol()
         FOR EACH item IN hEntry
            IF ISCHARACTER( item ) .AND. ;
               !( Left( item:__enumKey(), 1 ) == "_" )
               cSource += " *  $" + item:__enumKey() + "$" + hb_eol()
               FOR EACH cLine IN hb_ATokens( StrTran( item, Chr( 13 ) ), Chr( 10 ) )
                  cSource += " * " + cLine + hb_eol()
               NEXT
            ENDIF
         NEXT
         cSource += " *  $END$" + hb_eol()
         cSource += " */" + hb_eol()
         cSource += hb_eol()
      NEXT
   ENDIF

   RETURN cSource
