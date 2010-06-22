/*
 * $Id$
 */

/*
   Copyright 2009-2010 Viktor Szakats (harbour.01 syenar.hu)
   See COPYING for licensing terms.

   NOTE:       Purpose of this script is to take the source files
               in Harbour repo and convert them back to the filenames
               used in the original source distribution.
               This is to aid finding local modifications and
               apply them after an original source update.
               [vszakats]

   DISCLAIMER: This tool is targeted only to Harbour core
               maintainers. If you're not one of them you
               don't have to mess with it.
 */

#pragma warninglevel=3

#define _REN_PREFIX "# RENAME "

PROCEDURE Main( cMode )
   LOCAL files := {}
   LOCAL cFile := MemoRead( "Makefile" )
   LOCAL cLine

   IF ! Empty( cFile )

      cFile := StrTran( cFile, Chr( 13 ) + Chr( 10 ), Chr( 10 ) )
      cFile := StrTran( cFile, Chr( 9 ), " " )

      FOR EACH cLine IN hb_ATokens( cFile, Chr( 10 ) )
         IF ! Empty( cLine ) .AND. Left( AllTrim( cLine ), Len( _REN_PREFIX ) ) == _REN_PREFIX
            cLine := SubStr( cLine, Len( _REN_PREFIX ) + 1 )
            IF Len( hb_ATokens( cLine ) ) == 2
               AAdd( files, hb_ATokens( cLine ) )
            ELSEIF Len( hb_ATokens( cLine ) ) == 1
               AAdd( files, { hb_ATokens( cLine )[ 1 ], hb_ATokens( cLine )[ 1 ] } )
            ENDIF
         ENDIF
      NEXT

      IF ! Empty( files ) .AND. cMode != NIL
         SWITCH cMode
         CASE "T" ; original_to_hb( files ) ; EXIT
         CASE "F" ; hb_to_original( files ) ; EXIT
         ENDSWITCH
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE original_to_hb( files )
   LOCAL cDir := "ori_src"
   LOCAL file
   LOCAL changes := files_to_changes( files )
   LOCAL change

   FOR EACH file IN files
      hb_FCopy( cDir + hb_osPathSeparator() + file[ 2 ], file[ 1 ] )
      hb_FileEOLToNative( file[ 1 ] )
      FOR EACH change IN changes
         hb_FileTran( file[ 1 ], Chr( 34 ) + change[ 2 ] + Chr( 34 ), Chr( 34 ) + change[ 1 ] + Chr( 34 ) )
         hb_FileTran( file[ 1 ], "<" + change[ 2 ] + ">", "<" + change[ 1 ] + ">" )
      NEXT
   NEXT

   RETURN

STATIC PROCEDURE hb_to_original( files )
   LOCAL cDir := "ori_dst"
   LOCAL file
   LOCAL changes := files_to_changes( files )
   LOCAL change

   MakeDir( cDir )
   hb_FEval( cDir + hb_osPathSeparator() + "*", {| cFileName | FErase( cFileName ) } )

   FOR EACH file IN files
      hb_FCopy( file[ 1 ], cDir + hb_osPathSeparator() + file[ 2 ] )
      FOR EACH change IN changes
         hb_FileTran( cDir + hb_osPathSeparator() + file[ 2 ], Chr( 34 ) + change[ 1 ] + Chr( 34 ), Chr( 34 ) + change[ 2 ] + Chr( 34 ) )
         hb_FileTran( cDir + hb_osPathSeparator() + file[ 2 ], "<" + change[ 1 ] + ">", "<" + change[ 2 ] + ">" )
      NEXT
   NEXT

   RETURN

STATIC FUNCTION files_to_changes( files )
   LOCAL changes := {}
   LOCAL file

   FOR EACH file IN files
      IF Lower( FN_ExtGet( file[ 1 ] ) ) == ".h" .OR. ;
         Lower( FN_ExtGet( file[ 2 ] ) ) == ".h"
         IF !( file[ 1 ] == file[ 2 ] )
            AAdd( changes, file )
         ENDIF
      ENDIF
   NEXT

   RETURN changes

STATIC FUNCTION hb_FileEOLToNative( cFileName )
   LOCAL cFile := hb_MemoRead( cFileName )

   cFile := StrTran( cFile, Chr( 13 ) + Chr( 10 ), Chr( 10 ) )

   RETURN hb_MemoWrit( cFileName, StrTran( cFile, Chr( 10 ), hb_osNewLine() ) )

STATIC FUNCTION hb_FileTran( cFileName, cFrom, cTo )
   RETURN hb_MemoWrit( cFileName, StrTran( hb_MemoRead( cFileName ), cFrom, cTo ) )

STATIC FUNCTION FN_ExtGet( cFileName )
   LOCAL cExt

   hb_FNameSplit( cFileName,,, @cExt )

   RETURN cExt

/* TOFIX: Ugly hack to avoid #include "directry.ch" */
#define F_NAME          1       /* File name */

STATIC PROCEDURE hb_FEval( cMask, bBlock )
   LOCAL cDir
   LOCAL tmp

   hb_FNameSplit( cMask, @cDir )

   FOR EACH tmp IN Directory( cMask )
      Eval( bBlock, hb_FNameMerge( cDir, tmp[ F_NAME ] ) )
   NEXT

   RETURN
