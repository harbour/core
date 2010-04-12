/*
 * $Id$
 */

/*
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 * See COPYING for licensing terms.
 */

#define F_NAME          1       /* File name */

PROCEDURE Main()
   LOCAL cName
   LOCAL cHeaderDir
   LOCAL tmp, tmp1
   LOCAL aStuff

   FOR EACH tmp IN Directory( "*.txt" )
      hb_FNameSplit( tmp[ F_NAME ],, @cName )
      cHeaderDir := GetEnv( "HB_WITH_QT" ) + hb_osPathSeparator() + cName
      FOR EACH tmp1 IN hb_ATokens( StrTran( hb_MemoRead( tmp[ F_NAME ] ), Chr( 13 ) ), Chr( 10 ) )
         IF ! Empty( tmp1 ) .AND. ! ( Left( tmp1, 1 ) == "#" )
            aStuff := {}
            ProcessHeader( aStuff, cHeaderDir + hb_osPathSeparator() + tmp1 )
         ENDIF
      NEXT
   NEXT

   RETURN

STATIC PROCEDURE ProcessHeader( aStuff, cFileName )

   LOCAL cFile := hb_MemoRead( cFileName )
   LOCAL nPos
   LOCAL tmp
   LOCAL cHeader
   LOCAL cDir

   OutStd( "Loading:", cFileName, hb_osNewLine() )

   nPos := 1
   IF ( tmp := hb_At( '#include "', cFile, nPos ) ) > 0
      nPos := tmp + Len( '#include "' )
      IF ( tmp := hb_At( '"', cFile, nPos ) ) > 0
         cHeader := SubStr( cFile, nPos, tmp - nPos )
      ENDIF
   ENDIF

   IF ! Empty( cHeader )
      hb_FNameSplit( cFileName, @cDir )
      ProcessHeader( aStuff, DirAddPathSep( cDir ) + cHeader )
   ENDIF

   RETURN

STATIC FUNCTION DirAddPathSep( cDir )

   IF ! Empty( cDir ) .AND. !( Right( cDir, 1 ) == hb_osPathSeparator() )
      cDir += hb_osPathSeparator()
   ENDIF

   RETURN cDir
