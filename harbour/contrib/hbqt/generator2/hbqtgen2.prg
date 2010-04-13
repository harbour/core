/*
 * $Id$
 */

/*
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 * See COPYING for licensing terms.
 */

#define F_NAME          1       /* File name */

#define QM_ENUM         1
#define QM_METHOD       2
#define QM_SLOT         3
#define QM_SIGNAL       4

PROCEDURE Main()
   LOCAL cName
   LOCAL cHeaderDir
   LOCAL tmp, tmp1, tmp2
   LOCAL aStuff

   LOCAL aType := {;
      "QM_ENUM"   ,;
      "QM_METHOD" ,;
      "QM_SLOT"   ,;
      "QM_SIGNAL" }

   FOR EACH tmp IN Directory( "*.txt" )
      hb_FNameSplit( tmp[ F_NAME ],, @cName )
      cHeaderDir := GetEnv( "HB_WITH_QT" ) + hb_osPathSeparator() + cName
      FOR EACH tmp1 IN hb_ATokens( StrTran( hb_MemoRead( tmp[ F_NAME ] ), Chr( 13 ) ), Chr( 10 ) )
         IF ! Empty( tmp1 ) .AND. ! ( Left( tmp1, 1 ) == "#" )
            aStuff := {}
            ProcessHeader( aStuff, cHeaderDir + hb_osPathSeparator() + tmp1 )
            ASort( aStuff,,, {| x, y | x[ 1 ] < y[ 1 ] } )
            FOR EACH tmp2 IN aStuff
               OutStd( aType[ tmp2[ 1 ] ], tmp2[ 2 ], hb_osNewLine() )
            NEXT
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

   LoadStuff( aStuff, cFile )

   RETURN

STATIC FUNCTION DirAddPathSep( cDir )

   IF ! Empty( cDir ) .AND. !( Right( cDir, 1 ) == hb_osPathSeparator() )
      cDir += hb_osPathSeparator()
   ENDIF

   RETURN cDir

STATIC PROCEDURE LoadStuff( aStuff, cFile )
   LOCAL cLine
   LOCAL tmp, tmp1, tmp2
   LOCAL nMode := 0
   LOCAL nType
   LOCAL cInfo
   LOCAL lAllowBlock

   LOCAL aLine := hb_ATokens( StrTran( cFile, Chr( 13 ) ), Chr( 10 ) )

   FOR tmp := 1 TO Len( aLine )
      cLine := aLine[ tmp ]
      IF ( tmp1 := At( "//", cLine ) ) > 0
         cLine := Left( cLine, tmp1 - 1 )
      ENDIF
      cLine := AllTrim( cLine )
      IF ! Empty( cLine ) .AND. !( Left( cLine, 1 ) == "#" )
         DO CASE
         CASE cLine == "public:"
            nMode := QM_METHOD
            LOOP
         CASE cLine == "public Q_SLOTS:"
            nMode := QM_SLOT
            LOOP
         CASE cLine == "Q_SIGNALS:"
            nMode := QM_SIGNAL
            LOOP
         CASE cLine == "private:"
            nMode := 0
            LOOP
         ENDCASE
         IF ! Empty( nMode )
            nType := 0
            lAllowBlock := .F.
            DO CASE
            CASE nMode == QM_METHOD
               IF Left( cLine, Len( "enum " ) ) == "enum "
                  nType := QM_ENUM
               ELSE
                  tmp1 := At( "(", cLine )
                  IF tmp1 > 0 .AND. hb_At( ")", cLine, tmp1 + Len( "(" ) ) > 0
                     IF Left( cLine, Len( "inline " ) ) == "inline "
                        nType := nMode
                        lAllowBlock := .T.
                     ELSE
                        nType := nMode
                     ENDIF
                  ENDIF
               ENDIF
            CASE nMode == QM_SLOT
               nType := nMode
            CASE nMode == QM_SIGNAL
               nType := nMode
            ENDCASE
            IF ! Empty( nType )
               cInfo := GetLine( aLine, @tmp, lAllowBlock )
               IF ! Empty( cInfo )
                  AAdd( aStuff, { nType, cInfo } )
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   NEXT

   RETURN

STATIC FUNCTION GetLine( aLine, /* @ */ nPos, lAllowBlock )
   LOCAL cFullLine := ""
   LOCAL cLine
   LOCAL tmp

   DO WHILE nPos <= Len( aLine )
      cLine := aLine[ nPos ]
      IF ( tmp := At( "//", cLine ) ) > 0
         cLine := Left( cLine, tmp - 1 )
      ENDIF
      cLine := AllTrim( cLine )
      cFullLine += AllTrim( cLine )
      IF lAllowBlock .AND. ( tmp := At( "{", cFullLine ) ) > 0
         RETURN Left( cFullLine, tmp - 1 )
      ELSEIF ( tmp := At( ";", cFullLine ) ) > 0
         RETURN Left( cFullLine, tmp - 1 )
      ENDIF
      ++nPos
   ENDDO

   RETURN ""
