/*
 * $Id$
 */

#include "simpleio.ch"

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
      cHeaderDir := GetEnv( "HB_WITH_QT" ) + hb_ps() + cName
      FOR EACH tmp1 IN hb_ATokens( StrTran( hb_MemoRead( tmp[ F_NAME ] ), Chr( 13 ) ), Chr( 10 ) )
         IF ! Empty( tmp1 ) .AND. ! ( Left( tmp1, 1 ) == "#" )
            aStuff := { { .F., "" }, {} }
            ProcessHeader( aStuff, cHeaderDir + hb_ps() + tmp1, cName, tmp1 )
//          ASort( aStuff[ 2 ],,, {| x, y | x[ 1 ] < y[ 1 ] } )
//          FOR EACH tmp2 IN aStuff[ 2 ]
//             OutStd( aType[ tmp2[ 1 ] ], tmp2[ 2 ], hb_eol() )
//          NEXT
            IF Len( aStuff[ 1 ] ) >= 2
               cFile := ""
               cFile += hb_eol()
               cFile += "<CLASS>" + hb_eol()
               cFile += "QObject = " + iif( aStuff[ 1 ][ 1 ], "yes", "no" ) + hb_eol()
               cFile += "Inherit = " + aStuff[ 1 ][ 2 ] + hb_eol()
               cFile += "Type = " + cName + hb_eol()
               cFile += "New = " + "" + hb_eol()
               cFile += "</CLASS>" + hb_eol()
               DumpToQTH( @cFile, aStuff, QM_ENUM   )
               DumpToQTH( @cFile, aStuff, QM_METHOD )
               DumpToQTH( @cFile, aStuff, QM_SLOT   )
               DumpToQTH( @cFile, aStuff, QM_SIGNAL )
               hb_MemoWrit( tmp1 + ".qth", cFile )
            ENDIF
         ENDIF
      NEXT
   NEXT

   RETURN

STATIC PROCEDURE DumpToQTH( cFile, aStuff, nType )
   LOCAL tmp

   LOCAL aType := {;
      "ENUM"   ,;
      "METHOD" ,;
      "SLOT"   ,;
      "SIGNAL" }

   cFile += hb_eol()
   cFile += "<" + aType[ nType ] + ">" + hb_eol()
   FOR EACH tmp IN aStuff[ 2 ]
      IF tmp[ 1 ] == nType
         cFile += tmp[ 2 ] + hb_eol()
      ENDIF
   NEXT
   cFile += "</" + aType[ nType ] + ">" + hb_eol()

   RETURN

STATIC PROCEDURE ProcessHeader( aStuff, cFileName, cLib, cOriFileName )

   LOCAL cFile := hb_MemoRead( cFileName )
   LOCAL nPos
   LOCAL tmp
   LOCAL cHeader
   LOCAL cDir

   OutStd( "Loading:", cFileName, hb_eol() )

   nPos := 1
   IF ( tmp := hb_At( '#include "', cFile, nPos ) ) > 0
      nPos := tmp + Len( '#include "' )
      IF ( tmp := hb_At( '"', cFile, nPos ) ) > 0
         cHeader := SubStr( cFile, nPos, tmp - nPos )
      ENDIF
   ENDIF

   IF ! Empty( cHeader )
      hb_FNameSplit( cFileName, @cDir )
      ProcessHeader( aStuff, DirAddPathSep( cDir ) + cHeader, cLib, cOriFileName )
   ELSE
      LoadStuff( aStuff, cOriFileName, cFile, cLib )
   ENDIF

   RETURN

STATIC FUNCTION DirAddPathSep( cDir )

   IF ! Empty( cDir ) .AND. !( Right( cDir, 1 ) == hb_ps() )
      cDir += hb_ps()
   ENDIF

   RETURN cDir

STATIC PROCEDURE LoadStuff( aStuff, cFileName, cFile, cLib )
   LOCAL cLine
   LOCAL tmp, tmp1, tmp2
   LOCAL nMode := 0
   LOCAL nType
   LOCAL cInfo
   LOCAL lAllowBlock
   LOCAL lQ_OBJECT := .F.
   LOCAL lClass := .F.
   LOCAL cInherit := ""
   LOCAL cInheritT

   LOCAL aLine := hb_ATokens( StrTran( cFile, Chr( 13 ) ), Chr( 10 ) )

   FOR tmp := 1 TO Len( aLine )
      cLine := aLine[ tmp ]
      IF ( tmp1 := At( "//", cLine ) ) > 0
         cLine := Left( cLine, tmp1 - 1 )
      ENDIF
      cLine := AllTrim( cLine )
      IF ! Empty( cLine ) .AND. !( Left( cLine, 1 ) == "#" )
         DO CASE
         CASE ! lClass .AND. Left( cLine, Len( "class Q_" + Upper( SubStr( cLib, 3 ) ) + "_EXPORT " ) ) == "class Q_" + Upper( SubStr( cLib, 3 ) ) + "_EXPORT "
            // class Q_CORE_EXPORT QAbstractItemModel : public QObject
            cLine := SubStr( cLine, Len( "class Q_" + Upper( SubStr( cLib, 3 ) ) + "_EXPORT " ) + 1 )
            IF ( tmp1 := At( ":", cLine ) ) > 0
               cInheritT := StrTran( AllTrim( SubStr( cLine, tmp1 + 1 ) ), "public " )
               cLine := AllTrim( Left( cLine, tmp1 - 1 ) )
            ELSE
               cInheritT := ""
            ENDIF
            IF cLine == cFileName
               lClass := .T.
               cInherit := cInheritT
            ENDIF
            LOOP
         CASE lClass .AND. Left( cLine, Len( "};" ) ) == "};"
            lClass := .F.
            nMode := 0
         CASE lClass .AND. cLine == "Q_OBJECT"
            lQ_OBJECT := .T.
            LOOP
         CASE lClass .AND. cLine == "public:"
            nMode := QM_METHOD
            LOOP
         CASE lClass .AND. cLine == "public Q_SLOTS:"
            nMode := QM_SLOT
            LOOP
         CASE lClass .AND. cLine == "Q_SIGNALS:"
            nMode := QM_SIGNAL
            LOOP
         CASE lClass .AND. cLine == "private:"
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
               IF Left( cLine, Len( "inline " ) ) == "inline "
                  nType := nMode
                  lAllowBlock := .T.
               ELSE
                  nType := nMode
               ENDIF
            CASE nMode == QM_SIGNAL
               nType := nMode
            ENDCASE
            IF ! Empty( nType )
               cInfo := GetLine( aLine, @tmp, lAllowBlock )
               IF ! Empty( cInfo )
                  AAdd( aStuff[ 2 ], { nType, cInfo } )
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   NEXT

   aStuff[ 1 ][ 1 ] := lQ_OBJECT
   IF ! Empty( cInherit )
      aStuff[ 1 ][ 2 ] := cInherit
   ENDIF

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
