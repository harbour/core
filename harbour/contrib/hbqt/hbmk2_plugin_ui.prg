/*
 * $Id$
 */

/*
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu) (plugin)
 * Copyright 2010 Pritpal Bedi <bedipritpal@hotmail.com> (uic to prg converter)
 * www - http://harbour-project.org
 *
 * See COPYING for licensing terms.
 */

#define I_( x )                 hb_i18n_gettext( x )

FUNCTION hbmk2_plugin_ui( hbmk2 )
   LOCAL cRetVal := ""

   LOCAL cUIC_BIN

   LOCAL aUI
   LOCAL aUI_Dst

   LOCAL cSrc
   LOCAL cDst
   LOCAL tSrc
   LOCAL tDst

   LOCAL cTmp

   LOCAL cCommand
   LOCAL nError
   LOCAL lBuildIt

   SWITCH hbmk2[ "cSTATE" ]
   CASE "pre_all"

      /* Gather input parameters */

      aUI := {}
      aUI_Dst := {}

      FOR EACH cSrc IN hbmk2[ "params" ]
         IF Lower( hbmk2_FNameExtGet( cSrc ) ) == ".ui"
            cDst := hbmk2_FNameDirExtSet( "ui_" + hbmk2_FNameNameGet( cSrc ), hbmk2[ "cWorkDir" ], ".prg" )
            AAdd( aUI, cSrc )
            AAdd( aUI_Dst, cDst )
            hbmk2_AddInput_PRG( hbmk2, cDst )
         ENDIF
      NEXT

      hbmk2[ "vars" ][ "aUI" ] := aUI
      hbmk2[ "vars" ][ "aUI_Dst" ] := aUI_Dst

      EXIT

   CASE "pre_prg"

      IF ! Empty( hbmk2[ "vars" ][ "aUI" ] )

         /* Detect 'uic' tool location */

         cUIC_BIN := GetEnv( "UIC_BIN" )
         IF Empty( cUIC_BIN )
            IF Empty( GetEnv( "HB_QT_UIC_BIN" ) )
               IF hbmk2[ "cPLAT" ] == "win"
                  IF GetEnv( "HB_WITH_QT" ) == "no"
                     RETURN NIL
                  ELSE
                     cUIC_BIN := GetEnv( "HB_WITH_QT" ) + "\..\bin\uic.exe"
                     IF ! hb_FileExists( cUIC_BIN )
                        hbmk2_OutErr( hbmk2, "HB_WITH_QT points to incomplete QT installation. 'uic' executable not found." )
                        RETURN NIL
                     ENDIF
                  ENDIF
               ELSE
                  cUIC_BIN := hbmk2_FindInPath( "uic", GetEnv( "PATH" ) + hb_osPathListSeparator() + "/opt/qtsdk/qt/bin" )
                  IF Empty( cUIC_BIN )
                     cUIC_BIN := hbmk2_FindInPath( "uic-qt4", GetEnv( "PATH" ) + hb_osPathListSeparator() + "/opt/qtsdk/qt/bin" )
                     IF Empty( cUIC_BIN )
                        hbmk2_OutErr( hbmk2, "HB_QT_UIC_BIN not set, could not autodetect" )
                        RETURN NIL
                     ENDIF
                  ENDIF
               ENDIF
               IF hbmk2[ "lINFO" ]
                  hbmk2_OutStd( hbmk2, hb_StrFormat( "Using QT 'uic' executable: %1$s (autodetected)", cUIC_BIN ) )
               ENDIF
            ELSE
               IF hb_FileExists( GetEnv( "HB_QT_UIC_BIN" ) )
                  cUIC_BIN := GetEnv( "HB_QT_UIC_BIN" )
                  IF hbmk2[ "lINFO" ]
                     hbmk2_OutStd( hbmk2, hb_StrFormat( "Using QT 'uic' executable: %1$s", cUIC_BIN ) )
                  ENDIF
               ELSE
                  hbmk2_OutErr( hbmk2, "HB_QT_UIC_BIN points to non-existent file. Make sure to set it to full path and filename of 'uic' executable." )
                  RETURN NIL
               ENDIF
            ENDIF
         ENDIF

         /* Execute 'uic' commands on input files */

         FOR EACH cSrc, cDst IN hbmk2[ "vars" ][ "aUI" ], hbmk2[ "vars" ][ "aUI_Dst" ]

            IF hbmk2[ "lINC" ] .AND. ! hbmk2[ "lREBUILD" ]
               lBuildIt := ! hb_FGetDateTime( cDst, @tDst ) .OR. ;
                           ! hb_FGetDateTime( cSrc, @tSrc ) .OR. ;
                           tSrc > tDst
            ELSE
               lBuildIt := .T.
            ENDIF

            IF lBuildIt

               FClose( hb_FTempCreateEx( @cTmp ) )

               cCommand := cUIC_BIN +;
                           " " + hbmk2_FNameEscape( hbmk2_PathSepToTarget( hbmk2, cSrc ), hbmk2[ "nCmd_Esc" ], hbmk2[ "nCmd_FNF" ] ) +;
                           " -o " + hbmk2_FNameEscape( cTmp, hbmk2[ "nCmd_Esc" ], hbmk2[ "nCmd_FNF" ] )

               IF hbmk2[ "lTRACE" ]
                  IF ! hbmk2[ "lQUIET" ]
                     hbmk2_OutStd( hbmk2, I_( "'uic' command:" ) )
                  ENDIF
                  hbmk2_OutStdRaw( cCommand )
               ENDIF

               IF ! hbmk2[ "lDONTEXEC" ]
                  IF ( nError := hb_processRun( cCommand ) ) != 0
                     hbmk2_OutErr( hbmk2, hb_StrFormat( I_( "Error: Running 'uic' executable. %1$s" ), hb_ntos( nError ) ) )
                     IF ! hbmk2[ "lQUIET" ]
                        hbmk2_OutErrRaw( cCommand )
                     ENDIF
                     IF ! hbmk2[ "lIGNOREERROR" ]
                        FErase( cTmp )
                        cRetVal := "error"
                        EXIT
                     ENDIF
                  ELSE
                     IF ! ui_to_prg( hbmk2, cTmp, cDst, cSrc )
                        IF ! hbmk2[ "lIGNOREERROR" ]
                           FErase( cTmp )
                           cRetVal := "error"
                           EXIT
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
               FErase( cTmp )
            ENDIF
         NEXT
      ENDIF

      EXIT

   CASE "post_all"

      IF ! hbmk2[ "lINC" ] .OR. hbmk2[ "lCLEAN" ]
         AEval( hbmk2[ "vars" ][ "aUI_Dst" ], {| tmp | FErase( tmp ) } )
      ENDIF

      EXIT

   ENDSWITCH

   RETURN cRetVal

/* ----------------------------------------------------------------------- */

STATIC FUNCTION ui_to_prg( hbmk2, cFileNameSrc, cFileNameDst, cOriSrc )
   LOCAL aLinesPRG
   LOCAL cFile
   LOCAL cName

   IF hb_FileExists( cFileNameSrc )
      hb_FNameSplit( cOriSrc,, @cName )

      aLinesPRG := hbq_create( hb_MemoRead( cFileNameSrc ), "ui" + Upper( Left( cName, 1 ) ) + Lower( SubStr( cName, 2 ) ) )

      IF ! Empty( aLinesPRG )
         cFile := ""
         AEval( aLinesPRG, {| cLine | cFile += cLine + hb_osNewLine() } )
         IF hb_MemoWrit( cFileNameDst, cFile )
            RETURN .T.
         ELSE
            hbmk2_OutErr( hbmk2, hb_StrFormat( "Error: Cannot write file: %1$s", cFileNameDst ) )
         ENDIF
      ELSE
         hbmk2_OutErr( hbmk2, hb_StrFormat( "Error: Intermediate file (%1$s) is not an .uic file.", cFileNameSrc ) )
      ENDIF
   ELSE
      hbmk2_OutErr( hbmk2, hb_StrFormat( "Error: Cannot find intermediate file: %1$s", cFileNameSrc ) )
   ENDIF

   RETURN .F.

#define STRINGIFY( cStr )    '"' + cStr + '"'
#define PAD_30( cStr )       PadR( cStr, Max( Len( cStr ), 20 ) )
#define STRIP_SQ( cStr )     StrTran( StrTran( StrTran( StrTran( s, "[", " " ), "]", " " ), "\n", " " ), Chr( 10 ), " " )

STATIC FUNCTION hbq_create( cFile, cFuncName )
   LOCAL s
   LOCAL n
   LOCAL n1
   LOCAL cCls
   LOCAL cNam
   LOCAL lCreateFinished
   LOCAL cMCls
   LOCAL cMNam
   LOCAL cText
   LOCAL cCmd
   LOCAL aReg
   LOCAL item
   LOCAL aLinesPRG

   LOCAL regEx := hb_regexComp( "\bQ[A-Za-z_]+ \b" )

   LOCAL aLines := hb_ATokens( StrTran( cFile, Chr( 13 ) ), Chr( 10 ) )

   LOCAL aWidgets := {}
   LOCAL aCommands := {}

   lCreateFinished := .F.

   /* Pullout the widget */
   n := AScan( aLines, {| e | "void setupUi" $ e } )
   IF n == 0
      RETURN NIL
   ENDIF
   s     := AllTrim( aLines[ n ] )
   n     := At( "*", s )
   cMCls := AllTrim( SubStr( s, 1, n - 1 ) )
   cMNam := AllTrim( SubStr( s, n + 1 ) )
   hbq_stripFront( @cMCls, "(" )
   hbq_stripRear( @cMNam, ")" )

   AAdd( aWidgets, { cMCls, cMNam, cMCls + "()", cMCls + "():new()" } )

   /* Normalize */
   FOR EACH s IN aLines
      s := AllTrim( s )
      IF Right( s, 1 ) == ";"
         s := SubStr( s, 1, Len( s ) - 1 )
      ENDIF
      IF Left( s, 1 ) $ "/,*,{,}"
         s := ""
      ENDIF
   NEXT

   FOR EACH s IN aLines

      IF ! Empty( s )

         /* Replace Qt::* with actual values */
         hbq_replaceConstants( @s )

         IF "setupUi" $ s
            lCreateFinished := .T.

         ELSEIF Left( s, 1 ) == "Q" .AND. ! lCreateFinished .AND. ( n := At( "*", s ) ) > 0
            // We eill deal later - just skip

         ELSEIF hbq_notAString( s ) .AND. ! Empty( aReg := hb_regex( regEx, s ) )
            cCls := RTrim( aReg[ 1 ] )
            s := AllTrim( StrTran( s, cCls, "",, 1 ) )
            IF ( n := At( "(", s ) ) > 0
               cNam := SubStr( s, 1, n - 1 )
               AAdd( aWidgets, { cCls, cNam, cCls + "()", cCls + "():new" + SubStr( s, n ) } )
            ELSE
               cNam := s
               AAdd( aWidgets, { cCls, cNam, cCls + "()", cCls + "():new()" } )
            ENDIF

         ELSEIF hbq_isObjectNameSet( s )
            // Skip - we already know the object name and will set after construction

         ELSEIF ! Empty( cText := hbq_pullSetToolTip( aLines, s:__enumIndex() ) )
            n := At( "->", cText )
            cNam := AllTrim( SubStr( cText, 1, n - 1 ) )
            cCmd := hbq_formatCommand( SubStr( cText, n + 2 ), .T., aWidgets )
            AAdd( aCommands, { cNam, cCmd } )

         ELSEIF ! Empty( cText := hbq_pullText( aLines, s:__enumIndex() ) )
            n := At( "->", cText )
            cNam := AllTrim( SubStr( cText, 1, n - 1 ) )
            cCmd := hbq_formatCommand( SubStr( cText, n + 2 ), .T., aWidgets )
            AAdd( aCommands, { cNam, cCmd } )

         ELSEIF hbq_isValidCmdLine( s ) .AND. !( "->" $ s ) .AND. ( ( n := At( ".", s ) ) > 0  )  /* Assignment to objects on stack */
            cNam := SubStr( s, 1, n - 1 )
            cCmd := SubStr( s, n + 1 )
            cCmd := hbq_formatCommand( cCmd, .F., aWidgets )
            cCmd := hbq_setObjects( cCmd, aWidgets )
            cCmd := hbq_setObjects( cCmd, aWidgets )
            AAdd( aCommands, { cNam, cCmd } )

         ELSEIF !( Left( s, 1 ) $ '#/*"' ) .AND. ;          /* Assignment with properties from objects */
                        ( n := At( ".", s ) ) > 0 .AND. ;
                        At( "->", s ) > n
            cNam := SubStr( s, 1, n - 1 )
            cCmd := SubStr( s, n + 1 )
            cCmd := hbq_formatCommand( cCmd, .F., aWidgets )
            cCmd := hbq_setObjects( cCmd, aWidgets )
            cCmd := hbq_setObjects( cCmd, aWidgets )
            AAdd( aCommands, { cNam, cCmd } )

         ELSEIF ( n := At( "->", s ) ) > 0                  /* Assignments or calls to objects on heap */
            cNam := SubStr( s, 1, n - 1 )
            cCmd := hbq_formatCommand( SubStr( s, n + 2 ), .F., aWidgets )
            cCmd := hbq_setObjects( cCmd, aWidgets )
            AAdd( aCommands, { cNam, cCmd } )

         ELSEIF ( n := At( "= new", s ) ) > 0
            IF ( n1 := At( "*", s ) ) > 0 .AND. n1 < n
               s := AllTrim( SubStr( s, n1 + 1 ) )
            ENDIF
            n    := At( "= new", s )
            cNam := AllTrim( SubStr( s, 1, n - 1 ) )
            cCmd := AllTrim( SubStr( s, n + Len( "= new" ) ) )
            cCmd := hbq_setObjects( cCmd, aWidgets )
            n := At( "(", cCmd )
            cCls := SubStr( cCmd, 1, n - 1 )
            AAdd( aWidgets, { cCls, cNam, cCls + "()", cCls + "():new" + SubStr( cCmd, n ) } )

         ENDIF
      ENDIF
   NEXT

   aLinesPRG := {}

   AAdd( aLinesPRG, "/* WARNING: Automatically generated source file. DO NOT EDIT! */" )
   AAdd( aLinesPRG, "" )
   AAdd( aLinesPRG, '#include "hbqt.ch"' )

   AAdd( aLinesPRG, "" )
   AAdd( aLinesPRG, "FUNCTION " + cFuncName + "( qParent )" )
   AAdd( aLinesPRG, "   LOCAL oUI" )
   AAdd( aLinesPRG, "   LOCAL oWidget" )
   AAdd( aLinesPRG, "   LOCAL qObj := {=>}" )
   AAdd( aLinesPRG, "" )
   AAdd( aLinesPRG, "   hb_hCaseMatch( qObj, .F. )" )
   AAdd( aLinesPRG, "" )

   SWITCH cMCls
   CASE "QDialog"
      AAdd( aLinesPRG, "   oWidget := QDialog():new( qParent )" )
      EXIT
   CASE "QWidget"
      AAdd( aLinesPRG, "   oWidget := QWidget():new( qParent )" )
      EXIT
   CASE "QMainWindow"
      AAdd( aLinesPRG, "   oWidget := QMainWindow():new( qParent )" )
      EXIT
   ENDSWITCH
   AAdd( aLinesPRG, "  " )
   AAdd( aLinesPRG, "   oWidget:setObjectName( " + STRINGIFY( cMNam ) + " )" )
   AAdd( aLinesPRG, "  " )
   AAdd( aLinesPRG, "   qObj[ " + PAD_30( STRINGIFY( cMNam ) ) + " ] := oWidget" )
   AAdd( aLinesPRG, "  " )

   FOR EACH item IN aWidgets
      IF item:__enumIndex() > 1
         AAdd( aLinesPRG, "   qObj[ " + PAD_30( STRINGIFY( item[ 2 ] ) ) + " ] := " + StrTran( item[ 4 ], "o[", "qObj[" ) )
      ENDIF
   NEXT
   AAdd( aLinesPRG, "" )

   FOR EACH item IN aCommands
      cNam := item[ 1 ]
      cCmd := item[ 2 ]
      cCmd := StrTran( cCmd, "true" , ".T." )
      cCmd := StrTran( cCmd, "false", ".F." )

      IF "addWidget" $ cCmd
         IF hbq_occurs( cCmd, "," ) >= 4
            cCmd := StrTran( cCmd, "addWidget", "addWidget_1" )
         ENDIF
      ELSEIF "addLayout" $ cCmd
         IF hbq_occurs( cCmd, "," ) >= 4
            cCmd := StrTran( cCmd, "addLayout", "addLayout_1" )
         ENDIF
      ENDIF

      IF "setToolTip(" $ cCmd
         s := hbq_pullToolTip( cCmd )
         AAdd( aLinesPRG, "   qObj[ " + PAD_30( STRINGIFY( cNam ) ) + " ]:setToolTip( [" + STRIP_SQ( s ) + "] )" )

      ELSEIF "setPlainText(" $ cCmd
         s := hbq_pullToolTip( cCmd )
         AAdd( aLinesPRG, "   qObj[ " + PAD_30( STRINGIFY( cNam ) ) + " ]:setPlainText( [" + STRIP_SQ( s ) + "] )" )

      ELSEIF "setStyleSheet(" $ cCmd
         s := hbq_pullToolTip( cCmd )
         AAdd( aLinesPRG, "   qObj[ " + PAD_30( STRINGIFY( cNam ) ) + " ]:setStyleSheet( [" + STRIP_SQ( s ) + "] )" )

      ELSEIF "setText(" $ cCmd
         s := hbq_pullToolTip( cCmd )
         AAdd( aLinesPRG, "   qObj[ " + PAD_30( STRINGIFY( cNam ) ) + " ]:setText( [" + STRIP_SQ( s ) + "] )" )

      ELSEIF "setWhatsThis(" $ cCmd
         s := hbq_pullToolTip( cCmd )
         AAdd( aLinesPRG, "   qObj[ " + PAD_30( STRINGIFY( cNam ) ) + " ]:setWhatsThis( [" + STRIP_SQ( s ) + "] )" )

      ELSEIF "header()->" $ cCmd
         // TODO: how to handle : __qtreeviewitem->header()->setVisible( .F. )

      ELSEIF cCmd == "pPtr"
         // Nothing TO DO

      ELSE
         AAdd( aLinesPRG, "   qObj[ " + PAD_30( STRINGIFY( cNam ) ) + " ]:" + StrTran( cCmd, "o[", "qObj[" ) )

      ENDIF
   NEXT
   AAdd( aLinesPRG, "" )
   AAdd( aLinesPRG, "   oUI         := HbQtUI():new()" )
   AAdd( aLinesPRG, "   oUI:qObj    := qObj"    )
   AAdd( aLinesPRG, "   oUI:oWidget := oWidget" )
   AAdd( aLinesPRG, "" )
   AAdd( aLinesPRG, "   RETURN oUI" )
   AAdd( aLinesPRG, "" )

   RETURN aLinesPRG

STATIC FUNCTION hbq_formatCommand( cCmd, lText, widgets )
   LOCAL regDefine
   LOCAL aDefine
   LOCAL n
   LOCAL n1
   LOCAL cNam
   LOCAL cCmd1

   STATIC s_nn := 100

   IF lText == NIL
      lText := .T.
   ENDIF

   cCmd := StrTran( cCmd, "QApplication_translate"   , "q__tr"        )
   cCmd := StrTran( cCmd, "QApplication::UnicodeUTF8", '"UTF8"'       )
   cCmd := StrTran( cCmd, "QString()"                , '""'           )
   cCmd := StrTran( cCmd, "QSize("                   , "QSize():new(" )
   cCmd := StrTran( cCmd, "QRect("                   , "QRect():new(" )

   IF "::" $ cCmd
      regDefine := hb_regexComp( "\b[A-Za-z_]+\:\:[A-Za-z_]+\b" )
      aDefine := hb_regex( regDefine, cCmd )
      IF ! Empty( aDefine )
         cCmd := StrTran( cCmd, "::", "_" )    /* Qt Defines  - how to handle */
      ENDIF
   ENDIF

   IF ! lText .AND. At( ".", cCmd ) > 0
      // sizePolicy     setHeightForWidth(ProjectProperties->sizePolicy().hasHeightForWidth());
      //
      IF ( At( "setHeightForWidth(", cCmd ) ) > 0
         cNam := "__qsizePolicy" + hb_ntos( ++s_nn )
         n    := At( "(", cCmd )
         n1   := At( ".", cCmd )
         cCmd1 := hbq_setObjects( SubStr( cCmd, n + 1, n1 - n - 1 ), widgets )
         cCmd1 := StrTran( cCmd1, "->", ":" )
         AAdd( widgets, { "QSizePolicy", cNam, "QSizePolicy()", "QSizePolicy():configure(" + cCmd1 + ")" } )
         cCmd := 'setHeightForWidth(o[ "' + cNam + '" ]:' + SubStr( cCmd, n1 + 1 )
      ELSE
         cCmd := "pPtr"
      ENDIF
   ENDIF

   RETURN cCmd

STATIC FUNCTION hbq_isObjectNameSet( cString )
   RETURN "objectName" $ cString .OR. ;
          "ObjectName" $ cString

STATIC FUNCTION hbq_isValidCmdLine( cString )
   RETURN !( Left( cString, 1 ) $ '#/*"' )

STATIC FUNCTION hbq_notAString( cString )
   RETURN !( Left( cString, 1 ) == '"' )

STATIC FUNCTION hbq_occurs( cString, cCharToFind )
   LOCAL cChar
   LOCAL nCount

   nCount := 0
   FOR EACH cChar IN cString
      IF cChar == cCharToFind
         ++nCount
      ENDIF
   NEXT

   RETURN nCount

STATIC FUNCTION hbq_pullToolTip( cCmd )
   LOCAL n
   LOCAL cString := ""

   IF ( n := At( ', "', cCmd ) ) > 0
      cString := AllTrim( SubStr( cCmd, n + 2 ) )
      IF ( n := At( '", 0', cString ) ) > 0
         cString := AllTrim( SubStr( cString, 1, n ) )
         cString := StrTran( cString, '\"', '"' )
         cString := StrTran( cString, '""', "" )
         cString := SubStr( cString, 2, Len( cString ) - 2 )
      ENDIF
   ENDIF

   RETURN cString

STATIC PROCEDURE hbq_replaceConstants( /* @ */ cString )
   LOCAL aResult
   LOCAL cConst
   LOCAL cCmdB
   LOCAL cCmdE
   LOCAL cOR
   LOCAL n

   LOCAL regDefine := hb_regexComp( "\b[A-Za-z_]+\:\:[A-Za-z_]+\b" )

   IF hbq_occurs( cString, "|" ) > 0

      aResult := hb_regexAll( regDefine, cString )

      IF ! Empty( aResult )
         cOR := "hb_bitOr("
         FOR n := 1 TO Len( aResult )
            cOR += aResult[ n ][ 1 ]
            IF n < Len( aResult )
               cOR += ","
            ENDIF
         NEXT
         cOR += ")"
         cCmdB   := SubStr( cString, 1, At( aResult[ 1 ][ 1 ], cString ) - 1 )
         cConst  := aResult[ Len( aResult ) ][ 1 ]
         cCmdE   := SubStr( cString, At( cConst, cString ) + Len( cConst ) )
         cString := cCmdB + cOR + cCmdE
      ENDIF
   ENDIF

   IF "::" $ cString
      DO WHILE .T.
         aResult := hb_regex( regDefine, cString )
         IF Empty( aResult )
            EXIT
         ENDIF
         cString := StrTran( cString, aResult[ 1 ], StrTran( aResult[ 1 ], "::", "_" ) )
      ENDDO
   ENDIF

   RETURN

STATIC FUNCTION hbq_setObjects( cCmd, aWidgets )
   LOCAL n
   LOCAL cObj

   IF ( n := AScan( aWidgets, {| tmp | ( tmp[ 2 ] + "," ) $ cCmd } ) ) > 0
      cObj := aWidgets[ n ][ 2 ]
      cCmd := StrTran( cCmd, cObj + ",", 'o[ "' + cObj + '" ],' )
   ENDIF

   IF ( n := AScan( aWidgets, {| tmp | ( tmp[ 2 ] + ")" ) $ cCmd } ) ) > 0
      cObj := aWidgets[ n ][ 2 ]
      cCmd := StrTran( cCmd, cObj + ")", 'o[ "' + cObj + '" ])' )
   ENDIF

   IF ( n := AScan( aWidgets, {| tmp | ( tmp[ 2 ] + "->" ) $ cCmd } ) ) > 0
      cObj := aWidgets[ n ][ 2 ]
      cCmd := StrTran( cCmd, cObj + "->", 'o[ "' + cObj + '" ]:' )
   ENDIF

   RETURN cCmd

STATIC FUNCTION hbq_pullText( aLines, nFrom )
   LOCAL cString := ""
   LOCAL nLen := Len( aLines )
   LOCAL aKeyword := { "setText(", "setPlainText(", "setStyleSheet(", "setWhatsThis(" }

   IF AScan( aKeyword, {| tmp | tmp $ aLines[ nFrom ] } ) > 0
      cString := aLines[ nFrom ]
      nFrom++
      DO WHILE nFrom <= nLen
         IF !( Left( aLines[ nFrom ], 1 ) == '"' )
            EXIT
         ENDIF
         cString += aLines[ nFrom ]
         aLines[ nFrom ] := ""
         nFrom++
      ENDDO
   ENDIF

   RETURN cString

STATIC FUNCTION hbq_pullSetToolTip( aLines, nFrom )
   LOCAL cString := ""
   LOCAL nLen := Len( aLines )

   IF "#ifndef QT_NO_TOOLTIP" $ aLines[ nFrom ]
      nFrom++
      DO WHILE nFrom <= nLen
         IF "#endif // QT_NO_TOOLTIP" $ aLines[ nFrom ]
            EXIT
         ENDIF
         cString += aLines[ nFrom ]
         aLines[ nFrom ] := ""
         nFrom++
      ENDDO
   ENDIF

   RETURN cString

STATIC FUNCTION hbq_stripFront( /* @ */ cString, cTkn )
   LOCAL n
   LOCAL nLen := Len( cTkn )

   IF ( n := At( cTkn, cString ) ) > 0
      cString := SubStr( cString, n + nLen )
      RETURN .T.
   ENDIF

   RETURN .F.

STATIC FUNCTION hbq_stripRear( /* @ */ cString, cTkn )
   LOCAL n

   IF ( n := RAt( cTkn, cString ) ) > 0
      cString := SubStr( cString, 1, n - 1 )
      RETURN .T.
   ENDIF

   RETURN .F.
