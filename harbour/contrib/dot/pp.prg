#ifdef __HARBOUR__
   #include "hbextern.ch"
#endif

STATIC aDefRules     := {}, aDefResults   := {}
STATIC aTransRules   := {}, aTransResults := {}
STATIC aCommRules    := {}, aCommResults  := {}

STATIC nPendingLines := 0, aPendingLines  := {}

STATIC bDbgMatch := .F., bDbgExp := .F., bDbgPPO := .F., bLoadRules := .T., bCount := .T., bCCH := .F., bPP := .F.

STATIC nIfDef := 0, abIfDef := {}

STATIC hPP := NIL, sIncludeFile := NIL

STATIC asPaths := {}

PROCEDURE Main( sSource, sSwitch )

   LOCAL sIncludePath, nNext, sPath

   sIncludePath := GetE( "INCLUDE" )

   WHILE ( nNext := At( ';', sIncludePath ) ) > 0
      sPath := Left( sIncludePath, nNext - 1 )
      IF ! ( Right( sPath, 1 ) $ '\/' )
         sPath += '\'
      ENDIF
      aAdd( asPaths, sPath )
      sIncludePath := SubStr( sIncludePath, nNext + 1 )
   ENDDO
   IF ! ( Right( sIncludePath, 1 ) $ '\/' )
      sIncludePath += '\'
   ENDIF
   aAdd( asPaths, sIncludePath )

   IF sSwitch == NIL
      sSwitch := ''
   ELSE
      sSwitch := Upper( sSwitch )
      IF "-P" $ sSwitch
         bPP := .T.
      ENDIF
      IF "-U" $ sSwitch
         bLoadRules := .F.
      ENDIF
   ENDIF

   IF bLoadRules
      InitRules()
      InitResults()
   ELSE
      Alert( "Not using standard rules." )
   ENDIF

   IF bPP
      ProcessFile( sSource, sSwitch )
   ELSE
      RP_Dot()
   ENDIF

RETURN

PROCEDURE RP_Dot()

   LOCAL sLine := Space(256), bBlock, GetList := {}, sPPed, nNext, sBlock, sTemp

   bCount := .F.

   ProcessFile( "RP_Dot.ch" )

   ErrorBlock( {|| RP_Dot_Err() } )

   CLEAR SCREEN
   SET SCOREBOARD OFF

   @ 0,0 SAY "PP: "
   @ 0,4 SAY Space( 76 ) COLOR "N/R"

   DO WHILE .T.
      @ 24,00 SAY '.'
      @ 24,01 GET sLine PICTURE '@KS79'
      SET CURSOR ON
      READ

      sPPed := ProcessLine( DropTrailingWS( sLine ), {}, {}, {}, 1, '' )

      ExtractLeadingWS( @sPPed )
      DropTrailingWS( @sPPed )
      sTemp := sPPed

      @ 0,0 SAY "PP: "
      @ 0,4 SAY Pad( sPPed, 76 ) COLOR "N/R"
      DevPos( 1, 0 )

      BEGIN SEQUENCE

         WHILE ( nNext := At( ';', sTemp ) ) > 0
            sBlock := Left( sTemp, nNext - 1 )
            ExtractLeadingWS( @sBlock )
            DropTrailingWS( @sBlock )
            bBlock := &( "{|| " + sBlock + " }" )
            Eval( bBlock )
            sTemp  := DropTrailingWS( SubStr( sTemp, nNext + 1 ) )
            ExtractLeadingWS( @sTemp )
         ENDDO
         ExtractLeadingWS( @sTemp )
         DropTrailingWS( @sTemp )
         IF ! ( sTemp == '' )
            bBlock := &( "{|| " + sTemp + " }" )
            Eval( bBlock )
         ENDIF
         @ 0,0 SAY "PP: "
         @ 0,4 SAY Pad( sPPed, 76 ) COLOR "N/R"

      END SEQUENCE
   ENDDO

   CLEAR SCREEN

RETURN

FUNCTION RP_Dot_Err()

   Alert( "Sorry, could not execute last request." )
   BREAK

RETURN NIL

FUNCTION ProcessFile( sSource, sSwitch )

   LOCAL hSource, sBuffer, sLine, nPosition, nNewLineAt
   LOCAL nLen, nMaxPos, cChar, nClose, nBase, nNext, nLine := 0
   LOCAL sLeft, sRight, nPath, nPaths := Len( asPaths )

   IF At( '.', sSource ) == 0
     sSource += ".PRG"
   ENDIF

   hSource := FOpen( sSource, 0 )
   IF hPP != NIL
      nPath := 1
      WHILE hSource == -1 .AND. nPath <= nPaths
          hSource := FOpen( asPaths[nPath] + sSource, 0 )
          nPath++
      ENDDO
   ENDIF

   IF hSource == -1
      Alert( "ERROR! opening: [" + sSource + "] O/S Error: " + Str( FError(), 2 ) )
      RETURN .F.
   ENDIF

   IF hPP == NIL .AND. ProcName(1) == "MAIN"
      hPP := FCreate( StrTran( sSource, ".PRG", ".PP$" ) )
      IF hPP == -1
         Alert( "ERROR! creating '.PP$' file, O/S Error: " + Str( FError(), 2 ) )
         RETURN .F.
      ENDIF

      IF "-CCH" $ sSwitch
         bCCH := .T.
      ENDIF
      IF "-DE" $ sSwitch
         bDbgExp := .T.
      ENDIF
      IF "-DM" $ sSwitch
         bDbgMatch := .T.
      ENDIF
      IF "-DP" $ sSwitch
         bDbgPPO := .T.
      ENDIF
   ENDIF

   sBuffer   := Space( 16384 )
   nPosition := 1
   sLine     := ''

   WHILE ( nLen := FRead( hSource, @sBuffer, 16384 ) ) > 2

      nPosition := 1
      nMaxPos   := nLen - 1

      WHILE nPosition < nMaxPos

          cChar := SubStr( sBuffer, nPosition, 1 )

          DO CASE
             CASE ( cChar == '/' .AND. SubStr( sBuffer, nPosition + 1, 1 ) == '*' )

                WHILE .T.
                   nClose := At( "*/", SubStr( sBuffer, nPosition + 2 ) )

                   IF nClose == 0
                      nBase := nPosition + 2
                      WHILE ( nNext := At( Chr(10), SubStr( sBuffer, nBase ) ) ) > 0

                         nLine++
                         IF bCount
                            @ Row(), 0 SAY nLine
                         ENDIF

                         FWrite( hPP, ProcessLine( '', {}, {}, {}, nLine, sSource ) + Chr(13) + Chr(10) )
                         nBase += ( nNext + 1 )
                      ENDDO

                      FSeek( hSource, -1, 1 )
                      nLen := FRead( hSource, @sBuffer, 16384 )
                      IF nLen < 3
                         Alert( "ERROR! Unterminated '/**/'")
                      ENDIF
                      nMaxPos   := nLen - 1
                      nPosition := 1
                      LOOP
                   ELSE
                      nBase := nPosition + 2
                      WHILE ( nNext := At( Chr(10), SubStr( sBuffer, nBase ) ) ) > 0 .AND. ( nBase + nNext - nPosition ) <= nClose + 1
                         nLine++
                         IF bCount
                            @ Row(), 0 SAY nLine
                         ENDIF
                         FWrite( hPP, ProcessLine( '', {}, {}, {}, nLine, sSource ) + Chr(13) + Chr(10) )
                         nBase += ( nNext + 1 )
                      ENDDO

                      nPosition += ( nClose + 2 )
                      cChar := ''
                      EXIT
                   ENDIF
                ENDDO

             CASE ( cChar == '/' .AND. SubStr( sBuffer, nPosition + 1, 1 ) == '/' )

                WHILE .T.
                   nClose := At( Chr(10), SubStr( sBuffer, nPosition + 2 ) )

                   IF nClose == 0
                      FSeek( hSource, -1, 1 )
                      nLen := FRead( hSource, @sBuffer, 16384 )
                      IF nLen < 3
                         nPosition := nLen
                         cChar := ''
                         EXIT
                      ENDIF
                      nMaxPos   := nLen - 1
                      nPosition := 1
                      LOOP
                   ELSE
                      nLine++
                      IF bCount
                         @ Row(), 0 SAY nLine
                      ENDIF
                      FWrite( hPP, ProcessLine( sLine, {}, {}, {}, nLine, sSource ) + Chr(13) + Chr(10) )
                      nPosition += ( nClose + 1 )
                      sLine := ''
                      cChar := ''
                      EXIT
                   ENDIF
                ENDDO

             CASE ( cChar == '&' .AND. SubStr( sBuffer, nPosition + 1, 1 ) == '&' )

                WHILE .T.
                   nClose := At( Chr(10), SubStr( sBuffer, nPosition + 2 ) )

                   IF nClose == 0
                      FSeek( hSource, -1, 1 )
                      nLen := FRead( hSource, @sBuffer, 16384 )
                      IF nLen < 3
                         nPosition := nLen
                         cChar := ''
                         EXIT
                      ENDIF
                      nMaxPos   := nLen - 1
                      nPosition := 1
                      LOOP
                   ELSE
                      nLine++
                      IF bCount
                         @ Row(), 0 SAY nLine
                      ENDIF
                      FWrite( hPP, ProcessLine( sLine, {}, {}, {}, nLine, sSource ) + Chr(13) + Chr(10) )
                      nPosition += ( nClose + 1 )
                      sLine := ''
                      cChar := ''
                      EXIT
                   ENDIF
                ENDDO

             CASE ( cChar == '"' )

                WHILE .T.
                   nClose := At( '"', SubStr( sBuffer, nPosition + 1 ) )

                   IF nClose == 0
                      sLine += SubStr( sBuffer, nPosition )
                      nLen := FRead( hSource, @sBuffer, 16384 )
                      IF nLen < 3
                         Alert( "ERROR! Unterminated string" )
                         RETURN .F.
                      ENDIF
                      nMaxPos   := nLen - 1
                      nPosition := 1
                      LOOP
                   ELSE
                      sLine += SubStr( sBuffer, nPosition, nClose )
                      nPosition += ( nClose )
                      EXIT
                   ENDIF
                ENDDO

             CASE ( cChar == "'" )

                WHILE .T.
                   nClose := At( "'", SubStr( sBuffer, nPosition + 1 ) )

                   IF nClose == 0
                      sLine += SubStr( sBuffer, nPosition )
                      nLen := FRead( hSource, @sBuffer, 16384 )
                      IF nLen < 3
                         Alert( "ERROR! Unterminated string" )
                         RETURN .F.
                      ENDIF
                      nMaxPos   := nLen - 1
                      nPosition := 1
                      LOOP
                   ELSE
                      sLine += SubStr( sBuffer, nPosition, nClose )
                      nPosition += ( nClose )
                      EXIT
                   ENDIF
                ENDDO

             CASE cChar == Chr(9)
                sLine += "    "
                cChar := ''

             CASE cChar == Chr(10)

                DropTrailingWS( @sLine, @sRight )

                IF Right( sLine, 1 ) == ';'
                   nLen  := Len( sLine )
                   sLine := DropTrailingWS( Left( sLine, nLen - 1 ), @sRight )
                   nLine++
                   IF bCount
                      @ Row(), 0 SAY nLine
                   ENDIF
                   FWrite( hPP, ProcessLine( '', {}, {}, {}, nLine, sSource ) + Chr(13) + Chr(10) )

                   /* Skip leading spaces in continued next line. */
                   nPosition++
                   WHILE SubStr( sBuffer, nPosition, 1 ) $ ' ' + Chr(9)
                      nPosition++
                   ENDDO
                   nPosition--

                   //sLine += sRight

                   cChar := ' '
                ELSE
                   sLeft := ExtractLeadingWS( @sLine )
                   nLine++
                   IF bCount
                      @ Row(), 0 SAY nLine
                   ENDIF
                   IF Left( sLine, 1 ) == '*'
                      FWrite( hPP, ProcessLine( '', {}, {}, {}, nLine, sSource ) + Chr(13) + Chr(10) )
                      sLine := ''
                      cChar := ''
                   ELSE
                      sLine += sRight
                      FWrite( hPP, ProcessLine( sLeft + sLine, {}, {}, {}, nLine, sSource ) + Chr(13) + Chr(10) )
                      sLine := ''
                      cChar := ''
                   ENDIF
                ENDIF

             CASE cChar == Chr(13)
                nPosition++
                LOOP

             CASE cChar == Chr(28)
                nLine++
                IF bCount
                   @ Row(), 0 SAY nLine
                ENDIF
                sLeft := ExtractLeadingWS( @sLine )
                IF Left( sLine, 1 ) == '*'
                   FWrite( hPP, ProcessLine( '', {}, {}, {}, nLine, sSource ) + Chr(13) + Chr(10) )
                   sLine := ''
                   cChar := ''
                ELSE
                   FWrite( hPP, ProcessLine( sLeft + sLine, {}, {}, {}, nLine, sSource ) + Chr(13) + Chr(10) )
                   sLine := ''
                   cChar := ''
                ENDIF

          ENDCASE

          sLine += cChar
          nPosition++

      ENDDO

      FSeek( hSource, -2, 1 )

   ENDDO

   //? '"' + SubStr( Left( sBuffer, nLen ), nPosition ) + '"', nLen, nPosition

   FClose( hSource )

   sLeft := ExtractLeadingWS( @sLine )

   nLine++
   IF bCount
      @ Row(), 0 SAY nLine
   ENDIF
   IF Left( sLine, 1 ) == '*'
      FWrite( hPP, ProcessLine( '', {}, {}, {}, nLine, sSource ) + Chr(13) + Chr(10) )
      sLine := ''
      cChar := ''
   ELSE
      FWrite( hPP, ProcessLine( sLeft + sLine, {}, {}, {}, nLine, sSource ) + Chr(13) + Chr(10) )
      sLine := ''
      cChar := ''
   ENDIF

   IF ProcName(1) == 'MAIN'
      FClose( hPP )

      IF bCCH
         CompileToCCH( sSource )
      ENDIF
   ENDIF

RETURN .T.

FUNCTION ProcessLine( sLine, aDefined, aTranslated, aCommanded, nLine, sSource )

   LOCAL nNext, sDirective, bX, sToken, bNewLine, nRule
   LOCAL nNewLineAt, aLines, nLines, Counter, nOffset := 0
   LOCAL sLeft, sPassed := '', asOutLines := {}, sOut := ''
   LOCAL cChar, sLastToken, sTemp, cFirstChar, cLastChar
   LOCAL bString, nLen

   WHILE .T.

      //? "Raw Processing: '", sLine, "'", nPendingLines, nOffset

      IF ! sPassed == ''
         aAdd( asOutLines, ( sLeft + DropTrailingWS( sPassed ) ) )
         sPassed := ''
         //? "Pending Out: '" + aTail( asOutLines ) + "'"
         //WAIT
      ENDIF

      IF sLine == NIL
         sLine := ''
         sLeft := ''
      ELSE
         sLeft := ExtractLeadingWS( @sLine )
      ENDIF

      IF sLine == ''
         IF nPendingLines > 0
            sLine := aPendingLines[1]
            aDel( aPendingLines, 1 )
            nPendingLines--
            LOOP
         ENDIF

         EXIT
      ENDIF

      IF sIncludeFile != NIL
         aAdd( asOutLines, "#line " + LTrim( Str( nLine ) ) + ' "' + Upper( sSource ) + '"' )
         sIncludeFile := NIL
      ENDIF

      //? "Processing: '" + sLine +"'"
      //WAIT

      IF Left( sLine, 1 ) == '#'

         nNext := NextWS( sLine )

         IF nNext == 0
            sDirective := Upper( SubStr( sLine, 2 ) )
         ELSE
            sDirective := Upper( SubStr( sLine, 2, nNext - 2 /* Started at 2nd postion! */ ) )
            sLine := SubStr( sLine, nNext + 1 )
         ENDIF

         IF ( nLen := Len( sDirective ) ) < 4
            Alert( "ERROR! Unknown directive: '" + sDirective + "'" )
            sLine := ''
            LOOP
         ENDIF

         IF sDirective == "ELSE"

            abIfDef[ nIfDef ] := ! abIfDef[ nIfDef ]
            sLine := ''
            LOOP

         ELSEIF sDirective == Left( "ENDIF", nLen )

            IF nIfDef > 0
               nIfDef--
            ELSE
               Alert( "ERROR! #endif with no #ifdef in sight" )
            ENDIF

            sLine := ''
            LOOP

         ENDIF

         IF nNext == 0
            Alert( "ERROR! Invalid directive format." )
            sLine := ''
            LOOP
         ENDIF

         ExtractLeadingWS( @sLine )

         IF sDirective == Left( "DEFINE", nLen )

            CompileDefine( sLine )
            sLine := ''
            LOOP

         ELSEIF sDirective == Left( "UNDEF", nLen )

            RemoveDefine( sLine )
            sLine := ''
            LOOP

         ELSEIF sDirective == Left( "IFDEF", nLen )

            SetIfDef( sLine, .T. )
            sLine := ''
            LOOP

         ELSEIF sDirective == Left( "IFNDEF", nLen )

            SetIfDef( sLine, .F. )
            sLine := ''
            LOOP

         ELSEIF sDirective == Left( "INCLUDE", nLen )

            sLine := SubStr( sLine, 2 )
            DropTrailingWS( @sLine )
            sIncludeFile := Left( sLine, Len( sLine ) - 1 )
            ProcessFile( sIncludeFile )

            /* Thread safety! Might have changed in recursion. */
            sIncludeFile := Left( sLine, Len( sLine ) - 1 )

            sLine := ''
            LOOP

         ELSE

            IF Left( sDirective, 1 ) == 'X'
               bX := .T.
               sDirective := SubStr( sDirective, 2 )
               nLen--
            ELSE
               bX := .F.
            ENDIF

            IF sDirective == Left( 'TRANSLATE', nLen )

               CompileRule( sLine, aTransRules, aTransResults, bX, .T. )
               sLine := ''
               LOOP

            ELSEIF sDirective == Left( 'COMMAND', nLen )

               CompileRule( sLine, aCommRules, aCommResults, bX, .T. )
               sLine := ''
               LOOP

            ELSE

               Alert( "ERROR! Unknown directive: '" + sLine + "'" )
               sLine := ''
               LOOP

            ENDIF

         ENDIF

      ENDIF

      aCommanded := {}

      WHILE .T.

         IF nIfDef > 0 .AND. ! abIfDef[nIfDef]
            sLine := ''
            EXIT
         ENDIF

         IF ( sToken := NextToken( @sLine, .F. ) /* bCheckRules */  ) == NIL
            /* EOL */
            EXIT
         ELSE
            /* Reset Cyclic history. */
            aDefined    := {}
            aTranslated := {}
            /*aCommanded  := {}*/

            IF sPassed == ''
               bNewLine    := .T.
            ELSE
               bNewLine    := .F.
            ENDIF
         ENDIF

         //? "Token = '"  + sToken + "'"
         //WAIT

         /* Skip strings. */
         IF Left( sToken , 2 ) == "[[" .OR. ( cChar := Left( sToken , 1 ) ) == '[' .OR. cChar == '"' .OR. cChar == "'"

            bString := .T.

            /* Array Index. */
            IF cChar == '['

               cFirstChar := Left( sLastToken, 1 )
               cLastChar  := Right( DropTrailingWS( sLastToken ), 1 )

               IF cLastChar $ ']})' .OR. cFirstChar == '_' .OR. cFirstChar >= 'A' .OR. cFirstChar <= 'Z' .OR. cFirstChar >= 'a' .OR. cFirstChar <= 'z'
                  bString := .F.

                  #ifdef NOT_DEFINED
                     sTemp := NextExp( @sLine, '<', NIL, NIL, NIL )

                     sTemp := NextExp( @sLine, ',', NIL, NIL, sNextAnchor ) // Content
                     IF sTemp == NIL
                        Alert( "ERROR! Unbalanced '['" )
                        EXIT
                     ELSE
                        sToken +=  sTemp
                     ENDIF

                     sTemp := NextToken( @sLine, .T. ) /* bCheckRules */ // Close
                     IF sToken == NIL .OR. Left( sToken, 1 ) != ']'
                        Alert( "ERROR! Unbalanced '['" )
                        EXIT
                     ENDIF

                     sToken += sTemp
                     sPassed += sToken
                     LOOP
                  #endif

               ENDIF

            ENDIF

            IF bString
               sLine  := sToken + sLine
               sToken := NextExp( @sLine, '<', NIL, NIL, NIL )
               IF sToken == NIL
                  EXIT
               ENDIF
               sPassed += sToken
               LOOP
            ENDIF
         ENDIF

         IF ( nRule := MatchRule( sToken, @sLine, aDefRules, aDefResults, .F., .F. ) ) > 0
            IF aScan( aDefined, nRule ) > 0
               Alert( "ERROR! Circularity detected in #define " + sLine )
               EXIT
            ENDIF

            aAdd( aDefined, nRule )

            sLine   := sLeft + sPassed + sLine
            sPassed := ''

            WHILE ( nNewLineAt := At( ';', sLine ) ) > 0
               nPendingLines++
               IF nPendingLines > Len( aPendingLines )
                  aAdd( aPendingLines, Left( sLine, nNewLineAt - 1 ) )
               ELSE
                  aPendingLines[nPendingLines] := Left( sLine, nNewLineAt - 1 )
               ENDIF

               //? "Pending #", nPendingLines,  Left( sLine, nNewLineAt - 1 ), aPendingLines[nPendingLines]
               sLine := SubStr( sLine, nNewLineAt + 1 )
            ENDDO

            IF ! ( sLine == '' )
               nPendingLines++
               IF nPendingLines > Len( aPendingLines )
                  aAdd( aPendingLines, sLine )
               ELSE
                  aPendingLines[nPendingLines] := sLine
               ENDI

               //? "Pending #", nPendingLines, sLine, aPendingLines[nPendingLines]
               sLine := ''
            ENDIF

            //? "Lines Pending:", nPendingLines

            EXIT

         ENDIF

         IF ( nRule := MatchRule( sToken, @sLine, aTransRules, aTransResults, .F., .T. ) ) > 0

            //? "TRANSLATED: " + sLine
            //WAIT

            IF aScan( aTranslated, nRule ) > 0
               Alert( "ERROR! Circularity detected in #translate " + sToken )
               EXIT
            ENDIF

            aAdd( aTranslated, nRule )

            sLine   := sLeft + sPassed + sLine
            sPassed := ''

            WHILE ( nNewLineAt := At( ';', sLine ) ) > 0
               nPendingLines++
               IF nPendingLines > Len( aPendingLines )
                  aAdd( aPendingLines, Left( sLine, nNewLineAt - 1 ) )
               ELSE
                  aPendingLines[nPendingLines] := Left( sLine, nNewLineAt - 1 )
               ENDIF

               //? "Pending #", nPendingLines,  Left( sLine, nNewLineAt - 1 ), aPendingLines[nPendingLines]
               sLine := SubStr( sLine, nNewLineAt + 1 )
            ENDDO

            IF ! ( sLine == '' )
               nPendingLines++
               IF nPendingLines > Len( aPendingLines )
                  aAdd( aPendingLines, sLine )
               ELSE
                  aPendingLines[nPendingLines] := sLine
               ENDI

               //? "Pending #", nPendingLines,  sLine, aPendingLines[nPendingLines]
               sLine := ''
            ENDIF

            //? "Lines Pending:", nPendingLines

            EXIT

         ENDIF

         IF bNewLine .AND. ( nRule := MatchRule( sToken, @sLine, aCommRules, aCommResults, .T., .T. ) ) > 0

            //? "COMMANDED: " + sLine
            //? '"' + sLeft +'"', '"' + sPassed + '"'
            //WAIT

            IF aScan( aCommanded, nRule ) > 0
               Alert( "ERROR! Circularity detected in #command " + sLine )
               EXIT
            ENDIF

            aAdd( aCommanded, nRule )

            sLine := sLeft /*+ sPassed */ + sLine // Can't have sPassed for #COMMAND

            WHILE ( nNewLineAt := At( ';', sLine ) ) > 0
               nPendingLines++
               IF nPendingLines > Len( aPendingLines )
                  aAdd( aPendingLines, Left( sLine, nNewLineAt - 1 ) )
               ELSE
                  aPendingLines[nPendingLines] := Left( sLine, nNewLineAt - 1 )
               ENDIF

               //? "Pending #", nPendingLines,  Left( sLine, nNewLineAt - 1 ), aPendingLines[nPendingLines]
               sLine := SubStr( sLine, nNewLineAt + 1 )
            ENDDO

            IF ! ( sLine == '' )
               nPendingLines++
               IF nPendingLines > Len( aPendingLines )
                  aAdd( aPendingLines, sLine )
               ELSE
                  aPendingLines[nPendingLines] := sLine
               ENDI

               //? "Pending #", nPendingLines,  sLine, aPendingLines[nPendingLines]
               sLine := ''
            ENDIF

            //? "Lines Pending:", nPendingLines

            EXIT

         ENDIF

         sLastToken := sToken
         sPassed += sToken

      ENDDO

   ENDDO

   sOut := ''
   nLines := Len( asOutLines )
   //@ Row(), 0 SAY nLines
   FOR Counter := 1 TO nLines
       sOut += asOutLines[Counter]
       IF Counter < nLines .OR. ! ( sPassed == '' )
          sOut += ' ;'
       ENDIF
   NEXT

   IF ! ( sPassed == '' )
      sOut += ( sLeft + sPassed )
   ENDIF

   //? "Returning: " + sOut
   //WAIT

RETURN sOut

FUNCTION MatchRule( sKey, sLine, aRules, aResults, bStatement, bUpper )

   LOCAL Counter, nRules, sCommand, nRule, aMarkers, xMarker
   LOCAL aMP, nOptional := 0, sAnchor, cType, aList, nMarkerId
   LOCAL sToken, sWorkLine, sNextExp, sNextAnchor, nMatch, nMatches
   LOCAL sPad, asRevert := {}, bNext, sPreMatch, sTemp, nLen

   nRules   := Len( aRules )

   IF nRules == 0 .OR. sKey == NIL
      RETURN 0
   ENDIF

   nRule    := nRules + 1
   sPad     := ''

   IF bUpper
      sKey  := Upper( DropTrailingWS( sKey, @sPad ) )
   ELSE
      DropTrailingWS( @sKey, @sPad )
   ENDIF

   IF bDbgMatch
      ? "Matching Key: '" + sKey + "' Line: " + sLine
      WAIT
   ENDIF

   nLen := Max( Len( sKey ), 4 )

   WHILE .T.

      nRule--

      FOR Counter := nRule TO 1 STEP -1
         IF aRules[Counter][3]
            IF aRules[ Counter ][1] == sKey
               EXIT
            ENDIF
         ELSE
            IF Left( aRules[ Counter ][1], nLen ) == sKey
               EXIT
            ENDIF
         ENDIF
      NEXT

      IF Counter == 0
         IF bDbgMatch
            ? "No Prospects, returning: " + sLine
            WAIT
         ENDIF

         RETURN 0
      ELSE
         nRule := Counter
      ENDIF

      sWorkLine := sLine

      IF bDbgMatch
         ? "KEY: " + sKey + " Matching Rule: " + Str( nRule, 3 ) + " with: " + sWorkLine
         WAIT
      ENDIF

      IF aRules[nRule][2] == NIL
         nMatches := 0
      ELSE
         nMatches := Len( aRules[nRule][2] )
      ENDIF

      IF nMatches == 0
         IF bStatement .AND. ! ( sWorkLine == '' )
            IF bDbgMatch
               ? "Statement failed"
               WAIT
            ENDIF

            LOOP
         ENDIF

         sLine := ( PPOut( aResults[nRule], aMarkers ) + sPad + sWorkLine )
         RETURN nRule
      ENDIF

      aMarkers  := aResults[nRule][3]
      IF aMarkers != NIL
         aFill( aMarkers, NIL )
      ENDIF

      nMatch    := 1
      aMP       := aRules[nRule][2][1]
      nOptional := 0
      bNext     := .F.

      DO WHILE .T. //! ( sWorkLine == '' )

         aMP       := aRules[nRule][2][nMatch]

         nMarkerId := aMP[1]
         sAnchor   := aMP[3]
         cType     := aMP[4]
         aList     := aMP[5]

         /* "Used" non repeatable! */
         IF nMarkerID > 0 .AND. nMarkerID < 1000
            IF aMarkers[nMarkerID] != NIL
               IF bDbgMatch
                  ? "Used:", nMatch, nMarkerId, aMarkers[nMarkerId]
                  WAIT
               ENDIF

               IF nMatch < nMatches
                  nMatch++
                  LOOP
               ELSE
                  EXIT
               ENDIF
            ENDIF
         ELSEIF nMarkerId > 1000
            nMarkerId -= 1000
         ENDIF

         /* Head of new optional group. */
         IF aMP[2] > 0

            /* Do we have to look for a stopper? */
            IF sAnchor == NIL .AND. ValType( aList ) == 'A'

               nOptional := aMP[2]

               sToken    := NextToken( sWorkLine, .F. ) // Not using @ so sWorkLine is intact.
               IF sToken != NIL .AND. cType != ':'
                  sToken := Upper( DropTrailingWS( sToken ) )

                  /* Is it a stopper (the anchor of another acceptable match) ? */
                  IF bDbgMatch
                     ? "Stopper?: " + sToken
                  ENDIF

                  IF aRules[nRule][3]
                     nLen := 10
                  ELSE
                     nLen := Max( 4, Len( sToken) )
                  ENDIF

                  IF aScan( aList, {|sStopper| Left( sStopper, nLen ) == sToken } ) > 0

                     IF bDbgMatch
                        ? "Found stopper"
                     ENDIF

                     /* Rewind to beging of same level and then search for the stopper match */

                     /* Current level */
                     nOptional := aMP[2]

                     IF nOptional < 0
                        nOptional := Abs( nOptional )
                     ENDIF

                     WHILE nMatch > 1
                        nMatch--
                        IF aRules[nRule][2][nMatch][2] >= 0 .AND. aRules[nRule][2][nMatch][2] < nOPtional
                           EXIT
                        ENDIF
                     ENDDO
                     IF aRules[nRule][2][nMatch][2] >= 0 .AND. aRules[nRule][2][nMatch][2] < nOPtional
                        nMatch++
                     ENDIF

                     /* Now search for the stopper. */
                     WHILE nMatch < nMatches
                        nMatch++
                        aMP := aRules[nRule][2][nMatch]

                        IF aMP[2] >= 0 .AND. aMP[2] <= nOptional .AND. aMP[3] == sToken
                           EXIT
                        ENDIF
                     ENDDO

                     nOptional := 0
                     LOOP

                  ELSE

                     IF bDbgMatch
                        ? sToken, "Not a stopper."
                     ENDIF

                  ENDIF

                  IF bDbgMatch
                     WAIT
                  ENDIF

               ENDIF

            ENDIF

         ENDIF

         IF bDbgMatch
            IF sAnchor == NIL
               ? nMatch, 'of', nMatches, "NO Anchore!", nMarkerId, nOptional, aMP[2]
            ELSE
               ? nMatch, 'of', nMatches, "Searching for Anchore: '" + sAnchor + "'", nMarkerId, nOptional, aMP[2]
            ENDIF
            WAIT
         ENDIF

         IF nMatch < nMatches
            sNextAnchor := aRules[Counter][2][nMatch + 1][3]
         ENDIF

         sToken    := NIL
         xMarker   := NIL
         sPreMatch := sWorkLine

         IF ( sAnchor == NIL .OR. ;
              ( ( ( sToken := NextToken( @sWorkLine, .T. ) ) != NIL  .AND. ( DropTrailingWS( @sToken, @sPad ), nLen := Max( 4, Len( sToken ) ), Upper( sToken ) == Left( sAnchor, nLen ) ) ) ) ) ;
            .AND. ( nMarkerId == 0 .OR. ( ( xMarker := NextExp( @sWorkLine, cType, aList, NIL, sNextAnchor ) ) != NIL ) )

            IF bDbgMatch
               ? "sKey =", sKey, "Anchor =", sAnchor, "nMarkerId =", nMarkerId, "sToken =", sToken, "xMarker =", xMarker, "<="
            ENDIF

            IF ValType( xMarker ) == 'C'
               DropTrailingWS( @xMarker )
            ENDIF

            IF nMarkerId > 0
               /* Repeatable. */
               IF aMP[1] > 1000
                  IF aMarkers[nMarkerId] == NIL
                     aMarkers[nMarkerId] := {}
                  ENDIF
                  aAdd( aMarkers[nMarkerId], xMarker )

                  IF bDbgMatch
                     ? nMarkerId, "Repetable added: ", xMarker, Len( aMarkers[nMarkerId] )
                  ENDIF
               ELSE
                  aMarkers[nMarkerId] := xMarker
               ENDIF
            ENDIF

            IF aMP[2] > 0 .AND. nOptional < 0

               nOptional := aMP[2]

               /* Save. */
               aSize( asRevert, nOptional )
               asRevert[nOptional] := sPreMatch

               IF bDbgMatch
                  ? "*** Saved: " + asRevert[nOptional]
                  WAIT
               ENDIF

            ELSEIF aMP[2] > 0 .AND. nOptional >= 0 .AND. aMP[2] >= nOptional

               nOptional := aMP[2]

               /*
               sTemp := ''
               IF ! ( sToken == NIL )
                  sTemp := sToken + sPad
               ENDIF
               IF ! ( xMarker == NIL )
                  IF ValType( xMarker == 'A' )
                     nWords := Len( xMarker )
                     FOR Counter := 1 TO nWords
                        sTemp += xMarker[Counter]
                        IF Counter < nWords
                           sTemp += ','
                        ENDIF
                     NEXT
                  ELSE
                     sTemp += xMarker
                  ENDIF
               ENDIF
               sTemp += sWorkLine
               */

               /* Save. */
               aSize( asRevert, nOptional )
               asRevert[nOptional] := sPreMatch

               IF bDbgMatch
                  ? "*** Saved: " + asRevert[nOptional]
                  WAIT
               ENDIF

            ENDIF

            IF aMP[2] <> 0
               IF bDbgMatch
                  ? "Optional"
               ENDIF

               /* We reached the end of current optional group - Rewind, to 1st optional at same level. */
               IF nMatch == nMatches .OR. aRules[nRule][2][nMatch + 1][2] >= 0 .AND. aRules[nRule][2][nMatch + 1][2] <= Abs( aMP[2] )

                  /* Current level */
                  nOptional := aMP[2]

                  IF nOptional < 0
                     nOptional := Abs( nOptional )
                  ENDIF

                  WHILE nMatch > 1
                     nMatch--
                     IF aRules[nRule][2][nMatch][2] >= 0 .AND. aRules[nRule][2][nMatch][2] < nOPtional
                        EXIT
                     ENDIF
                  ENDDO
                  IF aRules[nRule][2][nMatch][2] >= 0 .AND. aRules[nRule][2][nMatch][2] < nOPtional
                     nMatch++
                  ENDIF

                  /* Continue matching at top of current group. */
                  nOptional := 0

                  IF bDbgMatch
                     ? "Rewinded to:", nMatch
                  ENDIF

                  LOOP

               ENDIF

            ENDIF

            IF bDbgMatch
               WAIT
            ENDIF

            IF nMatch == nMatches
               IF bStatement .AND. ! ( sWorkLine == '' )
                  bNext := .T.

                  IF bDbgMatch
                     ? "Unmatched remainder: " + sWorkLine
                     ? "Statement failed, try next rule...'"
                     WAIT
                  ENDIF

                  EXIT
               ENDIF

               sLine := ( PPOut( aResults[nRule], aMarkers ) + sPad + sWorkLine )
               RETURN nRule
            ELSE
               IF bDbgMatch
                  ? "Accepted:", sToken, xMarker
               ENDIF

               nMatch++
               LOOP
            ENDIF
         ELSE
            IF bDbgMatch
               ? "NO MATCH:", nMatch, "of", nMatches, sAnchor, sToken, nMarkerId, xMarker, nOptional, aMP[2]
            ENDIF

            IF aMP[2] <> 0
               /* Revert. */
               IF nOptional <> 0 .AND. aMP[2] < 0
                  sWorkLine := asRevert[Abs(nOptional)]

                  IF bDbgMatch
                     ? "*** Reverted: " + asRevert[nOptional]
                     WAIT
                  ENDIF
               ELSE
                  sWorkLine := sPreMatch
                  IF bDbgMatch
                     ? "*** Reclaimed token/marker: " + sWorkLine
                     WAIT
                  ENDIF
               ENDIF

               /* Optional (last) didn't match - Rule can still match. */
               IF nMatch == nMatches
                  IF bStatement .AND. ! ( sWorkLine == '' )
                     bNext := .T.

                     IF bDbgMatch
                        ? "Statement failed, try next rule..."
                        WAIT
                     ENDIF

                     EXIT
                  ENDIF

                  sLine := ( PPOut( aResults[nRule], aMarkers ) + sWorkLine )
                  RETURN nRule
               ELSE
                  /* Skip all same level optionals to next group. */
                  nOptional := aMP[2]
                  WHILE nMatch < nMatches
                     nMatch++
                     aMP := aRules[nRule][2][nMatch]
                     IF ( aMP[2] >= 0 ) .AND. ( aMP[2] <= Abs( nOptional ) )
                        EXIT
                     ENDIF
                  ENDDO

                  IF bDbgMatch
                     ? "Skipped to", nMatch, "of", nMatches, aMP[2], aMP[3], nOptional
                  ENDIF

                  IF ( aMP[2] >= 0 ) .AND. ( aMP[2] <= Abs( nOptional ) )
                     nOptional := 0
                     LOOP
                  ELSE
                     EXIT
                  ENDIF
               ENDIF
            ELSE
               IF bDbgMatch
                  ? "Match failed, try next rule..."
                  WAIT
               ENDIF

               bNext := .T.
               EXIT
            ENDIF
         ENDIF

      ENDDO

      IF bNext
         IF bDbgMatch
            ? "NEXT Rule requested."
         ENDIF

         LOOP
      ENDIF

      IF bDbgMatch
         ? "EOL."
         WAIT
      ENDIF

      IF nMatch < nMatches

         IF bDbgMatch
            ? "Checking if Rule remainder is optional."
         ENDIF

         /* Current and remainder of MP might be optional */
         IF aMP[2] <> 0
            IF nOptional <> 0 .AND. aMP[2] < 0
               sWorkLine := asRevert[Abs(nOptional)]

               IF bDbgMatch
                  ? "*** Reverted: " + asRevert[nOptional]
               ENDIF
            ELSE
               sWorkLine := sPreMatch

               IF bDbgMatch
                  ? "*** Reclaimed token/marker: " + sWorkLine
               ENDIF
            ENDIF

            IF bDbgMatch
               WAIT
            ENDIF

            nOptional := aMP[2]
            WHILE nMatch < nMatches
               nMatch++
               aMP := aRules[nRule][2][nMatch]
               IF ( aMP[2] == 0 )
                  EXIT
               ENDIF

               IF bDbgMatch
                  ? "Skipped:", aMP[2], aMP[3]
               ENDIF
            ENDDO

            IF ( aMP[2] == 0 )
               LOOP

               IF bDbgMatch
                  ? "Statement failed, try next rule..."
                  WAIT
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      sLine := ( PPOut( aResults[nRule], aMarkers ) + sWorkLine )
      RETURN nRule

   ENDDO

   Alert( "ERROR! Llogic failure" )

RETURN 0

FUNCTION NextToken( sLine, bCheckRules )

  LOCAL  sReturn := NIL, cChar, Counter, nLen, sPad

  //? "Called from: " + ProcName(1) + " Line: " + Str( ProcLine(1) ) + " Scaning: " + sLine

  IF Left( sLine, 2 ) == "=="

     sReturn := Left( sLine, 2 )

  ELSEIF Left( sLine, 2 ) == "<="

     sReturn := Left( sLine, 2 )

  ELSEIF Left( sLine, 2 ) == ">="

     sReturn := Left( sLine, 2 )

  ELSEIF Left( sLine, 2 ) == "<>"

     sReturn := Left( sLine, 2 )

  ELSEIF Left( sLine, 2 ) == "->"

     sReturn := Left( sLine, 2 )

  ELSEIF Left( sLine, 2 ) == ":="

     sReturn := Left( sLine, 2 )

  ELSEIF Left( sLine, 2 ) == "^="

     sReturn := Left( sLine, 2 )

  ELSEIF Left( sLine, 2 ) == "*="

     sReturn := Left( sLine, 2 )

  ELSEIF Left( sLine, 2 ) == "/="

     sReturn := Left( sLine, 2 )

  ELSEIF Left( sLine, 2 ) == "+="

     sReturn := Left( sLine, 2 )

  ELSEIF Left( sLine, 2 ) == "-="

     sReturn := Left( sLine, 2 )

  ELSEIF Left( sLine, 2 ) == "++"

     sReturn := Left( sLine, 2 )

  ELSEIF Left( sLine, 2 ) == "--"

     sReturn := Left( sLine, 2 )

  ELSEIF Left( sLine, 2 ) == "**"

     sReturn := Left( sLine, 2 )

  ELSEIF Left( sLine, 2 ) == "[["

     sReturn := Left( sLine, 2 )

  ELSEIF Upper( Left( sLine, 3 ) ) == ".T."

     sReturn := ".T."

  ELSEIF Upper( Left( sLine, 3 ) ) == ".F."

     sReturn := ".F."

  ELSEIF Left( sLine, 1 ) $ "?+-*/:=^!&()[]&@,'" + '"'

     sReturn := Left( sLine, 1 )

  ELSEIF Left( sLine, 1 ) == '.' .AND. Upper( SubStr( sLine, 2, 3 ) ) == 'AND' .AND. SubStr( sLine, 5, 1 ) == '.'

     sReturn := ".AND."

  ELSEIF Left( sLine, 1 ) == '.' .AND. Upper( SubStr( sLine, 2, 2 ) ) == 'OR' .AND. SubStr( sLine, 4, 1 ) == '.'

     sReturn := ".OR."

  ELSEIF Left( sLine, 1 ) = '.' .AND. Upper( SubStr( sLine, 2, 3 ) ) == 'NOT' .AND. SubStr( sLine, 5, 1 ) == '.'

     sReturn := "!"

     /* Skip the unaccounted letters ( .NOT. <-> ! ) */
     sLine := SubStr( sLine, 5 )

  ELSE

     nLen := Len( sLine )

     FOR Counter := 1 TO nLen
        cChar := SubStr( sLine, Counter, 1 )

        IF cChar $ "+-*/:=^!&()[]&@,"

           sReturn := Left( sLine, Counter - 1 )
           EXIT

        ELSEIF cChar == '.' .AND. Upper( SubStr( sLine, Counter + 1, 4 ) ) == 'AND.'

           sReturn := Left( sLine, Counter - 1 )
           EXIT

        ELSEIF cChar == '.' .AND. Upper( SubStr( sLine, Counter + 1, 3 ) ) == 'OR.'

           sReturn := Left( sLine, Counter - 1 )
           EXIT

        ELSEIF cChar == '.' .AND. Upper( SubStr( sLine, Counter + 1, 4 ) ) == 'NOT.'

           sReturn := Left( sLine, Counter - 1 )
           EXIT

        ELSEIF cChar == ' ' .OR. cChar == Chr(9)

           sReturn := Left( sLine, Counter - 1 )
           EXIT

        ENDIF

     NEXT

     IF sReturn == NIL .AND. ! (sLine == '' )
        sReturn := sLine
        sLine := ''
     ENDIF

  ENDIF

  IF ( sReturn == NIL )
//? "TOKEN = 'NIL' AT: ", sLine
  ELSE
//? "TOKEN = '" + sReturn + "' Len: ", Len( sReturn ), "AT: ", sLine
     sLine := SubStr( sLine, Len( sReturn ) + 1 )
     sReturn += ExtractLeadingWS( @sLine )
  ENDIF

  IF bCheckRules
     IF MatchRule( sReturn, @sLine, aDefRules, aDefResults, .F., .F. ) > 0
        RETURN NextToken( @sLine, .T. )
     ENDIF

     IF MatchRule( sReturn, @sLine, aTransRules, aTransResults, .F., .T. ) > 0
        RETURN NextToken( @sLine, .T. )
     ENDIF
  ENDIF

RETURN sReturn

FUNCTION NextExp( sLine, cType, aWords, aExp, sNextAnchor )

  LOCAL sExp, nClose, cChar, sTemp, Counter, sWorkLine, sPad, sToken

  DO CASE
     CASE cType == '*'
         IF sLine == ''
            sExp := NIL
         ELSE
            sExp  := sLine
         ENDIF

         sLine := ''
         //? "EXP (*): " + sExp
         RETURN sExp

     CASE cType == ':'
        sWorkLine := sLine
        sExp := NextToken( @sLine, .T. ) /* bCheckRules */

        IF( sExp == NIL )
           RETURN NIL
        ELSE
           sExp := Upper( sExp )
           DropTrailingWS( @sExp, @sPad )
        ENDIF

        IF aScan( aWords, sExp ) > 0
           //? "EXP = " + sExp
           RETURN sExp + sPad
        ELSE
           sLine := sWorkLine
           RETURN NIL
        ENDIF

     CASE cType == 'A'

        IF aExp == NIL
           aExp := {}
        ENDIF

     CASE cType == NIL

        RETURN "-"

  ENDCASE

  sToken := NextToken( @sLine, .T. )

  IF sToken == NIL
     RETURN NIL
  ENDIF

  IF Left( sToken, 1 ) == '('

     sExp  := sToken
     sExp  += ExtractLeadingWS( @sLine )

     IF sNextAnchor != '('
        sTemp := NextExp( @sLine, ',', NIL, NIL, sNextAnchor ) // Content
        IF sTemp == NIL
           Alert( "ERROR! Unbalanced '('" )
        ELSE
           sExp +=  sTemp
        ENDIF

        sToken := NextToken( @sLine, .T. ) /* bCheckRules */ // Close
        IF sToken == NIL
           Alert( "ERROR! Unbalanced '('" )
        ELSE
           sExp += sToken
        ENDIF
     ENDIF

  ELSEIF Left( sToken, 1 ) == '{'

     sExp  := sToken
     sExp  += ExtractLeadingWS( @sLine )

     IF sNextAnchor != '{'
        sTemp := NextExp( @sLine, ',', NIL, NIL, sNextAnchor ) // Content
        IF sTemp == NIL
           Alert( "ERROR! Unbalanced '{'" )
        ELSE
           sExp +=  sTemp
        ENDIF

        sToken := NextToken( @sLine, .T. ) /* bCheckRules */ // Close
        IF sTemp == NIL
           Alert( "ERROR! Unbalanced '{'" )
        ELSE
           sExp += sToken
        ENDIF
     ENDIF

  ELSEIF Left( sToken, 1 ) == '"'

     nClose := AT( '"', sLine )
     IF nClose == 0
        Alert( [ERROR! Unterminated '"'] )
        RETURN NIL
     ELSE
        /* Might have a trailing WS that belongs to the string. */
        DropTrailingWS( @sToken, @Stemp )

        sExp  := sTemp + Left( sLine, nClose - 1 )
        sExp  := '"' + sExp + '"'
        sLine := SubStr( sLine, nClose + 1 )
        sExp  += ExtractLeadingWS( @sLine )
     ENDIF

  ELSEIF Left( sToken, 1 ) == "'"

     nClose := AT( "'", sLine )
     IF nClose == 0
        Alert( "ERROR! Unterminated [']" )
        RETURN NIL
     ELSE
        /* Might have a trailing WS that belongs to the string. */
        DropTrailingWS( @sToken, @Stemp )

        sExp  := sTemp + Left( sLine, nClose - 1 )
        IF ! ( '"' $ sExp )
           sExp := '"' + sExp + '"'
        ELSE
           sExp := "'" + sExp + "'"
        ENDIF
        sLine := SubStr( sLine, nClose + 1 )
        sExp  += ExtractLeadingWS( @sLine )
     ENDIF

  ELSEIF Left( sToken, 2 ) == "[["
     nClose := AT( ']]', sLine )
     IF nClose == 0
        Alert( "ERROR! Unterminated '[['" )
        RETURN NIL
     ELSE
        sExp  := sToken + Left( sLine, nClose + 1 )

        sLine := SubStr( sLine, nClose + 2 )
        sExp  += ExtractLeadingWS( @sLine )
     ENDIF

  ELSEIF Left( sToken, 1 ) == "["
     IF bDbgExp
        ? "Open String: " + sToken
     ENDIF

     nClose := AT( ']', sLine )
     IF nClose == 0
        Alert( "ERROR! Unterminated '['" )
        RETURN NIL
     ELSE
        /* Might have a trailing WS that belongs to the string. */
        DropTrailingWS( @sToken, @Stemp )

        sExp  := sTemp + Left( sLine, nClose - 1 )
        IF ! ( '"' $ sExp )
           sExp := '"' + sExp + '"'
        ELSEIF ! ( "'" $ sExp )
           sExp := "'" + sExp + "'"
        ELSE
           sExp := "[" + sExp + "]"
        ENDIF
        sLine := SubStr( sLine, nClose + 1 )
        sExp  += ExtractLeadingWS( @sLine )

        IF bDbgExp
           ? "String = " + sExp, "Next: " + sLine
           WAIT
        ENDIF
     ENDIF

  ELSE

     sExp  := sToken

  ENDIF

  IF sExp == NIL
     RETURN NIL
  ENDIF

  /*
  IF ( ! ( sNextAnchor == NIL ) ) .AND. NextToken( @sLine, .F.. ) == sNextAnchor
     RETURN IIF( cType == 'A', aExp, sExp )
  ENDIF
  */

  cChar := Left( sExp, 1 )

  /* Is Identifier */
  IF cChar == '_' .OR. ( cChar >= 'A' .AND. cChar <= 'Z' ) .OR. ( cChar >= 'a' .AND. cChar <= 'z' )

     IF Left( sLine, 1 ) $ "([" .AND. ( sNextAnchor == NIL  .OR. ( ! ( sNextAnchor $ "([" ) ) )

        IF bDbgExp
           ? 'Start Function/Array: ' + sExp
        ENDIF

        sExp  += Left( sLine, 1 ) // Open
        sLine := SubStr( sLine, 2 )
        sExp  += ExtractLeadingWS( @sLine )

        IF bDbgExp
           ? 'Opened: ' + sExp + ' AT: ' + sLine
        ENDIF

        IF Left( sLine, 1 ) == ')'
           sExp  += Left( sLine, 1 ) // Open
           sLine := SubStr( sLine, 2 )
           sExp  += ExtractLeadingWS( @sLine )
        ELSE
           sTemp := NextExp( @sLine, ',', NIL, NIL, sNextAnchor ) // Content
           IF sTemp == NIL
              Alert( "ERROR! Unbalanced '('" )
           ELSE
              sExp += sTemp
           ENDIF

           IF bDbgExp
              ? 'Catptured: ' + sExp + ' AT: ' + sLine
           ENDIF

           sToken := NextToken( @sLine, .T. ) /* bCheckRules */ // Close
           IF sToken == NIL
              Alert( "ERROR! Unbalanced ')'" )
           ELSE
              sExp += sToken
           ENDIF
        ENDIF

        IF bDbgExp
           ? 'Closed Function: ' + sExp + ' AT: ' + sLine
        ENDIF

     ENDIF

  ENDIF

  IF bDbgExp
     ? "sLine: '" + sLine + "'"
     WAIT
  ENDIF

  DO WHILE .T.

      IF Right( sExp, 1 ) == "&"

         IF cType == 'A'
            sExp  += NextExp( @sLine, '<', NIL, NIL, sNextAnchor )
         ELSE
            sTemp := NextExp( @sLine, cType, NIL, NIL, sNextAnchor ) // Content
            IF sTemp == NIL
               Alert( "ERROR! Unbalanced '['" )
            ELSE
               sExp +=  sTemp
            ENDIF
         ENDIF

      ELSEIF Right( sExp, 1 ) $ "}])" .AND. Left( sLine, 1 ) == '[' .AND. ( sNextAnchor != "[" ) /*.AND. At( ']', sLine ) < At( '[', SubStr( sLine, 2 ) ) */

            sExp  += Left( sLine, 1 ) // Open
            sLine := SubStr( sLine, 2 )
            sExp  += ExtractLeadingWS( @sLine )

            IF cType == 'A'
               sExp  += NextExp( @sLine, '<', NIL, NIL, sNextAnchor )
            ELSE
               sTemp := NextExp( @sLine, cType, NIL, NIL, sNextAnchor ) // Content
               IF sTemp == NIL
                  Alert( "ERROR! Unbalanced '['" )
               ELSE
                  sExp +=  sTemp
               ENDIF
            ENDIF

            sToken := NextToken( @sLine, .T. ) /* bCheckRules */ // Close
            IF sToken == NIL .OR. Left( sToken, 1 ) != ']'
               Alert( "ERROR! Unbalanced '['" )
            ELSE
               sExp += sToken
            ENDIF

            LOOP // Might need other checks, like cType ',' or 'A'

      ELSEIF cType == 'A' .AND. Left( sLine, 1 ) == ',' //.AND. ( sNextAnchor != "," )

         aAdd( aExp, sExp )

         //sExp  += ','
         sLine := SubStr( sLine, 2 )
         //sExp  += ExtractLeadingWS( @sLine )

         NextExp( @sLine, 'A', NIL, aExp, sNextAnchor )
         /* aExp already done. */

         sExp := NIL

         EXIT

      ELSEIF cType == ',' .AND. Left( sLine, 1 ) == ',' //.AND. ( sNextAnchor != "," )

         sExp  += ','
         sLine := SubStr( sLine, 2 )
         sExp  += ExtractLeadingWS( @sLine )

         sTemp := NextExp( @sLine, ',', NIL, NIL, sNextAnchor )
         IF sTemp == NIL
            //? "???"
            //sExp += "NIL"
         ELSE
            sExp += sTemp
         ENDIF

         EXIT

      ELSEIF Left( sLine, 1 ) $ "+-*/:=^!" .AND. ( sNextAnchor == NIL .OR. ( ! ( sNextAnchor $ "+-*/:=^!<>" ) ) )

         sExp  += Left( sLine, 1 )

         IF bDbgExp
            ? "Grabber: '" + sExp + "'"
         ENDIF

         sLine := SubStr( sLine, 2 )
         sExp  += ExtractLeadingWS( @sLine )

         IF bDbgExp
            ? "Next : '" + sLine + "'"
            WAIT
         ENDIF

         IF cType == 'A'
            sExp  += NextExp( @sLine, '<', NIL, NIL, sNextAnchor )
         ELSE
            sTemp := NextExp( @sLine, cType, NIL, NIL, sNextAnchor ) // Content
            IF sTemp == NIL
               //Alert( "ERROR! Unbalanced: '" + Left( sLine, 1 ) + "'" )
            ELSE
               sExp +=  sTemp
            ENDIF
         ENDIF

         LOOP

      ELSEIF Upper( Left( sLine, 5 ) ) == ".AND."

         sExp  += ".AND."
         sLine := SubStr( sLine, 6 )
         sExp  += ExtractLeadingWS( @sLine )
         IF cType == 'A'
            sExp  += NextExp( @sLine, '<', NIL, NIL, sNextAnchor )
         ELSE
            sExp  += NextExp( @sLine, cType, NIL, NIL, sNextAnchor )
         ENDIF

         LOOP

      ELSEIF Upper( Left( sLine, 4 ) ) == ".OR."

         sExp  += ".OR."
         sLine := SubStr( sLine, 5 )
         sExp  += ExtractLeadingWS( @sLine )
         IF cType == 'A'
            sExp  += NextExp( @sLine, '<', NIL, NIL, sNextAnchor )
         ELSE
            sExp  += NextExp( @sLine, cType, NIL, NIL, sNextAnchor )
         ENDIF

         LOOP

      ELSEIF Upper( Left( sLine, 5 ) ) == ".NOT."

         sExp  += "!"
         sLine := SubStr( sLine, 6 )
         sExp  += ExtractLeadingWS( @sLine )
         IF cType == 'A'
            sExp  += NextExp( @sLine, '<', NIL, NIL, sNextAnchor )
         ELSE
            sExp  += NextExp( @sLine, cType, NIL, NIL, sNextAnchor )
         ENDIF

         LOOP

      ELSE
         //? "DONT CONTINUE: " + sLine
         EXIT
      ENDIF
  ENDDO

  IF cType == 'A'
      IF sExp != NIL
         aAdd( aExp, sExp )
      ENDIF

     IF bDbgExp
        IF ! ( ProcName(1) == "NEXTEXP" )
           ? "List Exp: " + '{'
           FOR Counter := 1 TO Len( aExp )
              ?? aExp[Counter]
              IF Counter < Len( aExp )
                 ?? ','
              ENDIF
           NEXT
           ?? '}'
        ENDIF
     ENDIF
  ELSE
     IF bDbgExp
        ? "EXP = " + sExp, " Next: " + sLine
     ENDIF
  ENDIF

  IF bDbgExp
     WAIT
  ENDIF

RETURN IIF( cType == 'A', aExp, sExp )

FUNCTION PPOut( aResults, aMarkers )

  LOCAL Counter, nResults, sResult := "", nMarker, nMatches, nMatch//, aMarkers := aResults[3]
  LOCAL xValue, nRepeats := 0, nDependee, nGroupStart

  IF aResults[1] == NIL
     nResults := 0
  ELSE
     nResults := Len( aResults[1] )
  ENDIF

  FOR Counter := 1 TO nResults

      IF bDbgPPO
         ? Counter, "Result:", sResult, nGroupStart, nRepeats
         WAIT
      ENDIF

     /* Normal mode. */
     IF nRepeats == 0

        nDependee := aResults[1][Counter][1]

        IF nDependee > 0
           nGroupStart := Counter

           IF aMarkers[ nDependee ] == NIL
              nRepeats := 0
           ELSE
              nRepeats := Len( aMarkers[ nDependee ] )
           ENDIF

           IF bDbgPPO
              ? Counter, nDependee, aMarkers, ValType( aMarkers ), nRepeats
              WAIT
           ENDIF

           IF nRepeats > 0
              IF ValType( aResults[1][Counter][2] ) == 'N'
                 sResult += ' '
                 xValue := aMarkers[ nDependee ][1] // For group head nDependee and nMaker _must_ be identical.
                 aDel( aMarkers[ nDependee ], 1 )
                 aSize( aMarkers[ nDependee ], nRepeats - 1 )
              ELSE
                 sResult += aResults[1][Counter][2]
                 LOOP
              ENDIF
           ELSE
              IF bDbgPPO
                 ? "Skipping other dependants"
                 WAIT
              ENDIF

              /* Skip all other dependants. */
              Counter++
              WHILE Counter < nResults .AND. aResults[1][Counter][1] == nDependee
                 Counter++
              ENDDO
              Counter-- // LOOP will increased.
              LOOP
           ENDIF

        ELSE

           IF ValType( aResults[1][Counter][2] ) == 'N'
              xValue := aMarkers[ aResults[1][Counter][2] ]
           ELSE
              sResult += aResults[1][Counter][2]
              LOOP
           ENDIF

        ENDIF

     ELSE /* Repeat mode. */

        /* Still in repeat group? */
        IF aResults[1][Counter][1] == nDependee
           IF ValType( aResults[1][Counter][2] ) == 'N'
              IF Right( sResult, 1 ) != ' '
                 sResult += ' '
              ENDIF
              xValue := aMarkers[ aResults[1][Counter][2] ][1]
              aDel( aMarkers[ aResults[1][Counter][2] ], 1 )
              aSize( aMarkers[ nDependee ], nRepeats - 1 )
           ELSE
              sResult += aResults[1][Counter][2]
              LOOP
           ENDIF
        ELSE
           nRepeats--

           IF nRepeats > 0
              IF bDbgPPO
                 ? "Looping to: ", nGroupStart, nRepeats
                 WAIT
              ENDIF

              Counter := nGroupStart - 1 // LOOP will increase
              LOOP
           ELSE
              IF bDbgPPO
                 ? "Repeats Finished: "
                 WAIT
              ENDIF

              /* Recheck this item in "normal" mode. */
              Counter--
              LOOP
           ENDIF
        ENDIF

     ENDIF

     nMarker := aResults[1][Counter][2]

     IF bDbgPPO
        ? "Outputing:", Counter, nMarker, nGroupStart, nRepeats
        WAIT
     ENDIF

     DO CASE
        /* <-x-> Ommit. */
        CASE aResults[2][Counter] == 0

        /* <x> Regular */
        CASE aResults[2][Counter] == 1
           IF ValType( xValue ) == 'A'
              nMatches := Len( xValue )
              FOR nMatch := 1 TO nMatches
                 sResult += xValue[nMatch]
                 IF nMatch < nMatches
                    sResult += ', '
                 ENDIF
              NEXT
           ELSE
              IF xValue != NIL
                 sResult += xValue
              ENDIF
           ENDIF

        /* #<x> Dumb */
        CASE aResults[2][Counter] == 2
           IF ValType( xValue ) == 'A'
              sResult += '"'
              nMatches := Len( xValue )
              FOR nMatch := 1 TO nMatches
                 sResult += xValue[nMatch]
                 IF nMatch < nMatches
                    sResult += ', '
                 ENDIF
              NEXT
              sResult += '"'
           ELSE
              IF xValue == NIL
                 sResult += '""'
              ELSE
                 IF Left( xValue, 1 ) $ '"['
                    sResult += '[' + xValue + ']'
                 ELSE
                    sResult += '"' + xValue + '"'
                 ENDIF
              ENDIF
           ENDIF

        /* <"x"> Normal */
        CASE aResults[2][Counter] == 3
           IF ValType( xValue ) == 'A'
              nMatches := Len( xValue )
              FOR nMatch := 1 TO nMatches
                 IF Left( xValue[nMatch], 1 ) $ '"['
                    sResult += '[' + xValue[nMatch] + ']'
                 ELSE
                    sResult += '"' + xValue[nMatch] + '"'
                 ENDIF

                 IF nMatch < nMatches
                    sResult += ', '
                 ENDIF
              NEXT
           ELSE
              IF ! ( xValue == NIL )
                 IF Left( xValue, 1 ) $ '"['
                    sResult += '[' + xValue + ']'
                 ELSE
                    sResult += '"' + xValue + '"'
                 ENDIF
              ENDIF
           ENDIF

        /* <(x)> Smart */
        CASE aResults[2][Counter] == 4
           IF ValType( xValue ) == 'A'
              nMatches := Len( xValue )
              FOR nMatch := 1 TO nMatches
                 IF Left( xValue[nMatch], 1 ) == '('
                    sResult += xValue[nMatch]
                 ELSE
                       IF Left( xValue[nMatch], 1 ) $ '"['
                          sResult += '[' + xValue[nMatch] + ']'
                       ELSE
                          sResult += '"' + xValue[nMatch] + '"'
                       ENDIF
                 ENDIF

                 IF nMatch < nMatches
                    sResult += ', '
                 ENDIF
              NEXT
           ELSE
              IF xValue != NIL
                 IF Left( xValue, 1 ) == '('
                    sResult += xValue
                 ELSE
                    IF Left( xValue, 1 ) $ '"['
                       sResult += '[' + xValue + ']'
                    ELSE
                       sResult += '"' + xValue + '"'
                    ENDIF
                 ENDIF
              ENDIF
           ENDIF

        /* <{x}> Blockify */
        CASE aResults[2][Counter] == 5
           IF ValType( xValue ) == 'A'
              nMatches := Len( xValue )
              FOR nMatch := 1 TO nMatches
                 sResult += "{||" + xValue[nMatch] + '}'
                 IF nMatch < nMatches
                    sResult += ', '
                 ENDIF
              NEXT
           ELSE
              IF xValue != NIL
                 sResult += "{||" + xValue + '}'
              ENDIF
           ENDIF

        /* <.x.> Logify */
        CASE aResults[2][Counter] == 6
           IF ValType( xValue ) == 'A'
              nMatches := Len( xValue )
              FOR nMatch := 1 TO nMatches
                 sResult += ".T."
                 IF nMatch < nMatches
                    sResult += ', '
                 ENDIF
              NEXT
           ELSE
              IF Empty( xValue )
                 sResult += ".F."
              ELSE
                 sResult += ".T."
              ENDIF
           ENDIF

     ENDCASE

     IF bDbgPPO
        ? "Bottom: ", Counter, nMarker, nGroupStart, nRepeats
        WAIT
     ENDIF

     IF nRepeats > 0 .AND. Counter == nResults
        nRepeats--
        Counter := nGroupStart - 1
     ENDIF

  NEXT

  IF bDbgPPO
     ? "*** OUT: " + sResult
     WAIT
  ENDIF

RETURN sResult

FUNCTION CompileRule( sRule, aRules, aResults, bX, bUpper )

   LOCAL nNext, sKey, sAnchor := NIL, nOptional := 0, cType := NIL, nId := 0, aRule := NIL, aMatch, aWords := NIL
   LOCAL aTemp, nOptionalAt, nMarkerAt, aMarkers := {}, Counter, nType, aResult := {}, sTemp, aModifiers, aValues
   LOCAL aRP, nAt, sResult, nCloseAt, sMarker, nCloseOptionalAt, sPad, nResults, nMarker, nMP, nMatches

   /*
   nMarkerID
   nOPTIONAL
   sAnchor
   cTYPE
   aLIST
   aNext
   */

   ExtractLeadingWS( @sRule )

   sKey := NextToken( @sRule, .F. ) /* bCheckRules */
   DropTrailingWS( @sKey )

   IF bUpper
      sKey := Upper( sKey )
   ENDIF

//? "KEY: '" + sKey + "'"

   aRule := { sKey, {}, bX }

   nNext := At( "=>", sRule )
   IF nNext == 0
      Alert( "ERROR! Invalid translation format: " + sRule )
      RETURN .F.
   ELSE
      sResult := SubStr( sRule, nNext + 2 )
      ExtractLeadingWS( @sResult )
      sRule   := Left( sRule, nNext - 1 )
   ENDIF

   DO WHILE ! ( Left( sRule, 2 ) == '' )

//? "Scaning: " + sRule

      IF Left( sRule, 1 ) == '<'
         nId++

         DO CASE
            CASE SubStr( sRule, 2, 1 ) == '*'
               cType := '*'
               nNext := At( '*>', sRule )
               IF nNext > 0
                  sMarker := SubStr( sRule, 3, nNext - 3 )
                  ExtractLeadingWS( @sMarker )
                  aAdd( aMarkers, sMarker )

                  sRule := SubStr( sRule, nNext + 2 )
                  ExtractLeadingWS( @sRule )

                  aMatch := { nId, nOptional, sAnchor, cType, aWords }
                  /* Next dependant optional will be marked as trailing. */
                  IF nOptional > 0
                     nOptional := ( -nOptional )
                  ENDIF
//? aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]

                  aAdd( aRule[2], aMatch )

                  sAnchor := NIL
                  aWords  := NIL
                  cType   := NIL

                  LOOP
               ELSE
                  Alert( "ERROR! Unblanced MP: '<*' at: " + sRule )
                  RETURN .F.
               ENDIF

            CASE SubStr( sRule, 2, 1 ) == '('
               cType := '('
               nNext := At( ')>', sRule )
               IF nNext > 0
                  sMarker := SubStr( sRule, 3, nNext - 3 )
                  ExtractLeadingWS( @sMarker )
                  aAdd( aMarkers, sMarker )

                  sRule := SubStr( sRule, nNext + 2 )
                  ExtractLeadingWS( @sRule )

                  aMatch := { nId, nOptional, sAnchor, cType, aWords }
                  /* Next dependant optional will be marked as trailing. */
                  IF nOptional > 0
                     nOptional := ( -nOptional )
                  ENDIF
//? aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]

                  aAdd( aRule[2], aMatch )

                  sAnchor := NIL
                  aWords  := NIL
                  cType   := NIL

                  LOOP
               ELSE
                  Alert( "ERROR! Unbalanced MP: '<(' at: " + sRule )
                  RETURN .F.
               ENDIF

            OTHERWISE
               sRule := SubStr( sRule, 2 )
               ExtractLeadingWS( @sRule )

         ENDCASE

         nCloseAt := At('>', sRule )
         nNext := At( ",...>", sRule )

         IF nNext > 0 .AND. nNext < nCloseAt
//? "Extended: '" + sRule + "'"
            cType := 'A'

            sMarker := Left( sRule, nNext - 1 )
            ExtractLeadingWS( @sMarker )
            aAdd( aMarkers, sMarker )

            sRule := SubStr( sRule, nNext + 4 )
            ExtractLeadingWS( @sRule )

            nNext    := 0
            nCloseAt := 1
         ELSE
            nNext := At( ':', sRule )
         ENDIF

         IF nNext > 0 .AND. nNext < nCloseAt
            cType := ':'

//? "LIST"

            sMarker := Left( sRule, nNext - 1 )
            ExtractLeadingWS( @sMarker )
            aAdd( aMarkers, sMarker )

            sRule := SubStr( sRule, nNext + 1 )
            ExtractLeadingWS( @sRule )

            aWords := {}

            DO WHILE ! ( Left( sRule, 1 ) == '>' )

               nNext := At( ',', sRule )
               IF nNext > 0 .AND. nNext < At( '>', sRule )
//? "Added: " + Left( sRule, nNext - 1 )
                  aAdd( aWords, Left( sRule, nNext - 1 ) )
                  sRule := SubStr( sRule, nNext + 1 )
                  ExtractLeadingWS( @sRule )
                  LOOP
               ELSE
                  nCloseAt := At( '>', sRule )
                  IF nCloseAt > 0
//? "Last: " + Left( sRule, nCloseAt - 1 )
                     aAdd( aWords, Left( sRule, nCloseAt - 1 ) )
                     EXIT
                  ELSE
                     Alert( "ERROR! Unblanced MP: ''<,...' at: " + sRule )
                     RETURN .F.
                  ENDIF
               ENDIF
            ENDDO
         ENDIF

         IF nCloseAt > 0
            IF cType == NIL
               cType := '<'
            ENDIF

            IF Len( aMarkers ) < nId
               sMarker := Left( sRule, nCloseAt - 1 )
               ExtractLeadingWS( @sMarker )
               aAdd( aMarkers, sMarker )
            ENDIF

            sRule := SubStr( sRule, nCloseAt + 1 )
            ExtractLeadingWS( @sRule )

            aMatch := { nId, nOptional, sAnchor, cType, aWords }
            /* Next dependant optional will be marked as trailing. */
            IF nOptional > 0
               nOptional := ( -nOptional )
            ENDIF
//? aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]

            aAdd( aRule[2], aMatch )
         ELSE
            Alert( "ERROR! Unbalanced MP: '<' at: " + sRule )
            RETURN .F.
         ENDIF

         sAnchor := NIL
         aWords  := NIL
         cType   := NIL

         LOOP

      ELSEIF Left( sRule, 1 ) == '['

         IF ! ( sAnchor == NIL )
//? "ORPHAN ANCHOR: " + sAnchor

            aMatch := { 0, nOptional, sAnchor, NIL, NIL }
            /* Next dependant optional will be marked as trailing. */
            IF nOptional > 0
               nOptional := ( -nOptional )
            ENDIF
//? aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]

            aAdd( aRule[2], aMatch )

            sAnchor := NIL
            aWords  := NIL
            cType   := NIL
         ENDIF

         nOptional := Abs( nOptional )
         nOptional++

//? "Optional:", nOptional

         sRule := SubStr( sRule, 2 )
         ExtractLeadingWS( @sRule )
         LOOP

      ELSEIF Left( sRule, 1 ) == ']'

         IF nOptional > 0
            nOptional--
         ELSE
            nOptional++
         ENDIF

         sRule := SubStr( sRule, 2 )
         ExtractLeadingWS( @sRule )
         LOOP

      ELSE
         IF ! ( sAnchor == NIL )
//? "ORPHAN ANCHOR: " + sAnchor

            aMatch := { 0, nOptional, sAnchor, NIL, NIL }
            /* Next dependant optional will be marked as trailing. */
            IF nOptional > 0
               nOptional := ( -nOptional )
            ENDIF
//? aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]

            aAdd( aRule[2], aMatch )

            sAnchor := NIL
            aWords  := NIL
            cType   := NIL
         ENDIF

         nNext := NextWS( sRule )

         IF nNext == 0
            nOptionalAt := At( '[', sRule )
            WHILE nOPtionalAt > 1 .AND. SubStr( sRule, nOptionalAt - 1, 1 ) == '\'
               sRule := Left( sRule, nOptionalAt - 2 ) + SubStr( sRule, nOptionalAt )
               nOptionalAt := At( '[', SubStr( sRule, nOptionalAt /* + 1 (1 char removed)*/ ) )
            ENDDO
         ELSE
            nOptionalAt := At( '[', sRule )
            WHILE nOPtionalAt > 1 .AND. nOptionalAt <= nNext .AND. SubStr( sRule, nOptionalAt - 1, 1 ) == '\'
               sRule := Left( sRule, nOptionalAt - 2 ) + SubStr( sRule, nOptionalAt )
               nNext--
               nOptionalAt := At( '[', SubStr( sRule, nOptionalAt /* + 1 (1 char removed)*/ ) )
            ENDDO
            IF nOPtionalAt > 0
               IF nOptionalAt > nNext
                  nOptionalAt := 0
               ELSE
                  nNext := 0
               ENDIF
            ENDIF
         ENDIF

         nAt := IIF( nNext == 0, nOptionalAt, nNext )

         IF nAt == 0
            nMarkerAt := At( '<', sRule )
            WHILE nMarkerAt > 1 .AND. SubStr( sRule, nMarkerAt - 1, 1 ) == '\'
               sRule := Left( sRule, nMarkerAt - 2 ) + SubStr( sRule, nMarkerAt )
               nMarkerAt := At( '<', SubStr( sRule, nMarkerAt /* + 1 (1 char removed)*/ ) )
            ENDDO
         ELSE
            nMarkerAt := At( '<', sRule )
            WHILE nMarkerAt > 1 .AND. nMarkerAt <= nAt .AND. SubStr( sRule, nMarkerAt - 1, 1 ) == '\'
               sRule := Left( sRule, nMarkerAt - 2 ) + SubStr( sRule, nMarkerAt )
               nAt--
               nMarkerAt := At( '<', SubStr( sRule, nMarkerAt /* + 1 (1 char removed)*/ ) )
            ENDDO
            IF nMarkerAt > 0
               IF nMarkerAt > nAt
                  nMarkerAt := 0
               ELSE
                  nAt         := 0
                  nOptionalAt := 0
                  nNext       := 0
               ENDIF
            ENDIF
         ENDIF

         nAt := IIF( nAt == 0, nMarkerAt, nAt )

         IF nAt == 0
            nCloseOptionalAt := At( ']', sRule )
            WHILE nCloseOptionalAt > 1 .AND. SubStr( sRule, nCloseOptionalAt - 1, 1 ) == '\'
               sRule := Left( sRule, nCloseOptionalAt - 2 ) + SubStr( sRule, nCloseOptionalAt )
               nCloseOptionalAt := At( ']', SubStr( sRule, nCloseOptionalAt /* + 1 (1 char removed)*/ ) )
            ENDDO
         ELSE
            nCloseOptionalAt := At( ']', sRule )
            WHILE nCloseOptionalAt > 1 .AND. nCloseOptionalAt <= nAt .AND. SubStr( sRule, nCloseOptionalAt - 1, 1 ) == '\'
               sRule := Left( sRule, nCloseOptionalAt - 2 ) + SubStr( sRule, nCloseOptionalAt )
               nAt--
               nCloseOptionalAt := At( ']', SubStr( sRule, nCloseOptionalAt /* + 1 (1 char removed)*/ ) )
            ENDDO
            IF nCloseOptionalAt > 0
               IF nCloseOptionalAt > nAt
                  nCloseOptionalAt := 0
               ELSE
                  nAt         := 0
                  nOptionalAt := 0
                  nNext       := 0
                  nMarkerAt   := 0
               ENDIF
            ENDIF
         ENDIF

         IF nNext > 0
            nNext := nAt
         ELSEIF nOptionalAt > 0
            nOPtionalAt := nAt
         ELSEIF nMarkerAt > 0
            nMarkerAt := nAt
         ENDIF

//? sRule
//? "MP scan:", nAt, nNext, nMarkerAt, nOptionalAt, nCloseOptionalAt
//WAIT

         IF nNext > 0

            sAnchor := Upper( Left( sRule, nNext - 1 ) )
            DropTrailingWS( @sAnchor )
//? "ANCHOR: " + sAnchor

            sRule := SubStr( sRule, nNext + 1 )
            ExtractLeadingWS( @sRule )

         ELSEIF nOptionalAt > 0

            sAnchor := Upper( Left( sRule, nOptionalAt - 1 ) )
            DropTrailingWS( @sAnchor )
//? "ANCHOR: " + sAnchor

            nOptional := Abs( nOptional )
            nOptional++
            sRule := SubStr( sRule, nOptionalAt + 1 )
            ExtractLeadingWS( @sRule )

         ELSEIF nMarkerAt > 0

            sAnchor := Upper( Left( sRule, nMarkerAt - 1 ) )
            DropTrailingWS( @sAnchor )
//? "ANCHOR: " + sAnchor

            sRule := SubStr( sRule, nMarkerAt )
            ExtractLeadingWS( @sRule )

         ELSEIF nCloseOptionalAt > 0
//? "Closing"
            sAnchor := Upper( Left( sRule, nCloseOptionalAt - 1 ) )
            DropTrailingWS( @sAnchor )
//? "ANCHOR: " + sAnchor

            IF ! ( sAnchor == NIL )
               aMatch := { 0, nOptional, sAnchor, NIL, NIL }
//? "Orphan:", aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]
//WAIT
               aAdd( aRule[2], aMatch )

               sAnchor := NIL
               aWords  := NIL
               cType   := NIL
            ENDIF

            IF nOptional > 0
               nOptional--
            ELSE
               nOptional++
            ENDIF

            sRule := SubStr( sRule, nCloseOptionalAt + 1 )
            ExtractLeadingWS( @sRule )

         ELSE

            sAnchor := Upper( sRule )
            DropTrailingWS( @sAnchor )
//? "ANCHOR: " + sAnchor

            sRule := ''

         ENDIF

      ENDIF

   ENDDO

   IF ! ( cType == NIL  .AND. sAnchor == NIL )

      aMatch := { 0, nOptional, sAnchor, cType, NIL }
      /* nOptional would be minused, but will not be used anymore! */
//? aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]

      aAdd( aRule[2], aMatch )
   ENDIF

   IF nOptional <> 0
      Alert( "ERROR! Internal logic failure - nOptional =" + Str( nOptional, 3 ) + "[" + Str( ProcLine(0), 4 ) )
      BREAK
   ENDIF

   aAdd( aRules, aRule )

   /*
   nMarkerID
   nOPTIONAL
   sAnchor
   cTYPE
   aLIST
   */

   nMatches := Len( aRule[2] )
   FOR Counter := 1 TO nMatches
      aMatch := aRule[2][Counter]

      /* Optional group start (marker), no anchor, and not a restricted pattern - have to build stop words list! */
      IF aMatch[1] > 0 .AND. aMatch[2] > 0 .AND. aMatch[3] == NIL .AND. aMatch[4] != ':'
         aWords    := {}
         nOptional := aMatch[2]

         nMP := Counter - 1
         WHILE nMP > 0
            aMatch := aRule[2][nMP]
            IF aMatch[2] >= 0 .AND. aMatch[2] < nOptional
               EXIT
            ENDIF
            IF aMatch[2] > 0 .AND. aMatch[2] == nOptional .AND. aMatch[3] != NIL
               aAdd( aWords, Upper( aMatch[3] ) )
            ENDIF
            nMP--
         ENDDO

         nMP := Counter + 1
         WHILE nMP <= nMatches
            aMatch := aRule[2][nMP]
            IF aMatch[2] >= 0 .AND. aMatch[2] < nOptional
               IF aMatch[3] != NIL
                  aAdd( aWords, Upper( aMatch[3] ) )
               ENDIF
               EXIT
            ENDIF
            IF aMatch[2] > 0 .AND. aMatch[2] == nOptional .AND. aMatch[3] != NIL
               aAdd( aWords, Upper( aMatch[3] ) )
            ENDIF
            nMP++
         ENDDO

         IF Len( aWords ) > 0
            aRule[2][Counter][5] := aWords
         ENDIF
      ENDIF

      //? aRule[1], aRule[2][Counter][1], aRule[2][Counter][2], aRule[2][Counter][3], aRule[2][Counter][4], aRule[2][Counter][5]
   NEXT
   //WAIT

   /*
   ? ''
   FOR Counter := 1 TO nId
      ?? aMarkers[Counter]
      IF Counter < nId
         ?? ' , '
      ENDIF
   NEXT
   WAIT
   */

   /* --------------------------------------------------------------- */

   //? [RP: "] + sResult + '"'

   nOptional  := 0
   aModifiers := {}//Array( nId )
   //aFill( aModifiers, 0 )
   aValues    := Array( nId )
   nId        := 0
   sPad       := ''

   DO WHILE ! ( sResult == '' )

      nOptionalAt := At( '[', sResult )
      WHILE nOPtionalAt > 1 .AND. SubStr( sResult, nOptionalAt - 1, 1 ) == '\'
         sResult := Left( sResult, nOptionalAt - 2 ) + SubStr( sResult, nOptionalAt )
         nOptionalAt := At( '[', SubStr( sResult, nOptionalAt /* + 1 (1 char removed)*/ ) )
      ENDDO

      IF nOptionalAt == 0
         nMarkerAt := At( '<', sResult )
         WHILE nMarkerAt > 1 .AND. SubStr( sResult, nMarkerAt - 1, 1 ) == '\'
            sResult := Left( sResult, nMarkerAt - 2 ) + SubStr( sResult, nMarkerAt )
            nMarkerAt := At( '<', SubStr( sResult, nMarkerAt /* + 1 (1 char removed)*/ ) )
         ENDDO
      ELSE
         nMarkerAt := At( '<', sResult )
         WHILE nMarkerAt > 1 .AND. nMarkerAt <= nOptionalAt .AND. SubStr( sResult, nMarkerAt - 1, 1 ) == '\'
            sResult := Left( sResult, nMarkerAt - 2 ) + SubStr( sResult, nMarkerAt )
            nOptionalAt--
            nMarkerAt := At( '<', SubStr( sResult, nMarkerAt /* + 1 (1 char removed)*/ ) )
         ENDDO
         IF nMarkerAt > 0
            IF nMarkerAt > nOptionalAt
               nMarkerAt := 0
            ELSE
               nOptionalAt := 0
            ENDIF
         ENDIF
      ENDIF

      nAt := IIF( nOptionalAt == 0, nMarkerAt, nOptionalAt )

      IF nAt == 0
         nCloseOptionalAt := At( ']', sResult )
         WHILE nCloseOptionalAt > 1 .AND. SubStr( sResult, nCloseOptionalAt - 1, 1 ) == '\'
            sResult := Left( sResult, nCloseOptionalAt - 2 ) + SubStr( sResult, nCloseOptionalAt )
            nCloseOptionalAt := At( ']', SubStr( sResult, nCloseOptionalAt /* + 1 (1 char removed)*/ ) )
         ENDDO
      ELSE
         nCloseOptionalAt := At( ']', sResult )
         WHILE nCloseOptionalAt > 1 .AND. nCloseOptionalAt <= nAt .AND. SubStr( sResult, nCloseOptionalAt - 1, 1 ) == '\'
            sResult := Left( sResult, nCloseOptionalAt - 2 ) + SubStr( sResult, nCloseOptionalAt )
            nAt--
            nCloseOptionalAt := At( ']', SubStr( sResult, nCloseOptionalAt /* + 1 (1 char removed)*/ ) )
         ENDDO
         IF nCloseOptionalAt > 0
            IF nCloseOptionalAt > nAt
               nCloseOptionalAt := 0
            ELSE
               nAt         := 0
               nOptionalAt := 0
               nMarkerAt   := 0
            ENDIF
         ENDIF
      ENDIF

      IF nOptionalAt > 0
         nOPtionalAt := nAt
      ELSEIF nMarkerAt > 0
         nMarkerAt := nAt
      ENDIF

      //? "RP Scan:", nAt, nMarkerAt, nOptionalAt, nCloseOptionalAt, sResult
      //WAIT

      IF nCloseOptionalAt > 0
         IF nCloseOptionalAt > 1
            sTemp := Left( sResult, nCloseOptionalAt - 1 )
            aRP := { nOptional, sPad + sTemp }
            aAdd( aResult, aRP )
            aAdd( aModifiers, -1 )
         ENDIF

         nOptional := 0 //--
         sResult := SubStr( sResult, nCloseOptionalAt + 1 )
         ExtractLeadingWS( @sResult, @sPad )
         LOOP
      ENDIF

      IF nOptionalAt > 0

         IF nOptional <> 0
            Alert( "ERROR! Nested repeatable RP.;" + sResult )
            BREAK
         ELSE
            nOptional := -1
         ENDIF

         /* The text preceding this new repeatable group. */
         IF nOptionalAt > 1
            sTemp := Left( sResult, nOptionalAt - 1 )
            aRP := { 0, sPad + sTemp }
            aAdd( aResult, aRP )
            aAdd( aModifiers, -1 )
         ENDIF

         sResult := SubStr( sResult, nOptionalAt + 1 )
         ExtractLeadingWS( @sResult )
         LOOP

      ELSEIF nMarkerAt > 0

         /* Resetting. */
         nType := 0

         IF nMarkerAt > 1
            IF SubStr( sResult, nMarkerAt - 1, 1 ) == '#'
               nType := 2
               IF nMarkerAt > 2
                  sTemp := Left( sResult, nMarkerAt - 2 )
                  aRP := { nOptional, sPad + DropExtraTrailingWS( sTemp ) }
                  aAdd( aResult, aRP )
                  aAdd( aModifiers, -1 )
               ENDIF
            ELSE
               sTemp := Left( sResult, nMarkerAt - 1 )
               aRP := { nOptional, sPad + DropExtraTrailingWS( sTemp ) }
               aAdd( aResult, aRP )
               aAdd( aModifiers, -1 )
            ENDIF

            sResult := SubStr( sResult, nMarkerAt )
            ExtractLeadingWS( @sResult )
         ENDIF

         /* <-x-> Ommit */
         IF SubStr( sResult, 2, 1 ) == '-'

            sResult := SubStr( sResult, 3 )
            ExtractLeadingWS( @sResult )

            nNext := At( "->", sResult )
            IF nNext == 0
               Alert( "ERROR! Unbalanced RP <-" )
            ELSE
               nType := 0
               sTemp := Left( sResult, nNext - 1 )
               nId   := aScan( aMarkers, sTemp )
               sResult := SubStr( sResult, nNext + 2 )
               ExtractLeadingWS( @sResult, @sPad )
               IF nId == 0
                  Alert( "ERROR! Unrecognized RP: '<-' : " + sTemp )
               ELSE
                  aRP := { nOptional, nId }
                  aAdd( aResult, aRP )
                  aAdd( aModifiers, nType )
               ENDIF
            ENDIF

         /* #<x> Dumb */
         ELSEIF nType == 2

            sResult := SubStr( sResult, 2 )
            ExtractLeadingWS( @sResult )

            nNext := At( '>', sResult )
            IF nNext == 0
               Alert( "ERROR! Unbalanced RP: #<" )
            ELSE
               /*nType := 2*/
               sTemp := Left( sResult, nNext - 1 )
               nId := aScan( aMarkers, sTemp )
               sResult := SubStr( sResult, nNext + 1 )
               ExtractLeadingWS( @sResult, @sPad )
               IF nId == 0
                  Alert( "ERROR! Unrecognized RP: '#<' : " + sTemp )
               ELSE
                  aRP := { nOptional, nId }
                  aAdd( aResult, aRP )
                  aAdd( aModifiers, nType )
               ENDIF
            ENDIF

         /* <"x"> Normal */
         ELSEIF SubStr( sResult, 2, 1 ) == '"'

            sResult := SubStr( sResult, 3 )
            ExtractLeadingWS( @sResult )

            nNext := At( '">', sResult )
            IF nNext == 0
               Alert( [ERROR! Unbalanced RP: <"] )
            ELSE
               nType := 3
               sTemp := Left( sResult, nNext - 1 )
               nId := aScan( aMarkers, sTemp )
               sResult := SubStr( sResult, nNext + 2 )
               ExtractLeadingWS( @sResult, @sPad )
               IF nId == 0
                  Alert( [ERROR! Unrecognized RP: '<"' : ] + sTemp )
               ELSE
                  aRP := { nOptional, nId }
                  aAdd( aResult, aRP )
                  aAdd( aModifiers, nType )
               ENDIF
            ENDIF

         /* <(x)> Smart */
         ELSEIF SubStr( sResult, 2, 1 ) == '('

            sResult := SubStr( sResult, 3 )
            ExtractLeadingWS( @sResult )

            nNext := At( ")>", sResult )
            IF nNext == 0
               Alert( "ERROR! Unbalanced RP: <(" )
            ELSE
               nType := 4
               sTemp := Left( sResult, nNext - 1 )
               nId := aScan( aMarkers, sTemp )
               sResult := SubStr( sResult, nNext + 2 )
               ExtractLeadingWS( @sResult, @sPad )
               IF nId == 0
                  Alert( "ERROR! Unrecognized RP: '<(' : " + sTemp )
               ELSE
                  aRP := { nOptional, nId }
                  aAdd( aResult, aRP )
                  aAdd( aModifiers, nType )
               ENDIF
            ENDIF

         /* <{x}> Blockify */
         ELSEIF SubStr( sResult, 2, 1 ) == '{'

            sResult := SubStr( sResult, 3 )
            ExtractLeadingWS( @sResult )

            nNext := At( "}>", sResult )
            IF nNext == 0
               Alert( "ERROR! Unbalanced RP: <{" + sTemp )
            ELSE
               nType := 5
               sTemp := Left( sResult, nNext - 1 )
               nId := aScan( aMarkers, sTemp )
               sResult := SubStr( sResult, nNext + 2 )
               ExtractLeadingWS( @sResult, @sPad )
               IF nId == 0
                  Alert( "ERROR! Unrecognized RP: '<{' : " + sTemp )
               ELSE
                  aRP := { nOptional, nId }
                  aAdd( aResult, aRP )
                  aAdd( aModifiers, nType )
               ENDIF
            ENDIF

         /* <.x.> Logify */
         ELSEIF SubStr( sResult, 2, 1 ) == '.'

            sResult := SubStr( sResult, 3 )
            ExtractLeadingWS( @sResult )

            nNext := At( ".>", sResult )
            IF nNext == 0
               Alert( "ERROR! Unbalanced RP: <." )
            ELSE
               nType := 6
               sTemp := Left( sResult, nNext - 1 )
               nId := aScan( aMarkers, sTemp )
               sResult := SubStr( sResult, nNext + 2 )
               ExtractLeadingWS( @sResult, @sPad )
               IF nId == 0
                  Alert( "ERROR! Unrecognized RP: '<.' : " + sTemp )
               ELSE
                  aRP := { nOptional, nId }
                  aAdd( aResult, aRP )
                  aAdd( aModifiers, nType )
               ENDIF
            ENDIF

         ELSE

            sResult := SubStr( sResult, 2 )
            ExtractLeadingWS( @sResult )

            nNext := At( '>', sResult )
            IF nNext == 0
               Alert( "ERROR! Unbalanced RP: <" )
            ELSE
               /* <x> Regular */
               nType := 1
               sTemp := Left( sResult, nNext - 1 )
               nId := aScan( aMarkers, sTemp )
               sResult := SubStr( sResult, nNext + 1 )
               ExtractLeadingWS( @sResult, @sPad )
               IF nId == 0
                  Alert( "ERROR! Unrecognized RP: '<' : " + sTemp )
               ELSE
                  aRP := { nOptional, nId }
                  aAdd( aResult, aRP )
                  aAdd( aModifiers, nType )
               ENDIF
            ENDIF

         ENDIF

         //? "RP #", nID, "Optional:", nOptional

         IF nOptional < 0
            nOptional := nID
            aRP[1]    := nOptional
         ENDIF

      ELSE
         aRP := { 0, sPad + sResult }
         aAdd( aResult, aRP )
         aAdd( aModifiers, -1 )
         sResult := ''
      ENDIF

   ENDDO

   IF nOptional <> 0
      Alert( "ERROR! Internal logic failure - nOptional =" + Str( nOptional, 3 ) + "[" + Str( ProcLine(0), 4 ) )
      BREAK
   ENDIF

   nResults := Len( aResult )
   FOR Counter := nResults TO 1 STEP -1

      /* Correcting the ID of the Marker this result relyes upon. */
      IF aResult[Counter][1] > 0
         nOptional := aResult[Counter][1]
         nMarker   := aResult[Counter][2]

         //? "Repeatable: ", nMarker, "Root: ", nOptional

         IF ValType( nMarker ) == 'N'
            nMP := aScan( aRule[2], {|aMP| aMP[1] == nMarker .OR. aMP[1] - 1000 == nMarker } )
            IF nMP == 0
               Alert( "ERROR! Internal logic failed! Missing marker # " + str( nMarker, 2 ) + " [" + Str(ProcLine(),4 ) + ']' )
            ELSE
               WHILE aRule[2][nMP][2] < 0
                  IF aRule[2][nMP][1] >= 0
                     aRule[2][nMP][1] += ( 1000 )
                     //? "Marked:", nMP, "As:", aRule[2][nMP][1]
                  ENDIF

                  nMP--
               ENDDO
               IF aRule[2][nMP][1] < 1000
                  aRule[2][nMP][1] += ( 1000 )
                  //? "Flagged:", nMP, "As:", aRule[2][nMP][1]
               ENDIF
            ENDIF
            //WAIT
         ENDIF
      ELSEIF aResult[Counter][1] < 0
         aResult[Counter][1] := nOptional
      ENDIF

      IF ValType( aResult[Counter][2] ) == 'C'
         aResult[Counter][2] := StrTran( aResult[Counter][2], '\', '' )
         //? "RP #", Counter, aResult[Counter][1], '"' + aResult[Counter][2] + '"'
      ELSE
         //? "RP #", Counter, aResult[Counter][1], aResult[Counter][2]
      ENDIF
   NEXT

   //WAIT

   aAdd( aResults, { aResult, aModifiers, aValues } )

RETURN NIL

FUNCTION RemoveDefine( sDefine )

   LOCAL nId

   IF ( nId := aScan( aDefRules, {|aDefine| aDefine[1] == sDefine } ) ) > 0
      aDel( aDefRules, nId )
      aSize( aDefRules, Len( aDefRules ) - 1 )
      aDel( aDefResults, nId )
      aSize( aDefResults, Len( aDefRules ) - 1 )
   ENDIF

RETURN nId

FUNCTION CompileDefine( sRule )

   LOCAL sKey, sResult, aRule, nCloseAt, nId, sMarker, nCommaAt, aMP
   LOCAL nNext, sToken, aRPs, sAnchor, aMarkers := {}, aResult, nMarkers

   ExtractLeadingWS( @sRule )

   sKey := NextToken( @sRule, .F. ) /* bCheckRules */
   DropTrailingWS( @sKey )

//? "KEY: '" + sKey + "'"
//? "Rest: '" + sRule + "'"

   IF ( nId := aScan( aDefRules, {|aDefine| aDefine[1] == sKey } ) ) > 0
      Alert( "Redefinition of " + sKey )
      aRule      := aDefRules[nId]
      //aRule[1]   := sKey
      aRule[2]   := {}
      aResult    := aDefResults[nId]
      aResult[1] := {} // aMPs
      aResult[2] := {} // aModifiers
      aResult[3] := {} // Markers place holders
   ELSE
      aRule   := { sKey, {}, .T. }
      aAdd( aDefRules, aRule )
      aResult := { {}, {}, {} } //1=MPs, 2=Modifiers 3=Marker place holders
      aAdd( aDefResults, aResult )
   ENDIF

   IF Left( sRule, 1 ) == '('

      /*Pseudo Function. */
      nCloseAt := At( ')', sRule )
      IF nCloseAt == 0
         Alert( "ERROR! Invalid #DEFINE efine format" )
         RETURN .F.
      ENDIF

      sResult := SubStr( sRule, nCloseAt + 1 )
      sRule   := SubStr( sRule, 2, nCloseAt - 2 )
      ExtractLeadingWS( @sRule )
      DropTrailingWS( @sRule )
      ExtractLeadingWS( @sResult )

      /* No paramaets */
      IF sRule == ''

//? "Added: '" + aRule[1] + "'"
//WAIT
         aAdd( aRule[2], { 0, 0 , '(', NIL, NIL } )
         aAdd( aRule[2], { 0, 0 , ')', NIL, NIL } )

         IF sResult == ''
            aResult[1] := NIL
            aResult[2] := NIL
            aResult[3] := NIL
         ELSE
            aResult[1] := { { 0, sResult } }
         ENDIF
      ELSE

//? "***'" + sRule + "'"
//WAIT
         nId      := 1
         sAnchor  := '('

         WHILE ( nCommaAt := At( ',', sRule ) ) > 0
            sMarker := Left( sRule, nCommaAt - 1 )
            sRule   := SubStr( sRule, nCommaAt + 1 )
            ExtractLeadingWS( @sRule )
            DropTrailingWS( @sMarker )

//? nId, "Marker: '" + sMarker + "'"
//WAIT
            aAdd( aMarkers, sMarker )
            aMP := { nId, 0, sAnchor, '<', NIL }
            aAdd( aRule[2], aMP )

            sAnchor := ','
            nId++
         ENDDO

         aAdd( aMarkers, sRule )
         aMP := { nId, 0, sAnchor, '<', NIL }
         aAdd( aRule[2], aMP )

         aMP := { 0, 0, ')', NIL, NIL }
         aAdd( aRule[2], aMP )

/*----------------------------------------- */

         aRPs := {}

         IF sResult == ''

            aResult[1] := NIL
            aResult[2] := NIL
            aResult[3] := NIL

         ELSE

            WHILE ( sToken := NextToken( @sResult, .F. ) ) != NIL //bCheckRules

               DropTrailingWS( @sToken )

//? "Token: '" + sToken + "'"

               IF ( nId := aScan( aMarkers, {|sMarker| sMarker == sToken } ) ) > 0
                  aAdd( aRPs, { 0, nId } )
                  aAdd( aResult[2], 1 )
               ELSE
                  aAdd( aRPs, { 0, sToken } )
                  aAdd( aResult[2], -1 )
               ENDIF

//? "ID:", nID
//WAIT

            ENDDO

            aResult[1] := aRPs
            aSize( aResult[3], Len( aMarkers ) )
            aFill( aResult[3], NIL )

         ENDIF

      ENDIF

   ELSE

      /* Plain. */

      sResult := sRule

      IF sResult == ''
         aResult[1] := NIL
         aResult[2] := NIL
         aResult[3] := NIL
      ELSE
         aResult[1] := { { 0, sResult } }
         aResult[2] := { -1 }
         aResult[3] := NIL
      ENDIF

   ENDIF

//? "Defines: ", Len( aDefRules )
//? "Results: ", Len( aDefResults )
//WAIT

RETURN Len( aDefRules )

FUNCTION ExtractLeadingWS( sLine, sWS )

   LOCAL Counter, cChar, nLen := Len( sLine )

   //? "Removing Leading: '" + sLine + "'"

   sWS := ''
   FOR Counter := 1 TO nLen
      cChar := SubStr( sLine, Counter, 1 )
      IF cChar $ ( ' ' + Chr(9) )
         sWS += cChar
      ELSE
         EXIT
      ENDIF
   NEXT

   IF Counter > 1
      sLine := SubStr( sLine, Counter )
   ENDIF

   //? "Removed: '" + sWs + "' Returning: " + sLine

RETURN sWS

FUNCTION DropTrailingWS( sLine, sWS )

   LOCAL nLen := Len( sLine ), cChar

   sWS := ''

   //? "Before Drop: '" + sLine + "'"

   WHILE nLen > 0 .AND. ( cChar := SubStr( sLine, nLen, 1 ) ) $ ( ' ' + Chr(9) )
      sWS := cChar + sWs
      nLen--
   ENDDO

   sLine := Left( sLine, nLen )

   //? "After Drop: '" + sLine + "'"

RETURN sLine

FUNCTION DropExtraTrailingWS( sLine )

   LOCAL nLen := Len( sLine )

//? "Before Extra: '" + sLine + "'"

   WHILE nLen > 2 .AND. ( SubStr( sLine, nLen, 1 ) $ ( ' ' + Chr(9) ) ) .AND. ( SubStr( sLine, nLen - 1, 1 ) $ ( ' ' + Chr(9) ) )
      nLen--
   ENDDO

   sLine := Left( sLine, nLen )

//? "After Extra: '" + sLine + "'"

RETURN sLine

FUNCTION SetIfDef( sDefine, bExist )

   LOCAL nId

   nIfDef++
   aSize( abIfDef, nIfDef )

   DropTrailingWS( @sDefine )

   nId := aScan( aDefRules, {|aDefine| aDefine[1] == sDefine } )

   IF bExist
      abIfDef[nIfDef] := ( nId > 0 )
   ELSE
      abIfDef[nIfDef] := ( nId == 0 )
   ENDIF

RETURN nIfDef

FUNCTION CompileToCCH( sSource )

   LOCAL hCCH, Counter, aRules, nRules, nRule, aRule, nMatches, nMatch, aMatch, nWords, nWord, aWords
   LOCAL aResults, nResults, nResult, aResult, nRPs, nRP, aRP, nIDs, nID, nModifier
   LOCAL sRulesArray, sResultsArray

   hCCH   := FCreate( StrTran( sSource, ".PRG", ".CCH" ) )

   FWrite( hCCH, "FUNCTION InitRules()" + Chr(13) + Chr(10) )

   FOR Counter := 1 TO 3
      IF Counter == 1
         aRules      := aDefRules
         sRulesArray := "aDefRules"
         FWrite( hCCH, Chr(13) + Chr(10) + "/* Defines */" + Chr(13) + Chr(10) + "aDefRules := {}" + Chr(13) + Chr(10) )
      ELSEIF Counter == 2
         aRules      := aTransRules
         sRulesArray := "aTransRules"
         FWrite( hCCH, Chr(13) + Chr(10) + "/* Translates */" + Chr(13) + Chr(10) + "aTransRules := {}" + Chr(13) + Chr(10) )
      ELSE
         aRules      := aCommRules
         sRulesArray := "aCommRules"
         FWrite( hCCH, Chr(13) + Chr(10) + "/* Commands */" + Chr(13) + Chr(10) + "aCommRules := {}" + Chr(13) + Chr(10) )
      ENDIF

      nRules := Len( aRules )

      FOR nRule := 1 TO nRules
         aRule  := aRules[nRule]

         FWrite( hCCH, "aAdd( " + sRulesArray + ", { '" + aRule[1] + "' " )

         IF aRule[2] == NIL
            nMatches := 0
         ELSE
            nMatches := Len( aRule[2] )
         ENDIF

         IF nMatches == 0
            FWrite( hCCH, ", " )
         ELSE
            FWrite( hCCH, ", { " )
            FOR nMatch := 1 TO nMatches
               aMatch := aRule[2][nMatch] //{ nId, nOptional, sAnchor, cType, aWords }
               FWrite( hCCH, "{ " + Str( aMatch[1], 4) + ", " + Str(aMatch[2],3) + ", " + IF( aMatch[3] == NIL, "NIL", "'" + aMatch[3] + "'" ) + ", " + IF( aMatch[4] == NIL, "NIL", "'" + aMatch[4] + "'" ) + ", " )
               IF aMatch[5] == NIL
                  FWrite( hCCH, "NIL" )
               ELSE
                  aWords := aMatch[5]
                  nWords := Len( aWords )
                  FWrite( hCCH, "{ " )
                  FOR nWord := 1 TO nWords
                     FWrite( hCCH, "'" + aWords[nWord] + "'" )
                     IF nWord < nWords
                        FWrite( hCCH, ", " )
                     ENDIF
                  NEXT
                  FWrite( hCCH, " }" )
               ENDIF
               FWrite( hCCH, " }" )

               IF nMatch < nMatches
                  FWrite( hCCH, ", " )
               ENDIF
            NEXT

            FWrite( hCCH, " }" )
         ENDIF

         IF aRule[3]
            FWrite( hCCH, " , .T." )
         ELSE
            FWrite( hCCH, " , .F." )
         ENDIF

         FWrite( hCCH, " } )" + Chr(13) + Chr(10) )
      NEXT
   NEXT

   FWrite( hCCH, Chr(13) + Chr(10) + "RETURN .T." + Chr(13) + Chr(10) )

   FWrite( hCCH, Chr(13) + Chr(10) + "FUNCTION InitResults()" + Chr(13) + Chr(10) )

   FOR Counter := 1 TO 3

      IF Counter == 1
         aResults      := aDefResults
         sResultsArray := "aDefResults"
         FWrite( hCCH, Chr(13) + Chr(10) + "/* Defines Results*/" + Chr(13) + Chr(10) + "aDefResults := {}" + Chr(13) + Chr(10) )
      ELSEIF Counter == 2
         aResults      := aTransResults
         sResultsArray := "aTransResults"
         FWrite( hCCH, Chr(13) + Chr(10) + "/* Translates Results*/" + Chr(13) + Chr(10) + "aTransResults := {}" + Chr(13) + Chr(10) )
      ELSE
         aResults      := aCommResults
         sResultsArray := "aCommResults"
         FWrite( hCCH, Chr(13) + Chr(10) + "/* Commands Results*/" + Chr(13) + Chr(10) + "aCommResults := {}" + Chr(13) + Chr(10) )
      ENDIF

      nResults := Len( aResults )

      FOR nResult := 1 TO nResults
         aResult  := aResults[nResult]

         FWrite( hCCH, "aAdd( " + sResultsArray + ", { " )

         IF aResult[1] == NIL
            nRPs := 0
         ELSE
            nRPs := Len( aResult[1] )
         ENDIF
         IF aResult[3] == NIL
            nIDs := 0
         ELSE
            nIDs := Len( aResult[3] )
         ENDIF

         IF nRPs == 0
            /*FWrite( hCCH, "NIL " )*/
         ELSE
            FWrite( hCCH, "{ " )
            FOR nRP := 1 TO nRPs
               aRP := aResult[1][nRP] //{ nLevel, xVal }

               FWrite( hCCH, "{ " + Str( aRP[1], 3) + ", " )
               IF ValType( aRP[2] ) == 'C'
                  FWrite( hCCH, "'" + aRP[2] + "' }" )
               ELSE
                  FWrite( hCCH, Str( aRP[2], 3 ) + " }" )
               ENDIF

               IF nRP < nRPs
                  FWrite( hCCH, ", " )
               ENDIF
            NEXT
            FWrite( hCCH, " }" )
         ENDIF

         IF nRPs == 0
            FWrite( hCCH, ", " )
            FWrite( hCCH, ", " )
         ELSE
            FWrite( hCCH, ", { " )
            FOR nModifier := 1 TO nRPs
               FWrite( hCCH, Str( aResult[2][nModifier], 2 ) )
               IF nModifier < nRPs
                  FWrite( hCCH, ", " )
               ENDIF
            NEXT
            FWrite( hCCH, "} " )

            FWrite( hCCH, ", { " )
            FOR nID := 1 TO nIDs
               FWrite( hCCH, "NIL" )
               IF nID < nIDs
                  FWrite( hCCH, ", " )
               ENDIF
            NEXT
            FWrite( hCCH, "} " )
         ENDIF
         FWrite( hCCH, " } )" + Chr(13) + Chr(10) )
      NEXT

   NEXT

   FWrite( hCCH, Chr(13) + Chr(10) + "RETURN .T. " + Chr(13) + Chr(10) )

   FClose( hCCH )

RETURN .T.

FUNCTION NextWS( sLine )

   LOCAL nSpaceAt, nTabAt

   nSpaceAt := At( ' ', sLine )
   nTabAt   := At( Chr(9), sLine )

   IF nTabAt == 0
      RETURN nSpaceAt
   ELSEIF nSpaceAt == 0
      RETURN nTabAt
   ENDIF

RETURN IIF( nSpaceAt > nTabAt, nTabAt, nSpaceAt )

FUNCTION InitRules()

/* Defines */
aDefRules := {}
aAdd( aDefRules, { '_SET_EXACT' ,  , .T. } )
aAdd( aDefRules, { '_SET_FIXED' ,  , .T. } )
aAdd( aDefRules, { '_SET_DECIMALS' ,  , .T. } )
aAdd( aDefRules, { '_SET_DATEFORMAT' ,  , .T. } )
aAdd( aDefRules, { '_SET_EPOCH' ,  , .T. } )
aAdd( aDefRules, { '_SET_PATH' ,  , .T. } )
aAdd( aDefRules, { '_SET_DEFAULT' ,  , .T. } )
aAdd( aDefRules, { '_SET_EXCLUSIVE' ,  , .T. } )
aAdd( aDefRules, { '_SET_SOFTSEEK' ,  , .T. } )
aAdd( aDefRules, { '_SET_UNIQUE' ,  , .T. } )
aAdd( aDefRules, { '_SET_DELETED' ,  , .T. } )
aAdd( aDefRules, { '_SET_CANCEL' ,  , .T. } )
aAdd( aDefRules, { '_SET_DEBUG' ,  , .T. } )
aAdd( aDefRules, { '_SET_TYPEAHEAD' ,  , .T. } )
aAdd( aDefRules, { '_SET_COLOR' ,  , .T. } )
aAdd( aDefRules, { '_SET_CURSOR' ,  , .T. } )
aAdd( aDefRules, { '_SET_CONSOLE' ,  , .T. } )
aAdd( aDefRules, { '_SET_ALTERNATE' ,  , .T. } )
aAdd( aDefRules, { '_SET_ALTFILE' ,  , .T. } )
aAdd( aDefRules, { '_SET_DEVICE' ,  , .T. } )
aAdd( aDefRules, { '_SET_EXTRA' ,  , .T. } )
aAdd( aDefRules, { '_SET_EXTRAFILE' ,  , .T. } )
aAdd( aDefRules, { '_SET_PRINTER' ,  , .T. } )
aAdd( aDefRules, { '_SET_PRINTFILE' ,  , .T. } )
aAdd( aDefRules, { '_SET_MARGIN' ,  , .T. } )
aAdd( aDefRules, { '_SET_BELL' ,  , .T. } )
aAdd( aDefRules, { '_SET_CONFIRM' ,  , .T. } )
aAdd( aDefRules, { '_SET_ESCAPE' ,  , .T. } )
aAdd( aDefRules, { '_SET_INSERT' ,  , .T. } )
aAdd( aDefRules, { '_SET_EXIT' ,  , .T. } )
aAdd( aDefRules, { '_SET_INTENSITY' ,  , .T. } )
aAdd( aDefRules, { '_SET_SCOREBOARD' ,  , .T. } )
aAdd( aDefRules, { '_SET_DELIMITERS' ,  , .T. } )
aAdd( aDefRules, { '_SET_DELIMCHARS' ,  , .T. } )
aAdd( aDefRules, { '_SET_WRAP' ,  , .T. } )
aAdd( aDefRules, { '_SET_MESSAGE' ,  , .T. } )
aAdd( aDefRules, { '_SET_MCENTER' ,  , .T. } )
aAdd( aDefRules, { '_SET_SCROLLBREAK' ,  , .T. } )
aAdd( aDefRules, { '_SET_COUNT' ,  , .T. } )
aAdd( aDefRules, { '_SET_CH' ,  , .T. } )
aAdd( aDefRules, { '_DFSET' , { {    1,   0, '(', '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )

/* Translates */
aTransRules := {}

/* Commands */
aCommRules := {}
aAdd( aCommRules, { 'DO' , { {    1,   0, 'WHILE', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'END' , { {    1,   0, NIL, '<', NIL } } , .F. } )
aAdd( aCommRules, { 'END' , { {    0,   0, 'SEQUENCE', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'ENDSEQUENCE' ,  , .F. } )
aAdd( aCommRules, { 'ENDDO' , { {    1,   0, NIL, '*', NIL } } , .F. } )
aAdd( aCommRules, { 'ENDIF' , { {    1,   0, NIL, '*', NIL } } , .F. } )
aAdd( aCommRules, { 'ENDCASE' , { {    1,   0, NIL, '*', NIL } } , .F. } )
aAdd( aCommRules, { 'ENDFOR' , { {    1,   1, NIL, '*', NIL } } , .F. } )
aAdd( aCommRules, { 'NEXT' , { {    1,   0, NIL, '<', NIL }, {    2,   1, 'TO', '<', NIL }, {    3,   1, 'STEP', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'DO' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '.PRG', NIL, NIL }, { 1002,   1, 'WITH', 'A', NIL } } , .F. } )
aAdd( aCommRules, { 'CALL' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '()', NIL, NIL }, { 1002,   1, 'WITH', 'A', NIL } } , .F. } )
aAdd( aCommRules, { 'STORE' , { {    1,   0, NIL, '<', NIL }, {    2,   0, 'TO', '<', NIL }, { 1003,   1, ',', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'ECHO', '*', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'HEADING', '*', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'MENU', '*', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'STATUS', '*', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'STEP', '*', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'SAFETY', '*', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'TALK', '*', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'PROCEDURE', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'PROCEDURE', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'EXACT', ':', { 'ON', 'OFF', '&' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'EXACT', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'FIXED', ':', { 'ON', 'OFF', '&' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'FIXED', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'DECIMALS', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'DECIMALS', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'PATH', NIL, NIL }, {    1,   0, 'TO', '*', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'PATH', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'DEFAULT', NIL, NIL }, {    1,   0, 'TO', '(', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'DEFAULT', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'CENTURY', ':', { 'ON', 'OFF', '&' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'CENTURY', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'EPOCH', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   0, 'FORMAT', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    1,   0, NIL, '<', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'AMERICAN', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'ANSI', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'BRITISH', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'FRENCH', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'GERMAN', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'ITALIAN', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'JAPANESE', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'USA', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'ALTERNATE', ':', { 'ON', 'OFF', '&' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'ALTERNATE', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'ALTERNATE', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'ALTERNATE', NIL, NIL }, {    1,   0, 'TO', '(', NIL }, {    2,   1, NIL, ':', { 'ADDITIVE' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'CONSOLE', ':', { 'ON', 'OFF', '&' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'CONSOLE', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'MARGIN', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'MARGIN', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'PRINTER', ':', { 'ON', 'OFF', '&' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'PRINTER', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'PRINTER', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'PRINTER', NIL, NIL }, {    1,   0, 'TO', '(', NIL }, {    2,   1, NIL, ':', { 'ADDITIVE' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'DEVICE', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, {    0,   0, 'SCREEN', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'DEVICE', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, {    0,   0, 'PRINTER', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'COLOR', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, {    1,   1, NIL, '*', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'COLOR', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'COLOUR', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, { 1001,   1, NIL, '*', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'CURSOR', ':', { 'ON', 'OFF', '&' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'CURSOR', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { '?' , { {    1,   1, NIL, 'A', NIL } } , .F. } )
aAdd( aCommRules, { '??' , { {    1,   1, NIL, 'A', NIL } } , .F. } )
aAdd( aCommRules, { 'EJECT' ,  , .F. } )
aAdd( aCommRules, { 'TEXT' ,  , .F. } )
aAdd( aCommRules, { 'TEXT' , { {    0,   0, 'TO', NIL, NIL }, {    1,   0, 'FILE', '(', NIL } } , .F. } )
aAdd( aCommRules, { 'TEXT' , { {    0,   0, 'TO', NIL, NIL }, {    0,   0, 'PRINTER', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'CLS' ,  , .F. } )
aAdd( aCommRules, { 'CLEAR' , { {    0,   0, 'SCREEN', NIL, NIL } } , .F. } )
aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL } } , .F. } )
aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'CLEAR', NIL, NIL } } , .F. } )
aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'CLEAR', NIL, NIL }, {    3,   0, 'TO', '<', NIL }, {    4,   0, ',', '<', NIL } } , .F. } )
aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, ',', '<', NIL }, {    4,   0, ',', '<', NIL }, {    5,   0, 'BOX', '<', NIL }, { 1006,   1, 'COLOR', '<', NIL } } , .F. } )
aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'TO', '<', NIL }, {    4,   0, ',', '<', NIL }, {    0,   1, 'DOUBLE', NIL, NIL }, { 1005,   1, 'COLOR', '<', NIL } } , .F. } )
aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'TO', '<', NIL }, {    4,   0, ',', '<', NIL }, { 1005,   1, 'COLOR', '<', NIL } } , .F. } )
aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'SAY', '<', NIL }, {    4,   1, 'PICTURE', '<', NIL }, { 1005,   1, 'COLOR', '<', NIL } } , .F. } )
aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'SAY', '<', NIL }, { 1004,   1, 'COLOR', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'BELL', ':', { 'ON', 'OFF', '&' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'BELL', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'CONFIRM', ':', { 'ON', 'OFF', '&' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'CONFIRM', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'ESCAPE', ':', { 'ON', 'OFF', '&' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'ESCAPE', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'INTENSITY', ':', { 'ON', 'OFF', '&' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'INTENSITY', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'SCOREBOARD', ':', { 'ON', 'OFF', '&' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'SCOREBOARD', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'DELIMITERS', ':', { 'ON', 'OFF', '&' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'DELIMITERS', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'DELIMITERS', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'DELIMITERS', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, {    0,   0, 'DEFAULT', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'DELIMITERS', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'FORMAT', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'FORMAT', NIL, NIL }, {    1,   0, 'TO', '<', NIL }, {    2,   0, '.', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'FORMAT', NIL, NIL }, {    1,   0, 'TO', ':', { '&' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'FORMAT', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'GET', '<', NIL }, {    4,   1, 'PICTURE', '<', NIL }, {    5,   1, 'VALID', '<', NIL }, {    6,   1, 'WHEN', '<', NIL }, { 1007,   1, 'SEND', '<', NIL } } , .F. } )
aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'SAY', '<', NIL }, { 1004,   1, NIL, 'A', { 'GET' } }, {    5,   0, 'GET', '<', NIL }, { 1006,   1, NIL, 'A', NIL } } , .F. } )
aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'GET', '<', NIL }, { 1004,   1, NIL, 'A', { 'RANGE' } }, {    5,   0, 'RANGE', '<', NIL }, {    6,   0, ',', '<', NIL }, { 1007,   1, NIL, 'A', NIL } } , .F. } )
aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'GET', '<', NIL }, { 1004,   1, NIL, 'A', { 'COLOR' } }, {    5,   0, 'COLOR', '<', NIL }, { 1006,   1, NIL, 'A', NIL } } , .F. } )
aAdd( aCommRules, { 'READ' , { {    0,   0, 'SAVE', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'READ' ,  , .F. } )
aAdd( aCommRules, { 'CLEAR' , { {    0,   0, 'GETS', NIL, NIL } } , .F. } )
aAdd( aCommRules, { '@' , { { 1001,   1, NIL, 'A', { 'COLOUR' } }, {    0,   0, 'COLOUR', NIL, NIL }, { 1002,   1, NIL, 'A', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'WRAP', ':', { 'ON', 'OFF', '&' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'WRAP', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'MESSAGE', NIL, NIL }, {    1,   0, 'TO', '<', NIL }, {    2,   1, NIL, ':', { 'CENTER', 'CENTRE' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'MESSAGE', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'PROMPT', '<', NIL }, {    4,   1, 'MESSAGE', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'MENU' , { {    1,   0, 'TO', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'SAVE' , { {    0,   0, 'SCREEN', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'RESTORE' , { {    0,   0, 'SCREEN', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SAVE' , { {    0,   0, 'SCREEN', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'RESTORE' , { {    0,   0, 'SCREEN', NIL, NIL }, {    1,   0, 'FROM', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'WAIT' , { {    1,   1, NIL, '<', NIL } } , .F. } )
aAdd( aCommRules, { 'WAIT' , { {    1,   1, NIL, '<', { 'TO' } }, {    2,   0, 'TO', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'ACCEPT' , { {    1,   1, NIL, '<', { 'TO' } }, {    2,   0, 'TO', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'INPUT' , { {    1,   1, NIL, '<', { 'TO' } }, {    2,   0, 'TO', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'KEYBOARD' , { {    1,   0, NIL, '<', NIL } } , .F. } )
aAdd( aCommRules, { 'CLEAR' , { {    0,   0, 'TYPEAHEAD', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'TYPEAHEAD', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'KEY', '<', NIL }, {    2,   0, 'TO', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'KEY', '<', NIL }, {    2,   0, 'TO', '<', NIL }, {    0,   0, '(', NIL, NIL }, {    3,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'KEY', '<', NIL }, {    2,   0, 'TO', ':', { '&' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'KEY', '<', NIL }, {    0,   1, 'TO', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'FUNCTION', '<', NIL }, {    0,   1, 'TO', NIL, NIL }, {    2,   1, NIL, '<', { 'TO' } } } , .F. } )
aAdd( aCommRules, { 'CLEAR' , { {    0,   0, 'MEMORY', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'RELEASE' , { {    1,   0, NIL, 'A', NIL } } , .F. } )
aAdd( aCommRules, { 'RELEASE' , { {    0,   0, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'RELEASE' , { {    0,   0, 'ALL', NIL, NIL }, {    1,   0, 'LIKE', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'RELEASE' , { {    0,   0, 'ALL', NIL, NIL }, {    1,   0, 'EXCEPT', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'RESTORE' , { {    1,   1, 'FROM', '(', NIL }, {    2,   1, NIL, ':', { 'ADDITIVE' } } } , .F. } )
aAdd( aCommRules, { 'SAVE' , { {    0,   0, 'ALL', NIL, NIL }, {    1,   0, 'LIKE', '<', NIL }, {    2,   0, 'TO', '(', NIL } } , .F. } )
aAdd( aCommRules, { 'SAVE' , { {    1,   0, 'TO', '(', NIL }, {    0,   0, 'ALL', NIL, NIL }, {    2,   0, 'LIKE', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'SAVE' , { {    0,   0, 'ALL', NIL, NIL }, {    1,   0, 'EXCEPT', '<', NIL }, {    2,   0, 'TO', '(', NIL } } , .F. } )
aAdd( aCommRules, { 'SAVE' , { {    1,   0, 'TO', '(', NIL }, {    0,   0, 'ALL', NIL, NIL }, {    2,   0, 'EXCEPT', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'SAVE' , { {    1,   1, 'TO', '(', NIL }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'ERASE' , { {    1,   0, NIL, '(', NIL } } , .F. } )
aAdd( aCommRules, { 'DELETE' , { {    1,   0, 'FILE', '(', NIL } } , .F. } )
aAdd( aCommRules, { 'RENAME' , { {    1,   0, NIL, '(', NIL }, {    2,   0, 'TO', '(', NIL } } , .F. } )
aAdd( aCommRules, { 'COPY' , { {    1,   0, 'FILE', '(', NIL }, {    2,   0, 'TO', '(', NIL } } , .F. } )
aAdd( aCommRules, { 'DIR' , { {    1,   1, NIL, '(', NIL } } , .F. } )
aAdd( aCommRules, { 'TYPE' , { { 1001,   0, NIL, '(', NIL }, {    2,   1, NIL, ':', { 'TO PRINTER' } }, { 1000,   1, 'TO', NIL, NIL }, { 1003,  -1, 'FILE', '(', NIL } } , .F. } )
aAdd( aCommRules, { 'TYPE' , { {    1,   0, NIL, '(', NIL }, {    2,   1, NIL, ':', { 'TO PRINTER' } } } , .F. } )
aAdd( aCommRules, { 'REQUEST' , { {    1,   0, NIL, 'A', NIL } } , .F. } )
aAdd( aCommRules, { 'CANCEL' ,  , .F. } )
aAdd( aCommRules, { 'QUIT' ,  , .F. } )
aAdd( aCommRules, { 'RUN' , { {    1,   0, NIL, '*', NIL } } , .F. } )
aAdd( aCommRules, { 'RUN' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { '!' , { {    1,   0, NIL, '*', NIL } } , .F. } )
aAdd( aCommRules, { 'RUN' , { {    1,   0, '=', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'RUN' , { {    1,   0, ':=', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'EXCLUSIVE', ':', { 'ON', 'OFF', '&' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'EXCLUSIVE', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'SOFTSEEK', ':', { 'ON', 'OFF', '&' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'SOFTSEEK', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'UNIQUE', ':', { 'ON', 'OFF', '&' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'UNIQUE', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    1,   0, 'DELETED', ':', { 'ON', 'OFF', '&' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'DELETED', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SELECT' , { {    1,   0, NIL, '<', NIL } } , .F. } )
aAdd( aCommRules, { 'SELECT' , { {    1,   0, NIL, '<', NIL }, {    2,   1, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'USE' ,  , .F. } )
aAdd( aCommRules, { 'USE' , { {    1,   0, NIL, '(', NIL }, {    2,   1, 'VIA', '<', NIL }, {    3,   1, 'ALIAS', '<', NIL }, {    4,   1, NIL, ':', { 'NEW' } }, {    5,   1, NIL, ':', { 'EXCLUSIVE' } }, {    6,   1, NIL, ':', { 'SHARED' } }, {    7,   1, NIL, ':', { 'READONLY' } }, { 1008,   1, 'INDEX', '(', NIL }, { 1009,   2, ',', '(', NIL } } , .F. } )
aAdd( aCommRules, { 'APPEND' , { {    0,   0, 'BLANK', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'PACK' ,  , .F. } )
aAdd( aCommRules, { 'ZAP' ,  , .F. } )
aAdd( aCommRules, { 'UNLOCK' ,  , .F. } )
aAdd( aCommRules, { 'UNLOCK' , { {    0,   0, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'COMMIT' ,  , .F. } )
aAdd( aCommRules, { 'GOTO' , { {    1,   0, NIL, '<', NIL } } , .F. } )
aAdd( aCommRules, { 'GO' , { {    1,   0, NIL, '<', NIL } } , .F. } )
aAdd( aCommRules, { 'GOTO' , { {    0,   0, 'TOP', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'GO' , { {    0,   0, 'TOP', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'GOTO' , { {    0,   0, 'BOTTOM', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'GO' , { {    0,   0, 'BOTTOM', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SKIP' ,  , .F. } )
aAdd( aCommRules, { 'SKIP' , { {    1,   0, NIL, '<', NIL } } , .F. } )
aAdd( aCommRules, { 'SKIP' , { {    1,   0, 'ALIAS', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'SKIP' , { {    1,   0, NIL, '<', NIL }, {    2,   0, 'ALIAS', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'SEEK' , { {    1,   0, NIL, '<', NIL }, {    2,   1, NIL, ':', { 'SOFTSEEK' } } } , .F. } )
aAdd( aCommRules, { 'FIND' , { {    1,   0, NIL, '*', NIL } } , .F. } )
aAdd( aCommRules, { 'FIND' , { {    1,   0, ':=', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'FIND' , { {    1,   0, '=', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'CONTINUE' ,  , .F. } )
aAdd( aCommRules, { 'LOCATE' , { {    1,   1, 'FOR', '<', NIL }, {    2,   1, 'WHILE', '<', NIL }, {    3,   1, 'NEXT', '<', NIL }, {    4,   1, 'RECORD', '<', NIL }, {    5,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'RELATION', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'RELATION', NIL, NIL }, {    1,   1, NIL, ':', { 'ADDITIVE' } }, {    2,   1, 'TO', '<', NIL }, {    3,  -1, 'INTO', '(', NIL }, {    0,   2, ',', NIL, NIL }, {    0,   3, 'TO', NIL, NIL }, { 1004,   2, NIL, '<', { ',' } }, { 1005,  -2, 'INTO', '(', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'FILTER', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'FILTER', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'FILTER', NIL, NIL }, {    1,   0, 'TO', ':', { '&' } } } , .F. } )
aAdd( aCommRules, { 'REPLACE' , { {    1,   1, NIL, '<', { 'FOR', 'WHILE', 'NEXT', 'RECORD', 'ALL' } }, {    2,  -1, 'WITH', '<', NIL }, { 1003,   2, ',', '<', NIL }, { 1004,  -2, 'WITH', '<', NIL }, {    5,   1, 'FOR', '<', NIL }, {    6,   1, 'WHILE', '<', NIL }, {    7,   1, 'NEXT', '<', NIL }, {    8,   1, 'RECORD', '<', NIL }, {    9,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'REPLACE' , { {    1,   0, NIL, '<', NIL }, {    2,   0, 'WITH', '<', NIL }, { 1003,   1, ',', '<', NIL }, { 1004,  -1, 'WITH', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'DELETE' , { {    1,   1, 'FOR', '<', NIL }, {    2,   1, 'WHILE', '<', NIL }, {    3,   1, 'NEXT', '<', NIL }, {    4,   1, 'RECORD', '<', NIL }, {    5,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'RECALL' , { {    1,   1, 'FOR', '<', NIL }, {    2,   1, 'WHILE', '<', NIL }, {    3,   1, 'NEXT', '<', NIL }, {    4,   1, 'RECORD', '<', NIL }, {    5,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'DELETE' ,  , .F. } )
aAdd( aCommRules, { 'RECALL' ,  , .F. } )
aAdd( aCommRules, { 'CREATE' , { {    1,   0, NIL, '(', NIL }, {    2,   1, 'FROM', '(', NIL }, {    3,   1, 'VIA', '<', NIL }, {    4,   1, 'ALIAS', '<', NIL }, {    5,   1, NIL, ':', { 'NEW' } } } , .F. } )
aAdd( aCommRules, { 'COPY' , { {    0,   1, 'STRUCTURE', NIL, NIL }, {    0,   1, 'EXTENDED', NIL, NIL }, {    1,   1, 'TO', '(', NIL } } , .F. } )
aAdd( aCommRules, { 'COPY' , { {    0,   1, 'STRUCTURE', NIL, NIL }, {    1,   1, 'TO', '(', NIL }, {    2,   1, 'FIELDS', 'A', NIL } } , .F. } )
aAdd( aCommRules, { 'COPY' , { {    1,   1, 'TO', '(', NIL }, {    0,   1, 'DELIMITED', NIL, NIL }, {    2,   2, 'WITH', '*', NIL }, {    3,   1, 'FIELDS', 'A', NIL }, {    4,   1, 'FOR', '<', NIL }, {    5,   1, 'WHILE', '<', NIL }, {    6,   1, 'NEXT', '<', NIL }, {    7,   1, 'RECORD', '<', NIL }, {    8,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'COPY' , { {    1,   1, 'TO', '(', NIL }, {    0,   1, 'SDF', NIL, NIL }, {    2,   1, 'FIELDS', 'A', NIL }, {    3,   1, 'FOR', '<', NIL }, {    4,   1, 'WHILE', '<', NIL }, {    5,   1, 'NEXT', '<', NIL }, {    6,   1, 'RECORD', '<', NIL }, {    7,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'COPY' , { {    1,   1, 'TO', '(', NIL }, {    2,   1, 'FIELDS', 'A', NIL }, {    3,   1, 'FOR', '<', NIL }, {    4,   1, 'WHILE', '<', NIL }, {    5,   1, 'NEXT', '<', NIL }, {    6,   1, 'RECORD', '<', NIL }, {    7,   1, NIL, ':', { 'REST' } }, {    8,   1, 'VIA', '<', NIL }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'APPEND' , { {    1,   1, 'FROM', '(', NIL }, {    0,   1, 'DELIMITED', NIL, NIL }, {    2,   2, 'WITH', '*', NIL }, {    3,   1, 'FIELDS', 'A', NIL }, {    4,   1, 'FOR', '<', NIL }, {    5,   1, 'WHILE', '<', NIL }, {    6,   1, 'NEXT', '<', NIL }, {    7,   1, 'RECORD', '<', NIL }, {    8,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'APPEND' , { {    1,   1, 'FROM', '(', NIL }, {    0,   1, 'SDF', NIL, NIL }, {    2,   1, 'FIELDS', 'A', NIL }, {    3,   1, 'FOR', '<', NIL }, {    4,   1, 'WHILE', '<', NIL }, {    5,   1, 'NEXT', '<', NIL }, {    6,   1, 'RECORD', '<', NIL }, {    7,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'APPEND' , { {    1,   1, 'FROM', '(', NIL }, {    2,   1, 'FIELDS', 'A', NIL }, {    3,   1, 'FOR', '<', NIL }, {    4,   1, 'WHILE', '<', NIL }, {    5,   1, 'NEXT', '<', NIL }, {    6,   1, 'RECORD', '<', NIL }, {    7,   1, NIL, ':', { 'REST' } }, {    8,   1, 'VIA', '<', NIL }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SORT' , { {    1,   1, 'TO', '(', NIL }, {    2,   1, 'ON', 'A', NIL }, {    3,   1, 'FOR', '<', NIL }, {    4,   1, 'WHILE', '<', NIL }, {    5,   1, 'NEXT', '<', NIL }, {    6,   1, 'RECORD', '<', NIL }, {    7,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'TOTAL' , { {    1,   1, 'TO', '(', NIL }, {    2,   1, 'ON', '<', NIL }, {    3,   1, 'FIELDS', 'A', NIL }, {    4,   1, 'FOR', '<', NIL }, {    5,   1, 'WHILE', '<', NIL }, {    6,   1, 'NEXT', '<', NIL }, {    7,   1, 'RECORD', '<', NIL }, {    8,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'UPDATE' , { {    1,   1, 'FROM', '(', NIL }, {    2,   1, 'ON', '<', NIL }, {    3,   1, 'REPLACE', '<', NIL }, {    4,  -1, 'WITH', '<', NIL }, { 1005,   2, ',', '<', NIL }, { 1006,  -2, 'WITH', '<', NIL }, {    7,   1, NIL, ':', { 'RANDOM' } } } , .F. } )
aAdd( aCommRules, { 'JOIN' , { {    1,   1, 'WITH', '(', NIL }, {    2,   1, 'TO', '<', NIL }, {    3,   1, 'FIELDS', 'A', NIL }, {    4,   1, 'FOR', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'COUNT' , { {    1,   1, 'TO', '<', NIL }, {    2,   1, 'FOR', '<', NIL }, {    3,   1, 'WHILE', '<', NIL }, {    4,   1, 'NEXT', '<', NIL }, {    5,   1, 'RECORD', '<', NIL }, {    6,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'SUM' , { {    1,   1, NIL, '<', { 'FOR', 'WHILE', 'NEXT', 'RECORD', 'ALL' } }, { 1002,   2, ',', '<', NIL }, {    3,  -1, 'TO', '<', NIL }, { 1004,   2, ',', '<', NIL }, {    5,   1, 'FOR', '<', NIL }, {    6,   1, 'WHILE', '<', NIL }, {    7,   1, 'NEXT', '<', NIL }, {    8,   1, 'RECORD', '<', NIL }, {    9,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'AVERAGE' , { {    1,   1, NIL, '<', { 'FOR', 'WHILE', 'NEXT', 'RECORD', 'ALL' } }, { 1002,   2, ',', '<', NIL }, {    3,  -1, 'TO', '<', NIL }, { 1004,   2, ',', '<', NIL }, {    5,   1, 'FOR', '<', NIL }, {    6,   1, 'WHILE', '<', NIL }, {    7,   1, 'NEXT', '<', NIL }, {    8,   1, 'RECORD', '<', NIL }, {    9,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'LIST' , { {    1,   1, NIL, 'A', { 'TO', 'FOR', 'WHILE', 'NEXT', 'RECORD', 'ALL' } }, {    2,   1, NIL, ':', { 'OFF' } }, {    3,   1, NIL, ':', { 'TO PRINTER' } }, {    0,   1, 'TO', NIL, NIL }, {    4,  -1, 'FILE', '(', NIL }, {    5,   1, 'FOR', '<', NIL }, {    6,   1, 'WHILE', '<', NIL }, {    7,   1, 'NEXT', '<', NIL }, {    8,   1, 'RECORD', '<', NIL }, {    9,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'DISPLAY' , { {    1,   1, NIL, 'A', { 'TO', 'FOR', 'WHILE', 'NEXT', 'RECORD' } }, {    2,   1, NIL, ':', { 'OFF' } }, {    3,   1, NIL, ':', { 'TO PRINTER' } }, {    0,   1, 'TO', NIL, NIL }, {    4,  -1, 'FILE', '(', NIL }, {    5,   1, 'FOR', '<', NIL }, {    6,   1, 'WHILE', '<', NIL }, {    7,   1, 'NEXT', '<', NIL }, {    8,   1, 'RECORD', '<', NIL }, {    9,   1, NIL, ':', { 'REST' } }, {   10,   1, NIL, ':', { 'ALL' } } } , .F. } )
aAdd( aCommRules, { 'REPORT' , { {    1,   0, 'FORM', '<', NIL }, {    2,   1, 'HEADING', '<', NIL }, {    3,   1, NIL, ':', { 'PLAIN' } }, {    4,   1, NIL, ':', { 'NOEJECT' } }, {    5,   1, NIL, ':', { 'SUMMARY' } }, {    6,   1, NIL, ':', { 'NOCONSOLE' } }, {    7,   1, NIL, ':', { 'TO PRINTER' } }, {    0,   1, 'TO', NIL, NIL }, {    8,  -1, 'FILE', '(', NIL }, {    9,   1, 'FOR', '<', NIL }, {   10,   1, 'WHILE', '<', NIL }, {   11,   1, 'NEXT', '<', NIL }, {   12,   1, 'RECORD', '<', NIL }, {   13,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'LABEL' , { {    1,   0, 'FORM', '<', NIL }, {    2,   1, NIL, ':', { 'SAMPLE' } }, {    3,   1, NIL, ':', { 'NOCONSOLE' } }, {    4,   1, NIL, ':', { 'TO PRINTER' } }, {    0,   1, 'TO', NIL, NIL }, {    5,  -1, 'FILE', '(', NIL }, {    6,   1, 'FOR', '<', NIL }, {    7,   1, 'WHILE', '<', NIL }, {    8,   1, 'NEXT', '<', NIL }, {    9,   1, 'RECORD', '<', NIL }, {   10,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'CLOSE' , { {    1,   0, NIL, '<', NIL } } , .F. } )
aAdd( aCommRules, { 'CLOSE' ,  , .F. } )
aAdd( aCommRules, { 'CLOSE' , { {    0,   0, 'DATABASES', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'CLOSE' , { {    0,   0, 'ALTERNATE', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'CLOSE' , { {    0,   0, 'FORMAT', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'CLOSE' , { {    0,   0, 'INDEXES', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'CLOSE' , { {    0,   0, 'PROCEDURE', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'CLOSE' , { {    0,   0, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'CLEAR' ,  , .F. } )
aAdd( aCommRules, { 'CLEAR' , { {    0,   0, 'ALL', NIL, NIL } } , .F. } )
aAdd( aCommRules, { 'INDEX' , { {    1,   0, 'ON', '<', NIL }, {    2,   1, 'TAG', '(', NIL }, {    3,   0, 'TO', '(', NIL }, {    4,   1, 'FOR', '<', NIL }, { 1005,   1, NIL, ':', { 'ALL' } }, {    6,   1, 'WHILE', '<', NIL }, {    7,   1, 'NEXT', '<', NIL }, {    8,   1, 'RECORD', '<', NIL }, { 1009,   1, NIL, ':', { 'REST' } }, {   10,   1, 'EVAL', '<', NIL }, {   11,   1, 'EVERY', '<', NIL }, { 1012,   1, NIL, ':', { 'UNIQUE' } }, {   13,   1, NIL, ':', { 'ASCENDING' } }, { 1014,   1, NIL, ':', { 'DESCENDING' } } } , .F. } )
aAdd( aCommRules, { 'INDEX' , { {    1,   0, 'ON', '<', NIL }, {    2,   0, 'TAG', '(', NIL }, {    3,   1, 'TO', '(', NIL }, {    4,   1, 'FOR', '<', NIL }, { 1005,   1, NIL, ':', { 'ALL' } }, {    6,   1, 'WHILE', '<', NIL }, {    7,   1, 'NEXT', '<', NIL }, {    8,   1, 'RECORD', '<', NIL }, { 1009,   1, NIL, ':', { 'REST' } }, {   10,   1, 'EVAL', '<', NIL }, {   11,   1, 'EVERY', '<', NIL }, { 1012,   1, NIL, ':', { 'UNIQUE' } }, {   13,   1, NIL, ':', { 'ASCENDING' } }, { 1014,   1, NIL, ':', { 'DESCENDING' } } } , .F. } )
aAdd( aCommRules, { 'INDEX' , { {    1,   0, 'ON', '<', NIL }, {    2,   0, 'TO', '(', NIL }, {    3,   1, NIL, ':', { 'UNIQUE' } } } , .F. } )
aAdd( aCommRules, { 'DELETE' , { {    1,   0, 'TAG', '(', NIL }, {    2,   1, 'IN', '(', NIL }, { 1003,   1, ',', '(', NIL }, { 1004,   2, 'IN', '(', NIL } } , .F. } )
aAdd( aCommRules, { 'REINDEX' , { {    1,   1, 'EVAL', '<', NIL }, {    2,   1, 'EVERY', '<', NIL } } , .F. } )
aAdd( aCommRules, { 'REINDEX' ,  , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'INDEX', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, { 1001,   1, NIL, '(', NIL }, { 1002,   2, ',', '(', NIL }, {    3,   1, NIL, ':', { 'ADDITIVE' } } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'ORDER', NIL, NIL }, {    1,   0, 'TO', '<', NIL }, { 1002,   1, 'IN', '(', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'ORDER', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, {    1,   0, 'TAG', '(', NIL }, { 1002,   1, 'IN', '(', NIL } } , .F. } )
aAdd( aCommRules, { 'SET' , { {    0,   0, 'ORDER', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )

RETURN .T.

FUNCTION InitResults()

/* Defines Results*/
aDefResults := {}
aAdd( aDefResults, { { {   0, '1' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '2' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '3' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '4' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '5' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '6' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '7' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '8' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '9' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '10' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '11' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '12' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '13' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '14' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '15' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '16' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '17' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '18' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '19' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '20' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '21' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '22' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '23' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '24' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '25' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '26' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '27' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '28' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '29' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '30' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '31' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '32' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '33' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '34' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '35' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '36' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '37' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '38' } }, { -1} , { }  } )
aAdd( aDefResults, { { {   0, '38' } }, { -1} , { }  } )
aAdd( aDefResults, { , ,  } )
aAdd( aDefResults, { { {   0, 'Set' }, {   0, '(' }, {   0, '_SET_DATEFORMAT' }, {   0, ',' }, {   0, 'if' }, {   0, '(' }, {   0, '__SetCentury' }, {   0, '(' }, {   0, ')' }, {   0, ',' }, {   0,   1 }, {   0, ',' }, {   0,   2 }, {   0, ')' }, {   0, ')' } }, { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  1, -1,  1, -1, -1} , { NIL, NIL}  } )

/* Translates Results*/
aTransResults := {}

/* Commands Results*/
aCommResults := {}
aAdd( aCommResults, { { {   0, 'while ' }, {   0,   1 } }, { -1,  1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'end' } }, { -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'end' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'end' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'enddo' } }, { -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'endif' } }, { -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'endcase' } }, { -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'next' } }, { -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'next' } }, { -1} , { NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'do ' }, {   0,   1 }, {   2, ' WITH ' }, {   2,   2 } }, { -1,  1, -1,  1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'call ' }, {   0,   1 }, {   2, ' WITH ' }, {   2,   2 } }, { -1,  1, -1,  1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0,   2 }, {   0, ' := ' }, {   3,   3 }, {   3, ' := ' }, {   0,   1 } }, {  1, -1,  1, -1,  1} , { NIL, NIL, NIL}  } )
aAdd( aCommResults, { , ,  } )
aAdd( aCommResults, { , ,  } )
aAdd( aCommResults, { , ,  } )
aAdd( aCommResults, { , ,  } )
aAdd( aCommResults, { , ,  } )
aAdd( aCommResults, { , ,  } )
aAdd( aCommResults, { , ,  } )
aAdd( aCommResults, { , ,  } )
aAdd( aCommResults, { { {   0, '_ProcReq_( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_EXACT, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_EXACT, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_FIXED, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_FIXED, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_DECIMALS, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_DECIMALS, 0 )' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_PATH, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_PATH, "" )' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_DEFAULT, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_DEFAULT, "" )' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '__SetCentury( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '__SetCentury( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_EPOCH, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_DATEFORMAT, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '_DFSET( "mm/dd/yyyy", "mm/dd/yy" )' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '_DFSET( "yyyy.mm.dd", "yy.mm.dd" )' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '_DFSET( "dd/mm/yyyy", "dd/mm/yy" )' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '_DFSET( "dd/mm/yyyy", "dd/mm/yy" )' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '_DFSET( "dd.mm.yyyy", "dd.mm.yy" )' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '_DFSET( "dd-mm-yyyy", "dd-mm-yy" )' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '_DFSET( "yyyy/mm/dd", "yy/mm/dd" )' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '_DFSET( "mm-dd-yyyy", "mm-dd-yy" )' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_ALTERNATE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_ALTERNATE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_ALTFILE, "" )' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_ALTFILE, ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  4, -1,  6, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_CONSOLE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_CONSOLE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_MARGIN, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_MARGIN, 0 )' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_PRINTER, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_PRINTER, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_PRINTFILE, "" )' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_PRINTFILE, ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  4, -1,  6, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_DEVICE, "SCREEN" )' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_DEVICE, "PRINTER" )' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'SetColor( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  2, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'SetColor( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'SET COLOR TO ' }, {   1,   1 } }, { -1,  1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'SetCursor( if(Upper(' }, {   0,   1 }, {   0, ') == "ON", 1, 0) )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'SetCursor( if(' }, {   0,   1 }, {   0, ', 1, 0) )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'QOut( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'QQOut( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '__Eject()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'text QOut, QQOut' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '__TextSave( ' }, {   0,   1 }, {   0, ' ) ; text QOut, __TextRestore' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '__TextSave("PRINTER") ; text QOut, __TextRestore' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'Scroll() ; SetPos(0,0)' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'CLS' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'Scroll( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ' ) ; SetPos( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'Scroll( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' ) ; SetPos( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'Scroll( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ' ) ; SetPos( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'DispBox( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   6, ' , ' }, {   6,   6 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'DispBox( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', 2 ' }, {   5, ', ' }, {   5,   5 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'DispBox( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', 1 ' }, {   5, ', ' }, {   5,   5 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'DevPos( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' ) ; DevOutPict( ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   5, ' , ' }, {   5,   5 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'DevPos( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' ) ; DevOut( ' }, {   0,   3 }, {   4, ' , ' }, {   4,   4 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_BELL, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_BELL, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_CONFIRM, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_CONFIRM, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_ESCAPE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_ESCAPE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_INTENSITY, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_INTENSITY, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_SCOREBOARD, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_SCOREBOARD, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_DELIMITERS, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_DELIMITERS, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_DELIMCHARS, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_DELIMCHARS, "::" )' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_DELIMCHARS, "::" )' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '_ProcReq_( ' }, {   0,   1 }, {   0, ' + ".FMT" ) ; __SetFormat( {|| ' }, {   0,   1 }, {   0, '()} )' } }, { -1,  4, -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '_ProcReq_( ' }, {   0,   1 }, {   0, ' + "." + ' }, {   0,   2 }, {   0, ' ) ; __SetFormat( {|| ' }, {   0,   1 }, {   0, '()} )' } }, { -1,  4, -1,  4, -1,  1, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'if ( Empty(' }, {   0,   1 }, {   0, ') ) ;   SET FORMAT TO ; else ;   __SetFormat( &("{||" + ' }, {   0,   1 }, {   0, ' + "()}") ) ; end' } }, { -1,  4, -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '__SetFormat()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'SetPos( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' ) ; AAdd( GetList, _GET_( ' }, {   0,   3 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ' ):display() ) ' }, {   7, ' ; ATail(GetList):' }, {   7,   7 } }, { -1,  1, -1,  1, -1,  1, -1,  3, -1,  1, -1,  5, -1,  5, -1, -1,  1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '@ ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' SAY ' }, {   0,   3 }, {   4,   4 }, {   0, ' ; @ Row(), Col()+1 GET ' }, {   0,   5 }, {   6,   6 } }, { -1,  1, -1,  1, -1,  1,  1, -1,  1,  1} , { NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '@ ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' GET ' }, {   0,   3 }, {   4,   4 }, {   0, ' VALID {|_1| RangeCheck(_1,, ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ')} ' }, {   7,   7 } }, { -1,  1, -1,  1, -1,  1,  1, -1,  1, -1,  1, -1,  1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '@ ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' GET ' }, {   0,   3 }, {   4,   4 }, {   0, ' SEND colorDisp(' }, {   0,   5 }, {   0, ') ' }, {   6,   6 } }, { -1,  1, -1,  1, -1,  1,  1, -1,  1, -1,  1} , { NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'ReadModal(GetList)' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'ReadModal(GetList) ; GetList := {}' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'ReadKill(.T.) ; GetList := {}' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '@ ' }, {   1,   1 }, {   0, ' COLOR ' }, {   2,   2 } }, { -1,  1, -1,  1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_WRAP, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_WRAP, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_MESSAGE, ' }, {   0,   1 }, {   0, ' ) ; Set( _SET_MCENTER, ' }, {   0,   2 }, {   0, ' )' } }, { -1,  1, -1,  6, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_MESSAGE, 0 ) ; Set( _SET_MCENTER, .f. )' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '__AtPrompt( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ' , ' }, {   0,   4 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0,   1 }, {   0, ' := __MenuTo( {|_1| if(PCount() == 0, ' }, {   0,   1 }, {   0, ', ' }, {   0,   1 }, {   0, ' := _1)}, ' }, {   0,   1 }, {   0, ' )' } }, {  1, -1,  1, -1,  1, -1,  2, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '__XSaveScreen()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '__XRestScreen()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0,   1 }, {   0, ' := SaveScreen( 0, 0, Maxrow(), Maxcol() )' } }, {  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'RestScreen( 0, 0, Maxrow(), Maxcol(), ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '__Wait( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0,   2 }, {   0, ' := __Wait( ' }, {   0,   1 }, {   0, ' )' } }, {  1, -1,  1, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0,   2 }, {   0, ' := __Accept( ' }, {   0,   1 }, {   0, ' )' } }, {  1, -1,  1, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'if ( !Empty(__Accept(' }, {   0,   1 }, {   0, ')) ) ; ' }, {   0,   2 }, {   0, ' := &( __AcceptStr() ) ; end' } }, { -1,  1, -1,  1, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__Keyboard( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '__Keyboard()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_TYPEAHEAD, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'SetKey( ' }, {   0,   1 }, {   0, ', {|p, l, v| ' }, {   0,   2 }, {   0, '(p, l, v)} )' } }, { -1,  1, -1,  1, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'SET KEY ' }, {   0,   1 }, {   0, ' TO ' }, {   0,   2 } }, { -1,  1, -1,  1} , { NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'if ( Empty(' }, {   0,   2 }, {   0, ') ) ;   SetKey( ' }, {   0,   1 }, {   0, ', NIL ) ; else ;   SetKey( ' }, {   0,   1 }, {   0, ', {|p, l, v| ' }, {   0,   2 }, {   0, '(p, l, v)} ) ; end' } }, { -1,  4, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'SetKey( ' }, {   0,   1 }, {   0, ', NIL )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '__SetFunction( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  1, -1,  1, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__MClear()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '__MXRelease( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  3, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '__MRelease("*", .t.)' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '__MRelease( ' }, {   0,   1 }, {   0, ', .t. )' } }, { -1,  2, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '__MRelease( ' }, {   0,   1 }, {   0, ', .f. )' } }, { -1,  2, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '__MRestore( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  4, -1,  6, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__MSave( ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ', .t. )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__MSave( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', .t. )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__MSave( ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ', .f. )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__MSave( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', .f. )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__MSave( ' }, {   0,   1 }, {   0, ', "*", .t. )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'FErase( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'FErase( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'FRename( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__CopyFile( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__Dir( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '__TypeFile( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' ) ' }, {   1, ' ; COPY FILE ' }, {   1,   1 }, {   1, ' TO ' }, {   1,   3 } }, { -1,  4, -1,  6, -1, -1,  4, -1,  4} , { NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__TypeFile( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  4, -1,  6, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'EXTERNAL ' }, {   0,   1 } }, { -1,  1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '__Quit()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '__Quit()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '__Run( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  2, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '__Run( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'RUN ' }, {   0,   1 } }, { -1,  1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '( run := ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '( run := ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_EXCLUSIVE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_EXCLUSIVE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_SOFTSEEK, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_SOFTSEEK, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_UNIQUE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_UNIQUE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_DELETED, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'Set( _SET_DELETED, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'dbSelectArea( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'dbSelectArea( ' }, {   0,   1 }, {   0, '(' }, {   0,   2 }, {   0, ') )' } }, { -1,  1, -1,  1, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'dbCloseArea()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'dbUseArea( ' }, {   0,   4 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ', ' }, {   0,   3 }, {   0, ', if(' }, {   0,   6 }, {   0, ' .or. ' }, {   0,   5 }, {   0, ', !' }, {   0,   5 }, {   0, ', NIL), ' }, {   0,   7 }, {   0, ' ) ' }, {   8, ' ; dbSetIndex( ' }, {   8,   8 }, {   8, ' )' }, {   9, ' ; dbSetIndex( ' }, {   9,   9 }, {   9, ' )' } }, { -1,  6, -1,  1, -1,  4, -1,  4, -1,  6, -1,  6, -1,  6, -1,  6, -1, -1,  4, -1, -1,  4, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'dbAppend()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '__dbPack()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '__dbZap()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'dbUnlock()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'dbUnlockAll()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'dbCommitAll()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'dbGoto(' }, {   0,   1 }, {   0, ')' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'dbGoto(' }, {   0,   1 }, {   0, ')' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'dbGoTop()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'dbGoTop()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'dbGoBottom()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'dbGoBottom()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'dbSkip(1)' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'dbSkip( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0,   1 }, {   0, ' -> ( dbSkip(1) )' } }, {  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0,   2 }, {   0, ' -> ( dbSkip(' }, {   0,   1 }, {   0, ') )' } }, {  1, -1,  1, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'dbSeek( ' }, {   0,   1 }, {   0, ', if( ' }, {   0,   2 }, {   0, ', .T., NIL ) )' } }, { -1,  1, -1,  6, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'dbSeek( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '( find := ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '( find := ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '__dbContinue()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '__dbLocate( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ' )' } }, { -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'dbClearRel()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'if ( !' }, {   0,   1 }, {   0, ' ) ;    dbClearRel() ; end ; dbSetRelation( ' }, {   0,   3 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   2 }, {   0, ' ) ' }, {   5, ' ; dbSetRelation( ' }, {   5,   5 }, {   5, ', ' }, {   5,   4 }, {   5, ', ' }, {   5,   4 }, {   5, ' )' } }, { -1,  6, -1,  4, -1,  5, -1,  3, -1, -1,  4, -1,  5, -1,  3, -1} , { NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'dbClearFilter(NIL)' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'dbSetFilter( ' }, {   0,   1 }, {   0, ', ' }, {   0,   1 }, {   0, ' )' } }, { -1,  5, -1,  3, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'if ( Empty(' }, {   0,   1 }, {   0, ') ) ;    dbClearFilter() ; else ;    dbSetFilter( ' }, {   0,   1 }, {   0, ', ' }, {   0,   1 }, {   0, ' ) ; end' } }, { -1,  4, -1,  5, -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'DBEval( {|| _FIELD->' }, {   0,   1 }, {   0, ' := ' }, {   0,   2 }, {   3, ' , _FIELD->' }, {   3,   3 }, {   3, ' := ' }, {   3,   4 }, {   0, '}, ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '_FIELD->' }, {   0,   1 }, {   0, ' := ' }, {   0,   2 }, {   3, ' ; _FIELD->' }, {   3,   3 }, {   3, ' := ' }, {   3,   4 } }, { -1,  1, -1,  1, -1,  1, -1,  1} , { NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'DBEval( {|| dbDelete()}, ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ' )' } }, { -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'DBEval( {|| dbRecall()}, ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ' )' } }, { -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'dbDelete()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'dbRecall()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '__dbCreate( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   4 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  1, -1,  6, -1,  4, -1} , { NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__dbCopyXStruct( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, '__dbCopyStruct( ' }, {   0,   1 }, {   0, ', { ' }, {   0,   2 }, {   0, ' } )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__dbDelim( .T., ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', { ' }, {   0,   3 }, {   0, ' }, ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__dbSDF( .T., ' }, {   0,   1 }, {   0, ', { ' }, {   0,   2 }, {   0, ' }, ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__dbCopy( ' }, {   0,   1 }, {   0, ', { ' }, {   0,   2 }, {   0, ' }, ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__dbDelim( .F., ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', { ' }, {   0,   3 }, {   0, ' }, ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__dbSDF( .F., ' }, {   0,   1 }, {   0, ', { ' }, {   0,   2 }, {   0, ' }, ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__dbApp( ' }, {   0,   1 }, {   0, ', { ' }, {   0,   2 }, {   0, ' }, ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__dbSort( ' }, {   0,   1 }, {   0, ', { ' }, {   0,   2 }, {   0, ' }, ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__dbTotal( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', { ' }, {   0,   3 }, {   0, ' }, ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ' )' } }, { -1,  4, -1,  5, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__dbUpdate( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   7 }, {   0, ', {|| _FIELD->' }, {   0,   3 }, {   0, ' := ' }, {   0,   4 }, {   5, ' , _FIELD->' }, {   5,   5 }, {   5, ' := ' }, {   5,   6 }, {   0, '} )' } }, { -1,  4, -1,  5, -1,  6, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__dbJoin( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', { ' }, {   0,   3 }, {   0, ' }, ' }, {   0,   4 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  4, -1,  5, -1} , { NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0,   1 }, {   0, ' := 0 ; DBEval( {|| ' }, {   0,   1 }, {   0, ' := ' }, {   0,   1 }, {   0, ' + 1}, ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ' )' } }, {  1, -1,  1, -1,  1, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0,   3 }, {   0, ' := ' }, {   4,   4 }, {   4, ' := ' }, {   0, ' 0 ; DBEval( {|| ' }, {   0,   3 }, {   0, ' := ' }, {   0,   3 }, {   0, ' + ' }, {   0,   1 }, {   4, ' , ' }, {   4,   4 }, {   4, ' := ' }, {   4,   4 }, {   4, ' + ' }, {   4,   2 }, {   0, '}, ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ' )' } }, {  1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'M->__Avg := ' }, {   0,   3 }, {   0, ' := ' }, {   4,   4 }, {   4, ' := ' }, {   0, ' 0 ; DBEval( {|| M->__Avg := M->__Avg + 1, ' }, {   0,   3 }, {   0, ' := ' }, {   0,   3 }, {   0, ' + ' }, {   0,   1 }, {   4, ' , ' }, {   4,   4 }, {   4, ' := ' }, {   4,   4 }, {   4, ' + ' }, {   4,   2 }, {   0, ' }, ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ' ) ; ' }, {   0,   3 }, {   0, ' := ' }, {   0,   3 }, {   0, ' / M->__Avg ' }, {   4, ' ; ' }, {   4,   4 }, {   4, ' := ' }, {   4,   4 }, {   4, ' / M->__Avg ' } }, { -1,  1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1,  1, -1,  1, -1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__dbList( ' }, {   0,   2 }, {   0, ', { ' }, {   0,   1 }, {   0, ' }, .t., ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ' )' } }, { -1,  6, -1,  5, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1,  6, -1,  4, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__DBList( ' }, {   0,   2 }, {   0, ', { ' }, {   0,   1 }, {   0, ' }, ' }, {   0,  10 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ' )' } }, { -1,  6, -1,  5, -1,  6, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1,  6, -1,  4, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__ReportForm( ' }, {   0,   1 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ' )' } }, { -1,  4, -1,  6, -1,  4, -1,  6, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1,  6, -1,  1, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, '__LabelForm( ' }, {   0,   1 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  4, -1,  6, -1,  4, -1,  6, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0,   1 }, {   0, '->( dbCloseArea() )' } }, {  1, -1} , { NIL}  } )
aAdd( aCommResults, { { {   0, 'dbCloseArea()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'dbCloseAll()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'Set(_SET_ALTFILE, "")' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, '__SetFormat(NIL)' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'dbClearIndex()' } }, { -1} , { }  } )
aAdd( aCommResults, { , ,  } )
aAdd( aCommResults, { { {   0, 'CLOSE DATABASES ; SELECT 1 ; CLOSE FORMAT' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'CLEAR SCREEN ; CLEAR GETS ' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'CLOSE DATABASES ; CLOSE FORMAT ; CLEAR MEMORY ; CLEAR GETS ; SET ALTERNATE OFF ; SET ALTERNATE TO' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'ordCondSet( ' }, {   0,   4 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   5,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ', RECNO(), ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   9,   9 }, {   0, ', ' }, {  14,  14 }, {   0, ' ) ;  ordCreate(' }, {   0,   3 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ', ' }, {   0,   1 }, {   0, ', ' }, {  12,  12 }, {   0, '    )' } }, { -1,  3, -1,  5, -1,  6, -1,  5, -1,  5, -1,  1, -1,  1, -1,  1, -1,  6, -1,  6, -1,  4, -1,  4, -1,  3, -1,  5, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'ordCondSet( ' }, {   0,   4 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   5,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ',     RECNO(), ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   9,   9 }, {   0, ', ' }, {  14,  14 }, {   0, ' ) ;  ordCreate(' }, {   0,   3 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ', ' }, {   0,   1 }, {   0, ', ' }, {  12,  12 }, {   0, '    )' } }, { -1,  3, -1,  5, -1,  6, -1,  5, -1,  5, -1,  1, -1,  1, -1,  1, -1,  6, -1,  6, -1,  4, -1,  4, -1,  3, -1,  5, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'dbCreateIndex( ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ', ' }, {   0,   1 }, {   0, ', if( ' }, {   0,   3 }, {   0, ', .t., NIL ) )' } }, { -1,  4, -1,  3, -1,  5, -1,  6, -1} , { NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'ordDestroy( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' ) ' }, {   3, ' ; ordDestroy( ' }, {   3,   3 }, {   3, ', ' }, {   3,   4 }, {   3, ' ) ' } }, { -1,  4, -1,  4, -1, -1,  4, -1,  4, -1} , { NIL, NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'ordCondSet(,,,, ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ',,,,,,,) ;  ordListRebuild()' } }, { -1,  5, -1,  1, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'ordListRebuild()' } }, { -1} , { }  } )
aAdd( aCommResults, { { {   0, 'if !' }, {   0,   3 }, {   0, ' ; ordListClear() ; end ' }, {   1, ' ; ordListAdd( ' }, {   1,   1 }, {   1, ' )' }, {   2, ' ; ordListAdd( ' }, {   2,   2 }, {   2, ' )' } }, { -1,  6, -1, -1,  4, -1, -1,  4, -1} , { NIL, NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'ordSetFocus( ' }, {   0,   1 }, {   2, ' , ' }, {   2,   2 }, {   0, ' )' } }, { -1,  1, -1,  4, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'ordSetFocus( ' }, {   0,   1 }, {   2, ' , ' }, {   2,   2 }, {   0, ' )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL}  } )
aAdd( aCommResults, { { {   0, 'ordSetFocus(0)' } }, { -1} , { }  } )

RETURN .T.
