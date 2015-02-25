/* This is an original work by Gary Baren and is placed in the public domain.

   Adapted to german spelling, 2015    ELCH

      Rev 1.1   15 Aug 1991 23:05:54   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.0   09 Jun 1991 00:26:56   GLENN
   Initial revision.
 */

FUNCTION NumToTxtDE( nAmount )

   STATIC sc_qualifiers := { "", " Tausend", " Million", " Milliarde", " Billion", " Billiarde", " Trillion", " Trilliarde", "Quadrillion" }
   STATIC sc_qualifiers_ext := { "", "", "en", "n", "en", "n", "en", "n", "en" }

   LOCAL nTemp, cResult, nTmp
   LOCAL nDiv := 10 ^ ( Int( sol10( nAmount := Max( 0, nAmount ) ) / 3 ) * 3 )

   nTemp   := Int( nAmount % nDiv )
   nAmount := Int( nAmount / nDiv )
   nTmp    := Int( sol10( nDiv ) / 3 ) + 1
   cResult := grp_to_words_de( nAmount ) + ;
      sc_qualifiers[ nTmp ] +;
      iif( nAmount > 1, sc_qualifiers_ext[ nTmp ], "" )

   IF nTemp > ( nDiv /= 1000 ) .AND. nDiv > 1
      cResult += " " + NumToTxtDE( nTemp )
   ELSE
      cResult += " " + grp_to_words_de( nTemp )
   ENDIF

   RETURN AllTrim( cResult )

STATIC FUNCTION grp_to_words_de( nGrp )

   STATIC sc_ones := { "Ein", "Zwei", "Drei", "Vier", "Fünf", "Sechs", "Sieben", "Acht", "Neun" }
   STATIC sc_teens := { " Zehn", " Elf", " Zwölf", ;
      " Dreizehn", " Vierzehn", " Fünfzehn", " Sechszehn", " Siebzehn", " Achtzehn", " Neunzehn" }
   STATIC sc_tens :=  { "", "", ;
      " Zwannzig", " Dreizig", " Vierzig", " Fünfzig", " Sechzig", " Siebzig", " Achtzig", " Neunzig" }

   LOCAL cResult, nTemp, nTmp, lEmpty

   nTemp   := Int( nGrp % 100 )
   nGrp    := Int( nGrp / 100 )
   cResult := iif( nGrp > 0, sc_ones[ nGrp ] + "hundert", "" )
   lEmpty  := Empty( cResult )

   IF lEmpty
      cResult += " "
   ENDIF

   DO CASE
   CASE nTemp > 19
      cResult += ;
         iif( ( nTmp := Int( nTemp % 10 ) ) > 0, iif( Empty( cResult ), sc_ones[ nTmp ], Lower( sc_ones[ nTmp ] ) ) + "und", "" ) +;
         iif( nTmp > 0 .OR. ! lEmpty, LTrim( Lower( sc_tens[ Int( nTemp / 10 ) + 1 ] ) ), sc_tens[ Int( nTemp / 10 ) + 1 ] )
   CASE nTemp < 20 .AND. nTemp > 9
      cResult += iif( Empty( cResult ), sc_teens[ Int( nTemp % 10 ) + 1 ], Lower( LTrim( sc_teens[ Int( nTemp % 10 ) + 1 ] ) ) )
   CASE nTemp < 10 .AND. nTemp > 0
      cResult += iif( Empty( cResult ), " " + sc_ones[ nTemp ], Lower( sc_ones[ nTemp ] ) )
   ENDCASE

   RETURN hb_UTF8ToStr( cResult )

STATIC FUNCTION sol10( nNumber )

   LOCAL nLen := 1

   DO WHILE nNumber > 9
      nNumber := Int( nNumber / 10 )
      ++nLen
   ENDDO

   RETURN nLen - 1
