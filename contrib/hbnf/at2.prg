/* This is an original work by Ralph Oliver (TRANSCOM SYSTEMS)
   and is placed in the public domain.

      Rev 1.2   15 Aug 1991 23:05:58   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   12 Jun 1991 00:46:28   GLENN
   Posted a referee suggestion: Around line 118, break out if no
   occurrences (note the IF/ENDIF before the NEXT and RETURN at the
   bottom of the function).

      Rev 1.0   07 Jun 1991 23:03:28   GLENN
   Initial revision.
 */

FUNCTION ft_At2( cSearch, cTarget, nOccurs, lCaseSens )

   LOCAL nCount, nPos, nPos2 := 0
   LOCAL cSubstr := cTarget

   // Set default parameters as necessary
   __defaultNIL( @lCaseSens, .T. )
   __defaultNIL( @nOccurs, 1 )

   FOR nCount := 1 TO nOccurs

      // Store position of next occurrence of cSearch
      IF lCaseSens
         nPos := At( cSearch, cSubstr )
      ELSE
         nPos := At( Upper( cSearch ), Upper( cSubstr ) )
      ENDIF

      // Store position of cSearch relative to original string
      nPos2 += nPos

      // Resize cSubstr
      cSubstr := SubStr( cSubstr, At( cSearch, cSubstr ) + 1 )

      // Breakout if there are no occurences here
      IF nPos == 0
         EXIT
      ENDIF
   NEXT

   RETURN nPos2

FUNCTION ft_RAt2( cSearch, cTarget, nOccurs, lCaseSens )

   LOCAL nCount, nPos, nPos2 := 0
   LOCAL cSubstr := cTarget

   // Set default parameters as necessary
   __defaultNIL( @lCaseSens, .T. )
   __defaultNIL( @nOccurs, 1 )

   FOR nCount := 1 TO nOccurs

      // Store position of next occurrence of cSearch
      IF lCaseSens
         nPos := RAt( cSearch, cSubstr )
      ELSE
         nPos := RAt( Upper( cSearch ), Upper( cSubstr ) )
      ENDIF

      // Store position of cSearch relative to original string
      nPos2 := nPos

      // Resize cSubstr
      cSubstr := Left( cSubstr, RAt( cSearch, cSubstr ) - 1 )

      // Breakout if there are no occurences here
      IF nPos == 0
         EXIT
      ENDIF
   NEXT

   RETURN nPos2
