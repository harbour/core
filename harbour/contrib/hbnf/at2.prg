/*
 * $Id$
 */

/*
 * Author....: Ralph Oliver,  TRANSCOM SYSTEMS
 * CIS ID....: 74030,703
 *
 * This is an original work by Ralph Oliver and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:05:58   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   12 Jun 1991 00:46:28   GLENN
 * Posted a referee suggestion: Around line 118, break out if no
 * occurrences (note the IF/ENDIF before the NEXT and RETURN at the
 * bottom of the function).
 *
 *    Rev 1.0   07 Jun 1991 23:03:28   GLENN
 * Initial revision.
 *
 *
 */

#ifdef FT_TEST

PROCEDURE Main()

   LOCAL cSearch, cTarget, var0

   CLS
   ? "TEST TO DEMONSTRATE EXAMPLES OF FT_AT2"
   ?
   cSearch := "t"
   ? "Find occurrences of 't' in: "
   cTarget := "This is the day that the Lord has made."
   ?? cTarget
   ?
   var0 := ft_at2( cSearch, cTarget )
   ? PadR( "FT_AT2( cSearch, cTarget ) -> ", 40 )
   ?? var0
   ?
   var0 := ft_at2( cSearch, cTarget, 2 )
   ? PadR( "FT_AT2( cSearch, cTarget, 2 ) -> ", 40 )
   ??var0
   ?
   var0 := ft_at2( cSearch, cTarget, 2, .F. )
   ? PadR( "FT_AT2( cSearch, cTarget, 2, .F. ) -> ", 40 )
   ??var0
   ?

   RETURN

#endif

FUNCTION FT_AT2( cSearch, cTarget, nOccurs, lCaseSens )

   LOCAL nCount, nPos, nPos2 := 0
   LOCAL cSubstr := cTarget

// Set default parameters as necessary.
   IF lCaseSens == NIL
      lCaseSens := .T.
   ENDIF

   IF nOccurs == NIL
      nOccurs := 1
   ENDIF

   FOR nCount := 1 TO nOccurs

      // Store position of next occurrence of cSearch.
      IF lCaseSens
         nPos := At( cSearch, cSubstr )

      ELSE
         nPos := At( Upper( cSearch ), Upper( cSubstr ) )

      ENDIF

      // Store position of cSearch relative to original string.
      nPos2 += nPos

      // Resize cSubstr
      cSubstr := SubStr( cSubstr, At( cSearch, cSubstr ) + 1 )

      // Breakout if there are no occurences here

      IF nPos == 0
         EXIT
      ENDIF

   NEXT

   RETURN nPos2

FUNCTION FT_RAT2( cSearch, cTarget, nOccurs, lCaseSens )

   LOCAL nCount, nPos, nPos2 := 0
   LOCAL cSubstr := cTarget

// Set default parameters as necessary.
   IF lCaseSens == NIL
      lCaseSens := .T.
   ENDIF
   IF nOccurs == NIL
      nOccurs := 1
   ENDIF
   FOR nCount := 1 TO nOccurs
      // Store position of next occurrence of cSearch.
      IF lCaseSens
         nPos := RAt( cSearch, cSubstr )
      ELSE
         nPos := RAt( Upper( cSearch ), Upper( cSubstr ) )
      ENDIF
      // Store position of cSearch relative to original string.
      nPos2 := nPos
      // Resize cSubstr
      cSubstr := SubStr( cSubstr, 1, RAt( cSearch, cSubstr ) - 1 )
      // Breakout if there are no occurences here
      IF nPos == 0
         EXIT
      ENDIF
   NEXT

   RETURN nPos2
