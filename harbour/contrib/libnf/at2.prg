/*
 * File......: AT2.prg
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


/*  $DOC$
 *  $FUNCNAME$
 *     FT_AT2()
 *  $CATEGORY$
 *     String
 *  $ONELINER$
 *     Find position of the nth occurrence of a substring
 *  $SYNTAX$
 *     FT_AT2( <cSearch>, <cTarget> [, <nOccurs> [, <lCaseSens> ] ] ) -> nPos
 *  $ARGUMENTS$
 *     <cSearch> is the character substring to search for.
 *
 *     <cTarget> is the character string to search.
 *
 *     <nOccurs> is the occurrence of cSearch to look for,
 *                defaults to 1.
 *
 *     <lCaseSens> is a logical value denoting case sensitivity.
 *                If .F., then search is NOT sensitive to case,
 *                defaults to .T.
 *  $RETURNS$
 *     The position of the nth occurrence of a substring
 *  $DESCRIPTION$
 *     This function will find the nth occurrence of a substring
 *     within a string.
 *  $EXAMPLES$
 *     cSearch := "t"
 *     cTarget := "This is the day that the Lord has made."
 *
 *     FT_AT2( cSearch, cTarget )            // Returns ( 9 )
 *
 *     FT_AT2( cSearch, cTarget, 2 )         // Returns ( 17 )
 *
 *     FT_AT2( cSearch, cTarget, 2, .F. )    // Returns ( 9 )
 *  $SEEALSO$
 *    FT_FINDITH()
 *  $END$
 */

#ifdef FT_TEST

FUNCTION MAIN()
   LOCAL cSearch,cTarget,var0
   CLS
   ? "TEST TO DEMONSTRATE EXAMPLES OF FT_AT2"
   ?
   cSearch := 't'
   ? "Find occurrences of 't' in: "
   cTarget := "This is the day that the Lord has made."
   ?? cTarget
   ?
   var0 := ft_at2( cSearch, cTarget )
   ? PADR("FT_AT2( cSearch, cTarget ) -> ",40)
   ?? var0
   ?
   var0 := ft_at2( cSearch, cTarget, 2 )
   ? PADR("FT_AT2( cSearch, cTarget, 2 ) -> ",40)
   ??var0
   ?
   var0 := ft_at2( cSearch, cTarget, 2, .F. )
   ? PADR("FT_AT2( cSearch, cTarget, 2, .F. ) -> ",40)
   ??var0
   ?
   RETURN NIL

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
         nPos := AT( cSearch, cSubstr )

      ELSE
         nPos := AT( UPPER( cSearch ), UPPER( cSubstr ) )

      ENDIF

      // Store position of cSearch relative to original string.
      nPos2 += nPos

      // Resize cSubstr
      cSubstr := SUBSTR( cSubstr, AT( cSearch, cSubstr ) +1 )

      // Breakout if there are no occurences here

      IF nPos == 0
           EXIT
      ENDIF


   NEXT

   RETURN ( nPos2 )

