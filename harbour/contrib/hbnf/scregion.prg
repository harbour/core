/*
 * $Id$
 */

/*
 * File......: scregion.prg
 * Author....: David A. Richardson
 * CIS ID....: ?
 *
 * This is an original work by David A. Richardson and is hereby placed
 * in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:05:46   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:56   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:14   GLENN
 * Nanforum Toolkit
 *
 */

STATIC aRgnStack := {}

FUNCTION FT_SAVRGN(nTop, nLeft, nBottom, nRight)

   RETURN CHR(nTop) + CHR(nLeft) + CHR(nBottom) + CHR(nRight) + ;
      SAVESCREEN(nTop, nLeft, nBottom, nRight)

FUNCTION FT_RSTRGN(cScreen, nTop, nLeft)

   IF PCOUNT() == 3
      RESTSCREEN(nTop, nLeft, (nTop - ASC(cScreen)) + ASC(SUBSTR(cScreen, 3)), ;
         (nLeft - ASC(SUBSTR(cScreen, 2))) + ASC(SUBSTR(cScreen, 4)), ;
         SUBSTR(cScreen, 5))
   ELSE
      RESTSCREEN(ASC(cScreen), ASC(SUBSTR(cScreen, 2)), ASC(SUBSTR(cScreen, 3)), ;
         ASC(SUBSTR(cScreen, 4)), SUBSTR(cScreen, 5))
   ENDIF

   RETURN NIL

/* NOTE: original NF accepted "pop all" if it contained
         extra character and _SET_EXACT was set to .F.
         Harbour version accepts "pop all" only. [vszakats] */

FUNCTION FT_RGNSTACK(cAction, nTop, nLeft, nBottom, nRight)

   STATIC nStackPtr := 0
   LOCAL nPopTop

   IF cAction == "push"

      ASIZE(aRgnStack, ++nStackPtr)[nStackPtr] := ;
         FT_SAVRGN(nTop, nLeft, nBottom, nRight)

   ELSEIF cAction == "pop" .OR. cAction == "pop all"

      nPopTop := IIF("all" $ cAction, 0, nStackPtr-1)

      DO WHILE nStackPtr > nPopTop
         FT_RSTRGN(aRgnStack[nStackPtr--])
      ENDDO

      ASIZE(aRgnStack, nStackPtr)

   ENDIF

   RETURN NIL
