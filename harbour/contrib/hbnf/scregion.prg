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

THREAD STATIC t_aRgnStack := {}

FUNCTION FT_SAVRGN( nTop, nLeft, nBottom, nRight )

   RETURN Chr( nTop ) + Chr( nLeft ) + Chr( nBottom ) + Chr( nRight ) + ;
      SaveScreen( nTop, nLeft, nBottom, nRight )

FUNCTION FT_RSTRGN( cScreen, nTop, nLeft )

   IF PCount() == 3
      RestScreen( nTop, nLeft, ( nTop - Asc(cScreen ) ) + Asc( SubStr( cScreen, 3 ) ), ;
         ( nLeft - Asc( SubStr( cScreen, 2 ) ) ) + Asc( SubStr( cScreen, 4 ) ), ;
         SubStr( cScreen, 5 ) )
   ELSE
      RestScreen( Asc( cScreen ), Asc( SubStr( cScreen, 2 ) ), Asc( SubStr( cScreen, 3 ) ), ;
         Asc( SubStr( cScreen, 4 ) ), SubStr( cScreen, 5 ) )
   ENDIF

   RETURN NIL

/* NOTE: original NF accepted "pop all" if it contained
         extra character and _SET_EXACT was set to .F.
         Harbour version accepts "pop all" only. [vszakats] */

FUNCTION FT_RGNSTACK( cAction, nTop, nLeft, nBottom, nRight )

   THREAD STATIC t_nStackPtr := 0
   LOCAL nPopTop

   IF cAction == "push"

      ASize( t_aRgnStack, ++t_nStackPtr )[ t_nStackPtr ] := ;
         FT_SAVRGN( nTop, nLeft, nBottom, nRight )

   ELSEIF cAction == "pop" .OR. cAction == "pop all"

      nPopTop := iif( "all" $ cAction, 0, t_nStackPtr - 1 )

      DO WHILE t_nStackPtr > nPopTop
         FT_RSTRGN( t_aRgnStack[ t_nStackPtr-- ] )
      ENDDO

      ASize( t_aRgnStack, t_nStackPtr )

   ENDIF

   RETURN NIL
