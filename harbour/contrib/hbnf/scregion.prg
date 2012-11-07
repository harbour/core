/*
 * $Id$
 */

/*
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

FUNCTION ft_SavRgn( nTop, nLeft, nBottom, nRight )

   RETURN hb_BChar( nTop ) + hb_BChar( nLeft ) + hb_BChar( nBottom ) + hb_BChar( nRight ) + ;
      SaveScreen( nTop, nLeft, nBottom, nRight )

FUNCTION ft_RstRgn( cScreen, nTop, nLeft )

   IF PCount() == 3
      RestScreen( nTop, nLeft, ;
         ( nTop  - hb_BCode( hb_BSubStr( cScreen, 1, 1 ) ) ) + hb_BCode( hb_BSubStr( cScreen, 3, 1 ) ), ;
         ( nLeft - hb_BCode( hb_BSubStr( cScreen, 2, 1 ) ) ) + hb_BCode( hb_BSubStr( cScreen, 4, 1 ) ), ;
         hb_BSubStr( cScreen, 5 ) )
   ELSE
      RestScreen( ;
         hb_BCode( hb_BSubStr( cScreen, 1, 1 ) ), ;
         hb_BCode( hb_BSubStr( cScreen, 2, 1 ) ), ;
         hb_BCode( hb_BSubStr( cScreen, 3, 1 ) ), ;
         hb_BCode( hb_BSubStr( cScreen, 4, 1 ) ), ;
         hb_BSubStr( cScreen, 5 ) )
   ENDIF

   RETURN NIL

/* NOTE: original NF accepted "pop all" if it contained
         extra character and _SET_EXACT was set to .F.
         Harbour version accepts "pop all" only. [vszakats] */

FUNCTION ft_RgnStack( cAction, nTop, nLeft, nBottom, nRight )

   THREAD STATIC t_nStackPtr := 0

   LOCAL nPopTop

   IF cAction == "push"

      ASize( t_aRgnStack, ++t_nStackPtr )[ t_nStackPtr ] := ;
         ft_SavRgn( nTop, nLeft, nBottom, nRight )

   ELSEIF cAction == "pop" .OR. cAction == "pop all"

      nPopTop := iif( "all" $ cAction, 0, t_nStackPtr - 1 )

      DO WHILE t_nStackPtr > nPopTop
         ft_RstRgn( t_aRgnStack[ t_nStackPtr-- ] )
      ENDDO

      ASize( t_aRgnStack, t_nStackPtr )

   ENDIF

   RETURN NIL
