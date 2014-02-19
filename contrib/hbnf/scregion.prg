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

PROCEDURE ft_RstRgn( cScreen, nTop, nLeft )

   IF PCount() == 3
      RestScreen( nTop, nLeft, ;
         ( nTop  - hb_BPeek( cScreen, 1 ) ) + hb_BPeek( cScreen, 3 ), ;
         ( nLeft - hb_BPeek( cScreen, 2 ) ) + hb_BPeek( cScreen, 4 ), ;
         hb_BSubStr( cScreen, 5 ) )
   ELSE
      RestScreen( ;
         hb_BPeek( cScreen, 1 ), ;
         hb_BPeek( cScreen, 2 ), ;
         hb_BPeek( cScreen, 3 ), ;
         hb_BPeek( cScreen, 4 ), ;
         hb_BSubStr( cScreen, 5 ) )
   ENDIF

   RETURN

/* NOTE: original NF accepted "pop all" if it contained
         extra character and _SET_EXACT was set to .F.
         Harbour version accepts "pop all" only. [vszakats] */

PROCEDURE ft_RgnStack( cAction, nTop, nLeft, nBottom, nRight )

   THREAD STATIC t_nStackPtr := 0

   LOCAL nPopTop

   SWITCH cAction
   CASE "push"

      ASize( t_aRgnStack, ++t_nStackPtr )[ t_nStackPtr ] := ;
         ft_SavRgn( nTop, nLeft, nBottom, nRight )
      EXIT

   CASE "pop"
   CASE "pop all"

      nPopTop := iif( "all" $ cAction, 0, t_nStackPtr - 1 )

      DO WHILE t_nStackPtr > nPopTop
         ft_RstRgn( t_aRgnStack[ t_nStackPtr-- ] )
      ENDDO

      ASize( t_aRgnStack, t_nStackPtr )
      EXIT

   ENDSWITCH

   RETURN
