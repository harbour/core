/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main()

   // open a text file here
   IF ft_FUse( __FILE__, 0 ) > 1

      DO WHILE ! ft_FEof()
         ? "line " + Str( ft_FRecNo(), 2 ) + " " + ft_FReadLn()
         ft_FSkip( 1 )
      ENDDO
      ?
      my_goto( 18 )
      my_goto( 2 )

      ft_FGoBot()
      ?
      ? "after ft_fgobot() now in line # " + hb_ntos( ft_FRecNo() )

      ft_FGoTop()
      ?
      ? "after ft_fgotop() now in line # " + hb_ntos( ft_FRecNo() )

      ?
      ? "ft_flastre() = " + hb_ntos( ft_FLastRe() )

      // close the file
      ft_FUse()
   ENDIF

   RETURN

STATIC PROCEDURE my_goto( n_go )

   ft_FGoto( n_go )
   ?
   ? "after ft_fgoto(" + hb_ntos( n_go ) + ")"
   ? "line " + hb_ntos( ft_FRecNo() ) + " is " + LTrim( ft_FReadLn() )

   RETURN
