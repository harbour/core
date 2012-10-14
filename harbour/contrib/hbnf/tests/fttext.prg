/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main()

   // open a text file here
   IF ft_fuse( __FILE__, 0 ) > 1

      DO WHILE ! ft_fEof()
         ? "line " + Str( ft_frecno(), 2 ) + " " + ft_freadln()
         ft_fskip( 1 )
      ENDDO
      ?
      my_goto( 18 )
      my_goto( 2 )

      ft_fgobot()
      ?
      ? "after ft_fgobot() now in line # " + hb_ntos( ft_frecno() )

      ft_fgotop()
      ?
      ? "after ft_fgotop() now in line # " + hb_ntos( ft_frecno() )

      ?
      ? "ft_flastre() = " + hb_ntos( ft_flastre() )

      // close the file
      ft_fuse()
   ENDIF

   RETURN

STATIC PROCEDURE my_goto( n_go )

   ft_fgoto( n_go )
   ?
   ? "after ft_fgoto(" + hb_ntos( n_go ) + ")"
   ? "line " + hb_ntos( ft_frecno() ) + " is " + LTrim( ft_freadln() )

   RETURN
