/*
 * $Id$
 */

/*
test program for hb_f*()
harbour clones for nanfor's ft_f*()
inplementation of :
  * hb_fuse()
  * hb_fskip()
  * hb_feof()
  * hb_frecno()
  * hb_freadln()
  * hb_flastrec()
  * hb_fgoto()
  * hb_fgotop()
  * hb_fgobottom()
*/

#require "hbmisc"

PROCEDURE Main()

   // open a text file here
   IF hb_FUse( __FILE__, 0 ) > 1

      DO WHILE ! hb_FEof()
         ? "line " + Str( hb_FRecNo(), 2 ) + " " + hb_FReadLn()
         hb_FSkip( 1 )
      ENDDO
      ?
      my_goto( 18 )
      my_goto( 2 )

      hb_FGoBottom()
      ?
      ? "after hb_fgobottom() now in line # " + hb_ntos( hb_FRecNo() )

      hb_FGoTop()
      ?
      ? "after hb_fgotop() now in line # " + hb_ntos( hb_FRecNo() )

      ?
      ? "hb_flastrec() = " + hb_ntos( hb_FLastRec() )

      // close the file
      hb_FUse()
   ENDIF

   RETURN

STATIC PROCEDURE my_goto( n_go )

   hb_FGoto( n_go )
   ?
   ? "after hb_fgoto(" + hb_ntos( n_go ) + ")"
   ? "line " + hb_ntos( hb_FRecNo() ) + " is " + LTrim( hb_FReadLn() )

   RETURN
