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

PROCEDURE Main()

   // open a text file here
   IF hb_fuse( "testhbf.prg", 0 ) > 1

      DO WHILE ! hb_FEof()
         ? "line " + Str( hb_frecno(), 2 ) + " " + hb_freadln()
         hb_fskip( 1 )
      ENDDO
      ?
      my_goto( 18 )
      my_goto( 2 )

      hb_fgobottom()
      ?
      ? "after hb_fgobottom() now in line # " + hb_ntos( hb_frecno() )

      hb_fgotop()
      ?
      ? "after hb_fgotop() now in line # " + hb_ntos( hb_frecno() )

      ?
      ? "hb_flastrec() = " + hb_ntos( hb_flastrec() )

      // close the file
      hb_fuse()
   ENDIF

   RETURN

STATIC PROCEDURE my_goto( n_go )

   hb_fgoto( n_go )
   ?
   ? "after hb_fgoto(" + hb_ntos( n_go ) + ")"
   ? "line " + hb_ntos( hb_frecno() ) + " is " + LTrim( hb_freadln() )

   RETURN
