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
tested with Borland 32bit only
*/

function test_nanforlib_clone()
// open a text file here
local handle
if ( handle := hb_fuse( "testhbf.prg", 0 ) ) > 1
   while !hb_feof()
      qout( "line " + str(hb_frecno(),2) + " " + hb_freadln() )
      hb_fskip(1)
   end
   qout("")
   my_goto(18)
   my_goto(2)

   hb_fgobottom()
   qout("")
   qout( "after hb_fgobottom() now in line # " + ltrim(str(hb_frecno())) )

   hb_fgotop()
   qout("")
   qout( "after hb_fgotop() now in line # " + ltrim(str(hb_frecno())) )

   qout("")
   qout( "hb_flastrec() = " + ltrim(str(hb_flastrec())) )

   // close the file
   hb_fuse()

end
return nil

static function my_goto( n_go )

hb_fgoto( n_go )
qout("")
qout( "after hb_fgoto("+ltrim(str(n_go))+")" )
qout( "line "+ ltrim(str(hb_frecno())) + " is " + ltrim( hb_freadln() ) )

return nil
