//
// $Id$
//

/*
__copyfile( csource, cdestination )
initial release : 23 June 1999 Andi Jahja
thanks for hbpp :)
*/

#command copy file <(src)> to <(dest)>  => __copyfile( <(src)>, <(dest)> )

function main()

copy file testcopy.prg to newcopy.prg

return nil
