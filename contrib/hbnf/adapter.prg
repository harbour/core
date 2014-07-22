/* This is an original work from 2012 by Viktor Szakats (vszakats.net/harbour)
   and is placed in the public domain. */

FUNCTION ft_Adapter()
   RETURN iif( IsColor(), 3 /* VGA */, 0 /* monochrome */ )
