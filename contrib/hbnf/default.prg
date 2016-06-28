/* This is an original work from 2012 by Viktor Szakats (vszakats.net/harbour)
   and is placed in the public domain. */

FUNCTION ft_Default( cDrive )

   BEGIN SEQUENCE WITH {|| Break() }
      hb_CurDrive( cDrive )
   END SEQUENCE

   RETURN hb_CurDrive()
