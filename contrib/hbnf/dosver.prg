/* This is an original work from 2014 by Viktor Szakats (vszakats.net/harbour)
   and is placed in the public domain. */

FUNCTION ft_DosVer()

   LOCAL cOS := OS()

   IF hb_LeftEq( cOS, "DOS " )
      cOS := SubStr( cOS, Len( "DOS " ) + 1 )
      IF " (" $ cOS
         cOS := Left( cOS, At( " (", cOS ) - 1 )
      ENDIF
      RETURN cOS
   ENDIF

   RETURN ""
