#require "hbmisc"

#include "fileio.ch"

PROCEDURE Main( cFile )

   // open a text file here
   IF hb_FUse( hb_defaultValue( cFile, __FILE__ ), FO_READ ) != F_ERROR

      DO WHILE ! hb_FAtEof()
         ? "line", Str( hb_FRecNo(), 2 ), hb_FReadLn()
         hb_FSkip( 1 )
      ENDDO
      ?
      my_goto( 18 )
      my_goto( 2 )

      hb_FGoBottom()
      ?
      ? "after hb_FGoBottom() now in line #", hb_ntos( hb_FRecNo() )

      hb_FGoTop()
      ?
      ? "after hb_FGoTop() now in line #", hb_ntos( hb_FRecNo() )

      ?
      ? "hb_FLastRec() =", hb_ntos( hb_FLastRec() )

      // close the file
      hb_FUse()
   ENDIF

   RETURN

STATIC PROCEDURE my_goto( n_go )

   hb_FGoto( n_go )
   ?
   ? "after hb_FGoto(", hb_ntos( n_go ), ")"
   ? "line", hb_ntos( hb_FRecNo() ), "is", LTrim( hb_FReadLn() )

   RETURN
