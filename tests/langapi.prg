/* Donated to the public domain by Viktor Szakats (vszakats.net/harbour) */

REQUEST HB_LANG_HU
REQUEST HB_LANG_KO

PROCEDURE Main()

   hb_langSelect( "en" )

   ? "Prev:", hb_langSelect()
   ? hb_langName()
   ? NationMsg( 1 )
   ? CMonth( Date() )
   ? CDoW( Date() )
   ? "---------"

   ? "Prev:", hb_langSelect( "hu" )
   ? hb_langName()
   ? NationMsg( 1 )
   ? CMonth( Date() )
   ? CDoW( Date() )
   ? "---------"

   ? "Prev:", hb_langSelect( "ko" )
   ? hb_langName()
   ? NationMsg( 1 )
   ? CMonth( Date() )
   ? CDoW( Date() )
   ? "---------"

   ? "Prev:", hb_langSelect( "NOTHERE" )
   ? hb_langName()
   ? NationMsg( 1 )
   ? CMonth( Date() )
   ? CDoW( Date() )
   ? "---------"

   RETURN
