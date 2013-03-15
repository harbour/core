/*
 * $Id$
 */

// ; Donated to the public domain by
//   Viktor Szakats (harbour syenar.net)

REQUEST HB_LANG_HU
REQUEST HB_LANG_KO

PROCEDURE Main()

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
