/*
 * $Id$
 */

PROCEDURE Main()

   ? CMonth( Date() )
   ? CMonth( Date() + 31 )
   ? CMonth( Date() + 60 )

   ? CDow( Date() )
   ? CDow( Date() + 6 )
   ? CDow( Date() + 7 )

   RETURN
