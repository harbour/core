/*
 * $Id$
 */

#require "gtalleg"

PROCEDURE Main()

#if defined( __HBSCRIPT__HBSHELL )
   hbshell_gtSelect( "GTALLEG" )
#endif

   CLS
   Alert( "Hello world!" )

   RETURN
