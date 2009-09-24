/*
 * $Id$
 */

/*
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
 * See COPYING for licensing terms.
 */

PROCEDURE Main()
   LOCAL nErrorLevel := 0

   OutStd( "! Hello from postinst.prg" + hb_osNewLine() )

   ErrorLevel( nErrorLevel )

   RETURN 
