/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://www.harbour-project.org
 *
 */

#include "hbwin.ch"

PROCEDURE Main()
   LOCAL a := {}
   LOCAL lAbort

   ? "0x" + hb_numtohex( WIN_SHFileOperation( NIL, WIN_FO_COPY, { "testcopy.prg", "olesrv1.prg" }, "testcopy1",;
                                              WIN_FOF_WANTMAPPINGHANDLE, @lAbort,;
                                              a, "Harbour SHFile 1" ) )
   ? lAbort

   ? "0x" + hb_numtohex( WIN_SHFileOperation( NIL, WIN_FO_COPY, "testcopy.prg" + Chr( 0 ) + "olesrv1.prg" + Chr( 0 ), "testcopy2",;
                                              WIN_FOF_WANTMAPPINGHANDLE, @lAbort,;
                                              a, "Harbour SHFile 2" ) )
   ? lAbort

   RETURN
