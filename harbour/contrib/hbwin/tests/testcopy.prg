/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.hu)
 * www - http://harbour-project.org
 *
 */

#include "hbwin.ch"

#include "simpleio.ch"

PROCEDURE Main()
   LOCAL a
   LOCAL lAbort
   LOCAL tmp

   ? "0x" + hb_numtohex( WIN_SHFileOperation( NIL, WIN_FO_COPY, { "testcopy.prg", "olesrv1.prg" }, { "testcopy1" },;
                                              NIL, @lAbort,;
                                              NIL, "Harbour SHFile 1" ) )
   ? lAbort

   hb_MemoWrit( "rename.txt", "hello1" )
   hb_MemoWrit( "rename1.txt", "hello2" )

   a := {}
   ? "0x" + hb_numtohex( WIN_SHFileOperation( NIL, WIN_FO_RENAME, { "rename.txt" }, { "rename1.txt" },;
                                              WIN_FOF_WANTMAPPINGHANDLE, @lAbort,;
                                              a, "Harbour SHFile 2" ) )
   ? lAbort
   FOR EACH tmp IN a
      ? tmp[ 1 ], tmp[ 2 ]
      FErase( tmp[ 1 ] )
      FErase( tmp[ 2 ] )
   NEXT

   FErase( "rename1.txt" )

   ? "0x" + hb_numtohex( WIN_SHFileOperation( NIL, WIN_FO_COPY, "testcopy.prg" + Chr( 0 ) + "olesrv1.prg" + Chr( 0 ), "testcopy2",;
                                              NIL, @lAbort,;
                                              NIL, "Harbour SHFile 3" ) )
   ? lAbort

   RETURN
