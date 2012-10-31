/*
 * $Id$
 */

#require "hbxdiff"

#include "hbxdiff.ch"

#include "simpleio.ch"

FUNCTION Diff( ... )

   LOCAL e

   FOR EACH e IN { ... }
      ? e
   NEXT

   RETURN 0

PROCEDURE Main()

   LOCAL pMMFOld, pMMFNew
   LOCAL cFileCtx

   pMMFOld := xdl_init_mmfile( XDLT_STD_BLKSIZE )
   pMMFNew := xdl_init_mmfile( XDLT_STD_BLKSIZE, XDL_MMF_ATOMIC )

   cFileCtx := hb_MemoRead( __FILE__ )

   xdl_write_mmfile( pMMFOld, @cFileCtx )
   xdl_write_mmfile( pMMFNew, cFileCtx + hb_eol() + Space( 3 ) + "RETURN NIL" + hb_eol() )

   xdl_diff( pMMFOld, pMMFNew, 0, 3, {| ... | Diff( ... ) } )
   xdl_diff( pMMFOld, pMMFNew, 0, 3, @Diff() )

   RETURN
