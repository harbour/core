#require "hbxdiff"

PROCEDURE Main()

   LOCAL pMMFOld := xdl_init_mmfile( XDLT_STD_BLKSIZE )
   LOCAL pMMFNew := xdl_init_mmfile( XDLT_STD_BLKSIZE, XDL_MMF_ATOMIC )

   LOCAL cFileCtx := hb_MemoRead( __FILE__ )

   xdl_write_mmfile( pMMFOld, @cFileCtx )
   xdl_write_mmfile( pMMFNew, cFileCtx + hb_eol() + Space( 3 ) + "RETURN NIL" + hb_eol() )

   xdl_diff( pMMFOld, pMMFNew, 0, 3, {| ... | Diff( ... ) } )
   xdl_diff( pMMFOld, pMMFNew, 0, 3, @Diff() )

   RETURN

STATIC FUNCTION Diff( ... )

   LOCAL e

   FOR EACH e IN { ... }
      ? e
   NEXT

   RETURN 0
