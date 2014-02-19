#require "hbxdiff"

#define _SIZE 62

PROCEDURE Main()

   LOCAL pMMF := xdl_init_mmfile( XDLT_STD_BLKSIZE, XDL_MMF_ATOMIC )
   LOCAL cFileCtx := hb_MemoRead( __FILE__ )
   LOCAL nSize

   ? xdl_mmfile_size( pMMF )
   ? xdl_mmfile_iscompact( pMMF )

   ? xdl_write_mmfile( pMMF, cFileCtx ), Len( cFileCtx )
   ? xdl_mmfile_size( pMMF )

   ? xdl_read_mmfile( pMMF, NIL, _SIZE, @nSize )
   ? nSize

   xdl_seek_mmfile( pMMF, 0 )

   ? xdl_read_mmfile( pMMF, NIL, _SIZE, @nSize )
   ? nSize

   ? xdl_read_mmfile( pMMF, NIL, _SIZE, @nSize )
   ? nSize

   xdl_seek_mmfile( pMMF, _SIZE )

   ? xdl_read_mmfile( pMMF, NIL, _SIZE, @nSize )
   ? nSize

   RETURN
