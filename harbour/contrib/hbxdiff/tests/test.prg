/*
 * $Id$
 */

#include "hbxdiff.ch"
#include "simpleio.ch"

#define _SIZE 62

PROCEDURE main()
   LOCAL pMMF
   LOCAL cFileCtx
   LOCAL nSize

   pMMF  := xdl_init_mmfile( XDLT_STD_BLKSIZE, XDL_MMF_ATOMIC )
   ? xdl_mmfile_size( pMMF )
   ? xdl_mmfile_iscompact( pMMF )

   cFileCtx := hb_memoRead( __FILE__ )

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
