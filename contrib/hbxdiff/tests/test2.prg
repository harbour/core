#require "hbxdiff"

#include "fileio.ch"

PROCEDURE Main()

   LOCAL pMMFOld := xdl_init_mmfile( XDLT_STD_BLKSIZE )
   LOCAL pMMFNew := xdl_init_mmfile( XDLT_STD_BLKSIZE, XDL_MMF_ATOMIC )

   LOCAL cFileCtx := hb_MemoRead( __FILE__ )

   LOCAL pDiff
   LOCAL hDif, hNew, hErr, hOld
   LOCAL cDiffName

   xdl_write_mmfile( pMMFOld, @cFileCtx )
   xdl_write_mmfile( pMMFNew, cFileCtx + hb_eol() + Space( 3 ) + "RETURN NIL" + hb_eol() )

   ? xdl_mmfile_cmp( pMMFOld, pMMFNew )

   cDiffName := hb_FNameName( __FILE__ ) + ".dif"

   IF ( hDif := FCreate( cDiffName ) ) != F_ERROR
      FWrite( hDif, "diff ---" + hb_eol() )
      xdl_diff( pMMFOld, pMMFNew, 0, 3, hDif )
      FClose( hDif )
   ELSE
      ? "Error"
   ENDIF

   pDiff := xdl_init_mmfile( XDLT_STD_BLKSIZE, XDL_MMF_ATOMIC )
   cFileCtx := hb_MemoRead( cDiffName )
   xdl_write_mmfile( pDiff, cFileCtx )

   hNew := FCreate( hb_FNameExtSet( cDiffName, ".new" ) )
   hErr := FCreate( hb_FNameExtSet( cDiffName, ".err" ) )
   hOld := FCreate( hb_FNameExtSet( cDiffName, ".old" ) )
   IF hNew != F_ERROR .AND. ;
      hErr != F_ERROR .AND. ;
      hOld != F_ERROR
      ? xdl_patch( pMMFOld, pDiff, XDL_PATCH_NORMAL, hNew, hErr )
      ? xdl_patch( pMMFNew, pDiff, XDL_PATCH_REVERSE, hOld, hErr )
   ELSE
      ? "Error"
   ENDIF
   FClose( hNew )
   FClose( hErr )
   FClose( hOld )

   RETURN
