/*
 * $Id$
 */

#include "hbxdiff.ch"
#include "fileio.ch"
#include "simpleio.ch"

PROCEDURE main()

   LOCAL pMMFOld, pMMFNew, pDiff
   LOCAL cFileCtx
   LOCAL hDif, hNew, hErr, hOld
   LOCAL cDiffName

   pMMFOld := xdl_init_mmfile( XDLT_STD_BLKSIZE )
   pMMFNew := xdl_init_mmfile( XDLT_STD_BLKSIZE, XDL_MMF_ATOMIC )

   cFileCtx := hb_memoRead( __FILE__ )

   xdl_write_mmfile( pMMFOld, @cFileCtx )
   xdl_write_mmfile( pMMFNew, cFileCtx + hb_eol() + Space( 3 ) + "RETURN NIL" + hb_eol() )

   ? xdl_mmfile_cmp( pMMFOld, pMMFNew )

   hb_FNameSplit( __FILE__, NIL, @cDiffName, NIL )
   cDiffName := hb_FNameMerge( NIL, cDiffName, ".dif" )

   hDif := FCreate( cDiffName , FC_NORMAL )
   IF FError() == 0
      FWrite( hDif, "diff ---" + hb_eol() )
      xdl_diff( pMMFOld, pMMFNew, 0, 3, hDif )

      FClose( hDif )
   ENDIF

   pDiff := xdl_init_mmfile( XDLT_STD_BLKSIZE, XDL_MMF_ATOMIC )
   cFileCtx := hb_memoRead( cDiffName )
   xdl_write_mmfile( pDiff, cFileCtx )

   hNew := FCreate( hb_FNameMerge( NIL, cDiffName, ".new" ), FC_NORMAL )
   hErr := FCreate( hb_FNameMerge( NIL, cDiffName, ".err" ), FC_NORMAL )
   hOld := FCreate( hb_FNameMerge( NIL, cDiffName, ".old" ), FC_NORMAL )
   IF FError() == 0
      ? xdl_patch( pMMFOld, pDiff, XDL_PATCH_NORMAL, hNew, hErr )
      ? xdl_patch( pMMFNew, pDiff, XDL_PATCH_REVERSE, hOld, hErr )

      FClose( hNew )
      FClose( hErr )
      FClose( hOld )
   ENDIF

   RETURN
