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
   xdl_write_mmfile( pMMFNew, cFileCtx + hb_eol() + Space( 3 ) + "RETURN" + hb_eol() )

   ? xdl_mmfile_cmp( pMMFOld, pMMFNew )

   //

   IF ( hDif := hb_vfOpen( cDiffName := hb_FNameName( __FILE__ ) + ".di2", FO_CREAT + FO_TRUNC + FO_WRITE ) ) != NIL
      hb_vfWrite( hDif, "diff ---" + hb_eol() )
      xdl_diff( pMMFOld, pMMFNew, 0, 3, hDif )
      hb_vfClose( hDif )
   ELSE
      ? "Error"
   ENDIF

   pDiff := xdl_init_mmfile( XDLT_STD_BLKSIZE, XDL_MMF_ATOMIC )
   cFileCtx := hb_MemoRead( cDiffName )
   xdl_write_mmfile( pDiff, cFileCtx )

   hNew := hb_vfOpen( hb_FNameExtSet( cDiffName, ".ne2" ), FO_CREAT + FO_TRUNC + FO_WRITE )
   hErr := hb_vfOpen( hb_FNameExtSet( cDiffName, ".er2" ), FO_CREAT + FO_TRUNC + FO_WRITE )
   hOld := hb_vfOpen( hb_FNameExtSet( cDiffName, ".ol2" ), FO_CREAT + FO_TRUNC + FO_WRITE )
   IF hNew != NIL .AND. ;
      hErr != NIL .AND. ;
      hOld != NIL
      ? xdl_patch( pMMFOld, pDiff, XDL_PATCH_NORMAL, hNew, hErr )
      ? xdl_patch( pMMFNew, pDiff, XDL_PATCH_REVERSE, hOld, hErr )
   ELSE
      ? "Error"
   ENDIF
   IF hNew != NIL
      hb_vfClose( hNew )
   ENDIF
   IF hErr != NIL
      hb_vfClose( hErr )
   ENDIF
   IF hOld != NIL
      hb_vfClose( hOld )
   ENDIF

   //

   IF ( hDif := FCreate( cDiffName := hb_FNameName( __FILE__ ) + ".dif" ) ) != F_ERROR
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
