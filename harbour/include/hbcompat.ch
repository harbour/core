/*** hbcompat.ch ***/

#ifdef __HARBOUR__

#ifdef __XHARBOUR__
  #include "gtinfo.ch"
  #include "gfx.ch"
#else
  #include "hbgtinfo.ch"
  #include "hbgfx.ch"
#endif


#ifdef __XHARBOUR__
  #xtranslate hb_gtSys                => gtSys
  #xtranslate hb_gtInfo([<xx,...>])   => gtInfo([<xx>])
  #xtranslate hb_gtVersion([<xx>])    => hb_gt_Version([<xx>])
#else
  #xtranslate gtSys                   => hb_gtSys
  #xtranslate gtInfo([<xx,...>])      => hb_gtInfo([<xx>])
  #xtranslate hb_gt_Version([<xx>])   => hb_gtVersion([<xx>])
  #xtranslate gtSetClipboard(<xx>)    => hb_gtInfo( GTI_CLIPBOARDDATA, <xx> )
  #xtranslate gtGetClipboard()        => hb_gtInfo( GTI_CLIPBOARDDATA )
  #xtranslate gtGetClipBoardSize()    => len( hb_gtInfo( GTI_CLIPBOARDDATA ) )
  #xtranslate gtPasteClipBoard([<n>]) => hb_gtInfo( GTI_CLIPBOARDPAST )
  #xtranslate gfxPrimitive([<xx,...>])=> hb_gfxPrimitive([<xx>])
  #xtranslate gfxText([<xx,...>])     => hb_gfxText([<xx>])
#endif

#ifndef __XHARBOUR__
   #xcommand DEFAULT => OTHERWISE
#endif

#endif /* __HARBOUR__ */
