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

#if !defined( HB_COMPAT_XHB )

#xtranslate HASH([<x,...>])          => HB_HASH([<x>])
#xtranslate HHASKEY([<x,...>])       => HB_HHASKEY([<x>])
#xtranslate HGETPOS([<x,...>])       => HB_HPOS([<x>])
#xtranslate HGET([<x,...>])          => HB_HGET([<x>])
#xtranslate HSET([<x,...>])          => HB_HSET([<x>])
#xtranslate HDEL([<x,...>])          => HB_HDEL([<x>])
#xtranslate HGETKEYAT([<x,...>])     => HB_HKEYAT([<x>])
#xtranslate HGETVALUEAT([<x,...>])   => HB_HVALUEAT([<x>])
#xtranslate HSETVALUEAT([<x,...>])   => HB_HVALUEAT([<x>])
#xtranslate HGETPAIRAT([<x,...>])    => HB_HPAIRAT([<x>])
#xtranslate HDELAT([<x,...>])        => HB_HDELAT([<x>])
#xtranslate HGETKEYS([<x,...>])      => HB_HKEYS([<x>])
#xtranslate HGETVALUES([<x,...>])    => HB_HVALUES([<x>])
#xtranslate HFILL([<x,...>])         => HB_HFILL([<x>])
#xtranslate HCLONE([<x,...>])        => HB_HCLONE([<x>])
#xtranslate HCOPY([<x,...>])         => HB_HCOPY([<x>])
#xtranslate HMERGE([<x,...>])        => HB_HMERGE([<x>])
#xtranslate HEVAL([<x,...>])         => HB_HEVAL([<x>])
#xtranslate HSCAN([<x,...>])         => HB_HSCAN([<x>])
#xtranslate HSETCASEMATCH( <h>,<l> ) => HB_HCASEMATCH( <h>,<l> ); <h>
#xtranslate HGETCASEMATCH([<x,...>]) => HB_HCASEMATCH([<x>])
#xtranslate HSETAUTOADD( <h>,<l> )   => HB_HAUTOADD( <h>,<l> ); <h>
#xtranslate HGETAUTOADD([<x,...>])   => HB_HAUTOADD([<x>])
#xtranslate HALLOCATE([<x,...>])     => HB_HALLOCATE([<x>])
#xtranslate HDEFAULT([<x,...>])      => HB_HDEFAULT([<x>])

#xtranslate NUMTOHEX(<n>)            => NTOC(<n>, 16)
#xtranslate HEXTONUM(<c>)            => CTON(<c>, 16)

#endif

#endif /* __HARBOUR__ */
