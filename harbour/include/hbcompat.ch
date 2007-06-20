/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for cross-compatibility between different Harbour flavours
 *
 * Copyright 1999-2007 {list of individual authors and e-mail addresses}
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

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
   #xtranslate hb_gtInfo([<x,...>])    => gtInfo(<x>)
   #xtranslate hb_gtVersion([<x>])     => hb_gt_Version(<x>)

   #xtranslate hb_isregex([<x>])       => hb_isregexstring(<x>)
   #xtranslate hb_pvalue([<x,...>])    => pvalue(<x>)

   #xtranslate hb_libLoad([<x,...>])   => libLoad(<x>)
   #xtranslate hb_libFree([<x,...>])   => libFree(<x>)

#else

   #xtranslate gtSys                   => hb_gtSys
   #xtranslate gtInfo([<x,...>])       => hb_gtInfo(<x>)
   #xtranslate hb_gt_Version([<x>])    => hb_gtVersion(<x>)
   #xtranslate gtSetClipboard(<x>)     => hb_gtInfo( GTI_CLIPBOARDDATA, <x> )
   #xtranslate gtGetClipboard()        => hb_gtInfo( GTI_CLIPBOARDDATA )
   #xtranslate gtGetClipBoardSize()    => len( hb_gtInfo( GTI_CLIPBOARDDATA ) )
   #xtranslate gtPasteClipBoard([<n>]) => hb_gtInfo( GTI_CLIPBOARDPAST )
   #xtranslate gtProcessMessages()     => nextkey()
   #xtranslate gfxPrimitive([<x,...>]) => hb_gfxPrimitive(<x>)
   #xtranslate gfxText([<x,...>])      => hb_gfxText(<x>)

   #xtranslate hb_isregexstring([<x>]) => hb_isregex(<x>)

   #xtranslate pvalue([<x,...>])       => hb_pvalue(<x>)

   #xtranslate libLoad([<x,...>])      => hb_libLoad(<x>)
   #xtranslate libFree([<x,...>])      => hb_libFree(<x>)

   #xtranslate HASH([<x,...>])         => HB_HASH(<x>)
   #xtranslate HHASKEY([<x,...>])      => HB_HHASKEY(<x>)
   #xtranslate HGETPOS([<x,...>])      => HB_HPOS(<x>)
   #xtranslate HGET([<x,...>])         => HB_HGET(<x>)
   #xtranslate HSET([<x,...>])         => HB_HSET(<x>)
   #xtranslate HDEL([<x,...>])         => HB_HDEL(<x>)
   #xtranslate HGETKEYAT([<x,...>])    => HB_HKEYAT(<x>)
   #xtranslate HGETVALUEAT([<x,...>])  => HB_HVALUEAT(<x>)
   #xtranslate HSETVALUEAT([<x,...>])  => HB_HVALUEAT(<x>)
   #xtranslate HGETPAIRAT([<x,...>])   => HB_HPAIRAT(<x>)
   #xtranslate HDELAT([<x,...>])       => HB_HDELAT(<x>)
   #xtranslate HGETKEYS([<x,...>])     => HB_HKEYS(<x>)
   #xtranslate HGETVALUES([<x,...>])   => HB_HVALUES(<x>)
   #xtranslate HFILL([<x,...>])        => HB_HFILL(<x>)
   #xtranslate HCLONE([<x,...>])       => HB_HCLONE(<x>)
   #xtranslate HCOPY([<x,...>])        => HB_HCOPY(<x>)
   #xtranslate HMERGE([<x,...>])       => HB_HMERGE(<x>)
   #xtranslate HEVAL([<x,...>])        => HB_HEVAL(<x>)
   #xtranslate HSCAN([<x,...>])        => HB_HSCAN(<x>)
   #xtranslate HSETCASEMATCH( <h>,<l> )=> ( HB_HCASEMATCH( <h>,<l> ), <h> )
   #xtranslate HGETCASEMATCH([<x,...>])=> HB_HCASEMATCH(<x>)
   #xtranslate HSETAUTOADD( <h>,<l> )  => ( HB_HAUTOADD( <h>,<l> ), <h> )
   #xtranslate HGETAUTOADD([<x,...>])  => HB_HAUTOADD(<x>)
   #xtranslate HALLOCATE([<x,...>])    => HB_HALLOCATE(<x>)
   #xtranslate HDEFAULT([<x,...>])     => HB_HDEFAULT(<x>)

   #xtranslate NUMTOHEX(<n>)           => NTOC(<n>, 16)
   #xtranslate HEXTONUM(<c>)           => CTON(<c>, 16)

   #xcommand TEXT INTO <v> => #pragma __text|<v>+=%s+HB_OSNEWLINE();<v>:=""

   /* SWITCH ... ; case ... ; DEFAULT ; ... ; END */
   #xcommand DEFAULT => OTHERWISE

   /* TRY / CATCH / FINALLY / END */
   #xcommand TRY  => BEGIN SEQUENCE WITH {|oErr| Break( oErr )}
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
   #xcommand FINALLY => ALWAYS

   /* EXTENDED CODEBLOCKs */
   #xtranslate \<|[<x,...>]| => {|<x>|
   #xcommand > [<*x*>]       => } <x>

#endif

#endif /* __HARBOUR__ */
