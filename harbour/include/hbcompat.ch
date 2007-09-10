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
   #xtranslate hb_methodName([<x,...>])=> methodName(<x>)
   #xtranslate hb_libLoad([<x,...>])   => libLoad(<x>)
   #xtranslate hb_libFree([<x,...>])   => libFree(<x>)
   #xtranslate hb_adler32([<x,...>])   => hb_checksum(<x>)
   #xtranslate hb_setLastKey([<x,...>])=> setLastKey(<x>)
   #xtranslate hb_CStr([<x,...>])      => CStr(<x>)

   #xtranslate hb_HexToNum([<c,...>])  => HexToNum(<c>)
   #xtranslate hb_NumToHex([<n,...>])  => NumToHex(<n>)
   #xtranslate hb_HexToStr([<c,...>])  => HexToStr(<c>)
   #xtranslate hb_StrToHex([<c,...>])  => StrToHex(<c>)

   #xtranslate hb_RAScan([<x,...>])    => RAScan(<x>)

   #xtranslate hb_ISPOINTER( <xValue> )=> ISPOINTER( <xValue> )

   #xtranslate hb_IniSetComment([<x,...>]) => hb_SetIniComment(<x>)
   #xtranslate hb_IniRead([<x,...>])       => hb_ReadIni(<x>)
   #xtranslate hb_IniWrite([<x,...>])      => hb_WriteIni(<x>)

   #xtranslate HBDebugger()            => TDebugger()
   #xtranslate HBBrwText()             => TBrwText()
   #xtranslate HBDbMenu()              => TDbMenu()
   #xtranslate HBDbMenuItem()          => TDbMenuItem()
   #xtranslate HBDbWindow()            => TDbWindow()
   #xtranslate HBDbBrowse()            => TDBGBrowser()
   #xtranslate HBDbArray()             => TDBGArray()
   #xtranslate HBDbHash()              => TDBGHash()
   #xtranslate HBDbObject()            => TDBGobject()

#else

   #xtranslate gtSys                   => hb_gtSys
   #xtranslate gtInfo([<x,...>])       => hb_gtInfo(<x>)
   #xtranslate hb_gt_Version([<x>])    => hb_gtVersion(<x>)

   #xtranslate gtSetClipboard(<x>)     => hb_gtInfo( GTI_CLIPBOARDDATA, <x> )
   #xtranslate gtGetClipboard()        => hb_gtInfo( GTI_CLIPBOARDDATA )
   #xtranslate gtGetClipBoardSize()    => Len( hb_gtInfo( GTI_CLIPBOARDDATA ) )
   #xtranslate gtPasteClipBoard([<n>]) => hb_gtInfo( GTI_CLIPBOARDPAST )
   #xtranslate gtProcessMessages()     => NextKey()
   #xtranslate gfxPrimitive([<x,...>]) => hb_gfxPrimitive(<x>)
   #xtranslate gfxText([<x,...>])      => hb_gfxText(<x>)

   #xtranslate hb_isregexstring([<x>]) => hb_isregex(<x>)
   #xtranslate pvalue([<x,...>])       => hb_pvalue(<x>)
   #xtranslate methodName([<x,...>])   => hb_methodName(<x>)
   #xtranslate libLoad([<x,...>])      => hb_libLoad(<x>)
   #xtranslate libFree([<x,...>])      => hb_libFree(<x>)
   #xtranslate hb_checksum([<x,...>])  => hb_adler32(<x>)
   #xtranslate setLastKey([<x,...>])   => hb_setLastKey(<x>)
   #xtranslate CStr([<x,...>])         => hb_CStr(<x>)

   #xtranslate HexToNum([<c,...>])     => hb_HexToNum(<c>)
   #xtranslate NumToHex([<n,...>])     => hb_NumToHex(<n>)
   #xtranslate HexToStr([<c,...>])     => hb_HexToStr(<c>)
   #xtranslate StrToHex([<c,...>])     => hb_StrToHex(<c>)

   #xtranslate RAScan([<x,...>])       => hb_RAScan(<x>)

   #xtranslate ISPOINTER( <xValue> )   => hb_ISPOINTER( <xValue> )

   #xtranslate hb_SetIniComment([<x,...>]) => hb_IniSetComment(<x>)
   #xtranslate hb_ReadIni([<x,...>])       => hb_IniRead(<x>)
   #xtranslate hb_WriteIni([<x,...>])      => hb_IniWrite(<x>)

   #xtranslate TDebugger()             => HBDebugger()
   #xtranslate TBrwText()              => HBBrwText()
   #xtranslate TDbMenu()               => HBDbMenu()
   #xtranslate TDbMenuItem()           => HBDbMenuItem()
   #xtranslate TDbWindow()             => HBDbWindow()
   #xtranslate TDBGBrowser()           => HBDbBrowse()
   #xtranslate TDBGArray()             => HBDbArray()
   #xtranslate TDBGHash()              => HBDbHash()
   #xtranslate TDBGobject()            => HBDbObject()

   #xtranslate Str(<x>,[<y>],[<y>],<z>)=> iif(<z>, LTrim(Str(<x>)), Str(<x>))
   #xtranslate hb_CMDARGARGV([<x,...>])=> hb_ARGV(0)

   #xtranslate HASH([<x,...>])         => hb_HASH(<x>)
   #xtranslate HHASKEY([<x,...>])      => hb_HHASKEY(<x>)
   #xtranslate HGETPOS([<x,...>])      => hb_HPOS(<x>)
   #xtranslate HGET([<x,...>])         => hb_HGET(<x>)
   #xtranslate HSET([<x,...>])         => hb_HSET(<x>)
   #xtranslate HDEL([<x,...>])         => hb_HDEL(<x>)
   #xtranslate HGETKEYAT([<x,...>])    => hb_HKEYAT(<x>)
   #xtranslate HGETVALUEAT([<x,...>])  => hb_HVALUEAT(<x>)
   #xtranslate HSETVALUEAT([<x,...>])  => hb_HVALUEAT(<x>)
   #xtranslate HGETPAIRAT([<x,...>])   => hb_HPAIRAT(<x>)
   #xtranslate HDELAT([<x,...>])       => hb_HDELAT(<x>)
   #xtranslate HGETKEYS([<x,...>])     => hb_HKEYS(<x>)
   #xtranslate HGETVALUES([<x,...>])   => hb_HVALUES(<x>)
   #xtranslate HFILL([<x,...>])        => hb_HFILL(<x>)
   #xtranslate HCLONE([<x,...>])       => hb_HCLONE(<x>)
   #xtranslate HCOPY([<x,...>])        => hb_HCOPY(<x>)
   #xtranslate HMERGE([<x,...>])       => hb_HMERGE(<x>)
   #xtranslate HEVAL([<x,...>])        => hb_HEVAL(<x>)
   #xtranslate HSCAN([<x,...>])        => hb_HSCAN(<x>)
   #xtranslate HSETCASEMATCH([<x,...>])=> hb_HSETCASEMATCH(<x>)
   #xtranslate HGETCASEMATCH([<x,...>])=> hb_HCASEMATCH(<x>)
   #xtranslate HSETAUTOADD([<x,...>])  => hb_HSETAUTOADD(<x>)
   #xtranslate HGETAUTOADD([<x,...>])  => hb_HAUTOADD(<x>)
   #xtranslate HALLOCATE([<x,...>])    => hb_HALLOCATE(<x>)
   #xtranslate HDEFAULT([<x,...>])     => hb_HDEFAULT(<x>)

   /* THROW => generate error */
   #xtranslate THROW(<oErr>) => (Eval(ErrorBlock(), <oErr>), Break(<oErr>))

   /* TEXT INTO <varname> */
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

   /* xHarbour operators: IN, HAS, LIKE, >>, <<, |, &, ^^ */
   #translate ( <exp1> IN <exp2> )     => ( (<exp1>) $ (<exp2>) )
   #translate ( <exp1> HAS <exp2> )    => ( HB_REGEXHAS( (<exp2>), (<exp1>) ) )
   #translate ( <exp1> LIKE <exp2> )   => ( HB_REGEXLIKE( (<exp2>), (<exp1>) ) )
   #translate ( <exp1> \<\< <exp2> )   => ( HB_BITSHIFT( (<exp1>), (<exp2>) ) )
   #translate ( <exp1> >> <exp2> )     => ( HB_BITSHIFT( (<exp1>), -(<exp2>) ) )
   #translate ( <exp1> | <exp2> )      => ( HB_BITOR( (<exp1>), (<exp2>) ) )
   #translate ( <exp1> & <exp2> )      => ( HB_BITAND( (<exp1>), (<exp2>) ) )
   #translate ( <exp1> ^^ <exp2> )     => ( HB_BITXOR( (<exp1>), (<exp2>) ) )

#endif

#endif /* __HARBOUR__ */
