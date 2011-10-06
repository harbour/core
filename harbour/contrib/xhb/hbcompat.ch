/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for cross-compatibility between different Harbour flavours
 *
 * Copyright 1999-2009 {list of individual authors and e-mail addresses}
 * www - http://harbour-project.org
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

#include "hbgtinfo.ch"
#include "hbgfx.ch"

#ifdef __XHARBOUR__

   #if defined( __PLATFORM__Windows ) .AND. !defined( __PLATFORM__WINDOWS )
      #define __PLATFORM__WINDOWS
   #endif
   #if defined( __PLATFORM__Linux ) .AND. !defined( __PLATFORM__LINUX )
      #define __PLATFORM__LINUX
   #endif

   #xtranslate hb_ScrMaxRow()              => gtInfo( HB_GTI_SCREENHEIGHT )
   #xtranslate hb_ScrMaxCol()              => gtInfo( HB_GTI_SCREENWIDTH )
   #xtranslate MaxRow(.T.)                 => gtInfo( HB_GTI_SCREENHEIGHT )
   #xtranslate MaxCol(.T.)                 => gtInfo( HB_GTI_SCREENWIDTH )
   #xtranslate hb_keyNext([<x>])           => NextKey(<x>)

   #xtranslate hb_osNewLine()              => hb_eol()
   #xtranslate hb_osPathSeparator()        => hb_ps()

   #xtranslate hb_dbPack()                 => __dbPack()
   #xtranslate hb_dbZap()                  => __dbZap()
   #xtranslate hb_dbDrop([<x,...>])        => dbDrop(<x>)
   #xtranslate hb_dbExists([<x,...>])      => dbExists(<x>)
   #xtranslate hb_FieldLen([<x>])          => FieldLen(<x>)
   #xtranslate hb_FieldDec([<x>])          => FieldDec(<x>)
   #xtranslate hb_FieldType([<x>])         => FieldType(<x>)

   #xtranslate hb_gtInfo( HB_GTI_INKEYREAD [, <x>] )     => SetInkeyBeforeBlock([<x>]) <-x->
   #xtranslate hb_gtInfo( HB_GTI_INKEYFILTER [, <x>] )   => SetInkeyAfterBlock([<x>]) <-x->

   #xtranslate hb_processOpen([<x,...>])   => hb_openProcess(<x>)
   #xtranslate hb_processClose([<x,...>])  => hb_closeProcess(<x>)

   #xtranslate hb_isregex([<x>])           => hb_isregexstring(<x>)
   #xtranslate hb_methodName([<x,...>])    => methodName(<x>)
   #xtranslate hb_libLoad([<x,...>])       => libLoad(<x>)
   #xtranslate hb_libFree([<x,...>])       => libFree(<x>)
   #xtranslate hb_adler32([<x,...>])       => hb_checksum(<x>)
   #xtranslate hb_setLastKey([<x,...>])    => setLastKey(<x>)
   #xtranslate hb_CStr([<x,...>])          => CStr(<x>)
   #xtranslate hb_valToExp([<x,...>])      => ValToPrgExp(<x>)
   #xtranslate hb_rddInfo([<x,...>])       => rddInfo(<x>)
   #xtranslate hb_idleSleep([<x,...>])     => SecondsSleep(<x>)
   #xtranslate hb_UserName()               => NetName(1)
   #xtranslate hb_FSize(<x>)               => FileSize(<x>)
   #xtranslate hb_WildMatch([<x,...>])     => WildMatch(<x>)
   #xtranslate hb_bitTest([<x,...>])       => hb_bitIsSet(<x>)
   #xtranslate hb_Deserialize(<x>)         => hb_DeserialNext(<x>)

   #xtranslate hb_HexToNum([<c,...>])      => HexToNum(<c>)
   #xtranslate hb_NumToHex([<n,...>])      => NumToHex(<n>)
   #xtranslate hb_HexToStr([<c,...>])      => HexToStr(<c>)
   #xtranslate hb_StrToHex([<c,...>])      => StrToHex(<c>)

   #xtranslate hb_AScan([<x,...>])         => AScan(<x>)
   #xtranslate hb_RAScan([<x,...>])        => RAScan(<x>)
   #xtranslate hb_AIns([<x,...>])          => AIns(<x>)
   #xtranslate hb_ADel([<x,...>])          => ADel(<x>)
   #xtranslate hb_At([<x,...>])            => At(<x>)

   #xtranslate hb_DateTime()               => DateTime()
   #xtranslate hb_Hour([<x>])              => Hour(<x>)
   #xtranslate hb_Minute([<x>])            => Minute(<x>)
   #xtranslate hb_TToS([<x>])              => TToS(<x>)
   #xtranslate hb_SToT([<x>])              => SToT(<x>)
   #xtranslate hb_TToC([<x,...>])          => TToC(<x>)
   #xtranslate hb_CToT([<x,...>])          => CToT(<x>)

   #xtranslate hb_GetEnv([<x,...>])        => GetEnv(<x>)
   #xtranslate hb_SetKey([<x,...>])        => SetKey(<x>)

   #xtranslate hb_i18n_gettext(<x>)        => i18n(<x>)

   #xtranslate hb_cdpSelect([<x,...>])     => hb_SetCodepage( <x> )

   #xtranslate hb_ARGV([<x,...>])          => hb_CMDARGARGV(<x>)

   #xtranslate hb_IniSetComment([<x,...>]) => hb_SetIniComment(<x>)
   #xtranslate hb_IniRead([<x,...>])       => hb_ReadIni(<x>)
   #xtranslate hb_IniWrite([<x,...>])      => hb_WriteIni(<x>)

   #xtranslate hb_DisableWaitLocks([<x>])  => DisableWaitLocks(<x>)

   #xtranslate hb_gtLock()                 => HBCONSOLELOCK()
   #xtranslate hb_gtUnLock()               => HBCONSOLEUNLOCK()

   /* MT functions */
   #xtranslate hb_mtvm()                   => hb_multiThread()
   #xtranslate hb_threadSelf()             => GetCurrentThread()
   #xtranslate hb_threadId( [<x,...>] )    => GetThreadId( <x> )
   #xtranslate hb_threadStart( <x,...> )   => StartThread( [<x>] )
   #xtranslate hb_threadJoin( <x> )        => JoinThread( <x> )
   #xtranslate hb_threadQuitRequest( <x> ) => KillThread( <x> )
   #xtranslate hb_threadWaitForAll()       => WaitForThreads()
   #xtranslate hb_threadTerminateAll()     => KillAllThreads()

   #xtranslate hb_mutexNotify(<x,...>)     => Notify(<x>)
   #xtranslate hb_mutexNotifyAll(<x,...>)  => NotifyAll(<x>)

   #xtranslate hb_mutexSubscribe(<x,...>)  => {|mtx, nTimeOut, xSubscribed| ;;
                                                local lSubscribed ;;
                                                xSubscribed := Subscribe( mtx, ;
                                                                          iif( hb_isNumeric( nTimeOut ), nTimeOut * 1000, ), ;
                                                                          @lSubscribed ) ;
                                                return lSubscribed ; }:eval( <x> )
   #xtranslate hb_mutexSubscribeNow(<x,...>) => {|mtx, nTimeOut, xSubscribed| ;;
                                                local lSubscribed ;;
                                                xSubscribed := SubscribeNow( mtx, ;
                                                                             iif( hb_isNumeric( nTimeOut ), nTimeOut * 1000, ), ;
                                                                             @lSubscribed ) ;
                                                return lSubscribed ; }:eval( <x> )

   #xtranslate hb_MutexLock( <x>, <n> )    => iif( !hb_isNumeric( <n> ), hb_MutexLock( <x> ) ;
                                                 iif( <n> <= 0, hb_MutexTryLock( <x> ), ;
                                                    hb_MutexTimeOutLock( <x>, <n> ) ) )

   /* Hash item functions */
   #xtranslate hb_HASH([<x,...>])          => HASH(<x>)
   #xtranslate hb_HHASKEY([<x,...>])       => HHASKEY(<x>)
   #xtranslate hb_HPOS([<x,...>])          => HGETPOS(<x>)
   #xtranslate hb_HGET([<x,...>])          => HGET(<x>)
   #xtranslate hb_HSET([<x,...>])          => HSET(<x>)
   #xtranslate hb_HDEL([<x,...>])          => HDEL(<x>)
   #xtranslate hb_HKEYAT([<x,...>])        => HGETKEYAT(<x>)
   #xtranslate hb_HVALUEAT([<x,...>])      => HGETVALUEAT(<x>)
   #xtranslate hb_HVALUEAT([<x,...>])      => HSETVALUEAT(<x>)
   #xtranslate hb_HPAIRAT([<x,...>])       => HGETPAIRAT(<x>)
   #xtranslate hb_HDELAT([<x,...>])        => HDELAT(<x>)
   #xtranslate hb_HKEYS([<x,...>])         => HGETKEYS(<x>)
   #xtranslate hb_HVALUES([<x,...>])       => HGETVALUES(<x>)
   #xtranslate hb_HFILL([<x,...>])         => HFILL(<x>)
   #xtranslate hb_HCLONE([<x,...>])        => HCLONE(<x>)
   #xtranslate hb_HCOPY([<x,...>])         => HCOPY(<x>)
   #xtranslate hb_HMERGE([<x,...>])        => HMERGE(<x>)
   #xtranslate hb_HEVAL([<x,...>])         => HEVAL(<x>)
   #xtranslate hb_HSCAN([<x,...>])         => HSCAN(<x>)
   #xtranslate hb_HSETCASEMATCH([<x,...>]) => HSETCASEMATCH(<x>)
   #xtranslate hb_HCASEMATCH([<x,...>])    => HGETCASEMATCH(<x>)
   #xtranslate hb_HSETAUTOADD([<x,...>])   => HSETAUTOADD(<x>)
   #xtranslate hb_HAUTOADD([<x,...>])      => HGETAUTOADD(<x>)
   #xtranslate hb_HALLOCATE([<x,...>])     => HALLOCATE(<x>)
   #xtranslate hb_HDEFAULT([<x,...>])      => HDEFAULT(<x>)

   /* Inet functions */
   #xtranslate hb_INETINIT([<x,...>])                => INETINIT(<x>)
   #xtranslate hb_INETCLEANUP([<x,...>])             => INETCLEANUP(<x>)
   #xtranslate hb_INETCREATE([<x,...>])              => INETCREATE(<x>)
   #xtranslate hb_INETCLOSE([<x,...>])               => INETCLOSE(<x>)
   #xtranslate hb_INETFD([<x,...>])                  => INETFD(<x>)
   #xtranslate hb_INETSTATUS([<x,...>])              => INETSTATUS(<x>)
   #xtranslate hb_INETERRORCODE([<x,...>])           => INETERRORCODE(<x>)
   #xtranslate hb_INETERRORDESC([<x,...>])           => INETERRORDESC(<x>)
   #xtranslate hb_INETCLEARERROR([<x,...>])          => INETCLEARERROR(<x>)
   #xtranslate hb_INETCOUNT([<x,...>])               => INETCOUNT(<x>)
   #xtranslate hb_INETADDRESS([<x,...>])             => INETADDRESS(<x>)
   #xtranslate hb_INETPORT([<x,...>])                => INETPORT(<x>)
   #xtranslate hb_INETTIMEOUT(<x>, <y>)              => INETSETTIMEOUT(<x>, <y>)
   #xtranslate hb_INETTIMEOUT(<x>)                   => INETGETTIMEOUT(<x>)
   #xtranslate hb_INETCLEARTIMEOUT([<x,...>])        => INETCLEARTIMEOUT(<x>)
   #xtranslate hb_INETTIMELIMIT(<x>, <y>)            => INETSETTIMELIMIT(<x>, <y>)
   #xtranslate hb_INETTIMELIMIT(<x>)                 => INETGETTIMELIMIT(<x>)
   #xtranslate hb_INETCLEARTIMELIMIT([<x,...>])      => INETCLEARTIMELIMIT(<x>)
   #xtranslate hb_INETPERIODCALLBACK(<x>, <y>)       => INETSETPERIODCALLBACK(<x>, <y>)
   #xtranslate hb_INETPERIODCALLBACK(<x>)            => INETGETPERIODCALLBACK(<x>)
   #xtranslate hb_INETCLEARPERIODCALLBACK([<x,...>]) => INETCLEARPERIODCALLBACK(<x>)
   #xtranslate hb_INETRECV([<x,...>])                => INETRECV(<x>)
   #xtranslate hb_INETRECVALL([<x,...>])             => INETRECVALL(<x>)
   #xtranslate hb_INETRECVLINE([<x,...>])            => INETRECVLINE(<x>)
   #xtranslate hb_INETRECVENDBLOCK([<x,...>])        => INETRECVENDBLOCK(<x>)
   #xtranslate hb_INETDATAREADY([<x,...>])           => INETDATAREADY(<x>)
   #xtranslate hb_INETSEND([<x,...>])                => INETSEND(<x>)
   #xtranslate hb_INETSENDALL([<x,...>])             => INETSENDALL(<x>)
   #xtranslate hb_INETGETHOSTS([<x,...>])            => INETGETHOSTS(<x>)
   #xtranslate hb_INETGETALIAS([<x,...>])            => INETGETALIAS(<x>)
   #xtranslate hb_INETSERVER([<x,...>])              => INETSERVER(<x>)
   #xtranslate hb_INETACCEPT([<x,...>])              => INETACCEPT(<x>)
   #xtranslate hb_INETCONNECT([<x,...>])             => INETCONNECT(<x>)
   #xtranslate hb_INETCONNECTIP([<x,...>])           => INETCONNECTIP(<x>)
   #xtranslate hb_INETDGRAMBIND([<x,...>])           => INETDGRAMBIND(<x>)
   #xtranslate hb_INETDGRAM([<x,...>])               => INETDGRAM(<x>)
   #xtranslate hb_INETDGRAMSEND([<x,...>])           => INETDGRAMSEND(<x>)
   #xtranslate hb_INETDGRAMRECV([<x,...>])           => INETDGRAMRECV(<x>)
   #xtranslate hb_INETCRLF([<x,...>])                => INETCRLF(<x>)
   #xtranslate hb_INETISSOCKET([<x,...>])            => ISINETSOCKET(<x>)
   #xtranslate hb_INETCLOSE([<x,...>])               => INETDESTROY(<x>)

   /* Some statement endings */
   #xcommand ENDSEQUENCE => END
   #xcommand ENDSWITCH => END
   #xcommand END SWITCH => END
   #xcommand ENDWITH => END
   #xcommand END WITH => END
   #xcommand END OBJECT => END

   #ifndef HB_SYMBOL_UNUSED
      #define HB_SYMBOL_UNUSED( symbol )  ( symbol := ( symbol ) )
   #endif

   #define HB_GTI_CLIPBOARDPASTE HB_GTI_CLIPBOARDPAST

#else

   #if defined( __PLATFORM__WINDOWS ) .AND. !defined( __PLATFORM__Windows )
      #define __PLATFORM__Windows
   #endif
   #if defined( __PLATFORM__LINUX ) .AND. !defined( __PLATFORM__Linux )
      #define __PLATFORM__Linux
   #endif

   /* these are used _by_ MaxRow()/MaxCol() */
   #define GTI_WINDOW         0  /* Maximum window size ('window' in CT terms) */
   #define GTI_SCREEN         1  /* Maximum screen size ('Screen' in CT terms) */
   #define GTI_CLIENT         2  /* Maximum possible client size of a window */
   #define GTI_MAX            3  /* Maximum possible window size (in Windows) */

   #xtranslate hb_eol()                        => hb_osNewLine()
   #xtranslate hb_ps()                         => hb_osPathSeparator()

   #xtranslate MaxRow(.T.)                     => hb_gtInfo( HB_GTI_VIEWPORTHEIGHT )
   #xtranslate MaxCol(.T.)                     => hb_gtInfo( HB_GTI_VIEWPORTWIDTH )
   #xtranslate NextKey([<x>])                  => hb_keyNext(<x>)

   #xtranslate Str(<x>,[<y>],[<y>],<z>)        => iif(<z>, hb_NToS(<x>), Str(<x>))

   #xuntranslate NetName(                      =>
   #xuntranslate MemoWrit(                     =>

   #xtranslate NetName(<n>)                    => iif( hb_isNumeric( <n> ) .AND. <n> == 1, hb_UserName(), NetName() )
   #xtranslate MemoWrit(<x>,<y>,<z>)           => iif( hb_isLogical(<z>) .AND. ! <z>, hb_MemoWrit(<x>,<y>), MemoWrit(<x>,<y>) )

   #xuntranslate AIns(                         =>
   #xuntranslate ADel(                         =>

   #xtranslate AIns(<a>,<n>,[<x,...>])         => hb_AIns(<a>,<n>,<x>)
   #xtranslate ADel(<a>,<n>,<l>)               => hb_ADel(<a>,<n>,<l>)

   #xtranslate AScan(<a>,<b>,[<c>],[<d>],<e>)  => hb_AScan(<a>,<b>,<c>,<d>,<e>)
   #xtranslate At(<a>,<b>,[<x,...>])           => hb_At(<a>,<b>,<x>)

   #xtranslate GetEnv([<x,...>])               => hb_GetEnv(<x>)
   #xtranslate SetKey([<x,...>])               => hb_SetKey(<x>)

   /* TEXT INTO <varname> */
   #xcommand TEXT INTO <v> => #pragma __text|<v>+=%s+hb_eol();<v>:=""

   /* SWITCH ... ; case ... ; DEFAULT ; ... ; END */
   #xcommand DEFAULT => OTHERWISE

   /* FOR EACH hb_enumIndex() */
   #xtranslate hb_enumIndex(<!v!>) => <v>:__enumIndex()

   /* TRY / CATCH / FINALLY / END */
   #xcommand TRY  => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
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
   /* NOTE: These macros can break some valid Harbour/Clipper constructs,
            so they are disabled by default. Enable them with care, or
            even better to switch to use HB_BIT*() functions directly.
            They are optimized by Harbour compiler the same way (and even
            more) as these C-like operators, without any bad side-effects. */
   #if defined( XHB_BITOP )
      #translate ( <exp1> | <exp2> )      => ( HB_BITOR( (<exp1>), (<exp2>) ) )
      #translate ( <exp1> & <exp2> )      => ( HB_BITAND( (<exp1>), (<exp2>) ) )
      #translate ( <exp1> ^^ <exp2> )     => ( HB_BITXOR( (<exp1>), (<exp2>) ) )
   #endif

   #command @ <row>, <col> PROMPT <prompt> [ MESSAGE <msg> ] [ COLOR <color> ] => ;
      __AtPrompt( <row>, <col>, <prompt>, <msg>, <color> )

   #command SET TRIMFILENAME <x:ON,OFF,&> => Set( _SET_TRIMFILENAME, <(x)> )
   #command SET TIME FORMAT [TO] <f>      => Set( _SET_TIMEFORMAT, <f> )

   #define HB_GTI_CLIPBOARDPAST HB_GTI_CLIPBOARDPASTE

   /* These also have wrapper function in xhb lib */

   #xtranslate gtSetClipboard(<x>)             => hb_gtInfo( HB_GTI_CLIPBOARDDATA, <x> )
   #xtranslate gtGetClipboard()                => hb_gtInfo( HB_GTI_CLIPBOARDDATA )
   #xtranslate gtGetClipBoardSize()            => Len( hb_gtInfo( HB_GTI_CLIPBOARDDATA ) )
   #xtranslate gtPasteClipBoard()              => hb_gtInfo( HB_GTI_CLIPBOARDPASTE )
   #xtranslate gtProcessMessages()             => NextKey()
   #xtranslate SetInkeyBeforeBlock([<x>])      => hb_gtInfo( HB_GTI_INKEYREAD [, <x>] ) <-x->
   #xtranslate SetInkeyAfterBlock([<x>])       => hb_gtInfo( HB_GTI_INKEYFILTER [, <x>] ) <-x->
   #xtranslate gfxPrimitive([<x,...>])         => hb_gfxPrimitive(<x>)
   #xtranslate gfxText([<x,...>])              => hb_gfxText(<x>)

   #xtranslate hb_openProcess([<x,...>])       => hb_processOpen(<x>)
   #xtranslate hb_closeProcess([<x,...>])      => hb_processClose(<x>)

   #xtranslate hb_isregexstring([<x>])         => hb_isregex(<x>)
   #xtranslate methodName([<x,...>])           => hb_methodName(<x>)
   #xtranslate libLoad([<x,...>])              => hb_libLoad(<x>)
   #xtranslate libFree([<x,...>])              => hb_libFree(<x>)
   #xtranslate hb_checksum([<x,...>])          => hb_adler32(<x>)
   #xtranslate setLastKey([<x,...>])           => hb_setLastKey(<x>)
   #xtranslate CStr([<x,...>])                 => hb_CStr(<x>)
   #xtranslate ValToPrgExp([<x,...>])          => hb_valToExp(<x>)
   #xtranslate SecondsSleep([<x,...>])         => hb_idleSleep(<x>)
   #xtranslate WildMatch([<x,...>])            => hb_WildMatch(<x>)
   #xtranslate hb_bitIsSet([<x,...>])          => hb_bitTest(<x>)
   #xtranslate hb_DeserialNext(<x>)            => hb_Deserialize(<x>)
   #xtranslate hb_funcptr(<x>)                 => __dynsn2sym(<x>)

   #xtranslate HexToNum([<c,...>])             => hb_HexToNum(<c>)
   #xtranslate NumToHex([<n,...>])             => hb_NumToHex(<n>)
   #xtranslate HexToStr([<c,...>])             => hb_HexToStr(<c>)
   #xtranslate StrToHex([<c,...>])             => hb_StrToHex(<c>)

   #xtranslate ISPOINTER( <xValue> )           => hb_ISPOINTER( <xValue> )

   #xtranslate hb_SetIniComment([<x,...>])     => hb_IniSetComment(<x>)
   #xtranslate hb_ReadIni([<x,...>])           => hb_IniRead(<x>)
   #xtranslate hb_WriteIni([<x,...>])          => hb_IniWrite(<x>)

   #xtranslate DisableWaitLocks([<x>])         => hb_DisableWaitLocks(<x>)

   #xtranslate HBCONSOLELOCK()                 => hb_gtLock()
   #xtranslate HBCONSOLEUNLOCK()               => hb_gtUnLock()

   #xtranslate hb_CMDARGARGV([<x,...>])        => hb_ARGV(<x>)

   #xtranslate RAScan([<x,...>])               => hb_RAScan(<x>)

   #xtranslate DateTime()                      => hb_DateTime()
   #xtranslate Hour([<x>])                     => hb_Hour(<x>)
   #xtranslate Minute([<x>])                   => hb_Minute(<x>)
   #xtranslate TToS([<x>])                     => hb_TToS(<x>)
   #xtranslate SToT([<x>])                     => hb_SToT(<x>)
   #xtranslate TToC([<x,...>])                 => hb_TToC(<x>)
   #xtranslate CToT([<x,...>])                 => hb_CToT(<x>)

   #xtranslate i18n(<x>)                       => hb_i18n_gettext(<x>)

   #xtranslate hb_SetCodepage([<x,...>])       => hb_cdpSelect( <x> )

   /* MT functions */
   #xtranslate hb_MultiThread()                => hb_mtvm()
   #xtranslate GetCurrentThread()              => hb_threadSelf()
   #xtranslate GetThreadId( [<x,...>] )        => hb_threadId( <x> )
   #xtranslate ThreadGetCurrentInternal()      => hb_threadId()
   #xtranslate IsSameThread( <x> [,<y>] )      => ( hb_threadId( <x> ) == hb_threadId( <y> ) )
   #xtranslate IsValidThread( <x> )            => ( hb_threadId( <x> ) != 0 )
   #xtranslate JoinThread( <x> )               => hb_threadJoin( <x> )
   #xtranslate KillThread( <x> )               => hb_threadQuitRequest( <x> )
   #xtranslate StopThread( <x> )               => hb_threadQuitRequest( <x> ); hb_threadJoin( <x> )
   #xtranslate KillAllThreads()                => hb_threadTerminateAll()
   #xtranslate WaitForThreads()                => hb_threadWaitForAll()

   #xtranslate ThreadSleep( <x> )              => hb_idleSleep( <x> / 1000 )

   #xtranslate DestroyMutex( <x> )             =>
   #xtranslate hb_MutexTryLock( <x> )          => hb_mutexLock( <x>, 0 )
   #xtranslate hb_MutexTimeOutLock( <x> )      => hb_mutexLock( <x>, 0 )
   #xtranslate hb_MutexTimeOutLock( <x>, <n> ) => hb_mutexLock( <x>, IIF( hb_isNumeric( <n> ), <n> / 1000, 0 ) )

   #xtranslate Notify( <x,...> )               => hb_mutexNotify( <x> )
   #xtranslate NotifyAll( <x,...> )            => hb_mutexNotifyAll( <x> )
   #xtranslate Subscribe( <x,...> )            => {|mtx, nTimeOut, lSubscribed| ;;
                                                   local xSubscribed ;;
                                                   lSubscribed := hb_mutexSubscribe( mtx, ;
                                                                                     iif( hb_isNumeric( nTimeOut ), nTimeOut / 1000, ), ;
                                                                                     @xSubscribed ) ;
                                                   return xSubscribed ; }:eval( <x> )
   #xtranslate SubscribeNow( <x,...> )         => {|mtx, nTimeOut, lSubscribed| ;;
                                                   local xSubscribed ;;
                                                   lSubscribed := hb_mutexSubscribeNow( mtx, ;
                                                                                        iif( hb_isNumeric( nTimeOut ), nTimeOut / 1000, ), ;
                                                                                        @xSubscribed ) ;
                                                   return xSubscribed ; }:eval( <x> )

   #xtranslate StartThread( [<x>] )            => hb_threadStart( <x> )
   #xtranslate StartThread( <x>, <y> [, <z,...>] ) => iif( valtype( <x> ) == "O" .and. hb_isString( <y> ), ;
                                                           hb_threadStart( {|...| (<x>):&(<y>)( ... ) } [, <z>] ), ;
                                                           hb_threadStart( <x>, <y> [, <z>] ) )

   /* not possible to well replicate xHarbour behavior because it's buggy
      these function results are different on different platform, chosen
      translation which returns compatible types (numeric) */
   #xtranslate ThreadGetCurrent()              => hb_threadId()
   #xtranslate GetSystemThreadId( [<x,...>] )  => hb_threadId( <x> )

   /* do not need translation */
   /* hb_MutexCreate()                         => hb_mutexCreate() */
   /* hb_MutexUnlock( <x> )                    => hb_mutexUnlock( <x> ) */

   /* do not need translation only when xHarbour code is compiled by Harbour */
   /* hb_MutexLock( <x> )                      => hb_mutexLock( <x> ) */

   /* functions I do not want to document as public .prg API in Harbour */
   /* ThreadInspect() */
   /* ThreadInspectEnd() */
   /* ThreadIsInspect() */

   /* functions which are not necessary in Harbour */
   /* hb_ThreadGetTryErrorArray() */
   /* ThreadIdleFence() */

   /* function which I can add but it's not very usable in real life */
   /* hb_ThreadCountStacks() */

   /* Hash item functions */
   #xtranslate HASH([<x,...>])             => hb_HASH(<x>)
   #xtranslate HHASKEY([<x,...>])          => hb_HHASKEY(<x>)
   #xtranslate HGETPOS([<x,...>])          => hb_HPOS(<x>)
   #xtranslate HGET([<x,...>])             => hb_HGET(<x>)
   #xtranslate HSET([<x,...>])             => hb_HSET(<x>)
   #xtranslate HDEL([<x,...>])             => hb_HDEL(<x>)
   #xtranslate HGETKEYAT([<x,...>])        => hb_HKEYAT(<x>)
   #xtranslate HGETVALUEAT([<x,...>])      => hb_HVALUEAT(<x>)
   #xtranslate HSETVALUEAT([<x,...>])      => hb_HVALUEAT(<x>)
   #xtranslate HGETPAIRAT([<x,...>])       => hb_HPAIRAT(<x>)
   #xtranslate HDELAT([<x,...>])           => hb_HDELAT(<x>)
   #xtranslate HGETKEYS([<x,...>])         => hb_HKEYS(<x>)
   #xtranslate HGETVALUES([<x,...>])       => hb_HVALUES(<x>)
   #xtranslate HFILL([<x,...>])            => hb_HFILL(<x>)
   #xtranslate HCLONE([<x,...>])           => hb_HCLONE(<x>)
   #xtranslate HCOPY([<x,...>])            => hb_HCOPY(<x>)
   #xtranslate HMERGE([<x,...>])           => hb_HMERGE(<x>)
   #xtranslate HEVAL([<x,...>])            => hb_HEVAL(<x>)
   #xtranslate HSCAN([<x,...>])            => hb_HSCAN(<x>)
   #xtranslate HSETCASEMATCH([<x,...>])    => hb_HSETCASEMATCH(<x>)
   #xtranslate HGETCASEMATCH([<x,...>])    => hb_HCASEMATCH(<x>)
   #xtranslate HSETAUTOADD([<x,...>])      => hb_HSETAUTOADD(<x>)
   #xtranslate HGETAUTOADD([<x,...>])      => hb_HAUTOADD(<x>)
   #xtranslate HALLOCATE([<x,...>])        => hb_HALLOCATE(<x>)
   #xtranslate HDEFAULT([<x,...>])         => hb_HDEFAULT(<x>)

   /* Associative hash array functions */
   #xtranslate HAAGETKEYAT([<x,...>])      => hb_HKEYAT(<x>)
   #xtranslate HAAGETVALUEAT([<x,...>])    => hb_HVALUEAT(<x>)
   #xtranslate HAADELAT([<x,...>])         => hb_HDELAT(<x>)
   #xtranslate HAAGETPOS([<x,...>])        => hb_HPOS(<x>)
   #xtranslate HAAGETREALPOS(<x>,<y>)      => iif( hb_isNumeric( <y> ) .AND. <y> >= 1 .AND. ;
                                                   int( <y> ) <= len( <x> ), int( <y> ), 0 )
   #xtranslate HGETVAAPOS(<x>)             => {|h| ;;
                                                local a := array( len( h ), v ;;
                                                for each v in a ;;
                                                   v := v:__enumIndex() ;;
                                                next ;;
                                                return a ; }:eval( <x> )
   #xtranslate HGETAACOMPATIBILITY(<x>)    => hb_HKEEPORDER(<x>)
   #xtranslate HSETAACOMPATIBILITY([<x,...>]) => {|h| ;;
                                                   hb_HKEEPORDER( h ) ;;
                                                   return .T. ; }:eval( <x> )

   /* Inet functions */
   #xtranslate INETINIT([<x,...>])                => hb_INETINIT(<x>)
   #xtranslate INETCLEANUP([<x,...>])             => hb_INETCLEANUP(<x>)
   #xtranslate INETCREATE([<x,...>])              => hb_INETCREATE(<x>)
   #xtranslate INETCLOSE([<x,...>])               => hb_INETCLOSE(<x>)
   #xtranslate INETFD([<x,...>])                  => hb_INETFD(<x>)
   #xtranslate INETSTATUS([<x,...>])              => hb_INETSTATUS(<x>)
   #xtranslate INETERRORCODE([<x,...>])           => hb_INETERRORCODE(<x>)
   #xtranslate INETERRORDESC([<x,...>])           => hb_INETERRORDESC(<x>)
   #xtranslate INETCLEARERROR([<x,...>])          => hb_INETCLEARERROR(<x>)
   #xtranslate INETCOUNT([<x,...>])               => hb_INETCOUNT(<x>)
   #xtranslate INETADDRESS([<x,...>])             => hb_INETADDRESS(<x>)
   #xtranslate INETPORT([<x,...>])                => hb_INETPORT(<x>)
   #xtranslate INETSETTIMEOUT([<x,...>])          => hb_INETTIMEOUT(<x>)
   #xtranslate INETGETTIMEOUT([<x,...>])          => hb_INETTIMEOUT(<x>)
   #xtranslate INETCLEARTIMEOUT([<x,...>])        => hb_INETCLEARTIMEOUT(<x>)
   #xtranslate INETSETTIMELIMIT([<x,...>])        => hb_INETTIMELIMIT(<x>)
   #xtranslate INETGETTIMELIMIT([<x,...>])        => hb_INETTIMELIMIT(<x>)
   #xtranslate INETCLEARTIMELIMIT([<x,...>])      => hb_INETCLEARTIMELIMIT(<x>)
   #xtranslate INETSETPERIODCALLBACK([<x,...>])   => hb_INETPERIODCALLBACK(<x>)
   #xtranslate INETGETPERIODCALLBACK([<x,...>])   => hb_INETPERIODCALLBACK(<x>)
   #xtranslate INETCLEARPERIODCALLBACK([<x,...>]) => hb_INETCLEARPERIODCALLBACK(<x>)
   #xtranslate INETRECV([<x,...>])                => hb_INETRECV(<x>)
   #xtranslate INETRECVALL([<x,...>])             => hb_INETRECVALL(<x>)
   #xtranslate INETRECVLINE([<x,...>])            => hb_INETRECVLINE(<x>)
   #xtranslate INETRECVENDBLOCK([<x,...>])        => hb_INETRECVENDBLOCK(<x>)
   #xtranslate INETDATAREADY([<x,...>])           => hb_INETDATAREADY(<x>)
   #xtranslate INETSEND([<x,...>])                => hb_INETSEND(<x>)
   #xtranslate INETSENDALL([<x,...>])             => hb_INETSENDALL(<x>)
   #xtranslate INETGETHOSTS([<x,...>])            => hb_INETGETHOSTS(<x>)
   #xtranslate INETGETALIAS([<x,...>])            => hb_INETGETALIAS(<x>)
   #xtranslate INETSERVER([<x,...>])              => hb_INETSERVER(<x>)
   #xtranslate INETACCEPT([<x,...>])              => hb_INETACCEPT(<x>)
   #xtranslate INETCONNECT([<x,...>])             => hb_INETCONNECT(<x>)
   #xtranslate INETCONNECTIP([<x,...>])           => hb_INETCONNECTIP(<x>)
   #xtranslate INETDGRAMBIND([<x,...>])           => hb_INETDGRAMBIND(<x>)
   #xtranslate INETDGRAM([<x,...>])               => hb_INETDGRAM(<x>)
   #xtranslate INETDGRAMSEND([<x,...>])           => hb_INETDGRAMSEND(<x>)
   #xtranslate INETDGRAMRECV([<x,...>])           => hb_INETDGRAMRECV(<x>)
   #xtranslate INETCRLF([<x,...>])                => hb_INETCRLF(<x>)
   #xtranslate ISINETSOCKET([<x,...>])            => hb_INETISSOCKET(<x>)
   #xtranslate INETDESTROY([<x,...>])             => iif( HB_INETISSOCKET( <x> ), hb_INETCLOSE( <x> ), )

   /* THROW => generate error */
   #xtranslate THROW(<oErr>) => (Eval(ErrorBlock(), <oErr>), Break(<oErr>))

#endif

#endif /* __HARBOUR__ */
