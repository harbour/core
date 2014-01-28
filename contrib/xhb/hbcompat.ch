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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

   #if defined( __PLATFORM__Windows ) .AND. ! defined( __PLATFORM__WINDOWS )
      #define __PLATFORM__WINDOWS
   #endif
   #if defined( __PLATFORM__Linux ) .AND. ! defined( __PLATFORM__LINUX )
      #define __PLATFORM__LINUX
   #endif

   #xtranslate hb_gtInfo( HB_GTI_INKEYREAD [, <x>] )   => SetInkeyBeforeBlock( [<x>] ) <-x->
   #xtranslate hb_gtInfo( HB_GTI_INKEYFILTER [, <x>] ) => SetInkeyAfterBlock( [<x>] ) <-x->

   #xtranslate hb_ScrMaxRow()                  => gtInfo( HB_GTI_SCREENHEIGHT )
   #xtranslate hb_ScrMaxCol()                  => gtInfo( HB_GTI_SCREENWIDTH )
   #xtranslate MaxRow( .T. )                   => gtInfo( HB_GTI_SCREENHEIGHT )
   #xtranslate MaxCol( .T. )                   => gtInfo( HB_GTI_SCREENWIDTH )
   #xtranslate hb_keyNext( [<x>] )             => NextKey( <x> )

   #xtranslate hb_osNewLine()                  => hb_eol()
   #xtranslate hb_osPathSeparator()            => hb_ps()

   #xtranslate hb_dbPack()                     => __dbPack()
   #xtranslate hb_dbZap()                      => __dbZap()
   #xtranslate hb_dbDrop( [<x,...>] )          => dbDrop( <x> )
   #xtranslate hb_dbExists( [<x,...>] )        => dbExists( <x> )
   #xtranslate hb_FieldLen( [<x>] )            => FieldLen( <x> )
   #xtranslate hb_FieldDec( [<x>] )            => FieldDec( <x> )
   #xtranslate hb_FieldType( [<x>] )           => FieldType( <x> )

   #xtranslate hb_processOpen( [<x,...>] )     => hb_OpenProcess( <x> )
   #xtranslate hb_processClose( [<x,...>] )    => hb_CloseProcess( <x> )

   #xtranslate hb_IsRegex( [<x>] )             => hb_IsRegexString( <x> )
   #xtranslate hb_MethodName( [<x,...>] )      => MethodName( <x> )
   #xtranslate hb_libLoad( [<x,...>] )         => LibLoad( <x> )
   #xtranslate hb_libFree( [<x,...>] )         => LibFree( <x> )
   #xtranslate hb_Adler32( [<x,...>] )         => hb_Checksum( <x> )
   #xtranslate hb_keySetLast( [<x,...>] )      => hb_SetLastKey( <x> )
   #xtranslate hb_CStr( [<x,...>] )            => CStr( <x> )
   #xtranslate hb_ValToExp( [<x,...>] )        => ValToPrgExp( <x> )
   #xtranslate hb_rddInfo( [<x,...>] )         => rddInfo( <x> )
   #xtranslate hb_idleSleep( [<x,...>] )       => SecondsSleep( <x> )
   #xtranslate hb_UserName()                   => NetName( 1 )
   #xtranslate hb_FSize( <x> )                 => FileSize( <x> )
   #xtranslate hb_WildMatch( [<x,...>] )       => WildMatch( <x> )
   #xtranslate hb_bitTest( [<x,...>] )         => hb_bitIsSet( <x> )
   #xtranslate hb_Deserialize( <x> )           => hb_DeserialNext( <x> )

   #xtranslate hb_HexToNum( [<c,...>] )        => HexToNum( <c> )
   #xtranslate hb_NumToHex( [<n,...>] )        => NumToHex( <n> )
   #xtranslate hb_HexToStr( [<c,...>] )        => HexToStr( <c> )
   #xtranslate hb_StrToHex( [<c,...>] )        => StrToHex( <c> )

   #xtranslate hb_AScan( [<x,...>] )           => AScan( <x> )
   #xtranslate hb_RAScan( [<x,...>] )          => RAScan( <x> )
   #xtranslate hb_AIns( [<x,...>] )            => AIns( <x> )
   #xtranslate hb_ADel( [<x,...>] )            => ADel( <x> )
   #xtranslate hb_At( [<x,...>] )              => At( <x> )

   #xtranslate hb_DateTime( [<x,...>] )        => DateTime( <x> )
   #xtranslate hb_Hour( [<x>] )                => Hour( <x> )
   #xtranslate hb_Minute( [<x>] )              => Minute( <x> )
   #xtranslate hb_TToS( [<x>] )                => TToS( <x> )
   #xtranslate hb_SToT( [<x>] )                => SToT( <x> )
   #xtranslate hb_TToC( [<x,...>] )            => TToC( <x> )
   #xtranslate hb_CToT( [<x,...>] )            => CToT( <x> )

   #xtranslate hb_GetEnv( [<x,...>] )          => GetEnv( <x> )
   #xtranslate hb_SetKey( [<x,...>] )          => SetKey( <x> )

   #xtranslate hb_i18n_gettext( <x> )          => i18n( <x> )

   #xtranslate hb_cdpSelect( [<x,...>] )       => hb_SetCodepage( <x> )

   #xtranslate hb_argv( [<x,...>] )            => hb_CmdArgArgV( <x> )

   #xtranslate hb_iniSetComment( [<x,...>] )   => hb_SetIniComment( <x> )
   #xtranslate hb_iniRead( [<x,...>] )         => hb_ReadIni( <x> )
   #xtranslate hb_iniWrite( [<x,...>] )        => hb_WriteIni( <x> )

   #xtranslate hb_DisableWaitLocks( [<x>] )    => DisableWaitLocks( <x> )

   #xtranslate hb_gtLock()                     => hbConsoleLock()
   #xtranslate hb_gtUnlock()                   => hbConsoleUnlock()

   /* MT functions */
   #xtranslate hb_mtvm()                       => hb_MultiThread()
   #xtranslate hb_threadSelf()                 => GetCurrentThread()
   #xtranslate hb_threadID( [<x,...>] )        => GetThreadId( <x> )
   #xtranslate hb_threadStart( <x,...> )       => StartThread( [<x>] )
   #xtranslate hb_threadJoin( <x> )            => JoinThread( <x> )
   #xtranslate hb_threadQuitRequest( <x> )     => KillThread( <x> )
   #xtranslate hb_threadWaitForAll()           => WaitForThreads()
   #xtranslate hb_threadTerminateAll()         => KillAllThreads()

   #xtranslate hb_mutexNotify( <x,...> )       => Notify( <x> )
   #xtranslate hb_mutexNotifyAll( <x,...> )    => NotifyAll( <x> )

   #xtranslate hb_mutexSubscribe( <x,...> )    => {| mtx, nTimeOut, xSubscribed | ;;
                                                  LOCAL lSubscribed ;;
                                                  xSubscribed := Subscribe( mtx, ;
                                                                            iif( HB_ISNUMERIC( nTimeOut ), nTimeOut * 1000, ), ;
                                                                            @lSubscribed ) ;
                                                  RETURN lSubscribed ; }:eval( <x> )
   #xtranslate hb_mutexSubscribeNow( <x,...> ) => {| mtx, nTimeOut, xSubscribed | ;;
                                                  LOCAL lSubscribed ;;
                                                  xSubscribed := SubscribeNow( mtx, ;
                                                                               iif( HB_ISNUMERIC( nTimeOut ), nTimeOut * 1000, ), ;
                                                                               @lSubscribed ) ;
                                                  RETURN lSubscribed ; }:eval( <x> )

   #xtranslate hb_mutexLock( <x>, <n> )        => iif( ! HB_ISNUMERIC( <n> ), hb_mutexLock( <x> ) ;
                                                     iif( <n> <= 0, hb_MutexTryLock( <x> ), ;
                                                        hb_MutexTimeOutLock( <x>, <n> ) ) )

   /* Hash item functions */
   #xtranslate hb_Hash( [<x,...>] )            => Hash( <x> )
   #xtranslate hb_HHasKey( <x>, <y> )          => HHasKey( <x>, <y> )
   #xtranslate hb_HPos( [<x,...>] )            => HGetPos( <x> )
   #xtranslate hb_HGet( [<x,...>] )            => HGet( <x> )
   #xtranslate hb_HSet( [<x,...>] )            => HSet( <x> )
   #xtranslate hb_HDel( [<x,...>] )            => HDel( <x> )
   #xtranslate hb_HKeyAt( [<x,...>] )          => HGetKeyAt( <x> )
   #xtranslate hb_HValueAt( [<x,...>] )        => HGetValueAt( <x> )
   #xtranslate hb_HValueAt( [<x,...>] )        => HSetValueAt( <x> )
   #xtranslate hb_HPairAt( [<x,...>] )         => HGetPairAt( <x> )
   #xtranslate hb_HDelAt( [<x,...>] )          => HDelAt( <x> )
   #xtranslate hb_HKeys( [<x,...>] )           => HGetKeys( <x> )
   #xtranslate hb_HValues( [<x,...>] )         => HGetValues( <x> )
   #xtranslate hb_HFill( [<x,...>] )           => HFill( <x> )
   #xtranslate hb_HClone( [<x,...>] )          => HClone( <x> )
   #xtranslate hb_HCopy( [<x,...>] )           => HCopy( <x> )
   #xtranslate hb_HMerge( [<x,...>] )          => HMerge( <x> )
   #xtranslate hb_HEval( [<x,...>] )           => HEval( <x> )
   #xtranslate hb_HScan( [<x,...>] )           => HScan( <x> )
   #xtranslate hb_HCaseMatch( [<x,...>] )      => HGetCaseMatch( <x> )
   #xtranslate hb_HAutoAdd( [<x,...>] )        => HGetAutoAdd( <x> )
   #xtranslate hb_HAllocate( [<x,...>] )       => HAllocate( <x> )
   #xtranslate hb_HDefault( [<x,...>] )        => HDefault( <x> )
   #if defined( HB_LEGACY_LEVEL5 )
   #xtranslate hb_HSetCaseMatch( [<x,...>] )   => HSetCaseMatch( <x> )
   #xtranslate hb_HSetAutoAdd( [<x,...>] )     => HSetAutoAdd( <x> )
   #endif

   /* Inet functions */
   #xtranslate hb_inetInit( [<x,...>] )                => inetInit( <x> )
   #xtranslate hb_inetCleanup( [<x,...>] )             => inetCleanup( <x> )
   #xtranslate hb_inetCreate( [<x,...>] )              => inetCreate( <x> )
   #xtranslate hb_inetClose( [<x,...>] )               => inetClose( <x> )
   #xtranslate hb_inetFD( [<x,...>] )                  => inetFD( <x> )
   #xtranslate hb_inetstatus( [<x,...>] )              => inetStatus( <x> )
   #xtranslate hb_inetErrorCode( [<x,...>] )           => inetErrorCode( <x> )
   #xtranslate hb_inetErrorDesc( [<x,...>] )           => inetErrorDesc( <x> )
   #xtranslate hb_inetClearError( [<x,...>] )          => inetClearError( <x> )
   #xtranslate hb_inetCount( [<x,...>] )               => inetCount( <x> )
   #xtranslate hb_inetAddress( [<x,...>] )             => inetAddress( <x> )
   #xtranslate hb_inetPort( [<x,...>] )                => inetPort( <x> )
   #xtranslate hb_inetTimeout( <x>, <y> )              => inetSetTimeout( <x>, <y> )
   #xtranslate hb_inetTimeout( <x> )                   => inetGetTimeout( <x> )
   #xtranslate hb_inetClearTimeout( [<x,...>] )        => inetClearTimeout( <x> )
   #xtranslate hb_inetTimeLimit( <x>, <y> )            => inetSetTimeLimit( <x>, <y> )
   #xtranslate hb_inetTimeLimit( <x> )                 => inetGetTimeLimit( <x> )
   #xtranslate hb_inetClearTimeLimit( [<x,...>] )      => inetClearTimeLimit( <x> )
   #xtranslate hb_inetPeriodCallback( <x>, <y> )       => inetSetPeriodCallback( <x>, <y> )
   #xtranslate hb_inetPeriodCallback( <x> )            => inetGetPeriodCallback( <x> )
   #xtranslate hb_inetClearPeriodCallback( [<x,...>] ) => inetClearPeriodCallback( <x> )
   #xtranslate hb_inetRecv( [<x,...>] )                => inetRecv( <x> )
   #xtranslate hb_inetRecvAll( [<x,...>] )             => inetRecvAll( <x> )
   #xtranslate hb_inetRecvLine( [<x,...>] )            => inetRecvLine( <x> )
   #xtranslate hb_inetRecvEndblock( [<x,...>] )        => inetRecvEndBlock( <x> )
   #xtranslate hb_inetDataReady( [<x,...>] )           => inetDataReady( <x> )
   #xtranslate hb_inetSend( [<x,...>] )                => inetSend( <x> )
   #xtranslate hb_inetSendAll( [<x,...>] )             => inetSendAll( <x> )
   #xtranslate hb_inetGetHosts( [<x,...>] )            => inetGetHosts( <x> )
   #xtranslate hb_inetGetAlias( [<x,...>] )            => inetGetAlias( <x> )
   #xtranslate hb_inetServer( [<x,...>] )              => inetServer( <x> )
   #xtranslate hb_inetAccept( [<x,...>] )              => inetAccept( <x> )
   #xtranslate hb_inetConnect( [<x,...>] )             => inetConnect( <x> )
   #xtranslate hb_inetConnectIP( [<x,...>] )           => inetConnectIP( <x> )
   #xtranslate hb_inetDGramBind( [<x,...>] )           => inetDGramBind( <x> )
   #xtranslate hb_inetDGram( [<x,...>] )               => inetDGram( <x> )
   #xtranslate hb_inetDGramSend( [<x,...>] )           => inetDGramSend( <x> )
   #xtranslate hb_inetDGramRecv( [<x,...>] )           => inetDGramRecv( <x> )
   #xtranslate hb_inetCRLF( [<x,...>] )                => inetCRLF( <x> )
   #xtranslate hb_inetIsSocket( [<x,...>] )            => inetIsSocket( <x> )
   #xtranslate hb_inetClose( [<x,...>] )               => inetDestroy( <x> )

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

   #if defined( __PLATFORM__WINDOWS ) .AND. ! defined( __PLATFORM__Windows )
      #define __PLATFORM__Windows
   #endif
   #if defined( __PLATFORM__LINUX ) .AND. ! defined( __PLATFORM__Linux )
      #define __PLATFORM__Linux
   #endif

   /* these are used _by_ MaxRow()/MaxCol() */
   #define GTI_WINDOW         0  /* Maximum window size ('window' in CT terms) */
   #define GTI_SCREEN         1  /* Maximum screen size ('Screen' in CT terms) */
   #define GTI_CLIENT         2  /* Maximum possible client size of a window */
   #define GTI_MAX            3  /* Maximum possible window size (in Windows) */

   #xtranslate hb_eol()                        => hb_osNewLine()
   #xtranslate hb_ps()                         => hb_osPathSeparator()

   #xtranslate MaxRow( .T. )                   => hb_gtInfo( HB_GTI_VIEWPORTHEIGHT )
   #xtranslate MaxCol( .T. )                   => hb_gtInfo( HB_GTI_VIEWPORTWIDTH )
   #xtranslate NextKey( [<x>] )                => hb_keyNext( <x> )

   #xtranslate Str( <x>, [<y>], [<y>], <z> )   => iif( <z>, hb_ntos( <x> ), Str( <x> ) )

   #xuntranslate NetName(                      =>
   #xuntranslate MemoWrit(                     =>

   #xtranslate NetName( <n> )                  => iif( HB_ISNUMERIC( <n> ) .AND. <n> == 1, hb_UserName(), NetName() )
   #xtranslate MemoWrit( <x>, <y>, <z> )       => iif( HB_ISLOGICAL( <z> ) .AND. ! <z>, hb_MemoWrit( <x>, <y> ), MemoWrit( <x>, <y> ) )

   #xuntranslate AIns(                         =>
   #xuntranslate ADel(                         =>

   #xtranslate AIns( <a>, <n>, [<x,...>] )     => hb_AIns( <a>, <n>, <x> )
   #xtranslate ADel( <a>, <n>, <l> )           => hb_ADel( <a>, <n>, <l> )

   #xtranslate AScan(<a>,<b>,[<c>],[<d>],<e>)  => hb_AScan( <a>, <b>, <c>, <d>, <e> )
   #xtranslate At( <a>, <b>, [<x,...>] )       => hb_At( <a>, <b>, <x> )

   #xtranslate GetEnv( [<x,...>] )             => hb_GetEnv( <x> )
   #xtranslate SetKey( [<x,...>] )             => hb_SetKey( <x> )

   /* TEXT INTO <varname> */
   #xcommand TEXT INTO <v> => #pragma __text|<v>+=%s+hb_eol();<v>:=""

   /* SWITCH ... ; case ... ; DEFAULT ; ... ; END */
   #xcommand DEFAULT => OTHERWISE

   /* FOR EACH hb_enumIndex() */
   #xtranslate hb_enumIndex( <!v!> ) => <v>:__enumIndex()

   /* TRY / CATCH / FINALLY / END */
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
   #xcommand FINALLY => ALWAYS

   /* EXTENDED CODEBLOCKs */
   #xtranslate \<|[<x,...>]| => {| <x> |
   #xcommand > [<*x*>]       => } <x>

   /* xHarbour operators: IN, HAS, LIKE, >>, <<, |, &, ^^ */
   #translate ( <exp1> IN <exp2> )     => ( ( <exp1> ) $ ( <exp2> ) )
   #translate ( <exp1> HAS <exp2> )    => hb_regexHas( <exp2>, <exp1> )
   #translate ( <exp1> LIKE <exp2> )   => hb_regexLike( <exp2>, <exp1> )
   #translate ( <exp1> \<\< <exp2> )   => hb_bitShift( <exp1>, <exp2> )
   #translate ( <exp1> >> <exp2> )     => hb_bitShift( <exp1>, -( <exp2> ) )
   /* NOTE: These macros can break some valid Harbour/Clipper constructs,
            so they are disabled by default. Enable them with care, or
            even better to switch to use HB_BIT*() functions directly.
            They are optimized by Harbour compiler the same way (and even
            more) as these C-like operators, without any bad side-effects. */
   #if defined( XHB_BITOP )
      #translate ( <exp1> | <exp2> )      => hb_bitOr( <exp1>, <exp2> )
      #translate ( <exp1> & <exp2> )      => hb_bitAnd( <exp1>, <exp2> )
      #translate ( <exp1> ^^ <exp2> )     => hb_bitXor( <exp1>, <exp2> )
   #endif

   #command @ <row>, <col> PROMPT <prompt> [ MESSAGE <msg> ] [ COLOR <color> ] => ;
      __AtPrompt( <row>, <col>, <prompt>, <msg>, <color> )

   #command SET TRIMFILENAME <x:ON,OFF,&> => Set( _SET_TRIMFILENAME, <(x)> )
   #command SET TIME FORMAT [TO] <f>      => Set( _SET_TIMEFORMAT, <f> )

   #define HB_GTI_CLIPBOARDPAST HB_GTI_CLIPBOARDPASTE

   /* These also have wrapper function in xhb lib */

   #xtranslate gtSetClipboard( <x> )           => hb_gtInfo( HB_GTI_CLIPBOARDDATA, <x> )
   #xtranslate gtGetClipboard()                => hb_gtInfo( HB_GTI_CLIPBOARDDATA )
   #xtranslate gtGetClipboardSize()            => Len( hb_gtInfo( HB_GTI_CLIPBOARDDATA ) )
   #xtranslate gtPasteClipboard()              => hb_gtInfo( HB_GTI_CLIPBOARDPASTE )
   #xtranslate gtProcessMessages()             => NextKey()
   #xtranslate SetInkeyBeforeBlock( [<x>] )    => hb_gtInfo( HB_GTI_INKEYREAD [, <x>] ) <-x->
   #xtranslate SetInkeyAfterBlock( [<x>] )     => hb_gtInfo( HB_GTI_INKEYFILTER [, <x>] ) <-x->
   #xtranslate GfxPrimitive( [<x,...>] )       => hb_gfxPrimitive( <x> )
   #xtranslate GfxText( [<x,...>] )            => hb_gfxText( <x> )

   #xtranslate hb_OpenProcess( [<x,...>] )     => hb_processOpen( <x> )
   #xtranslate hb_CloseProcess( [<x,...>] )    => hb_processClose( <x> )

   #xtranslate hb_IsRegexString( [<x>] )       => hb_IsRegex( <x> )
   #xtranslate MethodName( [<x,...>] )         => hb_MethodName( <x> )
   #xtranslate LibLoad( [<x,...>] )            => hb_libLoad( <x> )
   #xtranslate LibFree( [<x,...>] )            => hb_libFree( <x> )
   #xtranslate hb_Checksum( [<x,...>] )        => hb_Adler32( <x> )
   #xtranslate hb_SetLastKey( [<x,...>] )      => hb_keySetLast( <x> )
   #xtranslate CStr( [<x,...>] )               => hb_CStr( <x> )
   #xtranslate ValToPrgExp( [<x,...>] )        => hb_ValToExp( <x> )
   #xtranslate SecondsSleep( [<x,...>] )       => hb_idleSleep( <x> )
   #xtranslate WildMatch( [<x,...>] )          => hb_WildMatch( <x> )
   #xtranslate hb_bitIsSet( [<x,...>] )        => hb_bitTest( <x> )
   #xtranslate hb_DeserialNext( <x> )          => hb_Deserialize( <x> )
   #xtranslate hb_FuncPtr( <x> )               => __dynsN2Sym( <x> )

   #xtranslate HexToNum( [<c,...>] )           => hb_HexToNum( <c> )
   #xtranslate NumToHex( [<n,...>] )           => hb_NumToHex( <n> )
   #xtranslate HexToStr( [<c,...>] )           => hb_HexToStr( <c> )
   #xtranslate StrToHex( [<c,...>] )           => hb_StrToHex( <c> )

   #xtranslate IsPointer( <xValue> )           => HB_ISPOINTER( <xValue> )

   #xtranslate hb_SetIniComment( [<x,...>] )   => hb_iniSetComment( <x> )
   #xtranslate hb_ReadIni( [<x,...>] )         => hb_iniRead( <x> )
   #xtranslate hb_WriteIni( [<x,...>] )        => hb_iniWrite( <x> )

   #xtranslate DisableWaitLocks( [<x>] )       => hb_DisableWaitLocks( <x> )

   #xtranslate hbConsoleLock()                 => hb_gtLock()
   #xtranslate hbConsoleUnlock()               => hb_gtUnlock()

   #xtranslate hb_CmdArgArgV( [<x,...>] )      => hb_argv( <x> )

   #xtranslate RAScan( [<x,...>] )             => hb_RAScan( <x> )

   #xtranslate ASizeAlloc( <a> [, <n,...>] )   => AFill( <a> )
   #xtranslate ALenAlloc( <a> [, <n,...>] )    => Len( <a> )

   #xtranslate DateTime( [<x,...>] )           => hb_DateTime( <x> )
   #xtranslate Hour( [<x>] )                   => hb_Hour( <x> )
   #xtranslate Minute( [<x>] )                 => hb_Minute( <x> )
   #xtranslate TToS( [<x>] )                   => hb_TToS( <x> )
   #xtranslate SToT( [<x>] )                   => hb_SToT( <x> )
   #xtranslate TToC( [<x,...>] )               => hb_TToC( <x> )
   #xtranslate CToT( [<x,...>] )               => hb_CToT( <x> )

   #xtranslate i18n( <x> )                     => hb_i18n_gettext( <x> )

   #xtranslate hb_SetCodepage( [<x,...>] )     => hb_cdpSelect( <x> )

   /* MT functions */
   #xtranslate hb_MultiThread()                => hb_mtvm()
   #xtranslate GetCurrentThread()              => hb_threadSelf()
   #xtranslate GetThreadId( [<x,...>] )        => hb_threadID( <x> )
   #xtranslate ThreadGetCurrentInternal()      => hb_threadID()
   #xtranslate IsSameThread( <x> [,<y>] )      => ( hb_threadID( <x> ) == hb_threadID( <y> ) )
   #xtranslate IsValidThread( <x> )            => ( hb_threadID( <x> ) != 0 )
   #xtranslate JoinThread( <x> )               => hb_threadJoin( <x> )
   #xtranslate KillThread( <x> )               => hb_threadQuitRequest( <x> )
   #xtranslate StopThread( <x> )               => hb_threadQuitRequest( <x> ); hb_threadJoin( <x> )
   #xtranslate KillAllThreads()                => hb_threadTerminateAll()
   #xtranslate WaitForThreads()                => hb_threadWaitForAll()

   #xtranslate ThreadSleep( <x> )              => hb_idleSleep( <x> / 1000 )

   #xtranslate DestroyMutex( <x> )             =>
   #xtranslate hb_MutexTryLock( <x> )          => hb_mutexLock( <x>, 0 )
   #xtranslate hb_MutexTimeOutLock( <x> )      => hb_mutexLock( <x>, 0 )
   #xtranslate hb_MutexTimeOutLock( <x>, <n> ) => hb_mutexLock( <x>, iif( HB_ISNUMERIC( <n> ), <n> / 1000, 0 ) )

   #xtranslate Notify( <x,...> )               => hb_mutexNotify( <x> )
   #xtranslate NotifyAll( <x,...> )            => hb_mutexNotifyAll( <x> )
   #xtranslate Subscribe( <x,...> )            => {| mtx, nTimeOut, lSubscribed | ;;
                                                   local xSubscribed ;;
                                                   lSubscribed := hb_mutexSubscribe( mtx, ;
                                                                                     iif( HB_ISNUMERIC( nTimeOut ), nTimeOut / 1000, ), ;
                                                                                     @xSubscribed ) ;
                                                   return xSubscribed ; }:eval( <x> )
   #xtranslate SubscribeNow( <x,...> )         => {| mtx, nTimeOut, lSubscribed | ;;
                                                   local xSubscribed ;;
                                                   lSubscribed := hb_mutexSubscribeNow( mtx, ;
                                                                                        iif( HB_ISNUMERIC( nTimeOut ), nTimeOut / 1000, ), ;
                                                                                        @xSubscribed ) ;
                                                   return xSubscribed ; }:eval( <x> )

   #xtranslate StartThread( [<x>] )            => hb_threadStart( <x> )
   #xtranslate StartThread( <x>, <y> [, <z,...>] ) => iif( HB_ISOBJECT( <x> ) .AND. HB_ISSTRING( <y> ), ;
                                                           hb_threadStart( {| ... | ( <x> ):&( <y> )( ... ) } [, <z>] ), ;
                                                           hb_threadStart( <x>, <y> [, <z>] ) )

   /* not possible to well replicate xHarbour behavior because it's buggy
      these function results are different on different platform, chosen
      translation which returns compatible types (numeric) */
   #xtranslate ThreadGetCurrent()              => hb_threadID()
   #xtranslate GetSystemThreadId( [<x,...>] )  => hb_threadID( <x> )

   /* do not need translation */
   /* hb_mutexCreate()                         => hb_mutexCreate() */
   /* hb_mutexUnlock( <x> )                    => hb_mutexUnlock( <x> ) */

   /* do not need translation only when xHarbour code is compiled by Harbour */
   /* hb_mutexLock( <x> )                      => hb_mutexLock( <x> ) */

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
   #xtranslate Hash( [<x,...>] )           => hb_Hash( <x> )
   #xtranslate HHasKey( [<x,...>] )        => hb_HHasKey( <x> )
   #xtranslate HGetPos( [<x,...>] )        => hb_HPos( <x> )
   #xtranslate HGet( [<x,...>] )           => hb_HGet( <x> )
   #xtranslate HSet( [<x,...>] )           => hb_HSet( <x> )
   #xtranslate HDel( [<x,...>] )           => hb_HDel( <x> )
   #xtranslate HGetKeyAt( [<x,...>] )      => hb_HKeyAt( <x> )
   #xtranslate HGetValueAt( [<x,...>] )    => hb_HValueAt( <x> )
   #xtranslate HSetValueAt( [<x,...>] )    => hb_HValueAt( <x> )
   #xtranslate HGetPairAt( [<x,...>] )     => hb_HPairAt( <x> )
   #xtranslate HDelAt( [<x,...>] )         => hb_HDelAt( <x> )
   #xtranslate HGetKeys( [<x,...>] )       => hb_HKeys( <x> )
   #xtranslate HGetValues( [<x,...>] )     => hb_HValues( <x> )
   #xtranslate HFill( [<x,...>] )          => hb_HFill( <x> )
   #xtranslate HClone( [<x,...>] )         => hb_HClone( <x> )
   #xtranslate HCopy( [<x,...>] )          => hb_HCopy( <x> )
   #xtranslate HMerge( [<x,...>] )         => hb_HMerge( <x> )
   #xtranslate HEval( [<x,...>] )          => hb_HEval( <x> )
   #xtranslate HScan( [<x,...>] )          => hb_HScan( <x> )
   #xtranslate HSetCaseMatch( <x>[, <z>] ) => ( hb_HCaseMatch( <x>[, <z>] ), <x> )
   #xtranslate HGetCaseMatch( [<x,...>] )  => hb_HCaseMatch( <x> )
   #xtranslate HSetAutoAdd( <x>[, <z>] )   => ( hb_HAutoAdd( <x>[, <z>] ), <x> )
   #xtranslate HGetAutoAdd( [<x,...>] )    => hb_HAutoAdd( <x> )
   #xtranslate HAllocate( [<x,...>] )      => hb_HAllocate( <x> )
   #xtranslate HDefault( [<x,...>] )       => hb_HDefault( <x> )
   #xtranslate HSetPartition( [<x,...>] )  =>

   /* Associative hash array functions */
   #xtranslate haAGetKeyAt( [<x,...>] )    => hb_HKeyAt( <x> )
   #xtranslate haAGetValueAt( [<x,...>] )  => hb_HValueAt( <x> )
   #xtranslate haADelAt( [<x,...>] )       => hb_HDelAt( <x> )
   #xtranslate haAGetPos( [<x,...>] )      => hb_HPos( <x> )
   #xtranslate haAGetRealPos( <x>, <y> )   => iif( HB_ISNUMERIC( <y> ) .AND. <y> >= 1 .AND. ;
                                                   Int( <y> ) <= Len( <x> ), Int( <y> ), 0 )
   #xtranslate HGetVAAPos( <x> )           => {| h | ;;
                                                LOCAL a := Array( Len( h ), v ;;
                                                FOR EACH v IN a ;;
                                                   v := v:__enumIndex() ;;
                                                NEXT ;;
                                                RETURN a ; }:eval( <x> )
   #xtranslate HGetAACompatibility( <x> )  => hb_HKeepOrder( <x> )
   #xtranslate HSetAACompatibility( [<x,...>] ) => {| h | ;;
                                                   hb_HKeepOrder( h ) ;;
                                                   RETURN .T. ; }:eval( <x> )

   /* Inet functions */
   #xtranslate inetInit( [<x,...>] )                => hb_inetInit( <x> )
   #xtranslate inetCleanup( [<x,...>] )             => hb_inetCleanup( <x> )
   #xtranslate inetCreate( [<x,...>] )              => hb_inetCreate( <x> )
   #xtranslate inetClose( [<x,...>] )               => hb_inetClose( <x> )
   #xtranslate inetFD( [<x,...>] )                  => hb_inetFD( <x> )
   #xtranslate inetStatus( [<x,...>] )              => hb_inetstatus( <x> )
   #xtranslate inetErrorCode( [<x,...>] )           => hb_inetErrorCode( <x> )
   #xtranslate inetErrorDesc( [<x,...>] )           => hb_inetErrorDesc( <x> )
   #xtranslate inetClearError( [<x,...>] )          => hb_inetClearError( <x> )
   #xtranslate inetCount( [<x,...>] )               => hb_inetCount( <x> )
   #xtranslate inetAddress( [<x,...>] )             => hb_inetAddress( <x> )
   #xtranslate inetPort( [<x,...>] )                => hb_inetPort( <x> )
   #xtranslate inetSetTimeout( [<x,...>] )          => hb_inetTimeout( <x> )
   #xtranslate inetGetTimeout( [<x,...>] )          => hb_inetTimeout( <x> )
   #xtranslate inetClearTimeout( [<x,...>] )        => hb_inetClearTimeout( <x> )
   #xtranslate inetSetTimeLimit( [<x,...>] )        => hb_inetTimeLimit( <x> )
   #xtranslate inetGetTimeLimit( [<x,...>] )        => hb_inetTimeLimit( <x> )
   #xtranslate inetClearTimeLimit( [<x,...>] )      => hb_inetClearTimeLimit( <x> )
   #xtranslate inetSetPeriodCallback( [<x,...>] )   => hb_inetPeriodCallback( <x> )
   #xtranslate inetGetPeriodCallback( [<x,...>] )   => hb_inetPeriodCallback( <x> )
   #xtranslate inetClearPeriodCallback( [<x,...>] ) => hb_inetClearPeriodCallback( <x> )
   #xtranslate inetRecv( [<x,...>] )                => hb_inetRecv( <x> )
   #xtranslate inetRecvAll( [<x,...>] )             => hb_inetRecvAll( <x> )
   #xtranslate inetRecvLine( [<x,...>] )            => hb_inetRecvLine( <x> )
   #xtranslate inetRecvEndBlock( [<x,...>] )        => hb_inetRecvEndblock( <x> )
   #xtranslate inetDataReady( [<x,...>] )           => hb_inetDataReady( <x> )
   #xtranslate inetSend( [<x,...>] )                => hb_inetSend( <x> )
   #xtranslate inetSendAll( [<x,...>] )             => hb_inetSendAll( <x> )
   #xtranslate inetGetHosts( [<x,...>] )            => hb_inetGetHosts( <x> )
   #xtranslate inetGetAlias( [<x,...>] )            => hb_inetGetAlias( <x> )
   #xtranslate inetServer( [<x,...>] )              => hb_inetServer( <x> )
   #xtranslate inetAccept( [<x,...>] )              => hb_inetAccept( <x> )
   #xtranslate inetConnect( [<x,...>] )             => hb_inetConnect( <x> )
   #xtranslate inetConnectIP( [<x,...>] )           => hb_inetConnectIP( <x> )
   #xtranslate inetDGramBind( [<x,...>] )           => hb_inetDGramBind( <x> )
   #xtranslate inetDGram( [<x,...>] )               => hb_inetDGram( <x> )
   #xtranslate inetDGramSend( [<x,...>] )           => hb_inetDGramSend( <x> )
   #xtranslate inetDGramRecv( [<x,...>] )           => hb_inetDGramRecv( <x> )
   #xtranslate inetCRLF( [<x,...>] )                => hb_inetCRLF( <x> )
   #xtranslate inetIsSocket( [<x,...>] )            => hb_inetIsSocket( <x> )
   #xtranslate inetDestroy( [<x,...>] )             => iif( hb_inetIsSocket( <x> ), hb_inetClose( <x> ), )

   /* Throw() => generate error */
   #xtranslate Throw( <oErr> ) => ( Eval( ErrorBlock(), <oErr> ), Break( <oErr> ) )

#endif

#endif /* __HARBOUR__ */
