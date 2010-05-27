/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * xhb compatibility wrappers.
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
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

#include "common.ch"

#include "hbgtinfo.ch"

FUNCTION gtSetClipboard( x )            ; RETURN hb_gtInfo( HB_GTI_CLIPBOARDDATA, x )
FUNCTION gtGetClipboard()               ; RETURN hb_gtInfo( HB_GTI_CLIPBOARDDATA )
FUNCTION gtGetClipBoardSize()           ; RETURN Len( hb_gtInfo( HB_GTI_CLIPBOARDDATA ) )
FUNCTION gtPasteClipBoard()             ; RETURN hb_gtInfo( HB_GTI_CLIPBOARDPASTE )
FUNCTION gtProcessMessages()            ; RETURN NextKey()
FUNCTION gfxPrimitive( ... )            ; RETURN hb_gfxPrimitive( ... )
FUNCTION gfxText( ... )                 ; RETURN hb_gfxText( ... )

FUNCTION hb_openProcess( ... )          ; RETURN hb_processOpen( ... )
FUNCTION hb_closeProcess( ... )         ; RETURN hb_processClose( ... )

FUNCTION hb_isregexstring( x )          ; RETURN hb_isregex( x )
FUNCTION pvalue( ... )                  ; RETURN hb_pvalue( ... )
FUNCTION methodName( ... )              ; RETURN hb_methodName( ... )
FUNCTION libLoad( ... )                 ; RETURN hb_libLoad( ... )
FUNCTION libFree( ... )                 ; RETURN hb_libFree( ... )
FUNCTION hb_checksum( ... )             ; RETURN hb_adler32( ... )
FUNCTION setLastKey( ... )              ; RETURN hb_setLastKey( ... )
FUNCTION CStr( ... )                    ; RETURN hb_CStr( ... )
FUNCTION ValToPrgExp( ... )             ; RETURN hb_valToExp( ... )
FUNCTION IsDirectory( x )               ; RETURN hb_dirExists( x )
FUNCTION SecondsSleep( ... )            ; RETURN hb_idleSleep( ... )
FUNCTION NetName( n )                   ; RETURN iif( hb_isNumeric( n ) .AND. n == 1, hb_UserName(), NetName() )
FUNCTION FileSize( x )                  ; RETURN hb_FSize( x )
FUNCTION WildMatch( ... )               ; RETURN hb_WildMatch( ... )
FUNCTION hb_DeserialNext( x )           ; RETURN hb_Deserialize( x )
FUNCTION hb_funcptr( x )                ; RETURN __dynsn2sym( x )

FUNCTION HexToNum( ... )                ; RETURN hb_HexToNum( ... )
FUNCTION NumToHex( ... )                ; RETURN hb_NumToHex( ... )
FUNCTION HexToStr( ... )                ; RETURN hb_HexToStr( ... )
FUNCTION StrToHex( ... )                ; RETURN hb_StrToHex( ... )

FUNCTION ISPOINTER( x )                 ; RETURN hb_ISPOINTER( x )

FUNCTION hb_SetIniComment( ... )        ; RETURN hb_IniSetComment( ... )
FUNCTION hb_ReadIni( ... )              ; RETURN hb_IniRead( ... )
FUNCTION hb_WriteIni( ... )             ; RETURN hb_IniWrite( ... )

FUNCTION DisableWaitLocks( x )          ; RETURN hb_DisableWaitLocks( x )

FUNCTION HBCONSOLELOCK()                ; RETURN hb_gtLock()
FUNCTION HBCONSOLEUNLOCK()              ; RETURN hb_gtUnLock()

FUNCTION hb_CMDARGARGV( ... )           ; RETURN hb_ARGV( ... )

FUNCTION RAScan( ... )                  ; RETURN hb_RAScan( ... )

FUNCTION DateTime()                     ; RETURN hb_DateTime()
FUNCTION Hour( x )                      ; RETURN hb_Hour( x )
FUNCTION Minute( x )                    ; RETURN hb_Minute( x )
FUNCTION TToS( x )                      ; RETURN hb_TToS( x )
FUNCTION SToT( x )                      ; RETURN hb_SToT( x )
FUNCTION TToC( ... )                    ; RETURN hb_TToC( ... )
FUNCTION CToT( ... )                    ; RETURN hb_CToT( ... )

FUNCTION GetEnv( ... )                  ; RETURN hb_GetEnv( ... )
FUNCTION SetKey( ... )                  ; RETURN hb_SetKey( ... )

FUNCTION i18n( x )                      ; RETURN hb_i18n_gettext( x )

FUNCTION hb_SetCodepage( ... )          ; RETURN hb_cdpSelect( ... )

/* MT functions */
FUNCTION hb_MultiThread()               ; RETURN hb_mtvm()
FUNCTION GetCurrentThread()             ; RETURN hb_threadSelf()
FUNCTION GetThreadId( ... )             ; RETURN hb_threadId( ... )
FUNCTION ThreadGetCurrentInternal()     ; RETURN hb_threadId()
FUNCTION IsSameThread( x, y )           ; RETURN hb_threadId( x ) == hb_threadId( y )
FUNCTION IsValidThread( x )             ; RETURN hb_threadId( x ) != 0
FUNCTION JoinThread( x )                ; RETURN hb_threadJoin( x )
FUNCTION KillThread( x )                ; RETURN hb_threadQuitRequest( x )
FUNCTION StopThread( x )                ; hb_threadQuitRequest( x ) ; RETURN hb_threadJoin( x )
FUNCTION KillAllThreads()               ; RETURN hb_threadTerminateAll()
FUNCTION WaitForThreads()               ; RETURN hb_threadWaitForAll()

FUNCTION ThreadSleep( x )               ; RETURN hb_idleSleep( x / 1000 )

FUNCTION DestroyMutex( x )              ; HB_SYMBOL_UNUSED( x ) ; RETURN NIL

FUNCTION hb_MutexTryLock( x )           ; RETURN hb_mutexLock( x, 0 )
FUNCTION hb_MutexTimeOutLock( x, n )    ; RETURN hb_mutexLock( x, iif( hb_isNumeric( n ), n / 1000, 0 ) )

FUNCTION Notify( ... )                  ; RETURN hb_mutexNotify( ... )
FUNCTION NotifyAll( ... )               ; RETURN hb_mutexNotifyAll( ... )

FUNCTION Subscribe( ... )
   RETURN {| mtx, nTimeOut, lSubscribed |
            local xSubscribed
            lSubscribed := hb_mutexSubscribe( mtx, iif( hb_isNumeric( nTimeOut ), nTimeOut / 1000, ), @xSubscribed )
            return xSubscribed
          }:eval( ... )

FUNCTION SubscribeNow( ... )
   RETURN {| mtx, nTimeOut, lSubscribed |
            local xSubscribed
            lSubscribed := hb_mutexSubscribeNow( mtx, iif( hb_isNumeric( nTimeOut ), nTimeOut / 1000, ), @xSubscribed )
            return xSubscribed
          }:eval( ... )

FUNCTION StartThread( x, y, ... )

   IF PCount() == 1
      RETURN hb_threadStart( x )
   ENDIF

   RETURN iif( ISOBJECT( x ) .AND. hb_isString( y ), ;
                    hb_threadStart( {|...| (x):&(y)( ... ) }, ... ), ;
                    hb_threadStart( x, y, ... ) )

/* not possible to well replicate xHarbour behavior because it's buggy
   these function results are different on different platform, chosen
   translation which returns compatible types (numeric) */
FUNCTION ThreadGetCurrent()             ; RETURN hb_threadId()
FUNCTION GetSystemThreadId( ... )       ; RETURN hb_threadId( ... )

/* Hash item functions */
FUNCTION HASH( ... )                    ; RETURN hb_HASH( ... )
FUNCTION HHASKEY( ... )                 ; RETURN hb_HHASKEY( ... )
FUNCTION HGETPOS( ... )                 ; RETURN hb_HPOS( ... )
FUNCTION HGET( ... )                    ; RETURN hb_HGET( ... )
FUNCTION HSET( ... )                    ; RETURN hb_HSET( ... )
FUNCTION HDEL( ... )                    ; RETURN hb_HDEL( ... )
FUNCTION HGETKEYAT( ... )               ; RETURN hb_HKEYAT( ... )
FUNCTION HGETVALUEAT( ... )             ; RETURN hb_HVALUEAT( ... )
FUNCTION HSETVALUEAT( ... )             ; RETURN hb_HVALUEAT( ... )
FUNCTION HGETPAIRAT( ... )              ; RETURN hb_HPAIRAT( ... )
FUNCTION HDELAT( ... )                  ; RETURN hb_HDELAT( ... )
FUNCTION HGETKEYS( ... )                ; RETURN hb_HKEYS( ... )
FUNCTION HGETVALUES( ... )              ; RETURN hb_HVALUES( ... )
FUNCTION HFILL( ... )                   ; RETURN hb_HFILL( ... )
FUNCTION HCLONE( ... )                  ; RETURN hb_HCLONE( ... )
FUNCTION HCOPY( ... )                   ; RETURN hb_HCOPY( ... )
FUNCTION HMERGE( ... )                  ; RETURN hb_HMERGE( ... )
FUNCTION HEVAL( ... )                   ; RETURN hb_HEVAL( ... )
FUNCTION HSCAN( ... )                   ; RETURN hb_HSCAN( ... )
FUNCTION HSETCASEMATCH( ... )           ; RETURN hb_HSETCASEMATCH( ... )
FUNCTION HGETCASEMATCH( ... )           ; RETURN hb_HCASEMATCH( ... )
FUNCTION HSETAUTOADD( ... )             ; RETURN hb_HSETAUTOADD( ... )
FUNCTION HGETAUTOADD( ... )             ; RETURN hb_HAUTOADD( ... )
FUNCTION HALLOCATE( ... )               ; RETURN hb_HALLOCATE( ... )
FUNCTION HDEFAULT( ... )                ; RETURN hb_HDEFAULT( ... )

/* Inet functions */
FUNCTION INETINIT( ... )                ; RETURN hb_INETINIT( ... )
FUNCTION INETCLEANUP( ... )             ; RETURN hb_INETCLEANUP( ... )
FUNCTION INETCREATE( ... )              ; RETURN hb_INETCREATE( ... )
FUNCTION INETCLOSE( ... )               ; RETURN hb_INETCLOSE( ... )
FUNCTION INETFD( ... )                  ; RETURN hb_INETFD( ... )
FUNCTION INETSTATUS( ... )              ; RETURN hb_INETSTATUS( ... )
FUNCTION INETERRORCODE( ... )           ; RETURN hb_INETERRORCODE( ... )
FUNCTION INETERRORDESC( ... )           ; RETURN hb_INETERRORDESC( ... )
FUNCTION INETCLEARERROR( ... )          ; RETURN hb_INETCLEARERROR( ... )
FUNCTION INETCOUNT( ... )               ; RETURN hb_INETCOUNT( ... )
FUNCTION INETADDRESS( ... )             ; RETURN hb_INETADDRESS( ... )
FUNCTION INETPORT( ... )                ; RETURN hb_INETPORT( ... )
FUNCTION INETSETTIMEOUT( ... )          ; RETURN hb_INETTIMEOUT( ... )
FUNCTION INETGETTIMEOUT( ... )          ; RETURN hb_INETTIMEOUT( ... )
FUNCTION INETCLEARTIMEOUT( ... )        ; RETURN hb_INETCLEARTIMEOUT( ... )
FUNCTION INETSETTIMELIMIT( ... )        ; RETURN hb_INETTIMELIMIT( ... )
FUNCTION INETGETTIMELIMIT( ... )        ; RETURN hb_INETTIMELIMIT( ... )
FUNCTION INETCLEARTIMELIMIT( ... )      ; RETURN hb_INETCLEARTIMELIMIT( ... )
FUNCTION INETSETPERIODCALLBACK( ... )   ; RETURN hb_INETPERIODCALLBACK( ... )
FUNCTION INETGETPERIODCALLBACK( ... )   ; RETURN hb_INETPERIODCALLBACK( ... )
FUNCTION INETCLEARPERIODCALLBACK( ... ) ; RETURN hb_INETCLEARPERIODCALLBACK( ... )
FUNCTION INETRECV( ... )                ; RETURN hb_INETRECV( ... )
FUNCTION INETRECVALL( ... )             ; RETURN hb_INETRECVALL( ... )
FUNCTION INETRECVLINE( ... )            ; RETURN hb_INETRECVLINE( ... )
FUNCTION INETRECVENDBLOCK( ... )        ; RETURN hb_INETRECVENDBLOCK( ... )
FUNCTION INETDATAREADY( ... )           ; RETURN hb_INETDATAREADY( ... )
FUNCTION INETSEND( ... )                ; RETURN hb_INETSEND( ... )
FUNCTION INETSENDALL( ... )             ; RETURN hb_INETSENDALL( ... )
FUNCTION INETGETHOSTS( ... )            ; RETURN hb_INETGETHOSTS( ... )
FUNCTION INETGETALIAS( ... )            ; RETURN hb_INETGETALIAS( ... )
FUNCTION INETSERVER( ... )              ; RETURN hb_INETSERVER( ... )
FUNCTION INETACCEPT( ... )              ; RETURN hb_INETACCEPT( ... )
FUNCTION INETCONNECT( ... )             ; RETURN hb_INETCONNECT( ... )
FUNCTION INETCONNECTIP( ... )           ; RETURN hb_INETCONNECTIP( ... )
FUNCTION INETDGRAMBIND( ... )           ; RETURN hb_INETDGRAMBIND( ... )
FUNCTION INETDGRAM( ... )               ; RETURN hb_INETDGRAM( ... )
FUNCTION INETDGRAMSEND( ... )           ; RETURN hb_INETDGRAMSEND( ... )
FUNCTION INETDGRAMRECV( ... )           ; RETURN hb_INETDGRAMRECV( ... )
FUNCTION INETCRLF( ... )                ; RETURN hb_INETCRLF( ... )
FUNCTION ISINETSOCKET( ... )            ; RETURN hb_INETISSOCKET( ... )
FUNCTION INETDESTROY( x )               ; RETURN iif( HB_INETISSOCKET( x ), hb_INETCLOSE( x ), )

PROCEDURE THROW( oErr )                 ; Eval( ErrorBlock(), oErr ) ; Break( oErr ) ; RETURN
