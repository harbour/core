/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * http://harbour-project.org
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
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//
//                             Terminal Client
//
//                   Pritpal Bedi (pritpal@vouchcac.com)
//                               13 Feb 2009
//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//

#include "hbgtinfo.ch"
#include "fileio.ch"
#include "inkey.ch"
#include "setcurs.ch"

//----------------------------------------------------------------------//

#define WSABASEERR             10000
#define WSAECONNABORTED        (WSABASEERR+53)

//----------------------------------------------------------------------//

#define TIMER_RECEIVE          5001
#define TIMER_SEND             5002
#define TIMER_PING             5010
#define TIMER_CLOCK            5020
#define TIMER_REFRESH          5030

#define COMPILE( cStr )        &( "{|v| "+cStr+ "}" )
#define CR_LF                  chr(13)+chr(10)

#define VouchClientVersion     "0.9.4"

#define __TRACE__

//----------------------------------------------------------------------//

REQUEST Tone

//----------------------------------------------------------------------//

Static s_commSocket
static s_nTotalBytes       := 0
static s_nScreens          := 0
static s_nPing             := 0
static s_lReceiving        := .f.
static s_lSending          := .f.
static s_lTraceLog         := .f.
static s_nTrace            := 0
static s_aDat              := { {"",""} }

Function Main( cAddress, cPort, cAppln, cParams, cDirectory )
   LOCAL Socket, n, cText, cResponse

   ResolveParams( @cAddress, @cPort, @cAppln, @cParams, @cDirectory )

   TrmInitFont()

   SetCursor( SC_NONE )
   SetColor( "W/N" )
   SetBlink( .T. )

   CLS

   /* Comment out following line if you wish to receive log */
   // s_lTraceLog := .t.

   Hb_InetInit()

   TrmDispLogin( cAddress, cPort )

   Socket := Hb_InetConnect( cAddress, Val( cPort ) )
   IF Hb_InetErrorCode( Socket ) != 0
       DispOutAt( 17,0, padc( "Can't connect with " + cAddress +" : " + Hb_InetErrorDesc( Socket ),maxcol()+1 ), "w+/n" )
       DispOutAt( 18,0, padc( "Press a key to terminate the program...", maxcol()+1 ), "w+/n" )
       Inkey( 0 )
       RETURN  nil
   ENDIF

   // Wvt_SetTitle( "[ "+cAddress+" ][ "+cPort+" ]" )
   hb_gtInfo( HB_GTI_WINTITLE, "[ "+cAddress+" ][ "+cPort+" ]" )
   DispOutAt( 18,0, padc( "Connection Secured",maxcol()+1 ), "w+/n" )
   // Wvt_SetTitle( "[ "+cAddress+" ][ "+cPort+" ]"+"[Secured]" )
   hb_gtInfo( HB_GTI_WINTITLE, "[ "+cAddress+" ][ "+cPort+" ]"+"[Secured]" )

   cText := "VOUCH|"+ cAppln +"|"+ cParams +"|"+ cDirectory +"|"

   Hb_InetSend( Socket, cText + CR_LF )

   if TrmReceiveALine( Socket, @cResponse )
      if ( n := at( ";", cResponse ) ) > 0
         if substr( cResponse,1,n-1 ) == "CONNECT"
            TrmServeServer( Socket, cAddress, substr( cResponse,n+1 ) )
         endif
      endif
   endif

   Hb_InetClose( Socket )
   Hb_InetCleanup()

   RETURN 0

//----------------------------------------------------------------------//

STATIC FUNCTION ResolveParams( cAddress, cPort, cAppln, cParams, cDirectory )
   Local i, n, cLine, cVal, nLines, cTxt, cPath, cFile
   Local dat_  := {}
   LOCAL lFile

   if PCount() == 1
      cFile    := cAddress
      cAddress := ""
   else
      cFile := "vclient.ini"
   endif

   cPath := hb_DirBase()

   if !empty( cPath )
      n := rat( "\", cPath )
      if n > 0
         cPath := substr( cPath, 1, n )
      endif
   endif
   cFile := cPath + cFile

   lFile := hb_FileExists( cFile )

   if empty( cAddress ) .and. lFile
      alert( "File found: "+cFile )
      cTxt   := memoread( cFile )
      nLines := mlCount( cTxt,254,3,.f. )
      for i  := 1 to nLines
         if !empty( cLine := memoLine( cTxt,254,i,3,.f. ) )
            if ( n := at( "#",cLine ) ) > 0
               cLine := substr( cLine,1,n-1 )
            endif
            if ( n := at( "=",cLine ) ) > 0
               if !empty( cVal := alltrim( substr( cLine,n+1 ) ) )
                  aadd( dat_,{ lower( alltrim( substr( cLine,1,n-1 ) ) ),cVal } )
               endif
            endif
         endif
      next

      if !empty( dat_ )
         if ( n := ascan( dat_, {|e_| e_[ 1 ] == "serverip" } ) ) > 0
            cAddress := dat_[ n,2 ]
         endif
         if ( n := ascan( dat_, {|e_| e_[ 1 ] == "serverport" } ) ) > 0
            cPort := dat_[ n,2 ]
         endif
         if ( n := ascan( dat_, {|e_| e_[ 1 ] == "application" } ) ) > 0
            cAppln := dat_[ n,2 ]
         endif
         if ( n := ascan( dat_, {|e_| e_[ 1 ] == "parameters" } ) ) > 0
            cParams := dat_[ n,2 ]
         endif
         if ( n := ascan( dat_, {|e_| e_[ 1 ] == "initdirectory" } ) ) > 0
            cDirectory := dat_[ n,2 ]
         endif
      endif
   endif

   if !empty( cAddress )
      if empty( cPort ) .or. empty( cAppln )
         cAddress := ""
      endif
   endif

   // Defaults to Vouch Server
   //
   if empty( cAddress )
      cAddress   := "localhost"
      cPort      := "8085"
      cAppln     := "trm_app.exe"
      cParams    := ""
      cDirectory := hb_DirBase()
   endif

   if empty( cParams )
      cParams := ""
   endif
   if empty( cDirectory )
      cDirectory := ""
   endif

   Return nil

//----------------------------------------------------------------------//

Function TrmServeServer( Socket, cAddress, cServerInfo )
   Local nPort, a_, nError
   Local nSeconds := Seconds()
   Local nKey

   a_:= hb_aTokens( cServerInfo, ";" )
   nPort := val( a_[ 2 ] )

   s_commSocket := Hb_INetConnect( cAddress, nPort )

   do while Hb_INetErrorCode( s_commSocket ) != 0

      s_commSocket := Hb_INetConnect( cAddress, nPort )
      if Seconds()-nSeconds > 60 .or. Seconds()-nSeconds < 0
         exit
      endif
   enddo
   IF Hb_InetErrorCode( s_commSocket ) != 0
      Hb_INetClose( Socket )
      DispOutAt( 17,0, padc( "Can't connect with " + cAddress+": " + Hb_InetErrorDesc( s_commSocket ),maxcol()+1), "w+/n" )
      DispOutAt( 18,0, padc( "Press a key to terminate the program", maxcol()+1 ), "w+/n" )
      Inkey(0)
      quit
   ENDIF

   //  Very Important Factor   10-50 ok
   //
   Hb_INetTimeout( s_commSocket, -1 )

   Hb_INetSend( Socket, "ARCONNECTED" + CR_LF )
   Hb_INetClose( Socket )

// Wvt_SetTimer( TIMER_RECEIVE,   10 )
// Wvt_SetTimer( TIMER_SEND   ,    1 )
// Wvt_SetTimer( TIMER_CLOCK  , 5000 )
// Wvt_SetTimer( TIMER_PING   , 3000 )

   hb_threadStart( @Thread_Receive(), 0.01 )
   hb_threadStart( @Thread_Send()   , 1 / 1000 )
   hb_threadStart( @Thread_Ping()   , 3 )
   hb_threadStart( @Thread_Clock()  , 5 )

   do while .t.
      nKey := Inkey( 0, INKEY_ALL )

      IF s_commSocket != NIL .AND. ! Empty( nKey )
         s_lSending := .T.
         hb_inetSendAll( s_commSocket, hb_ntos( nKey ) + CR_LF )
         s_lSending := .F.
      ENDIF

      nError := hb_inetErrorCode( s_commSocket )
      IF AScan( { -2, WSAECONNABORTED, 10054 }, nError ) > 0
         EXIT
      ENDIF
   enddo

// Wvt_KillTimer( TIMER_RECEIVE )
// Wvt_KillTimer( TIMER_SEND    )
// Wvt_KillTimer( TIMER_CLOCK   )
// Wvt_KillTimer( TIMER_PING    )

   Return nil

//----------------------------------------------------------------------//

Function TrmReceiveServer()
   Local a_, b_, cBuffer, nBytes, cCommand, cData, cOdd, cEvn, n
   LOCAL cOdd1, cEvn1

   if !( s_lReceiving ) .and. ( s_commSocket != NIL )
      s_lReceiving := .t.
      if ( nBytes := Hb_INetDataReady( s_commSocket ) ) > 0

         Hb_INetTimeout( s_commSocket, 10 )
         cBuffer := Hb_INetRecvEndBlock( s_commSocket, "|/END\|", @nBytes )
         Hb_INetTimeout( s_commSocket, -1 )
         if nBytes > 0 .and. !empty( cBuffer )
            s_nTotalBytes += nBytes

            do while .t.
               cCommand := TrmFetchCommand( @cBuffer, @cData )
               if empty( cCommand )
                  exit
               endif

               do case
               case cCommand == "SCR"
                  s_nScreens++
                  a_:= Str2A( cData, "</E?>" )
                  b_:= hb_aTokens( a_[ 1 ], ";" )
                  aeval( b_, {|e,i| b_[ i ] := val( e ) } )

                  n := ( b_[ 3 ]-b_[ 1 ]+1 ) * ( b_[ 4 ]-b_[ 2 ]+1 )

                  cOdd1 := substr( a_[ 2 ], 1, b_[ 5 ] )
                  cOdd  := hb_zuncompress( cOdd1, n )
                  cEvn1 := substr( a_[ 2 ], b_[ 5 ]+1 )
                  cEvn  := hb_zuncompress( cEvn1, n )

                  RestScreen( b_[ 1 ], b_[ 2 ], b_[ 3 ], b_[ 4 ], CharMix( cOdd, cEvn ) )

               case cCommand == "CRS"
                  a_:= hb_aTokens( @cData, ";" )
                  SetPos( val( a_[ 1 ] ), val( a_[ 2 ] ) )
                  SetCursor( val( a_[ 3 ] ) )

               case cCommand == "ID"
                  // Verify if objects are serialized and executed accordingly

               case cCommand == "MUSIC"
                  PlayMusic( cData )

               case cCommand == "CLK_ONOFF"
                  SetClock( cData == "TRUE" )

               case cCommand == "CLK_INFO"
                  SetClockInfo( cData )

               case cCommand == "BLK"
                  BEGIN SEQUENCE
                     Eval( COMPILE( cData ) )
                  ENDSEQUENCE

               endcase
            enddo
         endif
      ENDIF

      s_lReceiving := .f.
   endif

   Return 0

STATIC PROCEDURE Thread_Receive( nWait )

   DO WHILE .T.
      TrmReceiveServer()
      hb_idleSleep( nWait )
   ENDDO

   RETURN

STATIC PROCEDURE Thread_Send( nWait )

   DO WHILE .T.
      inkey()
      hb_idleSleep( nWait )
   ENDDO

   RETURN

STATIC PROCEDURE Thread_Ping( nWait )

   DO WHILE .T.
      Keyboard( 1021 )
      hb_idleSleep( nWait )
   ENDDO

   RETURN

STATIC PROCEDURE Thread_Clock( nWait )

   DO WHILE .T.
      DispClock()
      hb_idleSleep( nWait )
   ENDDO

   RETURN

//----------------------------------------------------------------------//

Static Function TrmFetchCommand( cBuffer, cData )
   Local cToken, c, cCmd := ""
   Local n

   if left( @cBuffer,1 ) == "<"
      if ( n := at( ">", @cBuffer ) ) > 0
         c := substr( cBuffer, 2, n-2 )
         cBuffer := substr( cBuffer, n+1 )

         cToken := "</"+ c +">"
         if ( n := at( cToken, cBuffer ) ) > 0
            cData := substr( cBuffer, 1, n-1 )
            cBuffer := substr( cBuffer, n+len( cToken ) )

            cCmd := c
         endif
      endif
   endif

   Return cCmd

//----------------------------------------------------------------------//

Static Function TrmReceiveALine( Socket, cInfo )
   Local lRet := .t.
   Local nBytes

   do while .t.
      if Hb_InetDataReady( Socket, 100 ) > 0
         BEGIN SEQUENCE
            cInfo := Hb_InetRecvLine( Socket, @nBytes )
         RECOVER
            lRet := .f.
         END

         exit
      endif
   enddo

   Return lRet

//----------------------------------------------------------------------//

Static Function uiDebug( p1,p2,p3,p4,p5,p6,p7,p8,p9,p10 )
   #ifdef __TRACE__

   Local cDebug := ""

   if p1 != nil
      cDebug += uiXtos( p1 )
   endif
   if p2 != nil
      cDebug += "   " + uiXtos( p2 )
   endif
   if p3 != nil
      cDebug += "   " + uiXtos( p3 )
   endif
   if p4 != nil
      cDebug += "   " + uiXtos( p4 )
   endif
   if p5 != nil
      cDebug += "   " + uiXtos( p5 )
   endif
   if p6 != nil
      cDebug += "   " + uiXtos( p6 )
   endif
   if p7 != nil
      cDebug += "   " + uiXtos( p7 )
   endif
   if p8 != nil
      cDebug += "   " + uiXtos( p8 )
   endif
   if p9 != nil
      cDebug += "   " + uiXtos( p9 )
   endif
   if p10 != nil
      cDebug += "   " + uiXtos( p10 )
   endif

   if s_lTraceLog
      DbgTraceLog( cDebug )
   else
      wapi_OutputDebugString( cDebug )
   endif

   #endif
   Return nil

//----------------------------------------------------------------------//

Static Function TrmXtoS( xVar )
   Local cType := valtype( xVar )

   do case
   case cType $ "CM"

   case cType == "N"
      xVar := ltrim( str( xVar ) )

   case cType == "D"
      xVar := dtoc( xVar )

   case cType == "L"
      xVar := if( xVar, "T","F" )

   otherwise
      xVar := ""

   endcase

   Return xVar

//----------------------------------------------------------------------//

Static Function Str2A( cStr, cDel )
   Local a_:={}, n
   Local nlen

   nLen := len( cDel )

   do while .t.
      if ( n := at( cDel, cStr ) ) == 0
         exit
      endif

      aadd( a_, substr( cStr,1,n-1 ) )

      cStr := substr( cStr,n+nLen )
   enddo

   Return a_

//----------------------------------------------------------------------//

FUNCTION uiXtos( xVar )

   SWITCH ValType( xVar )
   CASE "C"
      RETURN xVar
   CASE "N"
      RETURN Str( xVar )
   CASE "D"
      RETURN DToC( xVar )
   CASE "L"
      RETURN iif( xVar, "Yes", "No " )
   ENDSWITCH

   RETURN "NIL"

//----------------------------------------------------------------------//

Static Function TrmInitFont()
   #define __JUSTGT__
   #ifdef __JUSTGT__
      /* set OEM font encoding for non unicode modes */
      hb_gtInfo( HB_GTI_CODEPAGE, 255 )

      /* Set EN CP-437 encoding */
      hb_cdpSelect( "EN" )
      hb_setTermCP( "EN" )

      #ifdef __WINCE__
         /* Set font size */
         hb_gtInfo( HB_GTI_FONTSIZE, 10 )
         hb_gtInfo( HB_GTI_FONTWIDTH, 5 )
      #endif

   #else
      Local cFont     := GetEnv( "VouchFont" )
      Local nSize     := val( GetEnv( "VouchFontSize" ) )
      Local nScrWidth

      Wvt_SetCodepage( 255 )
      hb_cdpSelect( "EN" )
      hb_setTermCP( "EN" )

      if empty( cFont )
         cFont := "Courier New"
      endif
      if empty( nSize )
         nScrWidth := Wvt_GetScreenWidth()

         if     nScrWidth >= 1280
            nSize := 28
         elseif nScrWidth >= 1200
            nSize := 22
         elseif nScrWidth >= 1000
            nSize := 18
         elseif nScrWidth >= 800
            nSize := 16
         else
            nSize := 15
         endif
      endif

      Wvt_SetFont( cFont, nSize, 0, 0 )
   #endif

   SetMode( 25,80 )

   return nil

//----------------------------------------------------------------------//

Static Function TrmDispLogin( cAddress, cPort )
   Local nMaxCol := maxcol()+1

   DispOutAt( 0,0, padc( "Vouch Client "+VouchClientVersion, nMaxCol ), "W+/r" )

   DispOutAt( 10,0, padc( "...Please Wait...", nMaxCol ), "W+/N" )
   DispOutAt( 12,0, padc( "Securing Server Connection", nMaxCol ), "W+/N" )
   DispOutAt( 13,0, padc( "Address[ "+cAddress+" ]  Port[ "+cPort+" ]", nMaxCol ), "W+/N" )

   DispOutAt( maxrow(), 0, padc( "the software that GROWS with you", nMaxCol ), "W+/r" )

   Return nil

//----------------------------------------------------------------------//

Static Function SetClockInfo( cData )
   Local lInfo, a_

   static aInfo := {}

   lInfo := aclone( aInfo )

   if cData != NIL
      a_:= hb_aTokens( cData, ";" )
      if len( a_ ) >= 3
         aInfo := { val( a_[ 1 ] ), val( a_[ 2 ] ), a_[ 3 ] }
      endif
   endif

   Return lInfo

//----------------------------------------------------------------------//

Static Function SetClock( lOnOff )
   Local oClock
   static lClock := .f.

   oClock := lClock

   if valtype( lOnOff ) == "L"
      lClock := lOnOff
   endif

   Return oClock

//----------------------------------------------------------------------//

Static Function DispClock()
   Local aInfo, nCrs, nRow, nCol

   if SetClock()
      if !empty( aInfo := SetClockInfo() )
         nCrs := SetCursor( SC_NONE )
         nRow := row()
         nCol := col()
         DispOutAt( aInfo[ 1 ], aInfo[ 2 ], time(), aInfo[ 3 ] )
         setpos( nRow,nCol )
         SetCursor( nCrs )
      endif
   endif

   Return nil

//----------------------------------------------------------------------//

Static Function PlayMusic( cTheme )

   do case
   case cTheme == "THUD"
      tone(60,0.5)

   case cTheme == "WAITON"
      tone(800,1); tone(1600,1)

   case cTheme == "WAITOFF"
      tone(1600,1); tone(800,1)

   case cTheme == "CHARGE"
      Eval( {|| tone(523,2),tone(698,2),tone(880,2),tone(1046,4),tone(880,2),tone(1046,8) } )

   case cTheme == "NANNYBOO"
      AEval( {{196,2},{196,2},{164,2},{220,2},{196,4},{164,4}}, {|a| tone(a[1],a[2]) } )

   case cTheme == "BADKEY"
      tone(480,0.25); tone(240,0.25)

   endcase

   Return nil

//----------------------------------------------------------------------//

#define S_LBL       1
#define S_TYP       2
#define S_LEN       3
#define S_DEC       4
#define S_PIC       5
#define S_ROW       6
#define S_COL       7
#define S_CLR       8
#define S_DEF       9

Static Function GetForm( cForm )
   Local cReply := ""
   Local i, scr
   Local aFields
   Local a_
   Local frm_:={}
   Local getlist := {}

   aFields := hb_aTokens( cForm, "^" )

   for i := 1 to len( aFields )
      //a_:= Str2A( aFields[ i ], "," )
      a_:= hb_aTokens( aFields[ i ], "," )

      a_[ S_LEN ] := val(a_[ S_LEN ])
      a_[ S_DEC ] := val(a_[ S_DEC ])
      a_[ S_ROW ] := val(a_[ S_ROW ])
      a_[ S_COL ] := val(a_[ S_COL ])

      if empty( a_[ S_CLR ] )
         a_[ S_CLR ] := "W+/BG,W+/B"
      endif
      if empty( a_[ S_PIC ] )
         a_[ S_PIC ] := "@ "
      endif

      do case
      case a_[ S_TYP ] == "C"
         a_[ S_DEF ] := pad( a_[ S_DEF ], a_[ S_LEN ] )
      case a_[ S_TYP ] == "N"
         a_[ S_DEF ] := val( a_[ S_DEF ] )
      case a_[ S_TYP ] == "D"
         a_[ S_DEF ] := ctod( a_[ S_DEF ] )
      case a_[ S_TYP ] == "L"
         a_[ S_DEF ] := if( a_[ S_DEF ] == "T", .t., .f. )
      endcase

      /*
      aadd( frm_, { a_[ S_LBL ], a_[ S_TYP ], a_[ S_LEN ], a_[ S_DEC ], ;
                a_[ S_PIC ], a_[ S_ROW ], a_[ S_COL ], a_[ S_CLR ], a_[ S_DEF ] } )
      */
      aadd( frm_, a_ )
   next

   scr := savescreen( 0,0,maxrow(),maxcol() )
   cls
   for i := 1 to len( frm_ )
      DispOutAt( frm_[ i,S_ROW ], frm_[ i,S_COL ]-10, frm_[ i,S_LBL ], "W+/B" )
      @ frm_[ i,S_ROW ], frm_[ i,S_COL ] GET frm_[ i,S_DEF ] ;
        PICTURE frm_[ i,S_PIC ] COLOR frm_[ i,S_CLR ]
   next
   READ

   RestScreen( 0,0,maxrow(),maxcol(),scr )
   for i := 1 to len( frm_ )
      cReply += TrmXtos( frm_[ i,S_DEF ] ) + "^"
   next

   Return cReply

//----------------------------------------------------------------------//

Static Function dbgTraceLog( cString, cFile  )
   Local lRet := .f.
   Local nBytes

   static nHandle

   if nHandle == NIL
      if ( nHandle := fopen( cFile,FO_WRITE ) ) == F_ERROR
         if ( nHandle := fcreate( cFile ) ) == F_ERROR
            Return .f.
         endif
      endif
   endif

   if nHandle != F_ERROR
      fseek( nHandle, 0, FS_END )
      nBytes := fwrite( nHandle, cString+chr(13)+chr(10), len( cString )+2 )

      lRet := nBytes == len( cString )+2
   endif

   Return lRet

//----------------------------------------------------------------------//
