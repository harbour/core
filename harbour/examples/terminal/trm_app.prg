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
//                           Terminal Application
//
//                   Pritpal Bedi (pritpal@vouchcac.com)
//                               13 Feb 2009
//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
/*
   Just comment it out if you want a normal application
   This is the only requirement to turn your application
   as a remote server.
*/

#include "hbgtinfo.ch"


#define __REMOTE__

#define SND_SCREEN                      1    //  Through Timer Only
#define SND_CODEBLOCK                   2    //  Application
#define SND_CLOCKINFO                   3
#define SND_CLOCKONOFF                  4
#define SND_MUSIC                       5

//----------------------------------------------------------------------//

ANNOUNCE HB_NOSTARTUPWINDOW

//----------------------------------------------------------------------//

FUNCTION Main( cServerInfo )
   LOCAL aMenu    := {}
   LOCAL aOptions := {}
   LOCAL nSel
   LOCAL nServerPort

   SetColor( "N/W,W/B,W+/N" )
   CLS
   ? " "

   #ifdef __REMOTE__
      // This can be redefined in case user want another format
      //
      // cServerInfo will be supplied by the Remote Server
      //
      RmtSvrSetInfo( cServerInfo )

      IF ( nServerPort := RmtSvrSetInfo( 1 ) ) != NIL .and. nServerPort > 0
         IF !RmtSvrInitialize( hb_ntos( nServerPort ), 60/*nTimeoutClient*/, 0.5 /*nTimeRefresh*/ )
            Quit
         ENDIF
         hb_gtInfo( HB_GTI_WINTITLE, hb_ntos( nServerPort ) )
      ENDIF
   #endif

   aadd( aMenu, { "Play Music", {|| App_PlayMusic() } } )
   aadd( aMenu, { " "         , {|| NIL             } } )
   aadd( aMenu, { "Show Clock", {|| App_DispClock() } } )

   aeval( aMenu, {|e_| aadd( aOptions, e_[ 1 ] ) } )

   DO WHILE .t.
      nSel := AChoice( 10,30,20,50, aOptions )

      IF nSel == 0
         EXIT
      ENDIF

      Eval( aMenu[ nSel,2 ] )
   ENDDO

   RETURN nil

//----------------------------------------------------------------------//

FUNCTION App_DispClock()

   RETURN nil

//----------------------------------------------------------------------//

FUNCTION App_PlayMusic()
   LOCAL cTheme   := "CHARGE"
   LOCAL aOptions := {"THUD","WAITON","WAITOFF","CHARGE","NANNYBOO","BADKEY" }
   LOCAL cScr     := SaveScreen( 0, 0, maxrow(), maxcol() )
   LOCAL nSel

   #ifdef __REMOTE__
   DO WHILE .t.
      nSel := AChoice( 10, 10, 17, 20, aOptions )
      RestScreen( 0, 0, maxrow(), maxcol(), cScr )
      IF nSel == 0
         RETURN nil
      ENDIF
      cTheme := aOptions[ nSel ]
      RmtSvrSendClient( SND_MUSIC, cTheme )
   ENDDO
   #endif

   DO CASE

   case cTheme == "THUD"
      #ifndef __REMOTE__
      tone(60,0.5)
      #endif

   case cTheme == "WAITON"
      #ifndef __REMOTE__
      tone(800,1); tone(1600,1)
      #endif

   case cTheme == "WAITOFF"
      #ifndef __REMOTE__
      tone(1600,1); tone(800,1)
      #endif

   case cTheme == "CHARGE"
      #ifndef __REMOTE__
      Eval( {|| tone(523,2),tone(698,2),tone(880,2),tone(1046,4),tone(880,2),tone(1046,8) } )
      #endif

   case cTheme == "NANNYBOO"
      #ifndef __REMOTE__
      AEval( {{196,2},{196,2},{164,2},{220,2},{196,4},{164,4}}, {|a| tone(a[1],a[2]) } )
      #endif

   case cTheme == "BADKEY"
      #ifndef __REMOTE__
      tone(480,0.25); tone(240,0.25)
      #endif

   endcase

   #ifdef __REMOTE__
   RmtSvrSendClient( SND_MUSIC, cTheme )
   #endif

   RETURN nil

//----------------------------------------------------------------------//
