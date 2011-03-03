/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour NETIO server management QT client
 *
 * Copyright 2011 Pritpal Bedi <bedipritpal@hotmail.com>
 * Copyright 2009-2011 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#define DAT_CONNSOCKET              1
#define DAT_SERIAL                  2
#define DAT_ACTIVATED               3
#define DAT_IP                      4
#define DAT_PORT                    5
#define DAT_TIMEIN                  6
#define DAT_TIMEOUT                 7
#define DAT_BYTESIN                 8
#define DAT_BYTESOUT                9
#define DAT_OPENFILES               10

#include "fileio.ch"
#include "hbclass.ch"
#include "hbqtgui.ch"

#include "xbp.ch"
#include "gra.ch"
#include "appevent.ch"

#define RGB( r, g, b )   GraMakeRGBColor( { r, g, b } )

/*----------------------------------------------------------------------*/

#define _NETIOMGM_IPV4_DEF  "127.0.0.1"
#define _NETIOMGM_PORT_DEF  2940

PROCEDURE Main( ... )
   LOCAL cParam

   LOCAL cIP := _NETIOMGM_IPV4_DEF
   LOCAL nPort := _NETIOMGM_PORT_DEF
   LOCAL cPassword := ""

   FOR EACH cParam IN { ... }
      DO CASE
      CASE Lower( Left( cParam, 6 ) ) == "-addr="
         hbnetiocon_IPPortSplit( SubStr( cParam, 7 ), @cIP, @nPort )
         IF Empty( nPort )
            nPort := _NETIOMGM_PORT_DEF
         ENDIF
      CASE Lower( Left( cParam, 6 ) ) == "-pass="
         cPassword := SubStr( cParam, 7 )
         hb_StrClear( @cParam )
      CASE Lower( cParam ) == "--version"
         RETURN
      CASE Lower( cParam ) == "-help" .OR. ;
           Lower( cParam ) == "--help"
         HB_Usage()
         RETURN
      OTHERWISE
         OutStd( "Warning: Unkown parameter ignored: " + cParam + hb_eol() )
      ENDCASE
   NEXT

   NetIOMgmtClient():new():create( cIP, nPort, cPassword )

   RETURN

/*----------------------------------------------------------------------*/

PROCEDURE hbnetiocon_IPPortSplit( cAddr, /* @ */ cIP, /* @ */ nPort )
   LOCAL tmp

   IF ! Empty( cAddr )
      cIP := cAddr
      IF ( tmp := At( ":", cIP ) ) > 0
         nPort := Val( SubStr( cIP, tmp + Len( ":" ) ) )
         cIP := Left( cIP, tmp - 1 )
      ELSE
         nPort := NIL
      ENDIF
   ENDIF

   RETURN

/*----------------------------------------------------------------------*/

STATIC FUNCTION MyClientInfo()
   LOCAL hInfo := { => }

   hb_hKeepOrder( hInfo, .T. )

   hInfo[ "OS()"          ] := OS()
   hInfo[ "Version()"     ] := Version()
   hInfo[ "hb_Compiler()" ] := hb_Compiler()
   hInfo[ "NetName()"     ] := NetName()
   hInfo[ "hb_UserName()" ] := hb_UserName()

   RETURN hInfo

/*----------------------------------------------------------------------*/
//                           NetIOMgmtClient
/*----------------------------------------------------------------------*/

CLASS NetIOMgmtClient
   DATA   pConnection

   DATA   lProcessing                             INIT .F.
   DATA   nNumConxn                               INIT 0
   DATA   oDlg
   DATA   oBrw
   DATA   cTitle
   DATA   pMtx
   DATA   oSys
   DATA   lSystemTrayAvailable
   DATA   oSysMenu
   DATA   qTimer
   DATA   qTimerRefresh
   DATA   qLayout
   DATA   qAct1
   DATA   qAct2
   DATA   nPrevWindowState
   DATA   lChanging                               INIT .F.
   DATA   lQuit                                   INIT .F.
   DATA   nCurRec                                 INIT 1
   DATA   aIPs                                    INIT {}
   DATA   nRefreshInterval                        INIT 3000
   DATA   aData                                   INIT { { NIL, ;                   // hSock
                                                           0  , ;                   // nSerial
                                                           .F., ;                   // lActive
                                                           "               ", ;     // cIP
                                                           0  , ;                   // nPort
                                                           "                  ", ;  // time-in
                                                           "                  ", ;  // time-out
                                                           0  , ;                   // bytes-in
                                                           0  , ;                   // bytes-out
                                                           NIL, ;                   // conn socket
                                                           0  } }                   // files opened

   METHOD new()
   METHOD create( cIP, nPort, cPassword )
   METHOD execEvent( cEvent, p )

   METHOD buildToolBar()
   METHOD buildSystemTray()
   METHOD buildBrowser()

   METHOD refresh()
   METHOD confirmExit()
   METHOD showDlgBySystemTrayIconCommand()

   METHOD terminate()
   METHOD buildColumns()

   METHOD skipBlock( nHowMany )
   METHOD goTop()
   METHOD goBottom()
   METHOD lastRec()
   METHOD recNo()
   METHOD goto( nRec )
   METHOD manageIPs()

   /* Information retrieval from the daemon */
   METHOD cmdConnInfo( lManagement )
   METHOD cmdConnStop( cIPPort )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD NetIOMgmtClient:new()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD NetIOMgmtClient:create( cIP, nPort, cPassword )
   LOCAL nEvent, mp1, mp2, oXbp

   ::pConnection := netio_getconnection( cIP, nPort,, cPassword )
   cPassword := NIL

   IF Empty( ::pConnection )
      MsgBox( "Cannot connect to server." )
   ELSE
      netio_funcexec( ::pConnection, "hbnetiomgm_setclientinfo", MyClientInfo() )
      netio_OpenItemStream( ::pConnection, "hbnetiomgm_regnotif", .T. )

      QResource():registerResource_1( hbqtres_netiosrq(), ":/resource" )

      ::pMtx            := hb_mutexCreate()
      ::cTitle          := "NetIO Server [" + cIP + ":" + ;
                                            hb_ntos( int( nPort ) ) + "]"

      ::oDlg            := XbpDialog():new( , , { 20,20 }, { 870,300 } )
      ::oDlg:title      := ::cTitle
      ::oDlg:taskList   := .T.
      ::oDlg:close      := {|| ::confirmExit() }
      ::oDlg:create()
      ::oDlg:drawingArea:setFontCompoundName( "10.Ariel" )
      ::oDlg:setWindowIcon( ":/harbour.png" )

      ::buildToolBar()

      ::qLayout := QGridLayout()
      ::qLayout:setContentsMargins( 0,0,0,0 )
      ::qLayout:setHorizontalSpacing( 0 )
      ::qLayout:setVerticalSpacing( 0 )
      //
      ::oDlg:drawingArea:setLayout( ::qLayout )

      ::buildBrowser()

      ::oDlg:oWidget:connect( QEvent_WindowStateChange, {|e| ::execEvent( "QEvent_WindowStateChange", e ) } )
      ::oDlg:oWidget:connect( QEvent_Hide             , {|e| ::execEvent( "QEvent_Hide"             , e ) } )
      //
      ::buildSystemTray()

      SetAppWindow( ::oDlg )
      SetAppFocus( ::oDlg )

      DO WHILE nEvent <> xbeP_Quit
         nEvent := AppEvent( @mp1, @mp2, @oXbp )
         oXbp:handleEvent( nEvent, mp1, mp2 )
      ENDDO

      netio_OpenItemStream( ::pConnection, "hbnetiomgm_regnotif", .F. )

      ::pConnection := NIL

      ::oDlg:destroy()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD NetIOMgmtClient:execEvent( cEvent, p )
   LOCAL qEvent, oMenu, txt_, s, cTmp, qItem, n
#if 0
   LOCAL cTmp1, oXbp
#endif

   SWITCH cEvent
   CASE "browser_contextMenu"
      oMenu := XbpMenu():new():create()
      oMenu:addItem( { "Terminate", {|| ::terminate() } } )
      oMenu:popup( ::oBrw, p )
      EXIT
   CASE "tool_button_clicked"
      SWITCH p
      CASE "Help"
         Hb_Usage()
         EXIT
      CASE "About"
         txt_:= {}
         AAdd( txt_, "<b>Harbour NetIO Management Client</b>" )
         AAdd( txt_, "Developed by:" )
         AAdd( txt_, "Pritpal Bedi" )
         AAdd( txt_, "Viktor Szakats" )
         AAdd( txt_, "" )
         AAdd( txt_, "built with:" )
         AAdd( txt_, HB_VERSION() )
         AAdd( txt_, HB_COMPILER() )
         AAdd( txt_, "Qt " + QT_VERSION_STR() )
         AAdd( txt_, "" )
         AAdd( txt_, "Visit the project website at:" )
         AAdd( txt_, "<a href='http://harbour-project.org/'>http://harbour-project.org/</a>" )
         s := ""
         aeval( txt_, {|e| s += e + chr( 10 ) } )
         MsgBox( s, " About NetIO Management Client" )
         EXIT
      CASE "Terminate"
         ::terminate()
         EXIT
      CASE "Exit"
         PostAppEvent( xbeP_Quit, , , ::oDlg )
         EXIT
      CASE "IPs"
         ::cmdConnInfo( .f. )
         //::manageIPs()
         EXIT
      ENDSWITCH
      EXIT
   CASE "QEvent_WindowStateChange"
      qEvent := p
      ::nPrevWindowState := qEvent:oldState()
      EXIT
   CASE "QEvent_Hide"
      IF ::lSystemTrayAvailable
         qEvent := p
         IF ! ::lChanging
            ::lChanging := .t.
            IF qEvent:spontaneous()
               IF empty( ::qTimer )
                  ::qTimer := QTimer()
                  ::qTimer:setSingleShot( .t. )
                  ::qTimer:setInterval( 250 )
                  ::qTimer:connect( "timeout()", {|| ::execEvent( "qTimer_timeOut" ) } )
               ENDIF
               ::qTimer:start()
               qEvent:ignore()
            ENDIF
            ::lChanging := .f.
         ENDIF
      ENDIF
      EXIT
   CASE "qTimerRefresh_timeOut"
      ::cmdConnInfo( .f. )
      EXIT
   CASE "qTimer_timeOut"
      ::oDlg:hide()
      ::oSys:setToolTip( "Connected to Harbour NetIO Server: " + ::oDlg:title )
      ::oSys:show()
      EXIT
   CASE "qSystemTrayIcon_activated"
      IF     p == QSystemTrayIcon_Trigger
         ::showDlgBySystemTrayIconCommand()
      ELSEIF p == QSystemTrayIcon_DoubleClick
      ELSEIF p == QSystemTrayIcon_Context
      ELSEIF p == QSystemTrayIcon_MiddleClick
      ENDIF
      EXIT
   CASE "qSystemTrayIcon_show"
      ::showDlgBySystemTrayIconCommand()
      EXIT
   CASE "qSystemTrayIcon_close"
      PostAppEvent( xbeP_Quit, NIL, NIL, ::oDlg )
      EXIT
   CASE "ips_buttonDelete"
      IF p:q_listIPs:currentRow() >= 0
         qItem := p:q_listIPs:takeItem( p:q_listIPs:currentRow() )
         IF ( n := ascan( ::aIPs, {|e_| e_[ 1 ] == qItem:text() } ) ) > 0
            hb_adel( ::aIPs, n, .t. )
         ENDIF
      ENDIF
      EXIT
#if 0
   CASE "ips_buttonSave"
      FOR n := 1 TO p:q_listIPs:count()
         qItem := p:q_listIPs:item( n - 1 )
         IF ( n := ascan( ::aIPs, {|e_| e_[ 1 ] == qItem:text() } ) ) > 0
            ::aIPs[ n, 2 ] := iif( qItem:checkState() == Qt_Checked, "Y", " " )
         ENDIF
      NEXT
      oXbp := XbpFileDialog():new( ::oDlg ):create()
      oXbp:title := "File name to save IPs ?"
      oXbp:center := .t.
      cTmp := oXbp:saveAs( iif( empty( ::netiosrv[ _NETIOSRV_cINI ] ), "", ::netiosrv[ _NETIOSRV_cINI ] ) )
      IF ! empty( cTmp )
         cTmp1 := ""
         aeval( ::aIPs, {|e_| cTmp1 += "netiosrv_ip=" + e_[ 1 ] + ";" + e_[ 2 ] + chr( 13 ) + chr( 10 ) } )
         hb_memowrit( cTmp, cTmp1 )
      ENDIF
      p:done( 0 )
      EXIT
#endif
   CASE "ips_buttonAdd"
      IF ! empty( cTmp := QInputDialog():getText( ::oDlg:oWidget, "Manage Connections", "IPv4:IPv6:" ) )
         qItem := QListWidgetItem()
         qItem:setFlags( Qt_ItemIsUserCheckable + Qt_ItemIsEnabled + Qt_ItemIsSelectable )
         qItem:setText( cTmp )
         qItem:setCheckState( 0 )
         p:q_listIPs:addItem( qItem )
         aadd( ::aIPs, { cTmp, " " } )
      ENDIF
      EXIT
   ENDSWITCH

   RETURN 0

/*----------------------------------------------------------------------*/

METHOD NetIOMgmtClient:manageIPs()
   LOCAL oUI, a_, qItem

   oUI := hbqtui_ManageIPs( ::oDlg:oWidget )
   oUI:setWindowFlags( Qt_Dialog )

   oUI:q_buttonAdd:connect( "clicked()", {|| ::execEvent( "ips_buttonAdd", oUI ) } )
   oUI:q_buttonDelete:connect( "clicked()", {|| ::execEvent( "ips_buttonDelete", oUI ) } )
   oUI:q_buttonSave:connect( "clicked()", {|| ::execEvent( "ips_buttonSave", oUI ) } )

   FOR EACH a_ IN ::aIPs
      qItem := QListWidgetItem()
      qItem:setFlags( Qt_ItemIsUserCheckable + Qt_ItemIsEnabled + Qt_ItemIsSelectable )
      qItem:setText( a_[ 1 ] )
      qItem:setCheckState( iif( empty( a_[ 2 ] ), Qt_Unchecked, Qt_Checked ) )
      oUI:q_listIPs:addItem( qItem )
   NEXT

   oUI:exec()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD NetIOMgmtClient:showDlgBySystemTrayIconCommand()

   ::oSys:hide()

   IF hb_bitAnd( ::nPrevWindowState, Qt_WindowMaximized ) == Qt_WindowMaximized
      ::oDlg:oWidget:showMaximized()
   ELSEIF hb_bitAnd( ::nPrevWindowState, Qt_WindowFullScreen ) == Qt_WindowFullScreen
      ::oDlg:oWidget:showFullScreen()
   ELSE
      ::oDlg:oWidget:showNormal()
   ENDIF

   ::oDlg:oWidget:raise()
   ::oDlg:oWidget:activateWindow()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD NetIOMgmtClient:buildBrowser()
   LOCAL s

   ::oBrw := XbpBrowse():new():create( ::oDlg:drawingArea, , { 0,0 }, ::oDlg:currentSize() )
   ::oBrw:setFontCompoundName( "10.Courier" )

   ::qLayout:addWidget( ::oBrw:oWidget, 0, 0, 1, 1 )

   ::oBrw:skipBlock     := {|n| ::skipBlock( n ) }
   ::oBrw:goTopBlock    := {| | ::goTop()        }
   ::oBrw:goBottomBlock := {| | ::goBottom()     }
   //
   ::oBrw:firstPosBlock := {| | 1                }
   ::oBrw:lastPosBlock  := {| | ::lastRec()      }

   ::oBrw:posBlock      := {| | ::recNo()        }
   ::oBrw:goPosBlock    := {|n| ::goto( n )      }
   ::oBrw:phyPosBlock   := {| | ::recNo()        }

   ::oBrw:hbContextMenu := {|mp1| ::execEvent( "browser_contextMenu", mp1 ) }

   s := "selection-background-color: qlineargradient(x1: 0, y1: 0, x2: 0.5, y2: 0.5, stop: 0 #FF92BB, stop: 1 gray); "
   ::oBrw:setStyleSheet( s )

   ::oBrw:cursorMode    := XBPBRW_CURSOR_ROW

   ::buildColumns()

   ::oBrw:oWidget:show()

   ::qTimerRefresh := QTimer()
   ::qTimerRefresh:setInterval( ::nRefreshInterval )
   ::qTimerRefresh:connect( "timeout()", {|| ::execEvent( "qTimerRefresh_timeOut" ) } )
   ::qTimerRefresh:start()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD NetIOMgmtClient:terminate()

   IF ! ::lProcessing
      ::lProcessing := .t.
      IF ConfirmBox( , ;
             "Terminating: " + ::aData[ ::recNo(), DAT_IP ] + " : " + hb_ntos( ::aData[ ::recNo(), DAT_PORT ] ), ;
             "Critical, be careful", ;
             , ;
             XBPMB_CRITICAL ) == XBPMB_RET_OK

         ::cmdConnStop( ::aData[ ::recNo(), DAT_IP ] )
      ENDIF
      ::lProcessing := .f.
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD NetIOMgmtClient:refresh()
   LOCAL qRect
   ::oBrw:refreshAll()
   ::oBrw:forceStable()
   qRect := ::oDlg:oWidget:geometry()
   qRect:setHeight( qRect:height() + 3 )
   ::oDlg:oWidget:setGeometry( qRect )
   qRect:setHeight( qRect:height() - 3 )
   ::oDlg:oWidget:setGeometry( qRect )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD NetIOMgmtClient:skipBlock( nHowMany )
   LOCAL nRecs, nCurPos, nSkipped

   nRecs    := len( ::aData )
   nCurPos  := ::nCurRec

   IF nHowMany >= 0
      IF ( nCurpos + nHowMany ) > nRecs
         nSkipped := nRecs - nCurpos
         ::nCurRec := nRecs
      ELSE
         nSkipped := nHowMany
         ::nCurRec += nHowMany
      ENDIF
   ELSE
      IF ( nCurpos + nHowMany ) < 1
         nSkipped := 1 - nCurpos
         ::nCurRec := 1
      ELSE
         nSkipped := nHowMany
         ::nCurRec += nHowMany
      ENDIF
   ENDIF

   RETURN nSkipped

/*----------------------------------------------------------------------*/

METHOD NetIOMgmtClient:goTop()
   ::nCurRec := 1
   ::refresh()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD NetIOMgmtClient:goBottom()
   ::nCurRec := len( ::aData )
   ::refresh()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD NetIOMgmtClient:goto( nRec )
   IF nRec > 0 .AND. nRec <= len( ::aData )
      ::nCurRec := nRec
      ::refresh()
   ENDIF
   RETURN .t.

/*----------------------------------------------------------------------*/

METHOD NetIOMgmtClient:lastRec()
   RETURN len( ::aData )

/*----------------------------------------------------------------------*/

METHOD NetIOMgmtClient:recNo()
   RETURN ::nCurRec

/*----------------------------------------------------------------------*/

METHOD NetIOMgmtClient:buildColumns()
   LOCAL aPP, oXbpColumn
   LOCAL nClrBG  := GRA_CLR_WHITE
   LOCAL nClrHFg := GRA_CLR_BLACK    //YELLOW
   LOCAL nClrHBg := GRA_CLR_DARKGRAY //BLUE

   aPP := {}
   aadd( aPP, { XBP_PP_COL_HA_CAPTION      , "Sr"              } )
   aadd( aPP, { XBP_PP_COL_HA_FGCLR        , nClrHFg           } )
   aadd( aPP, { XBP_PP_COL_HA_BGCLR        , nClrHBg           } )
   aadd( aPP, { XBP_PP_COL_HA_HEIGHT       , 20                } )
   aadd( aPP, { XBP_PP_COL_DA_FGCLR        , GRA_CLR_BLACK     } )
   aadd( aPP, { XBP_PP_COL_DA_BGCLR        , nClrBG            } )
   aadd( aPP, { XBP_PP_COL_DA_HILITE_FGCLR , GRA_CLR_WHITE     } )
   aadd( aPP, { XBP_PP_COL_DA_HILITE_BGCLR , GRA_CLR_DARKGRAY  } )
   aadd( aPP, { XBP_PP_COL_DA_ROWHEIGHT    , 20                } )
   aadd( aPP, { XBP_PP_COL_DA_ROWWIDTH     , 40                } )
   //
   oXbpColumn          := XbpColumn():new()
   oXbpColumn:dataLink := {|| strzero( ::aData[ ::recNo(), DAT_SERIAL ], 4 ) }
   oXbpColumn:create( , , , , aPP )
   ::oBrw:addColumn( oXbpColumn )

   aPP := {}
   aadd( aPP, { XBP_PP_COL_HA_CAPTION      , "Actv"            } )
   aadd( aPP, { XBP_PP_COL_HA_FGCLR        , nClrHFg           } )
   aadd( aPP, { XBP_PP_COL_HA_BGCLR        , nClrHBg           } )
   aadd( aPP, { XBP_PP_COL_HA_HEIGHT       , 20                } )
   aadd( aPP, { XBP_PP_COL_DA_FGCLR        , GRA_CLR_BLACK     } )
   aadd( aPP, { XBP_PP_COL_DA_BGCLR        , nClrBG            } )
   aadd( aPP, { XBP_PP_COL_DA_HILITE_FGCLR , GRA_CLR_WHITE     } )
   aadd( aPP, { XBP_PP_COL_DA_HILITE_BGCLR , GRA_CLR_DARKGRAY  } )
   aadd( aPP, { XBP_PP_COL_DA_ROWHEIGHT    , 20                } )
   aadd( aPP, { XBP_PP_COL_DA_ROWWIDTH     , 45                } )
   //
   oXbpColumn          := XbpColumn():new()
   oXbpColumn:dataLink := {|| iif( ::aData[ ::recNo(), DAT_ACTIVATED ], " ", "X" ) }
   oXbpColumn:create( , , , , aPP )
   ::oBrw:addColumn( oXbpColumn )

   aPP := {}
   aadd( aPP, { XBP_PP_COL_HA_CAPTION      , "IP:Port"         } )
   aadd( aPP, { XBP_PP_COL_HA_FGCLR        , nClrHFg           } )
   aadd( aPP, { XBP_PP_COL_HA_BGCLR        , nClrHBg           } )
   aadd( aPP, { XBP_PP_COL_HA_HEIGHT       , 20                } )
   aadd( aPP, { XBP_PP_COL_DA_FGCLR        , GRA_CLR_BLACK     } )
   aadd( aPP, { XBP_PP_COL_DA_BGCLR        , nClrBG            } )
   aadd( aPP, { XBP_PP_COL_DA_HILITE_FGCLR , GRA_CLR_WHITE     } )
   aadd( aPP, { XBP_PP_COL_DA_HILITE_BGCLR , GRA_CLR_DARKGRAY  } )
   aadd( aPP, { XBP_PP_COL_DA_ROWHEIGHT    , 20                } )
   aadd( aPP, { XBP_PP_COL_DA_ROWWIDTH     , 200               } )
   //
   oXbpColumn          := XbpColumn():new()
   oXbpColumn:dataLink := {|| ::aData[ ::recNo(), DAT_IP ] }
   oXbpColumn:create( , , , , aPP )
   ::oBrw:addColumn( oXbpColumn )

   aPP := {}
   aadd( aPP, { XBP_PP_COL_HA_CAPTION      , "DateTime IN"     } )
   aadd( aPP, { XBP_PP_COL_HA_FGCLR        , nClrHFg           } )
   aadd( aPP, { XBP_PP_COL_HA_BGCLR        , nClrHBg           } )
   aadd( aPP, { XBP_PP_COL_HA_HEIGHT       , 20                } )
   aadd( aPP, { XBP_PP_COL_DA_FGCLR        , GRA_CLR_BLACK     } )
   aadd( aPP, { XBP_PP_COL_DA_BGCLR        , nClrBG            } )
   aadd( aPP, { XBP_PP_COL_DA_HILITE_FGCLR , GRA_CLR_WHITE     } )
   aadd( aPP, { XBP_PP_COL_DA_HILITE_BGCLR , GRA_CLR_DARKGRAY  } )
   aadd( aPP, { XBP_PP_COL_DA_ROWHEIGHT    , 20                } )
   aadd( aPP, { XBP_PP_COL_DA_ROWWIDTH     , 160               } )
   //
   oXbpColumn          := XbpColumn():new()
   oXbpColumn:dataLink := {|| ::aData[ ::recNo(), DAT_TIMEIN ] }
   oXbpColumn:create( , , , , aPP )
   ::oBrw:addColumn( oXbpColumn )

   aPP := {}
   aadd( aPP, { XBP_PP_COL_HA_CAPTION      , "DateTime OUT"    } )
   aadd( aPP, { XBP_PP_COL_HA_FGCLR        , nClrHFg           } )
   aadd( aPP, { XBP_PP_COL_HA_BGCLR        , nClrHBg           } )
   aadd( aPP, { XBP_PP_COL_HA_HEIGHT       , 20                } )
   aadd( aPP, { XBP_PP_COL_DA_FGCLR        , GRA_CLR_BLACK     } )
   aadd( aPP, { XBP_PP_COL_DA_BGCLR        , nClrBG            } )
   aadd( aPP, { XBP_PP_COL_DA_HILITE_FGCLR , GRA_CLR_WHITE     } )
   aadd( aPP, { XBP_PP_COL_DA_HILITE_BGCLR , GRA_CLR_DARKGRAY  } )
   aadd( aPP, { XBP_PP_COL_DA_ROWHEIGHT    , 20                } )
   aadd( aPP, { XBP_PP_COL_DA_ROWWIDTH     , 160               } )
   //
   oXbpColumn          := XbpColumn():new()
   oXbpColumn:dataLink := {|| ::aData[ ::recNo(), DAT_TIMEOUT ] }
   oXbpColumn:create( , , , , aPP )
   ::oBrw:addColumn( oXbpColumn )

   aPP := {}
   aadd( aPP, { XBP_PP_COL_HA_CAPTION      , "BytesIN"         } )
   aadd( aPP, { XBP_PP_COL_HA_FGCLR        , nClrHFg           } )
   aadd( aPP, { XBP_PP_COL_HA_BGCLR        , nClrHBg           } )
   aadd( aPP, { XBP_PP_COL_HA_HEIGHT       , 20                } )
   aadd( aPP, { XBP_PP_COL_DA_FGCLR        , GRA_CLR_BLACK     } )
   aadd( aPP, { XBP_PP_COL_DA_BGCLR        , nClrBG            } )
   aadd( aPP, { XBP_PP_COL_DA_HILITE_FGCLR , GRA_CLR_WHITE     } )
   aadd( aPP, { XBP_PP_COL_DA_HILITE_BGCLR , GRA_CLR_DARKGRAY  } )
   aadd( aPP, { XBP_PP_COL_DA_ROWHEIGHT    , 20                } )
   aadd( aPP, { XBP_PP_COL_DA_ROWWIDTH     , 90                } )
   //
   oXbpColumn          := XbpColumn():new()
   oXbpColumn:dataLink   := {|| ::aData[ ::recNo(), DAT_BYTESIN ] }
   oXbpColumn:create( , , , , aPP )
   ::oBrw:addColumn( oXbpColumn )

   aPP := {}
   aadd( aPP, { XBP_PP_COL_HA_CAPTION      , "BytesOUT"        } )
   aadd( aPP, { XBP_PP_COL_HA_FGCLR        , nClrHFg           } )
   aadd( aPP, { XBP_PP_COL_HA_BGCLR        , nClrHBg           } )
   aadd( aPP, { XBP_PP_COL_HA_HEIGHT       , 20                } )
   aadd( aPP, { XBP_PP_COL_DA_FGCLR        , GRA_CLR_BLACK     } )
   aadd( aPP, { XBP_PP_COL_DA_BGCLR        , nClrBG            } )
   aadd( aPP, { XBP_PP_COL_DA_HILITE_FGCLR , GRA_CLR_WHITE     } )
   aadd( aPP, { XBP_PP_COL_DA_HILITE_BGCLR , GRA_CLR_DARKGRAY  } )
   aadd( aPP, { XBP_PP_COL_DA_ROWHEIGHT    , 20                } )
   aadd( aPP, { XBP_PP_COL_DA_ROWWIDTH     , 90                } )
   //
   oXbpColumn            := XbpColumn():new()
   oXbpColumn:dataLink   := {|| ::aData[ ::recNo(), DAT_BYTESOUT ] }
   oXbpColumn:create( , , , , aPP )
   ::oBrw:addColumn( oXbpColumn )

   aPP := {}
   aadd( aPP, { XBP_PP_COL_HA_CAPTION      , "Files"           } )
   aadd( aPP, { XBP_PP_COL_HA_FGCLR        , nClrHFg           } )
   aadd( aPP, { XBP_PP_COL_HA_BGCLR        , nClrHBg           } )
   aadd( aPP, { XBP_PP_COL_HA_HEIGHT       , 20                } )
   aadd( aPP, { XBP_PP_COL_DA_FGCLR        , GRA_CLR_BLACK     } )
   aadd( aPP, { XBP_PP_COL_DA_BGCLR        , nClrBG            } )
   aadd( aPP, { XBP_PP_COL_DA_HILITE_FGCLR , GRA_CLR_WHITE     } )
   aadd( aPP, { XBP_PP_COL_DA_HILITE_BGCLR , GRA_CLR_DARKGRAY  } )
   aadd( aPP, { XBP_PP_COL_DA_ROWHEIGHT    , 20                } )
   aadd( aPP, { XBP_PP_COL_DA_ROWWIDTH     , 60                } )
   //
   oXbpColumn            := XbpColumn():new()
   oXbpColumn:dataLink   := {|| ::aData[ ::recNo(), DAT_OPENFILES ] }
   oXbpColumn:create( , , , , aPP )
   ::oBrw:addColumn( oXbpColumn )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD NetIOMgmtClient:buildToolBar()
   LOCAL oTBar

   oTBar := XbpToolBar():new( ::oDlg )
   oTBar:imageWidth  := 40
   oTBar:imageHeight := 40
   oTBar:create( , , { 0, ::oDlg:currentSize()[ 2 ]-60 }, { ::oDlg:currentSize()[ 1 ], 60 } )
   oTBar:oWidget:setAllowedAreas( Qt_LeftToolBarArea + Qt_RightToolBarArea + Qt_TopToolBarArea + Qt_BottomToolBarArea )
   oTBar:oWidget:setFocusPolicy( Qt_NoFocus )

   oTBar:buttonClick := {|oButton| ::execEvent( "tool_button_clicked", oButton:key ) }

   oTBar:addItem( "Exit"     , ":/exit.png"     , , , , , "Exit"      )
   oTBar:addItem( , , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "Terminate", ":/terminate.png", , , , , "Terminate" )
   oTBar:addItem( "ManageIPs", ":/refresh.png"  , , , , , "IPs"       )
   oTBar:addItem( "About"    , ":/about.png"    , , , , , "About"     )
   oTBar:addItem( "Help"     , ":/help.png"     , , , , , "Help"      )

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD NetIOMgmtClient:confirmExit()

   IF ConfirmBox( , "Do you want to exit the management client?", " Please confirm", XBPMB_YESNO, XBPMB_CRITICAL ) == XBPMB_RET_YES
      PostAppEvent( xbeP_Quit, , , ::oDlg )
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD NetIOMgmtClient:buildSystemTray()

   IF empty( ::oSys )
      ::oSys := QSystemTrayIcon( ::oDlg:oWidget )
      IF ( ::lSystemTrayAvailable := ::oSys:isSystemTrayAvailable() )
         ::oSys:setIcon( ":/harbour.png" )
         ::oSys:connect( "activated(QSystemTrayIcon::ActivationReason)", {|p| ::execEvent( "qSystemTrayIcon_activated", p ) } )

         ::oSysMenu := QMenu()
         ::qAct1 := ::oSysMenu:addAction( ":/fullscreen.png", "&Show" )
         ::oSysMenu:addSeparator()
         ::qAct2 := ::oSysMenu:addAction( ":/exit.png", "&Exit" )

         ::qAct1:connect( "triggered(bool)", {|| ::execEvent( "qSystemTrayIcon_show"  ) } )
         ::qAct2:connect( "triggered(bool)", {|| ::execEvent( "qSystemTrayIcon_close" ) } )

         ::oSys:setContextMenu( ::oSysMenu )
         ::oSys:hide()
         ::oSys:setToolTip( "Connected to Harbour NetIO Server: " + ::oDlg:title )
      ENDIF
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD NetIOMgmtClient:cmdConnStop( cIPPort )

   IF Empty( ::pConnection )
      MsgBox( "Not Connected" )
   ELSE
      netio_funcexec( ::pConnection, "hbnetiomgm_stop", cIPPort )
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD NetIOMgmtClient:cmdConnInfo( lManagement )
   LOCAL aArray
   LOCAL hConn
   LOCAL aData
   LOCAL d_

   IF Empty( ::pConnection )
      MsgBox( "Not Connected" )
   ELSE
      IF ::lProcessing
         RETURN NIL
      ENDIF

      ::lProcessing := .t.

      aArray := netio_funcexec( ::pConnection, iif( lManagement, "hbnetiomgm_adminfo", "hbnetiomgm_conninfo" ) )
      IF ! empty( aArray )
         aData := {}
         FOR EACH hConn IN aArray
            d_:= array( 10 )
            d_[ DAT_CONNSOCKET ] := NIL
            d_[ DAT_SERIAL     ] := hConn[ "nThreadID"      ]
            d_[ DAT_ACTIVATED  ] := .t.
            d_[ DAT_IP         ] := hConn[ "cAddressPeer"   ]
            d_[ DAT_PORT       ] := 0
            d_[ DAT_TIMEIN     ] := hb_TToC( hConn[ "tStart" ], "YYYY.MM.DD", "HH:MM:SS" )
            d_[ DAT_TIMEOUT    ] := space( 19 )
            d_[ DAT_BYTESIN    ] := hConn[ "nBytesReceived" ]
            d_[ DAT_BYTESOUT   ] := hConn[ "nBytesSent"     ]
            d_[ DAT_OPENFILES  ] := hConn[ "nFilesCount"    ]
            aadd( aData, d_ )
         NEXT
         ::aData := aData
      ELSE
         ::aData := { { NIL, 0, .F., space( 17 ), 0, space( 19 ), space( 19 ), 0, 0, NIL, 0 } }
      ENDIF
      ::lProcessing := .f.
      ::refresh()
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION AppSys()
   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC PROCEDURE HB_Logo()

   MsgBox( "Harbour NETIO Server Management Console " + StrTran( Version(), "Harbour " ) + hb_eol() +;
           "Copyright (c) 2009-2011, Pritpal Bedi, Viktor Szakats" + hb_eol() + ;
           "http://harbour-project.org/" + hb_eol() +;
           hb_eol() )

   RETURN

/*----------------------------------------------------------------------*/

STATIC PROCEDURE HB_Usage()
   LOCAL aMsg := {}
   LOCAL cMsg

   AAdd( aMsg,               "Syntax:"                                                                                 )
   AAdd( aMsg,                                                                                                         )
   AAdd( aMsg,               "  netiocui [options]"                                                                    )
   AAdd( aMsg,                                                                                                         )
   AAdd( aMsg,               "Options:"                                                                                )
   AAdd( aMsg,                                                                                                         )
   AAdd( aMsg,               "  -addr=<ip[:port]>  connect to netio server on IPv4 address <ip:port>"                  )
   AAdd( aMsg, hb_StrFormat( "                     Default: %1$s:%2$d", _NETIOMGM_IPV4_DEF, _NETIOMGM_PORT_DEF )       )
   AAdd( aMsg,               "  -pass=<passwd>     connect to netio server with password"                              )
   AAdd( aMsg,                                                                                                         )
   AAdd( aMsg,               "  --version          display version header only"                                        )
   AAdd( aMsg,               "  -help|--help       this help"                                                          )

   cMsg := ""
   aeval( aMsg, {|e| cMsg += e + chr( 10 ) } )
   MsgBox( cMsg )

   RETURN

/*----------------------------------------------------------------------*/
