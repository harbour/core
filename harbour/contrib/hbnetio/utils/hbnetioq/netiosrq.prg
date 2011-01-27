/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for alternative RDD IO API which uses own
 *    very simple TCP/IP file server.
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 * Copyright 2011 Pritpal Bedi <bedipritpal@hotmail.com>
 *
 * See COPYING for licensing terms.
 *
 */

/* netio_mtserver() needs MT HVM version */
REQUEST HB_MT
REQUEST __HB_EXTERN__

#define _RPC_FILTER "HBNETIOSRV_RPCMAIN"

#define _NETIOSRV_nPort             1
#define _NETIOSRV_cIFAddr           2
#define _NETIOSRV_cRootDir          3
#define _NETIOSRV_lRPC              4
#define _NETIOSRV_cRPCFFileName     5
#define _NETIOSRV_hRPCFHRB          6
#define _NETIOSRV_lEncryption       7
#define _NETIOSRV_pListenSocket     8
#define _NETIOSRV_nCompressionLevel 9
#define _NETIOSRV_nStrategy         10
#define _NETIOSRV_cPassword         11
#define _NETIOSRV_MAX_              11

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

#include "hbhrb.ch"
#include "fileio.ch"
#include "hbclass.ch"
#include "common.ch"
#include "hbqtgui.ch"
#include "hbnetio.ch"

#include "Xbp.ch"
#include "Gra.ch"
#include "Appevent.ch"

#define RGB( r, g, b )   GraMakeRGBColor( { r, g, b } )

/*----------------------------------------------------------------------*/

Function Main( ... )
   LOCAL netiosrv[ _NETIOSRV_MAX_ ]
   LOCAL cParam, cExt, cFile

   netiosrv[ _NETIOSRV_nPort       ] := 2941
   netiosrv[ _NETIOSRV_cIFAddr     ] := "0.0.0.0"
   netiosrv[ _NETIOSRV_cRootDir    ] := hb_dirBase()
   netiosrv[ _NETIOSRV_lRPC        ] := .F.
   netiosrv[ _NETIOSRV_lEncryption ] := .F.

   FOR EACH cParam IN { ... }
      DO CASE
      CASE Lower( Left( cParam, 6 ) ) == "-port="
         netiosrv[ _NETIOSRV_nPort ] := Val( SubStr( cParam, 7 ) )
      CASE Lower( Left( cParam, 7 ) ) == "-iface="
         netiosrv[ _NETIOSRV_cIFAddr ] := SubStr( cParam, 8 )
      CASE Lower( Left( cParam, 9 ) ) == "-rootdir="
         netiosrv[ _NETIOSRV_cRootDir ] := SubStr( cParam, 10 )
      CASE Lower( Left( cParam, 6 ) ) == "-pass="
         netiosrv[ _NETIOSRV_cPassword ]:= SubStr( cParam, 7 )
         hb_StrClear( @cParam )
      CASE Lower( Left( cParam, 5 ) ) == "-rpc="
         netiosrv[ _NETIOSRV_cRPCFFileName ] := SubStr( cParam, 6 )
         hb_FNameSplit( netiosrv[ _NETIOSRV_cRPCFFileName ], NIL, NIL, @cExt )
         cExt := Lower( cExt )
         SWITCH cExt
            CASE ".prg"
            CASE ".hbs"
            CASE ".hrb"
               EXIT
            OTHERWISE
               cExt := FileSig( cFile )
         ENDSWITCH
         SWITCH cExt
            CASE ".prg"
            CASE ".hbs"
               cFile := HB_COMPILEBUF( HB_ARGV( 0 ), "-n2", "-w", "-es2", "-q0",;
                                       "-D" + "__HBSCRIPT__HBNETIOSRV", netiosrv[ _NETIOSRV_cRPCFFileName ] )
               IF cFile != NIL
                  netiosrv[ _NETIOSRV_hRPCFHRB ] := hb_hrbLoad( HB_HRB_BIND_FORCELOCAL, cFile )
               ENDIF
               EXIT
            OTHERWISE
               netiosrv[ _NETIOSRV_hRPCFHRB ] := hb_hrbLoad( HB_HRB_BIND_FORCELOCAL, netiosrv[ _NETIOSRV_cRPCFFileName ] )
               EXIT
         ENDSWITCH
         netiosrv[ _NETIOSRV_lRPC ] := ! Empty( netiosrv[ _NETIOSRV_hRPCFHRB ] ) .AND. ! Empty( hb_hrbGetFunSym( netiosrv[ _NETIOSRV_hRPCFHRB ], _RPC_FILTER ) )
         IF ! netiosrv[ _NETIOSRV_lRPC ]
            netiosrv[ _NETIOSRV_cRPCFFileName ] := NIL
            netiosrv[ _NETIOSRV_hRPCFHRB ] := NIL
         ENDIF
      CASE Lower( cParam ) == "-rpc"
         netiosrv[ _NETIOSRV_lRPC ] := .T.
      CASE Lower( cParam ) == "--version"
         RETURN NIL
      CASE Lower( cParam ) == "-help" .OR. ;
         Lower( cParam ) == "--help"
         HB_Usage()
         RETURN NIL
      OTHERWISE
         OutStd( "Warning: Unkown parameter ignored: " + cParam + hb_eol() )
      ENDCASE
   NEXT

   NetIOServer():new():create( netiosrv )

   RETURN ( NIL )

/*----------------------------------------------------------------------*/

CLASS NetIOServer
   DATA   nNumConxn                               INIT 0
   DATA   oDlg
   DATA   oBrw
   DATA   cTitle
   DATA   pMtx
   DATA   oSys
   DATA   lSystemTrayAvailable
   DATA   oSysMenu
   DATA   qTimer
   DATA   qLayout
   DATA   qAct1
   DATA   qAct2
   DATA   nPrevWindowState
   DATA   lChanging                               INIT .f.
   DATA   lQuit                                   INIT .f.
   DATA   nCurRec                                 INIT 1
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
   METHOD create( netiosrv )
   METHOD execEvent( cEvent, p )

   METHOD custom_netio_server( pConnectionSocket )
   METHOD register_connection( pConnectionSocket )
   METHOD unregister_connection( pConnectionSocket )

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

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD NetIOServer:new()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD NetIOServer:create( netiosrv )
   LOCAL nEvent, mp1, mp2, oXbp

   netiosrv[ _NETIOSRV_pListenSocket ] := ;
      netio_mtserver( netiosrv[ _NETIOSRV_nPort     ],;
                      netiosrv[ _NETIOSRV_cIFAddr   ],;
                      netiosrv[ _NETIOSRV_cRootDir  ],;
                      iif( Empty( netiosrv[ _NETIOSRV_hRPCFHRB ] ), netiosrv[ _NETIOSRV_lRPC ], ;
                                 hb_hrbGetFunSym( netiosrv[ _NETIOSRV_hRPCFHRB ], _RPC_FILTER ) ),;
                      netiosrv[ _NETIOSRV_cPassword ], ;
                      netiosrv[ _NETIOSRV_nCompressionLevel ], ;
                      netiosrv[ _NETIOSRV_nStrategy ], ;
                      {|p| ::custom_netio_server( p ) } )

   netiosrv[ _NETIOSRV_lEncryption ] := ! Empty( netiosrv[ _NETIOSRV_cPassword ] )

   IF Empty( netiosrv[ _NETIOSRV_pListenSocket ] )
      MsgBox( "Cannot start server." )

   ELSE
      QResource():registerResource_1( hbqtres_netiosrq(), ":/resource" )

      ::pMtx            := hb_mutexCreate()
      ::cTitle          := "NetIO Server [ " + netiosrv[ _NETIOSRV_cIFAddr ] + " : " + ;
                                               ltrim( str( int( netiosrv[ _NETIOSRV_nPort ] ) ) ) + " : " + ;
                                               netiosrv[ _NETIOSRV_cRootDir ] + " ]"

      ::oDlg            := XbpDialog():new( , , { 20,20 }, { 850,300 } )
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

      netio_serverStop( netiosrv[ _NETIOSRV_pListenSocket ] )
      netiosrv[ _NETIOSRV_pListenSocket ] := NIL

      ::oDlg:destroy()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD NetIOServer:custom_netio_server( pConnectionSocket )
   ::register_connection( pConnectionSocket )
   BEGIN SEQUENCE
      netio_server( pConnectionSocket )
   END SEQUENCE
   ::unregister_connection( pConnectionSocket )
   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD NetIOServer:register_connection( pConnectionSocket )
   LOCAL aPeer, cIP := "", nPort := 0

   netio_srvStatus( pConnectionSocket, NETIO_SRVINFO_PEERADDRESS, @aPeer )
   IF hb_isArray( aPeer )
      IF len( aPeer ) >= 2
         cIP := xtos( aPeer[ 2 ] )
      ENDIF
      IF len( aPeer ) >= 3
         nPort := val( xtos( aPeer[ 3 ] ) )
      ENDIF
   ENDIF

   IF hb_mutexLock( ::pMtx )
      IF ::aData[ 1,2 ] == 0
         ::aData[ 1 ] := { pConnectionSocket, 1, .t., pad( cIP, 15 ), nPort, dtoc( date() ) + "  " + time(), space( 18 ), 0, 0, 0 }
      ELSE
         aadd( ::aData, { pConnectionSocket, len( ::aData ) + 1, .t., pad( cIP, 15 ), nPort, dtoc( date() ) + "  " + time(), space( 18 ), 0, 0, 0 } )
      ENDIF
      hb_mutexUnlock( ::pMtx )
   ENDIF
   ::nNumConxn++
   ::oDlg:title := ::cTitle + " - " + ltrim( str( ::nNumConxn, 6, 0 ) )
   ::refresh()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD NetIOServer:unregister_connection( pConnectionSocket )
   LOCAL n

   ::nNumConxn--
   ::oDlg:title := ::cTitle + " - " + ltrim( str( ::nNumConxn, 6, 0 ) )
   if ( n := ascan( ::aData, {|e_| e_[ DAT_CONNSOCKET ] == pConnectionSocket } ) ) > 0
      IF hb_mutexLock( ::pMtx )
         ::aData[ n, DAT_ACTIVATED  ] := .f.
         ::aData[ n, DAT_TIMEOUT    ] := dtoc( date() ) + "  " + time()
         ::aData[ n, DAT_CONNSOCKET ] := NIL
         hb_mutexUnlock( ::pMtx )
      ENDIF
   ENDIF
   ::refresh()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD NetIOServer:execEvent( cEvent, p )
   LOCAL qEvent, oMenu, txt_, s

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
         AAdd( txt_, "<b>Harbour NetIO Server</b>" )
         AAdd( txt_, "Developed by:" )
         AAdd( txt_, "Przemyslaw Czerpak" )
         AAdd( txt_, "Viktor Szakats" )
         AAdd( txt_, "Pritpal Bedi" )
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
         MsgBox( s, " About NetIO Server" )
         EXIT
      CASE "Terminate"
         ::terminate()
         EXIT
      CASE "Exit"
         PostAppEvent( xbeP_Quit, , , ::oDlg )
         EXIT
      ENDSWITCH
      EXIT
   CASE "QEvent_WindowStateChange"
      qEvent := QWindowStateChangeEventFromPointer( p )
      ::nPrevWindowState := qEvent:oldState()
      EXIT
   CASE "QEvent_Hide"
      IF ::lSystemTrayAvailable
         qEvent := QHideEventFromPointer( p )
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
   CASE "qTimer_timeOut"
      ::oDlg:hide()
      ::oSys:setToolTip( "Harbour NetIO Server: " + ::oDlg:title )
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
   ENDSWITCH

   RETURN 0

/*----------------------------------------------------------------------*/

METHOD NetIOServer:showDlgBySystemTrayIconCommand()

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

METHOD NetIOServer:buildBrowser()
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

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD NetIOServer:terminate()

   IF ::aData[ ::recNo(), DAT_ACTIVATED ] .AND. ::aData[ ::recNo(), DAT_CONNSOCKET ] != NIL
      IF ConfirmBox( , ;
             "Terminating: " + ::aData[ ::recNo(), DAT_IP ] + " : " + hb_ntos( ::aData[ ::recNo(), DAT_PORT ] ), ;
             "Critical, be careful", ;
             , ;
             XBPMB_CRITICAL ) == XBPMB_RET_OK

         netio_serverStop( ::aData[ ::recNo(), DAT_CONNSOCKET ], .t. )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD NetIOServer:refresh()
   ::oBrw:refreshAll()
   ::oBrw:forceStable()
   ::oDlg:oWidget:setGeometry( ::oDlg:oWidget:geometry() )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD NetIOServer:skipBlock( nHowMany )
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

METHOD NetIOServer:goTop()
   ::nCurRec := 1
   ::refresh()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD NetIOServer:goBottom()
   ::nCurRec := len( ::aData )
   ::refresh()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD NetIOServer:goto( nRec )
   IF nRec > 0 .AND. nRec <= len( ::aData )
      ::nCurRec := nRec
      ::refresh()
   ENDIF
   RETURN .t.

/*----------------------------------------------------------------------*/

METHOD NetIOServer:lastRec()
   RETURN len( ::aData )

/*----------------------------------------------------------------------*/

METHOD NetIOServer:recNo()
   RETURN ::nCurRec

/*----------------------------------------------------------------------*/

METHOD NetIOServer:buildColumns()
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
   aadd( aPP, { XBP_PP_COL_HA_CAPTION      , "IP"              } )
   aadd( aPP, { XBP_PP_COL_HA_FGCLR        , nClrHFg           } )
   aadd( aPP, { XBP_PP_COL_HA_BGCLR        , nClrHBg           } )
   aadd( aPP, { XBP_PP_COL_HA_HEIGHT       , 20                } )
   aadd( aPP, { XBP_PP_COL_DA_FGCLR        , GRA_CLR_BLACK     } )
   aadd( aPP, { XBP_PP_COL_DA_BGCLR        , nClrBG            } )
   aadd( aPP, { XBP_PP_COL_DA_HILITE_FGCLR , GRA_CLR_WHITE     } )
   aadd( aPP, { XBP_PP_COL_DA_HILITE_BGCLR , GRA_CLR_DARKGRAY  } )
   aadd( aPP, { XBP_PP_COL_DA_ROWHEIGHT    , 20                } )
   aadd( aPP, { XBP_PP_COL_DA_ROWWIDTH     , 150               } )
   //
   oXbpColumn          := XbpColumn():new()
   oXbpColumn:dataLink := {|| ::aData[ ::recNo(), DAT_IP ] }
   oXbpColumn:create( , , , , aPP )
   ::oBrw:addColumn( oXbpColumn )

   aPP := {}
   aadd( aPP, { XBP_PP_COL_HA_CAPTION      , "Port"            } )
   aadd( aPP, { XBP_PP_COL_HA_FGCLR        , nClrHFg           } )
   aadd( aPP, { XBP_PP_COL_HA_BGCLR        , nClrHBg           } )
   aadd( aPP, { XBP_PP_COL_HA_HEIGHT       , 20                } )
   aadd( aPP, { XBP_PP_COL_DA_FGCLR        , GRA_CLR_BLACK     } )
   aadd( aPP, { XBP_PP_COL_DA_BGCLR        , nClrBG            } )
   aadd( aPP, { XBP_PP_COL_DA_HILITE_FGCLR , GRA_CLR_WHITE     } )
   aadd( aPP, { XBP_PP_COL_DA_HILITE_BGCLR , GRA_CLR_DARKGRAY  } )
   aadd( aPP, { XBP_PP_COL_DA_ROWHEIGHT    , 20                } )
   aadd( aPP, { XBP_PP_COL_DA_ROWWIDTH     , 55                } )
   //
   oXbpColumn          := XbpColumn():new()
   oXbpColumn:dataLink := {|| str( ::aData[ ::recNo(), DAT_PORT ], 5, 0 ) }
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
   aadd( aPP, { XBP_PP_COL_DA_ROWWIDTH     , 152               } )
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
   aadd( aPP, { XBP_PP_COL_DA_ROWWIDTH     , 152               } )
   //
   oXbpColumn          := XbpColumn():new()
   oXbpColumn:dataLink := {|| ::aData[ ::recNo(), DAT_TIMEOUT ] }
   oXbpColumn:create( , , , , aPP )
   ::oBrw:addColumn( oXbpColumn )

   aPP := {}
   aadd( aPP, { XBP_PP_COL_HA_CAPTION      , "KbIN"            } )
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
   oXbpColumn:dataLink   := {|n| iif( empty( ::aData[ ::recNo(), DAT_CONNSOCKET ] ), n := ::aData[ ::recNo(), DAT_BYTESIN ], ;
                                 netio_srvStatus( ::aData[ ::recNo(), DAT_CONNSOCKET ], NETIO_SRVINFO_BYTESRECEIVED, @n ) ), ;
                                 ::aData[ ::recNo(), DAT_BYTESIN ] := n, str( n, 10, 0 ) }
   oXbpColumn:create( , , , , aPP )
   ::oBrw:addColumn( oXbpColumn )

   aPP := {}
   aadd( aPP, { XBP_PP_COL_HA_CAPTION      , "KbOUT"           } )
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
   oXbpColumn:dataLink   := {|n| iif( empty( ::aData[ ::recNo(), DAT_CONNSOCKET ] ), n := ::aData[ ::recNo(), DAT_BYTESOUT ], ;
                                 netio_srvStatus( ::aData[ ::recNo(), DAT_CONNSOCKET ], NETIO_SRVINFO_BYTESSENT, @n ) ), ;
                                 ::aData[ ::recNo(), DAT_BYTESOUT ] := n, str( n, 10, 0 ) }
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
   oXbpColumn:dataLink   := {|n| iif( empty( ::aData[ ::recNo(), DAT_CONNSOCKET ] ), n := 0, ;
                                netio_srvStatus( ::aData[ ::recNo(), DAT_CONNSOCKET ], NETIO_SRVINFO_FILESCOUNT, @n ) ), str( n, 5, 0 ) }
   oXbpColumn:create( , , , , aPP )
   ::oBrw:addColumn( oXbpColumn )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD NetIOServer:buildToolBar()
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
   oTBar:addItem( "About"    , ":/about.png"    , , , , , "About"     )
   oTBar:addItem( "Help"     , ":/help.png"     , , , , , "Help"      )

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD NetIOServer:confirmExit()

   IF ConfirmBox( , "Do you want to shut-down the server ?", " Please confirm", XBPMB_YESNO, XBPMB_CRITICAL ) == XBPMB_RET_YES
      PostAppEvent( xbeP_Quit, , , ::oDlg )
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD NetIOServer:buildSystemTray()

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
         ::oSys:setToolTip( "Harbour NetIO Server: " + ::oDlg:title )
      ENDIF
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION AppSys()
   RETURN ( NIL )

/*----------------------------------------------------------------------*/

STATIC FUNCTION FileSig( cFile )
   LOCAL hFile
   LOCAL cBuff, cSig, cExt

   cExt := ".prg"
   hFile := FOpen( cFile, FO_READ )
   IF hFile != F_ERROR
      cSig := hb_hrbSignature()
      cBuff := Space( Len( cSig ) )
      FRead( hFile, @cBuff, Len( cSig ) )
      FClose( hFile )
      IF cBuff == cSig
         cExt := ".hrb"
      ENDIF
   ENDIF

   RETURN cExt

/*----------------------------------------------------------------------*/

STATIC PROCEDURE ShowConfig( netiosrv )
   LOCAL cMsg := ""

   cMsg += "Listening on: "      + netiosrv[ _NETIOSRV_cIFAddr ] + ":" + hb_ntos( netiosrv[ _NETIOSRV_nPort ] ) + hb_eol()
   cMsg += "Root filesystem: "   + netiosrv[ _NETIOSRV_cRootDir ] + hb_eol()
   cMsg += "RPC support: "       + iif( netiosrv[ _NETIOSRV_lRPC ], "enabled", "disabled" ) + hb_eol()
   cMsg += "Encryption: "        + iif( netiosrv[ _NETIOSRV_lEncryption ], "enabled", "disabled" ) + hb_eol()
   cMsg += "RPC filter module: " + iif( Empty( netiosrv[ _NETIOSRV_hRPCFHRB ] ), iif( netiosrv[ _NETIOSRV_lRPC ], "not set (WARNING: unsafe open server)", "not set" ), netiosrv[ _NETIOSRV_cRPCFFileName ] )

   MsgBox( cMsg )

   RETURN

/*----------------------------------------------------------------------*/

STATIC PROCEDURE HB_Logo()

   MsgBox( "Harbour NETIO Server " + HBRawVersion() + hb_eol() +;
           "Copyright (c) 2009-2011, Przemyslaw Czerpak" + hb_eol() + ;
           "http://harbour-project.org/" )

   RETURN

/*----------------------------------------------------------------------*/

STATIC PROCEDURE HB_Usage()
   LOCAL aMsg := {}
   LOCAL cMsg

   AAdd( aMsg,               "Syntax:"                                                                                    )
   AAdd( aMsg,               " "                                                                                          )
   AAdd( aMsg,               "  netiosrv [options]"                                                                       )
   AAdd( aMsg,               " "                                                                                          )
   AAdd( aMsg,               "Options:"                                                                                   )
   AAdd( aMsg,               " "                                                                                          )
   AAdd( aMsg,               "  -port=<port>        accept incoming connections on IP port <port>"                        )
   AAdd( aMsg,               "  -iface=<ipaddr>     accept incoming connections on IPv4 interface <ipaddress>"            )
   AAdd( aMsg,               "  -rootdir=<rootdir>  use <rootdir> as root directory for served file system"               )
   AAdd( aMsg,               "  -rpc                accept RPC requests"                                                  )
   AAdd( aMsg,               "  -rpc=<file.hrb>     set RPC processor .hrb module to <file.hrb>"                          )
   AAdd( aMsg, hb_StrFormat( "                      file.hrb needs to have an entry function named %1$s()", _RPC_FILTER ) )
   AAdd( aMsg,               "  -pass=<passwd>      set server password"                                                  )
   AAdd( aMsg,               " "                                                                                          )
   AAdd( aMsg,               "  --version           display version header only"                                          )
   AAdd( aMsg,               "  -help|--help        this help"                                                            )

   cMsg := ""
   aeval( aMsg, {|e| cMsg += e + chr( 10 ) } )
   MsgBox( cMsg )

   RETURN

/*----------------------------------------------------------------------*/

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour " )

/*----------------------------------------------------------------------*/

STATIC FUNCTION xtos( xVrb )

   SWITCH valtype( xVrb )
   CASE "C"
      RETURN xVrb
   CASE "N"
      RETURN ltrim( str( xVrb ) )
   CASE "L"
      RETURN iif( xVrb, "T", "F" )
   CASE "M"
      RETURN xVrb
   CASE "A"
      RETURN "A"
   OTHERWISE
      RETURN ""
   ENDSWITCH

   RETURN NIL

/*----------------------------------------------------------------------*/
