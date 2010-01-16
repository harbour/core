/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for alternative RDD IO API which uses own
 *    very simple TCP/IP file server.
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 *    ...
 *
 * See COPYING for licensing terms.
 *
 */

#include "hbhrb.ch"

#include "color.ch"
#include "hbgtinfo.ch"
#include "inkey.ch"
#include "setcurs.ch"

/* netio_mtserver() needs MT HVM version */
REQUEST HB_MT

#define _RPC_FILTER "HBNETIOSRV_RPCMAIN"

/* enable this if you need all core functions in RPC support */
#ifdef HB_EXTERN
   REQUEST __HB_EXTERN__
#endif

#define _NETIOSRV_nPort             1
#define _NETIOSRV_cIFAddr           2
#define _NETIOSRV_cRootDir          3
#define _NETIOSRV_lRPC              4
#define _NETIOSRV_cRPCFFileName     5
#define _NETIOSRV_cRPCFHRB          6
#define _NETIOSRV_lEncryption       7
#define _NETIOSRV_pListenSocket     8
#define _NETIOSRV_MAX_              8

PROCEDURE Main( ... )
   LOCAL netiosrv[ _NETIOSRV_MAX_ ]

   LOCAL cParam
   LOCAL cCommand
   LOCAL cPassword

   LOCAL bKeyDown
   LOCAL bKeyUp
   LOCAL bKeyIns
   LOCAL bKeyPaste

   LOCAL GetList := {}

   LOCAL aHistory, nHistIndex

   HB_Logo()

   netiosrv[ _NETIOSRV_nPort ]         := 2941
   netiosrv[ _NETIOSRV_cIFAddr ]       := "0.0.0.0"
   netiosrv[ _NETIOSRV_cRootDir ]      := hb_dirBase()
   netiosrv[ _NETIOSRV_lRPC ]          := .F.
   netiosrv[ _NETIOSRV_lEncryption ]   := .F.

   FOR EACH cParam IN { ... }
      DO CASE
      CASE Lower( Left( cParam, 6 ) ) == "-port="
         netiosrv[ _NETIOSRV_nPort ] := Val( SubStr( cParam, 7 ) )
      CASE Lower( Left( cParam, 7 ) ) == "-iface="
         netiosrv[ _NETIOSRV_cIFAddr ] := SubStr( cParam, 8 )
      CASE Lower( Left( cParam, 9 ) ) == "-rootdir="
         netiosrv[ _NETIOSRV_cRootDir ] := SubStr( cParam, 10 )
      CASE Lower( Left( cParam, 6 ) ) == "-pass="
         cPassword := SubStr( cParam, 7 )
         hb_StrClear( @cParam )
      CASE Lower( Left( cParam, 5 ) ) == "-rpc="
         netiosrv[ _NETIOSRV_cRPCFFileName ] := SubStr( cParam, 6 )
         netiosrv[ _NETIOSRV_cRPCFHRB ] := hb_hrbLoad( netiosrv[ _NETIOSRV_cRPCFFileName ] )
         netiosrv[ _NETIOSRV_lRPC ] := ! Empty( netiosrv[ _NETIOSRV_cRPCFHRB ] ) .AND. ! Empty( hb_hrbGetFunSym( netiosrv[ _NETIOSRV_cRPCFHRB ], _RPC_FILTER ) )
         IF ! netiosrv[ _NETIOSRV_lRPC ]
            netiosrv[ _NETIOSRV_cRPCFFileName ] := NIL
            netiosrv[ _NETIOSRV_cRPCFHRB ] := NIL
         ENDIF
      CASE Lower( cParam ) == "-rpc"
         netiosrv[ _NETIOSRV_lRPC ] := .T.
      CASE Lower( cParam ) == "--version"
         RETURN
      CASE Lower( cParam ) == "-help" .OR. ;
           Lower( cParam ) == "--help"
         HB_Usage()
         RETURN
      OTHERWISE
         OutStd( "Warning: Unkown parameter ignored: " + cParam + hb_osNewLine() )
      ENDCASE
   NEXT

   SetCancel( .F. )

   netiosrv[ _NETIOSRV_pListenSocket ] := ;
      netio_mtserver( netiosrv[ _NETIOSRV_nPort ],;
                      netiosrv[ _NETIOSRV_cIFAddr ],;
                      netiosrv[ _NETIOSRV_cRootDir ],;
                      iif( Empty( netiosrv[ _NETIOSRV_cRPCFHRB ] ), netiosrv[ _NETIOSRV_lRPC ], hb_hrbGetFunSym( netiosrv[ _NETIOSRV_cRPCFHRB ], _RPC_FILTER ) ),;
                      @cPassword )

   netiosrv[ _NETIOSRV_lEncryption ] := ! Empty( cPassword )
   cPassword := NIL

   IF Empty( netiosrv[ _NETIOSRV_pListenSocket ] )
      OutStd( "Cannot start server." + hb_osNewLine() )
   ELSE
      ShowConfig( netiosrv )

      OutStd( hb_osNewLine() )
      OutStd( "hbnetiosrv command prompt:", hb_osNewLine() )

      aHistory := { "quit" }
      nHistIndex := Len( aHistory ) + 1

      /* Command prompt */
      DO WHILE .T.

         cCommand := Space( 128 )

         QQOut( "hbnetiosrv$ " )
         @ Row(), Col() GET cCommand PICTURE "@S" + hb_ntos( MaxCol() - Col() + 1 ) COLOR hb_ColorIndex( SetColor(), CLR_STANDARD ) + "," + hb_ColorIndex( SetColor(), CLR_STANDARD )

         SetCursor( iif( ReadInsert(), SC_INSERT, SC_NORMAL ) )

         bKeyIns  := SetKey( K_INS, ;
            {|| SetCursor( iif( ReadInsert( ! ReadInsert() ), ;
                             SC_NORMAL, SC_INSERT ) ) } )
         bKeyUp   := SetKey( K_UP, ;
            {|| iif( nHistIndex > 1, ;
                     cCommand := PadR( aHistory[ --nHistIndex ], Len( cCommand ) ), ) } )
         bKeyDown := SetKey( K_DOWN, ;
            {|| cCommand := PadR( iif( nHistIndex < Len( aHistory ), ;
                aHistory[ ++nHistIndex ], ;
                ( nHistIndex := Len( aHistory ) + 1, "" ) ), Len( cCommand ) ) } )
         bKeyPaste := SetKey( K_ALT_V, {|| hb_gtInfo( HB_GTI_CLIPBOARDPASTE ) } )

         READ

         SetKey( K_DOWN, bKeyPaste )
         SetKey( K_DOWN, bKeyDown  )
         SetKey( K_UP,   bKeyUp    )
         SetKey( K_INS,  bKeyIns   )

         QQOut( hb_osNewLine() )
         QQOut( hb_osNewLine() )

         cCommand := AllTrim( cCommand )

         IF Empty( aHistory ) .OR. ! ATail( aHistory ) == cCommand
            IF Len( aHistory ) < 64
               AAdd( aHistory, cCommand )
            ELSE
               ADel( aHistory, 1 )
               aHistory[ Len( aHistory ) ] := cCommand
            ENDIF
         ENDIF
         nHistIndex := Len( aHistory ) + 1

         /* TODO: - on the fly change of RPC filter modules
                  - listing active connections
                  - listing open files
                  - listing active locks
                  - activity meters (transferred bytes, bandwidth, etc)
                  - showing number of connections
                  - showing number of open files
                  - listing transferred bytes
                  - gracefully shutting down server by waiting for connections to close and not accept new ones
                  - pausing server */

         DO CASE
         CASE Lower( cCommand ) == "quit"
            EXIT
         CASE Lower( cCommand ) == "config"
            ShowConfig( netiosrv )
         CASE Lower( cCommand ) == "sysinfo"
            QQOut( "OS: "         + OS(), hb_osNewLine() )
            QQOut( "Harbour: "    + Version(), hb_osNewLine() )
            QQOut( "C Compiler: " + hb_Compiler(), hb_osNewLine() )
            QQOut( "Memory: "     + hb_ntos( Memory( 0 ) ) + "KB", hb_osNewLine() )
         CASE Lower( cCommand ) == "help"
            QQOut( "config  - Show server configuration", hb_osNewLine() )
            QQOut( "sysinfo - Show system/build information", hb_osNewLine() )
            QQOut( "quit    - Stop server and exit", hb_osNewLine() )
         CASE ! Empty( cCommand )
            QQOut( "Error: Unknown command.", hb_osNewLine() )
         ENDCASE
      ENDDO

      netio_serverstop( netiosrv[ _NETIOSRV_pListenSocket ] )
      netiosrv[ _NETIOSRV_pListenSocket ] := NIL

      OutStd( hb_osNewLine() )
      OutStd( "Server stopped.", hb_osNewLine() )
   ENDIF

   RETURN

STATIC PROCEDURE ShowConfig( netiosrv )

   QQOut( "Listening on: "      + netiosrv[ _NETIOSRV_cIFAddr ] + ":" + hb_ntos( netiosrv[ _NETIOSRV_nPort ] ), hb_osNewLine() )
   QQOut( "Root filesystem: "   + netiosrv[ _NETIOSRV_cRootDir ], hb_osNewLine() )
   QQOut( "RPC support: "       + iif( netiosrv[ _NETIOSRV_lRPC ], "enabled", "disabled" ), hb_osNewLine() )
   QQOut( "Encryption: "        + iif( netiosrv[ _NETIOSRV_lEncryption ], "enabled", "disabled" ), hb_osNewLine() )
   QQOut( "RPC filter module: " + iif( Empty( netiosrv[ _NETIOSRV_cRPCFHRB ] ), iif( netiosrv[ _NETIOSRV_lRPC ], "not set (WARNING: unsafe open server)", "not set" ), netiosrv[ _NETIOSRV_cRPCFFileName ] ), hb_osNewLine() )

   RETURN

STATIC PROCEDURE HB_Logo()

   OutStd( "Harbour NETIO Server " + HBRawVersion() + hb_osNewLine() +;
           "Copyright (c) 2009, Przemyslaw Czerpak" + hb_osNewLine() + ;
           "http://www.harbour-project.org/" + hb_osNewLine() +;
           hb_osNewLine() )

   RETURN

STATIC PROCEDURE HB_Usage()

   OutStd(               "Syntax:"                                                                                   , hb_osNewLine() )
   OutStd(                                                                                                             hb_osNewLine() )
   OutStd(               "  netiosrv [options]"                                                                      , hb_osNewLine() )
   OutStd(                                                                                                             hb_osNewLine() )
   OutStd(               "Options:"                                                                                  , hb_osNewLine() )
   OutStd(                                                                                                             hb_osNewLine() )
   OutStd(               "  -port=<port>        accept incoming connections on IP port <port>"                       , hb_osNewLine() )
   OutStd(               "  -iface=<ipaddr>     accept incoming connections on IPv4 interface <ipaddress>"           , hb_osNewLine() )
   OutStd(               "  -rootdir=<rootdir>  use <rootdir> as root directory for served file system"              , hb_osNewLine() )
   OutStd(               "  -rpc                accept RPC requests"                                                 , hb_osNewLine() )
   OutStd(               "  -rpc=<file.hrb>     set RPC processor .hrb module to <file.hrb>"                         , hb_osNewLine() )
   OutStd( hb_StrFormat( "                      file.hrb needs to have an entry function named %1$s()", _RPC_FILTER ), hb_osNewLine() )
   OutStd(               "  -pass=<passwd>      set server password"                                                 , hb_osNewLine() )
   OutStd(                                                                                                             hb_osNewLine() )
   OutStd(               "  --version           display version header only"                                         , hb_osNewLine() )
   OutStd(               "  -help|--help        this help"                                                           , hb_osNewLine() )

   RETURN

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour " )
