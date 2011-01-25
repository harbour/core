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
 *    ...
 *
 * See COPYING for licensing terms.
 *
 */

#include "color.ch"
#include "fileio.ch"
#include "inkey.ch"
#include "setcurs.ch"

#include "hbnetio.ch"

#include "hbgtinfo.ch"
#include "hbhrb.ch"
#include "hbsocket.ch"

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
#define _NETIOSRV_hRPCFHRB          6
#define _NETIOSRV_lEncryption       7
#define _NETIOSRV_pListenSocket     8
#define _NETIOSRV_hConnection       9
#define _NETIOSRV_mtxConnection     10
#define _NETIOSRV_MAX_              10

#define _NETIOSRV_CONN_pConnection  1
#define _NETIOSRV_CONN_tStart       2
#define _NETIOSRV_CONN_MAX_         2

PROCEDURE Main( ... )
   LOCAL netiosrv[ _NETIOSRV_MAX_ ]

   LOCAL cParam
   LOCAL cCommand
   LOCAL cPassword

   LOCAL bKeyDown
   LOCAL bKeyUp
   LOCAL bKeyIns
   LOCAL bKeyPaste
   LOCAL bKeyTab

   LOCAL GetList   := {}
   LOCAL lQuit     := .F.
   LOCAL hCommands
   LOCAL nSavedRow
   LOCAL nPos
   LOCAL aCmd

   LOCAL cExt
   LOCAL cFile

   LOCAL aHistory, nHistIndex

   SET DATE ANSI
   SET CENTURY ON

   HB_Logo()

   netiosrv[ _NETIOSRV_nPort ]         := 2941
   netiosrv[ _NETIOSRV_cIFAddr ]       := "0.0.0.0"
   netiosrv[ _NETIOSRV_cRootDir ]      := hb_dirBase()
   netiosrv[ _NETIOSRV_lRPC ]          := .F.
   netiosrv[ _NETIOSRV_lEncryption ]   := .F.
   netiosrv[ _NETIOSRV_hConnection ]   := { => }
   netiosrv[ _NETIOSRV_mtxConnection ] := hb_mutexCreate()

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
         RETURN
      CASE Lower( cParam ) == "-help" .OR. ;
           Lower( cParam ) == "--help"
         HB_Usage()
         RETURN
      OTHERWISE
         OutStd( "Warning: Unkown parameter ignored: " + cParam + hb_eol() )
      ENDCASE
   NEXT

   SetCancel( .F. )

   netiosrv[ _NETIOSRV_pListenSocket ] := ;
      netio_mtserver( netiosrv[ _NETIOSRV_nPort ],;
                      netiosrv[ _NETIOSRV_cIFAddr ],;
                      netiosrv[ _NETIOSRV_cRootDir ],;
                      iif( Empty( netiosrv[ _NETIOSRV_hRPCFHRB ] ), netiosrv[ _NETIOSRV_lRPC ], hb_hrbGetFunSym( netiosrv[ _NETIOSRV_hRPCFHRB ], _RPC_FILTER ) ),;
                      cPassword,;
                      NIL,;
                      NIL,;
                      {| pConnectionSocket | netiosrv_callback( netiosrv, pConnectionSocket ) } )

   netiosrv[ _NETIOSRV_lEncryption ] := ! Empty( cPassword )
   cPassword := NIL

   IF Empty( netiosrv[ _NETIOSRV_pListenSocket ] )
      OutStd( "Cannot start server." + hb_eol() )
   ELSE
      ShowConfig( netiosrv )

      OutStd( hb_eol() )
      OutStd( "Type a command or '?' for help.", hb_eol() )

      lQuit      := .F.
      aHistory   := { "quit" }
      nHistIndex := Len( aHistory ) + 1
      hCommands  := hbnetiosrv_LoadCmds( {|| lQuit := .T. },;            /* codeblock to quit */
                                         {|| ShowConfig( netiosrv ) } )  /* codeblock to display config both uses local vars */

      /* Command prompt */
      DO WHILE ! lQuit

         cCommand := Space( 128 )

         QQOut( "hbnetiosrv$ " )
         nSavedRow := Row()

         @ nSavedRow, Col() GET cCommand PICTURE "@S" + hb_ntos( MaxCol() - Col() + 1 ) COLOR hb_ColorIndex( SetColor(), CLR_STANDARD ) + "," + hb_ColorIndex( SetColor(), CLR_STANDARD )

         SetCursor( iif( ReadInsert(), SC_INSERT, SC_NORMAL ) )

         bKeyIns   := SetKey( K_INS,;
            {|| SetCursor( iif( ReadInsert( ! ReadInsert() ),;
                             SC_NORMAL, SC_INSERT ) ) } )
         bKeyUp    := SetKey( K_UP,;
            {|| iif( nHistIndex > 1,;
                     cCommand := PadR( aHistory[ --nHistIndex ], Len( cCommand ) ), ),;
                     ManageCursor( cCommand ) } )
         bKeyDown  := SetKey( K_DOWN,;
            {|| cCommand := PadR( iif( nHistIndex < Len( aHistory ),;
                aHistory[ ++nHistIndex ],;
                ( nHistIndex := Len( aHistory ) + 1, "" ) ), Len( cCommand ) ),;
                     ManageCursor( cCommand ) } )
         bKeyPaste := SetKey( K_ALT_V, {|| hb_gtInfo( HB_GTI_CLIPBOARDPASTE )})

         bKeyTab   := SetKey( K_TAB, {|| CompleteCmd( @cCommand, hCommands ) } )

         READ

         /* Positions the cursor on the line previously saved */
         SetPos( nSavedRow, MaxCol() - 1 )

         SetKey( K_ALT_V, bKeyPaste )
         SetKey( K_DOWN,  bKeyDown  )
         SetKey( K_UP,    bKeyUp    )
         SetKey( K_INS,   bKeyIns   )
         SetKey( K_TAB,   bKeyTab   )

         QQOut( hb_eol() )

         cCommand := AllTrim( cCommand )

         IF Empty( cCommand )
            LOOP
         ENDIF

         IF Empty( aHistory ) .OR. ! ATail( aHistory ) == cCommand
            IF Len( aHistory ) < 64
               AAdd( aHistory, cCommand )
            ELSE
               ADel( aHistory, 1 )
               aHistory[ Len( aHistory ) ] := cCommand
            ENDIF
         ENDIF
         nHistIndex := Len( aHistory ) + 1

         nPos := iif( Empty( cCommand ), 0, hb_HPos( hCommands, Lower( cCommand ) ) )
         IF nPos > 0
            aCmd := hb_HValueAt( hCommands, nPos )
            Eval( aCmd[ 2 ], cCommand, netiosrv )
         ELSE
            QQOut( "Error: Unknown command '" + cCommand + "'.", hb_eol() )
         ENDIF
      ENDDO

      netio_serverstop( netiosrv[ _NETIOSRV_pListenSocket ] )
      netiosrv[ _NETIOSRV_pListenSocket ] := NIL

      OutStd( hb_eol() )
      OutStd( "Server stopped.", hb_eol() )
   ENDIF

   RETURN

STATIC FUNCTION netiosrv_callback( netiosrv, pConnectionSocket )

   netiosrv_conn_register( netiosrv, pConnectionSocket )

   BEGIN SEQUENCE
      netio_server( pConnectionSocket )
   END SEQUENCE

   netiosrv_conn_unregister( netiosrv, pConnectionSocket )

   RETURN NIL

STATIC PROCEDURE netiosrv_conn_register( netiosrv, pConnectionSocket )
   LOCAL nconn[ _NETIOSRV_CONN_MAX_ ]

   nconn[ _NETIOSRV_CONN_pConnection ] := pConnectionSocket
   nconn[ _NETIOSRV_CONN_tStart ]      := hb_DateTime()

   hb_mutexLock( netiosrv[ _NETIOSRV_mtxConnection ] )

   IF !( pConnectionSocket $ netiosrv[ _NETIOSRV_hConnection ] )
      netiosrv[ _NETIOSRV_hConnection ][ pConnectionSocket ] := nconn
   ENDIF

   hb_mutexUnlock( netiosrv[ _NETIOSRV_mtxConnection ] )

   RETURN

STATIC PROCEDURE netiosrv_conn_unregister( netiosrv, pConnectionSocket )

   hb_mutexLock( netiosrv[ _NETIOSRV_mtxConnection ] )

   IF pConnectionSocket $ netiosrv[ _NETIOSRV_hConnection ]
      hb_HDel( netiosrv[ _NETIOSRV_hConnection ], pConnectionSocket )
   ENDIF

   hb_mutexUnlock( netiosrv[ _NETIOSRV_mtxConnection ] )

   RETURN

PROCEDURE cmdConnInfo( netiosrv )
   LOCAL nconn

   LOCAL nStatus
   LOCAL nFilesCount
   LOCAL nBytesSent
   LOCAL nBytesReceived
   LOCAL aAddressPeer

   hb_mutexLock( netiosrv[ _NETIOSRV_mtxConnection ] )

   QQOut( "Number of connections: " + hb_ntos( Len( netiosrv[ _NETIOSRV_hConnection ] ) ), hb_eol() )

   FOR EACH nconn IN netiosrv[ _NETIOSRV_hConnection ]

      nFilesCount := 0
      nBytesSent := 0
      nBytesReceived := 0
      aAddressPeer := NIL

      nStatus := ;
      netio_srvStatus( nconn[ _NETIOSRV_CONN_pConnection ], NETIO_SRVINFO_FILESCOUNT   , @nFilesCount    )
      netio_srvStatus( nconn[ _NETIOSRV_CONN_pConnection ], NETIO_SRVINFO_BYTESSENT    , @nBytesSent     )
      netio_srvStatus( nconn[ _NETIOSRV_CONN_pConnection ], NETIO_SRVINFO_BYTESRECEIVED, @nBytesReceived )
      netio_srvStatus( nconn[ _NETIOSRV_CONN_pConnection ], NETIO_SRVINFO_PEERADDRESS  , @aAddressPeer   )

      QQOut( "#" + hb_ntos( nconn:__enumIndex() ) + " " +;
             hb_TToC( nconn[ _NETIOSRV_CONN_tStart ] ) + " " +;
             PadR( ConnStatusStr( nStatus ), 12 ) + " " +;
             "fcnt: " + Str( nFilesCount ) + " " +;
             "send: " + Str( nBytesSent ) + " " +;
             "recv: " + Str( nBytesReceived ) + " " +;
             AddrToIPPort( aAddressPeer ), hb_eol() )
   NEXT

   hb_mutexUnlock( netiosrv[ _NETIOSRV_mtxConnection ] )

   RETURN

STATIC FUNCTION ConnStatusStr( nStatus )

   SWITCH nStatus
   CASE NETIO_SRVSTAT_RUNNING     ; RETURN "RUNNING"
   CASE NETIO_SRVSTAT_WRONGHANDLE ; RETURN "WRONGHANDLE"
   CASE NETIO_SRVSTAT_CLOSED      ; RETURN "CLOSED"
   CASE NETIO_SRVSTAT_STOPPED     ; RETURN "STOPPED"
   CASE NETIO_SRVSTAT_DATASTREAM  ; RETURN "DATASTREAM"
   CASE NETIO_SRVSTAT_ITEMSTREAM  ; RETURN "ITEMSTREAM"
   ENDSWITCH

   RETURN "UNKNOWN"

STATIC FUNCTION AddrToIPPort( aAddr )
   LOCAL cIP

   IF hb_isArray( aAddr ) .AND. ;
      ( aAddr[ HB_SOCKET_ADINFO_FAMILY ] == HB_SOCKET_AF_INET .OR. ;
        aAddr[ HB_SOCKET_ADINFO_FAMILY ] == HB_SOCKET_AF_INET6 )
      cIP := aAddr[ HB_SOCKET_ADINFO_ADDRESS ] + ":" + hb_ntos( aAddr[ HB_SOCKET_ADINFO_PORT ] )
   ELSE
      cIP := ""
   ENDIF

   RETURN cIP

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

/* Complete the command line, based on the first characters that the user typed. [vailtom] */
STATIC PROCEDURE CompleteCmd( cCommand, hCommands )
   LOCAL s := Lower( AllTrim( cCommand ) )
   LOCAL n

   /* We need at least one character to search */
   IF Len( s ) > 1
      FOR EACH n IN hCommands
         IF s == Lower( Left( n:__enumKey(), Len( s ) ) )
            cCommand := PadR( n:__enumKey(), Len( cCommand ) )
            ManageCursor( cCommand )
            RETURN
         ENDIF
      NEXT
   ENDIF
   RETURN

/* Adjusted the positioning of cursor on navigate through history. [vailtom] */
STATIC PROCEDURE ManageCursor( cCommand )
   KEYBOARD Chr( K_HOME ) + iif( ! Empty( cCommand ), Chr( K_END ), "" )
   RETURN

STATIC PROCEDURE ShowConfig( netiosrv )

   QQOut( "Listening on: "      + netiosrv[ _NETIOSRV_cIFAddr ] + ":" + hb_ntos( netiosrv[ _NETIOSRV_nPort ] ), hb_eol() )
   QQOut( "Root filesystem: "   + netiosrv[ _NETIOSRV_cRootDir ], hb_eol() )
   QQOut( "RPC support: "       + iif( netiosrv[ _NETIOSRV_lRPC ], "enabled", "disabled" ), hb_eol() )
   QQOut( "Encryption: "        + iif( netiosrv[ _NETIOSRV_lEncryption ], "enabled", "disabled" ), hb_eol() )
   QQOut( "RPC filter module: " + iif( Empty( netiosrv[ _NETIOSRV_hRPCFHRB ] ), iif( netiosrv[ _NETIOSRV_lRPC ], "not set (WARNING: unsafe open server)", "not set" ), netiosrv[ _NETIOSRV_cRPCFFileName ] ), hb_eol() )

   RETURN

STATIC PROCEDURE HB_Logo()

   OutStd( "Harbour NETIO Server " + HBRawVersion() + hb_eol() +;
           "Copyright (c) 2009-2011, Przemyslaw Czerpak" + hb_eol() + ;
           "http://harbour-project.org/" + hb_eol() +;
           hb_eol() )

   RETURN

STATIC PROCEDURE HB_Usage()

   OutStd(               "Syntax:"                                                                                   , hb_eol() )
   OutStd(                                                                                                             hb_eol() )
   OutStd(               "  netiosrv [options]"                                                                      , hb_eol() )
   OutStd(                                                                                                             hb_eol() )
   OutStd(               "Options:"                                                                                  , hb_eol() )
   OutStd(                                                                                                             hb_eol() )
   OutStd(               "  -port=<port>        accept incoming connections on IP port <port>"                       , hb_eol() )
   OutStd(               "  -iface=<ipaddr>     accept incoming connections on IPv4 interface <ipaddress>"           , hb_eol() )
   OutStd(               "  -rootdir=<rootdir>  use <rootdir> as root directory for served file system"              , hb_eol() )
   OutStd(               "  -rpc                accept RPC requests"                                                 , hb_eol() )
   OutStd(               "  -rpc=<file.hrb>     set RPC processor .hrb module to <file.hrb>"                         , hb_eol() )
   OutStd( hb_StrFormat( "                      file.hrb needs to have an entry function named %1$s()", _RPC_FILTER ), hb_eol() )
   OutStd(               "  -pass=<passwd>      set server password"                                                 , hb_eol() )
   OutStd(                                                                                                             hb_eol() )
   OutStd(               "  --version           display version header only"                                         , hb_eol() )
   OutStd(               "  -help|--help        this help"                                                           , hb_eol() )

   RETURN

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour " )
