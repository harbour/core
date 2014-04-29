/*
 * xHarbour Project source code:
 * TIP Class oriented Internet protocol library (FTP)
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
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

/* 2007-04-19, Hannes Ziegler <hz AT knowlexbase.com>
   Added method :RMD()
   Added method :listFiles()
   Added method :MPut()
   Changed method :DownloadFile() to enable display of progress
   Changed method :UploadFile() to enable display of progress

   2007-06-01, Toninho@fwi
   Added method UserCommand( cCommand, lPasv, lReadPort, lGetReply )

   2007-07-12, miguelangel@marchuet.net
   Added method :NoOp()
   Added method :Rest( nPos )
   Changed method :LS( cSpec )
   Changed method :List( cSpec )
   Changed method :TransferStart()
   Changed method :Stor( cFile )
   Changed method :UploadFile( cLocalFile, cRemoteFile )
   Changed method :DownloadFile( cLocalFile, cRemoteFile )

   Added support to Port transfer mode
   Added method :Port()
   Added method :SendPort()

   Cleaned unused variables.

   2007-09-08 21:34 UTC+0100 Patrick Mast <patrick/dot/mast/at/xharbour.com>
     * Formatting
     + METHOD StartCleanLogFile()
       Starts a clean log file, overwriting current logfile.
     + METHOD fileSize( cFileSpec )
       Calculates the filesize of the given files specifications.
     + DATA cLogFile
       Holds the filename of the current logfile.
     ! Fixed logfilename in New(), now its not limited to 9999 log files anymore
     ! Fixed MGet() due to changes in hb_ATokens()
     ! Fixed listFiles() due to changes in hb_ATokens()
     ! listFiles() is still buggy. Needs to be fixed.
 */

#include "hbclass.ch"

#include "directry.ch"
#include "fileio.ch"

#include "tip.ch"

#define _PORT_MIN  16000
#define _PORT_MAX  24000

STATIC s_nPort := _PORT_MIN

CREATE CLASS TIPClientFTP FROM TIPClient

   VAR nDataPort
   VAR cDataServer
   VAR bUsePasv
   VAR RegBytes
   VAR RegPasv
   // Socket opened in response to a port command
   VAR SocketControl
   VAR SocketPortServer

   METHOD New( oUrl, xTrace, oCredentials )
   METHOD Open( cUrl )
   METHOD Read( nLen )
   METHOD Write( cData, nLen )
   METHOD Close()
   METHOD TransferStart()
   METHOD Commit()

   METHOD GetReply()
   METHOD Pasv()
   METHOD TypeI()
   METHOD TypeA()
   METHOD NoOp()
   METHOD Rest( nPos )
   METHOD List( cSpec )
   METHOD UserCommand( cCommand, lPasv, lReadPort, lGetReply )
   METHOD Pwd()
   METHOD Cwd( cPath )
   METHOD Dele( cPath )
   METHOD Port()
   METHOD SendPort()
   METHOD Retr( cFile )
   METHOD Stor( cFile )
   METHOD Quit()
   METHOD ScanLength()
   METHOD ReadAuxPort( cLocalFile )
   METHOD mget( cSpec, cLocalPath )

   // Method below contributed by Rafa Carmona

   METHOD LS( cSpec )
   METHOD Rename( cFrom, cTo )
   METHOD UploadFile( cLocalFile, cRemoteFile )    // new method for file upload
   METHOD DownLoadFile( cLocalFile, cRemoteFile )  // new method to download file
   METHOD MKD( cPath )                             // new method to create an directory on ftp server

   METHOD RMD( cPath )
   METHOD listFiles( cFileSpec )
   METHOD MPut
   METHOD fileSize( cFileSpec )

ENDCLASS


METHOD New( oUrl, xTrace, oCredentials ) CLASS TIPClientFTP

   ::super:new( oUrl, iif( HB_ISLOGICAL( xTrace ) .AND. xTrace, "ftp", xTrace ), oCredentials )

   ::nDefaultPort := 21
   ::nConnTimeout := 3000
   ::bUsePasv     := .T.
   ::nAccessMode  := TIP_RW  // a read-write protocol

   ::nDefaultSndBuffSize := 65536
   ::nDefaultRcvBuffSize := 65536

   // precompilation of regex for better prestations
   ::RegBytes := hb_regexComp( "\(([0-9]+)[ )a-zA-Z]" )
   ::RegPasv  := hb_regexComp( "([0-9]*) *, *([0-9]*) *, *([0-9]*) *, *([0-9]*) *, *([0-9]*) *, *([0-9]*)" )

   RETURN Self

METHOD Open( cUrl ) CLASS TIPClientFTP

   IF HB_ISSTRING( cUrl )
      ::oUrl := TUrl():New( cUrl )
   ENDIF

   IF Len( ::oUrl:cUserid ) == 0 .OR. ;
      Len( ::oUrl:cPassword ) == 0
      RETURN .F.
   ENDIF

   IF ! ::super:Open()
      RETURN .F.
   ENDIF

   IF ::GetReply()
      ::inetSendAll( ::SocketCon, "USER " + ::oUrl:cUserid + ::cCRLF )
      IF ::GetReply()
         ::inetSendAll( ::SocketCon, "PASS " + ::oUrl:cPassword + ::cCRLF )
         // set binary by default
         IF ::GetReply() .AND. ::TypeI()
            RETURN .T.
         ENDIF
      ENDIF
   ENDIF

   RETURN .F.

METHOD GetReply() CLASS TIPClientFTP

   LOCAL nLen
   LOCAL cRep := ::cReply := ::inetRecvLine( ::SocketCon, @nLen, 128 )

   IF cRep == NIL
      RETURN .F.
   ENDIF

   // now, if the reply has a "-" as fourth character, we need to proceed...
   DO WHILE ! Empty( cRep ) .AND. SubStr( cRep, 4, 1 ) == "-"
      ::cReply := ::inetRecvLine( ::SocketCon, @nLen, 128 )
      cRep := iif( HB_ISSTRING( ::cReply ), ::cReply, "" )
   ENDDO

   // 4 and 5 are error codes
   IF ::inetErrorCode( ::SocketCon ) != 0 .OR. Val( Left( ::cReply, 1 ) ) >= 4
      RETURN .F.
   ENDIF

   RETURN .T.

METHOD Pasv() CLASS TIPClientFTP

   LOCAL aRep

   ::inetSendAll( ::SocketCon, "PASV" + ::cCRLF )

   IF ! ::GetReply()
      RETURN .F.
   ENDIF

   aRep := hb_regex( ::RegPasv, ::cReply )

   IF Empty( aRep )
      RETURN .F.
   ENDIF

   ::cDataServer := aRep[ 2 ] + "." + aRep[ 3 ] + "." + aRep[ 4 ] + "." + aRep[ 5 ]
   ::nDataPort := Val( aRep[ 6 ] ) * 256 + Val( aRep[ 7 ] )

   RETURN .T.

METHOD Close() CLASS TIPClientFTP

   ::InetTimeOut( ::SocketCon )

   ::Quit()

   RETURN ::super:Close()

METHOD Quit() CLASS TIPClientFTP

   ::inetSendAll( ::SocketCon, "QUIT" + ::cCRLF )

   RETURN ::GetReply()

METHOD TypeI() CLASS TIPClientFTP

   ::inetSendAll( ::SocketCon, "TYPE I" + ::cCRLF )

   RETURN ::GetReply()

METHOD TypeA() CLASS TIPClientFTP

   ::inetSendAll( ::SocketCon, "TYPE A" + ::cCRLF )

   RETURN ::GetReply()

METHOD NoOp() CLASS TIPClientFTP

   ::inetSendAll( ::SocketCon, "NOOP" + ::cCRLF )

   RETURN ::GetReply()

METHOD Rest( nPos ) CLASS TIPClientFTP

   ::inetSendAll( ::SocketCon, "REST " + hb_ntos( hb_defaultValue( nPos, 0 ) ) + ::cCRLF )

   RETURN ::GetReply()

METHOD CWD( cPath ) CLASS TIPClientFTP

   ::inetSendAll( ::SocketCon, "CWD " + cPath + ::cCRLF )

   RETURN ::GetReply()

METHOD PWD() CLASS TIPClientFTP

   ::inetSendAll( ::SocketCon, "PWD"  + ::cCRLF )
   IF ! ::GetReply()
      RETURN .F.
   ENDIF
   ::cReply := SubStr( ::cReply, At( '"', ::cReply ) + 1, ;
      RAt( '"', ::cReply ) - At( '"', ::cReply ) - 1 )

   RETURN .T.

METHOD DELE( cPath ) CLASS TIPClientFTP

   ::inetSendAll( ::SocketCon, "DELE " + cPath + ::cCRLF )

   RETURN ::GetReply()

// scan last reply for an hint of length
METHOD ScanLength() CLASS TIPClientFTP

   LOCAL aBytes := hb_regex( ::RegBytes, ::cReply )

   IF ! Empty( aBytes )
      ::nLength := Val( aBytes[ 2 ] )
   ENDIF

   RETURN .T.

METHOD TransferStart() CLASS TIPClientFTP

   LOCAL skt

   ::SocketControl := ::SocketCon

   IF ::bUsePasv
      skt := hb_inetConnectIP( ::cDataServer, ::nDataPort )
      IF skt != NIL .AND. ::inetErrorCode( skt ) == 0
         // Get the start message from the control connection
         IF ! ::GetReply()
            hb_inetClose( skt )
            RETURN .F.
         ENDIF

         ::InetTimeOut( skt )

         /* Set internal socket send buffer to 64KB,
            this should fix the speed problems some users have reported */
         IF ! Empty( ::nDefaultSndBuffSize )
            ::InetSndBufSize( skt, ::nDefaultSndBuffSize )
         ENDIF

         IF ! Empty( ::nDefaultRcvBuffSize )
            ::InetRcvBufSize( skt, ::nDefaultRcvBuffSize )
         ENDIF

         ::SocketCon := skt
      ENDIF
   ELSE
      ::SocketCon := hb_inetAccept( ::SocketPortServer )
      IF Empty( ::SocketCon )
         ::bInitialized := .F.
         ::SocketCon := ::SocketControl
         ::GetReply()
         RETURN .F.
      ENDIF
      hb_inetSetRcvBufSize( ::SocketCon, 65536 )
      hb_inetSetSndBufSize( ::SocketCon, 65536 )
   ENDIF

   RETURN .T.

METHOD Commit() CLASS TIPClientFTP

   hb_inetClose( ::SocketCon )

   ::SocketCon := ::SocketControl
   ::bInitialized := .F.

   IF ! ::GetReply()
      RETURN .F.
   ENDIF

   // error code?
   IF hb_LeftEq( ::cReply, "5" )
      RETURN .F.
   ENDIF

   RETURN .T.

METHOD List( cSpec ) CLASS TIPClientFTP

   LOCAL cStr

   IF ::bUsePasv .AND. ! ::Pasv()
      // ::bUsePasv := .F.
      RETURN NIL
   ENDIF
   IF ! ::bUsePasv .AND. ! ::Port()
      RETURN NIL
   ENDIF

   hb_default( @cSpec, "" )

   IF ! Empty( cSpec )
      cSpec := " " + cSpec
   ENDIF

   ::inetSendAll( ::SocketCon, "LIST" + cSpec + ::cCRLF )
   cStr := ::ReadAuxPort()
   ::bEof := .F.

   RETURN cStr

METHOD UserCommand( cCommand, lPasv, lReadPort, lGetReply ) CLASS TIPClientFTP

   IF ::bUsePasv .AND. hb_defaultValue( lPasv, .T. ) .AND. ! ::Pasv()
      RETURN .F.
   ENDIF

   ::inetSendAll( ::SocketCon, hb_defaultValue( cCommand, "" ) )

   IF hb_defaultValue( lReadPort, .T. )
      lReadPort := ::ReadAuxPort()
   ENDIF

   IF hb_defaultValue( lGetReply, .F. )
      lGetReply := ::GetReply()
   ENDIF

   RETURN .T.

METHOD ReadAuxPort( cLocalFile ) CLASS TIPClientFTP

   LOCAL cRet
   LOCAL cList := ""
   LOCAL nFile := F_ERROR

   IF ! ::TransferStart()
      RETURN NIL
   ENDIF
   IF ! Empty( cLocalFile )
      nFile := FCreate( cLocalFile )
   ENDIF
   DO WHILE ( cRet := ::super:Read( 512 ) ) != NIL .AND. Len( cRet ) > 0
      IF nFile != F_ERROR
         FWrite( nFile, cRet )
      ELSE
         cList += cRet
      ENDIF
   ENDDO

   hb_inetClose( ::SocketCon )
   ::SocketCon := ::SocketControl
   IF ::GetReply()
      IF nFile != F_ERROR
         FClose( nFile )
         RETURN .T.
      ENDIF
      RETURN cList
   ENDIF

   RETURN NIL

METHOD Stor( cFile ) CLASS TIPClientFTP

   IF ::bUsePasv .AND. ! ::Pasv()
      // ::bUsePasv := .F.
      RETURN .F.
   ENDIF

   ::inetSendAll( ::SocketCon, "STOR " + cFile + ::cCRLF )

   // It is important not to delete these lines in order not to disrupt the timing of
   // the responses, which can lead to failures in transfers.
   IF ! ::bUsePasv
      ::GetReply()
   ENDIF

   RETURN ::TransferStart()

METHOD Port() CLASS TIPClientFTP

   LOCAL nPort

   ::SocketPortServer := hb_inetCreate( ::nConnTimeout )

   DO WHILE ( nPort := ++s_nPort ) < _PORT_MAX
      hb_inetServer( nPort, ::SocketPortServer )
      IF ::inetErrorCode( ::SocketPortServer ) == 0
         RETURN ::SendPort()
      ENDIF
   ENDDO

   s_nPort := _PORT_MIN

   RETURN .F.

METHOD SendPort() CLASS TIPClientFTP

   LOCAL cAddr
   LOCAL cPort, nPort

   cAddr := StrTran( hb_inetGetHosts( NetName() )[ 1 ], ".", "," )
   nPort := hb_inetPort( ::SocketPortServer )
   cPort := "," + hb_ntos( Int( nPort / 256 ) ) +  "," + hb_ntos( Int( nPort % 256 ) )

   ::inetSendAll( ::SocketCon, "PORT " + cAddr + cPort + ::cCRLF )

   RETURN ::GetReply()

METHOD Read( nLen ) CLASS TIPClientFTP

   LOCAL cRet

   IF ! ::bInitialized

      IF ! Empty( ::oUrl:cPath ) .AND. ! ::CWD( ::oUrl:cPath )
         ::bEof := .T.  // no data for this transaction
         RETURN NIL
      ENDIF

      IF Empty( ::oUrl:cFile )
         RETURN ::List()
      ENDIF

      IF ! ::Retr( ::oUrl:cFile )
         ::bEof := .T.  // no data for this transaction
         RETURN NIL
      ENDIF

      // now channel is open
      ::bInitialized := .T.
   ENDIF

   IF ( cRet := ::super:Read( nLen ) ) == NIL
      ::Commit()
      ::bEof := .T.
   ENDIF

   RETURN cRet

/* FTP transfer wants commit only at end. */
METHOD Write( cData, nLen ) CLASS TIPClientFTP

   IF ! ::bInitialized

      IF Empty( ::oUrl:cFile )
         RETURN -1
      ENDIF

      IF ! Empty( ::oUrl:cPath ) .AND. ! ::CWD( ::oUrl:cPath )
         RETURN -1
      ENDIF

      IF ! ::Stor( ::oUrl:cFile )
         RETURN -1
      ENDIF

      // now channel is open
      ::bInitialized := .T.
   ENDIF

   RETURN ::super:Write( cData, nLen, .F. )

METHOD Retr( cFile ) CLASS TIPClientFTP

   IF ::bUsePasv .AND. ! ::Pasv()
      // ::bUsePasv := .F.
      RETURN .F.
   ENDIF

   ::inetSendAll( ::SocketCon, "RETR " + cFile + ::cCRLF )

   IF ::TransferStart()
      ::ScanLength()
      RETURN .T.
   ENDIF

   RETURN .F.

METHOD MGET( cSpec, cLocalPath ) CLASS TIPClientFTP

   LOCAL cStr, cFile

   IF ::bUsePasv .AND. ! ::Pasv()
      // ::bUsePasv := .F.
      RETURN .F.
   ENDIF

   hb_default( @cLocalPath, "" )

   ::inetSendAll( ::SocketCon, "NLST " + hb_defaultValue( cSpec, "" ) + ::cCRLF )

   cStr := ::ReadAuxPort()

   FOR EACH cFile IN hb_ATokens( StrTran( cStr, Chr( 13 ) ), Chr( 10 ) )
      IF ! Empty( cFile )
         ::downloadfile( cLocalPath + RTrim( cFile ), RTrim( cFile ) )
      ENDIF
   NEXT

   RETURN cStr

METHOD MPUT( cFileSpec, cAttr ) CLASS TIPClientFTP

   LOCAL cPath, cFile, cExt, aFile
   LOCAL cStr

   IF ! HB_ISSTRING( cFileSpec )
      RETURN 0
   ENDIF

   hb_FNameSplit( cFileSpec, @cPath, @cFile, @cExt  )

   cStr := ""
   FOR EACH aFile IN Directory( cPath + cFile + cExt, cAttr )
      IF ::UploadFile( cPath + aFile[ F_NAME ], aFile[ F_NAME ] )
         cStr += e"\r\n" + aFile[ F_NAME ]
      ENDIF
   NEXT

   /* QUESTION: Shouldn't this return an array?
                Why emulate a platform specific and ill-defined format? */
   RETURN SubStr( cStr, Len( e"\r\n" ) + 1 )

METHOD UploadFile( cLocalFile, cRemoteFile ) CLASS TIPClientFTP

   LOCAL cPath
   LOCAL cFile
   LOCAL cExt

   hb_FNameSplit( cLocalFile, @cPath, @cFile, @cExt  )

   ::bEof := .F.
   ::oUrl:cFile := hb_defaultValue( cRemoteFile, cFile + cExt )

   IF ! ::bInitialized

      IF Empty( ::oUrl:cFile )
         RETURN .F.
      ENDIF

      IF ! Empty( ::oUrl:cPath ) .AND. ! ::CWD( ::oUrl:cPath )
         RETURN .F.
      ENDIF

      IF ! ::bUsePasv .AND. ! ::Port()
         RETURN .F.
      ENDIF

      IF ! ::Stor( ::oUrl:cFile )
         RETURN .F.
      ENDIF

      // now channel is open
      ::bInitialized := .T.
   ENDIF

   RETURN ::WriteFromFile( cLocalFile )

METHOD LS( cSpec ) CLASS TIPClientFTP

   LOCAL cStr

   IF ::bUsePasv .AND. ! ::Pasv()
      // ::bUsePasv := .F.
      RETURN .F.
   ENDIF

   IF ! ::bUsePasv .AND. ! ::Port()
      RETURN .F.
   ENDIF

   ::inetSendAll( ::SocketCon, "NLST " + hb_defaultValue( cSpec, "" ) + ::cCRLF )
   IF ::GetReply()
      cStr := ::ReadAuxPort()
   ELSE
      cStr := ""
   ENDIF

   RETURN cStr

METHOD Rename( cFrom, cTo ) CLASS TIPClientFTP

   ::inetSendAll( ::SocketCon, "RNFR " + hb_defaultValue( cFrom, "" ) + ::cCRLF )
   IF ::GetReply()
      ::inetSendAll( ::SocketCon, "RNTO " + hb_defaultValue( cTo, "" ) + ::cCRLF )
      RETURN ::GetReply()
   ENDIF

   RETURN .F.

METHOD DownLoadFile( cLocalFile, cRemoteFile ) CLASS TIPClientFTP

   LOCAL cPath
   LOCAL cFile
   LOCAL cExt

   hb_FNameSplit( cLocalFile, @cPath, @cFile, @cExt  )

   ::bEof := .F.
   ::oUrl:cFile := hb_defaultValue( cRemoteFile, cFile + cExt )

   IF ! ::bInitialized

      IF ! Empty( ::oUrl:cPath ) .AND. ! ::CWD( ::oUrl:cPath )
         ::bEof := .T.  // no data for this transaction
         RETURN .F.
      ENDIF

      IF ! ::bUsePasv .AND. ! ::Port()
         RETURN .F.
      ENDIF

      IF ! ::Retr( ::oUrl:cFile )
         ::bEof := .T.  // no data for this transaction
         RETURN .F.
      ENDIF

      // now channel is open
      ::bInitialized := .T.
   ENDIF

   RETURN ::ReadToFile( cLocalFile, , ::nLength )


// Create a new folder
METHOD MKD( cPath ) CLASS TIPClientFTP

   ::inetSendAll( ::SocketCon, "MKD " + cPath + ::cCRLF )

   RETURN ::GetReply()


// Delete an existing folder
METHOD RMD( cPath ) CLASS TIPClientFTP

   ::inetSendAll( ::SocketCon, "RMD " + cPath + ::cCRLF )

   RETURN ::GetReply()


// Return total file size for <cFileSpec>
METHOD fileSize( cFileSpec ) CLASS TIPClientFTP

   LOCAL aFile
   LOCAL nSize := 0

   FOR EACH aFile IN ::ListFiles( cFileSpec )
      nSize += aFile[ F_SIZE ]
   NEXT

   RETURN nSize


// Parse the :list() string into a Directory() compatible 2-dim array
METHOD listFiles( cFileSpec ) CLASS TIPClientFTP

   LOCAL aMonth := { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" }
   LOCAL cList, aList, aFile, cEntry, nStart, nEnd
   LOCAL cYear, cMonth, cDay, cTime

   cList := ::list( cFileSpec )

   IF Empty( cList )
      RETURN {}
   ENDIF

   aList := hb_ATokens( StrTran( cList, Chr( 13 ) ), Chr( 10 ) )

   FOR EACH cEntry IN aList DESCEND

      IF Empty( cEntry )

         hb_ADel( aList, cEntry:__enumIndex(), .T. )

      ELSE

         aFile  := Array( F_LEN + 3 )
         nStart := 1
         nEnd   := hb_At( " ", cEntry, nStart )

         // file permissions (attributes)
         aFile[ F_ATTR ] := SubStr( cEntry, nStart, nEnd - nStart )
         nStart          := nEnd

         // # of links
         DO WHILE SubStr( cEntry, ++nStart, 1 ) == " "
         ENDDO
         nEnd               := hb_At( " ", cEntry, nStart )
         aFile[ F_LEN + 1 ] := Val( SubStr( cEntry, nStart, nEnd - nStart ) )
         nStart             := nEnd

         // owner name
         DO WHILE SubStr( cEntry, ++nStart, 1 ) == " "
         ENDDO
         nEnd               := hb_At( " ", cEntry, nStart )
         aFile[ F_LEN + 2 ] := SubStr( cEntry, nStart, nEnd - nStart )
         nStart             := nEnd

         // group name
         DO WHILE SubStr( cEntry, ++nStart, 1 ) == " "
         ENDDO
         nEnd               := hb_At( " ", cEntry, nStart )
         aFile[ F_LEN + 3 ] := SubStr( cEntry, nStart, nEnd - nStart )
         nStart             := nEnd

         // file size
         DO WHILE SubStr( cEntry, ++nStart, 1 ) == " "
         ENDDO
         nEnd            := hb_At( " ", cEntry, nStart )
         aFile[ F_SIZE ] := Val( SubStr( cEntry, nStart, nEnd - nStart ) )
         nStart          := nEnd

         // Month
         DO WHILE SubStr( cEntry, ++nStart, 1 ) == " "
         ENDDO
         nEnd          := hb_At( " ", cEntry, nStart )
         cMonth        := SubStr( cEntry, nStart, nEnd - nStart )
         cMonth        := PadL( hb_AScan( aMonth, cMonth, , , .T. ), 2, "0" )
         nStart        := nEnd

         // Day
         DO WHILE SubStr( cEntry, ++nStart, 1 ) == " "
         ENDDO
         nEnd          := hb_At( " ", cEntry, nStart )
         cDay          := SubStr( cEntry, nStart, nEnd - nStart )
         nStart        := nEnd

         // year
         DO WHILE SubStr( cEntry, ++nStart, 1 ) == " "
         ENDDO
         nEnd          := hb_At( " ", cEntry, nStart )
         cYear         := SubStr( cEntry, nStart, nEnd - nStart )
         nStart        := nEnd

         IF ":" $ cYear
            cTime := cYear
            cYear := StrZero( Year( Date() ), 4 )
         ELSE
            cTime := ""
         ENDIF

         // file name
         DO WHILE SubStr( cEntry, ++nStart, 1 ) == " "
         ENDDO

         aFile[ F_NAME ] := SubStr( cEntry, nStart )
         aFile[ F_DATE ] := hb_SToD( cYear + cMonth + cDay )
         aFile[ F_TIME ] := cTime

         aList[ cEntry:__enumIndex() ] := aFile

      ENDIF
   NEXT

   RETURN aList
