/*
 * TIP Class oriented Internet protocol library (FTP)
 *
 * Copyright 2007 Hannes Ziegler <hz AT knowlexbase.com> (RMD(), listFiles(), MPut())
 * Copyright 2007 Toninho@fwi (UserCommand())
 * Copyright 2007 miguelangel@marchuet.net (NoOp(), Rest(), Port(), SendPort())
 * Copyright 2007 Patrick Mast <patrick/dot/mast/at/xharbour.com> (fileSize())
 * Copyright 2005 Rafa Carmona (LS(), Rename(), UploadFile(), DownloadFile(), MKD())
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

#include "hbclass.ch"

#include "directry.ch"

#include "tip.ch"

#define _PORT_MIN  16000
#define _PORT_MAX  24000

STATIC s_nPort := _PORT_MIN
STATIC s_mutexPort := hb_mutexCreate()

CREATE CLASS TIPClientFTP INHERIT TIPClient

   VAR nDataPort
   VAR cDataServer
   VAR bUsePasv     INIT .T.
   VAR RegBytes
   VAR RegPasv
   // Socket opened in response to a port command
   VAR SocketControl
   VAR SocketPortServer

   METHOD New( oUrl, xTrace, oCredentials )
   METHOD Open( cUrl )
   METHOD Close()
   METHOD GetReply()
   METHOD Commit()
   METHOD ScanLength()
   METHOD TransferStart()

   METHOD Pasv()
   METHOD Quit()
   METHOD TypeI()
   METHOD TypeA()
   METHOD NoOp()
   METHOD Rest( nPos )
   METHOD Pwd()
   METHOD Cwd( cPath )
   METHOD Dele( cPath )
   METHOD LS( cSpec )
   METHOD Rename( cFrom, cTo )
   METHOD MKD( cPath )
   METHOD RMD( cPath )
   METHOD Retr( cFile )
   METHOD Stor( cFile )
   METHOD List( cSpec )
   METHOD UserCommand( cCommand, lPasv, lReadPort, lGetReply )

   METHOD Port()
   METHOD SendPort()
   METHOD ReadAuxPort()
   METHOD Read( nLen )
   METHOD Write( cData, nLen )
   METHOD MGet( cSpec, cLocalPath )
   METHOD MPut( cFileSpec, cAttr )
   METHOD UploadFile( cLocalFile, cRemoteFile )
   METHOD DownloadFile( cLocalFile, cRemoteFile )
   METHOD ListFiles( cFileSpec )
   METHOD FileSize( cFileSpec )

ENDCLASS

METHOD New( oUrl, xTrace, oCredentials ) CLASS TIPClientFTP

   ::super:new( oUrl, iif( hb_defaultValue( xTrace, .F. ), "ftp", xTrace ), oCredentials )

   ::nDefaultPort := 21
   ::nConnTimeout := 3000
   ::nAccessMode  := TIP_RW  // a read-write protocol

   ::nDefaultSndBuffSize := ::nDefaultRcvBuffSize := 65536

   // precompilation of regex for better prestations
   ::RegBytes := hb_regexComp( "\(([0-9]+)[ )a-zA-Z]" )
   ::RegPasv  := hb_regexComp( "([0-9]*) *, *([0-9]*) *, *([0-9]*) *, *([0-9]*) *, *([0-9]*) *, *([0-9]*)" )

   RETURN Self

METHOD Open( cUrl ) CLASS TIPClientFTP

   IF HB_ISSTRING( cUrl )
      ::oUrl := TUrl():New( cUrl )
   ENDIF

   IF ! ::oUrl:cUserid == "" .AND. ;
      ! ::oUrl:cPassword == ""

      IF ::super:Open()
         IF ::GetReply()
            ::inetSendAll( ::SocketCon, "USER " + ::oUrl:cUserid + ::cCRLF )
            IF ::GetReply()
               ::inetSendAll( ::SocketCon, "PASS " + ::oUrl:cPassword + ::cCRLF )
               IF ::GetReply() .AND. ::TypeI()  // set binary by default
                  RETURN .T.
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN .F.

METHOD Close() CLASS TIPClientFTP

   ::InetTimeOut( ::SocketCon )

   ::Quit()

   RETURN ::super:Close()

METHOD GetReply() CLASS TIPClientFTP

   LOCAL nLen
   LOCAL cRep := ::cReply := ::inetRecvLine( ::SocketCon, @nLen, 128 )

   IF cRep == NIL
      RETURN .F.
   ENDIF

   // now, if the reply has a "-" as fourth character, we need to proceed...
   DO WHILE ! Empty( cRep ) .AND. SubStr( cRep, 4, 1 ) == "-"
      cRep := ::cReply := hb_defaultValue( ::inetRecvLine( ::SocketCon, @nLen, 128 ), "" )
   ENDDO

   // 4 and 5 are error codes
   RETURN ::inetErrorCode( ::SocketCon ) == 0 .AND. Val( Left( ::cReply, 1 ) ) < 4

METHOD Commit() CLASS TIPClientFTP

   hb_inetClose( ::SocketCon )

   ::SocketCon := ::SocketControl
   ::bInitialized := .F.

   // error code?
   RETURN ::GetReply() .AND. ! hb_LeftEq( ::cReply, "5" )

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

         ::SocketCon := skt
      ELSE
         RETURN .F.
      ENDIF
   ELSE
      ::SocketCon := hb_inetAccept( ::SocketPortServer )
      IF Empty( ::SocketCon )
         ::bInitialized := .F.
         ::SocketCon := ::SocketControl
         ::GetReply()
         RETURN .F.
      ENDIF
   ENDIF

   /* Set internal socket send buffer to 64 KiB, this should
      resolve the speed problems some users have reported */
   IF HB_ISNUMERIC( ::nDefaultSndBuffSize )
      ::InetSndBufSize( ::SocketCon, ::nDefaultSndBuffSize )
   ENDIF
   IF HB_ISNUMERIC( ::nDefaultRcvBuffSize )
      ::InetRcvBufSize( ::SocketCon, ::nDefaultRcvBuffSize )
   ENDIF

   RETURN .T.

METHOD Pasv() CLASS TIPClientFTP

   LOCAL aRep

   ::inetSendAll( ::SocketCon, "PASV" + ::cCRLF )

   IF ! ::GetReply()
      RETURN .F.
   ENDIF

   IF Empty( aRep := hb_regex( ::RegPasv, ::cReply ) )
      RETURN .F.
   ENDIF

   ::cDataServer := aRep[ 2 ] + "." + aRep[ 3 ] + "." + aRep[ 4 ] + "." + aRep[ 5 ]
   ::nDataPort := Val( aRep[ 6 ] ) * 256 + Val( aRep[ 7 ] )

   RETURN .T.

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

   ::inetSendAll( ::SocketCon, "REST " + hb_ntos( Int( hb_defaultValue( nPos, 0 ) ) ) + ::cCRLF )

   RETURN ::GetReply()

METHOD PWD() CLASS TIPClientFTP

   ::inetSendAll( ::SocketCon, "PWD" + ::cCRLF )
   IF ::GetReply()
      ::cReply := SubStr( ::cReply, At( '"', ::cReply ) + 1, ;
         RAt( '"', ::cReply ) - At( '"', ::cReply ) - 1 )
      RETURN .T.
   ENDIF

   RETURN .F.

METHOD CWD( cPath ) CLASS TIPClientFTP

   ::inetSendAll( ::SocketCon, "CWD " + cPath + ::cCRLF )

   RETURN ::GetReply()

METHOD Dele( cPath ) CLASS TIPClientFTP

   ::inetSendAll( ::SocketCon, "DELE " + cPath + ::cCRLF )

   RETURN ::GetReply()

METHOD LS( cSpec ) CLASS TIPClientFTP

   IF ::bUsePasv .AND. ! ::Pasv()
#if 0
      ::bUsePasv := .F.
#endif
      RETURN .F.
   ENDIF

   IF ! ::bUsePasv .AND. ! ::Port()
      RETURN .F.
   ENDIF

   ::inetSendAll( ::SocketCon, "NLST " + hb_defaultValue( cSpec, "" ) + ::cCRLF )

   RETURN hb_defaultValue( ::ReadAuxPort(), "" )

METHOD Rename( cFrom, cTo ) CLASS TIPClientFTP

   ::inetSendAll( ::SocketCon, "RNFR " + hb_defaultValue( cFrom, "" ) + ::cCRLF )
   IF ::GetReply()
      ::inetSendAll( ::SocketCon, "RNTO " + hb_defaultValue( cTo, "" ) + ::cCRLF )
      RETURN ::GetReply()
   ENDIF

   RETURN .F.

// Create a new directory
METHOD MKD( cPath ) CLASS TIPClientFTP

   ::inetSendAll( ::SocketCon, "MKD " + cPath + ::cCRLF )

   RETURN ::GetReply()

// Delete an existing directory
METHOD RMD( cPath ) CLASS TIPClientFTP

   ::inetSendAll( ::SocketCon, "RMD " + cPath + ::cCRLF )

   RETURN ::GetReply()

METHOD Retr( cFile ) CLASS TIPClientFTP

   IF ::bUsePasv .AND. ! ::Pasv()
#if 0
      ::bUsePasv := .F.
#endif
      RETURN .F.
   ENDIF

   ::inetSendAll( ::SocketCon, "RETR " + cFile + ::cCRLF )

   IF ::TransferStart()
      ::ScanLength()
      RETURN .T.
   ENDIF

   RETURN .F.

METHOD Stor( cFile ) CLASS TIPClientFTP

   IF ::bUsePasv .AND. ! ::Pasv()
#if 0
      ::bUsePasv := .F.
#endif
      RETURN .F.
   ENDIF

   ::inetSendAll( ::SocketCon, "STOR " + cFile + ::cCRLF )

   /* It is important not to delete these lines in order not to disrupt the timing of
      the responses, which can lead to failures in transfers. */
   IF ! ::bUsePasv
      ::GetReply()
   ENDIF

   RETURN ::TransferStart()

METHOD List( cSpec ) CLASS TIPClientFTP

   LOCAL cStr

   IF ::bUsePasv .AND. ! ::Pasv()
#if 0
      ::bUsePasv := .F.
#endif
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
      lReadPort := ::ReadAuxPort()  /* QUESTION: is this assignment intentional? */
   ENDIF

   IF hb_defaultValue( lGetReply, .F. )
      lGetReply := ::GetReply()  /* QUESTION: is this assignment intentional? */
   ENDIF

   RETURN .T.

METHOD Port() CLASS TIPClientFTP

   ::SocketPortServer := hb_inetCreate( ::nConnTimeout )

   hb_mutexLock( s_mutexPort )

   DO WHILE ++s_nPort < _PORT_MAX
      hb_inetServer( s_nPort, ::SocketPortServer )
      IF ::inetErrorCode( ::SocketPortServer ) == 0
         hb_mutexUnlock( s_mutexPort )
         RETURN ::SendPort()
      ENDIF
   ENDDO

   s_nPort := _PORT_MIN

   hb_mutexUnlock( s_mutexPort )

   RETURN .F.

METHOD SendPort() CLASS TIPClientFTP

   LOCAL nPort := hb_inetPort( ::SocketPortServer )

   ::inetSendAll( ::SocketCon, "PORT " + ;
      StrTran( hb_inetGetHosts( NetName() )[ 1 ], ".", "," ) + "," + ;
      hb_ntos( Int( nPort / 256 ) ) + "," + hb_ntos( Int( nPort % 256 ) ) + ;
      ::cCRLF )

   RETURN ::GetReply()

METHOD ReadAuxPort() CLASS TIPClientFTP

   LOCAL cRet
   LOCAL cList

   IF ::TransferStart()

      cList := ""
      DO WHILE ( cRet := ::super:Read( 512 ) ) != NIL .AND. ! cRet == ""
         cList += cRet
      ENDDO

      hb_inetClose( ::SocketCon )
      ::SocketCon := ::SocketControl

      IF ::GetReply()
         RETURN cList
      ENDIF
   ENDIF

   RETURN NIL

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

METHOD MGet( cSpec, cLocalPath ) CLASS TIPClientFTP

   LOCAL cStr, cFile

   IF ::bUsePasv .AND. ! ::Pasv()
#if 0
      ::bUsePasv := .F.
#endif
      RETURN NIL
   ENDIF

   hb_default( @cLocalPath, "" )

   ::inetSendAll( ::SocketCon, "NLST " + hb_defaultValue( cSpec, "" ) + ::cCRLF )

   IF ( cStr := ::ReadAuxPort() ) != NIL
      FOR EACH cFile IN hb_ATokens( cStr, .T. )
         cFile := RTrim( cFile )
         IF ! cFile == ""
            ::Downloadfile( cLocalPath + cFile, cFile )
         ENDIF
      NEXT
   ENDIF

   RETURN cStr

METHOD MPut( cFileSpec, cAttr ) CLASS TIPClientFTP

   LOCAL aFile
   LOCAL cStr

   IF ! HB_ISSTRING( cFileSpec )
      RETURN NIL
   ENDIF

   cStr := ""
   FOR EACH aFile IN hb_vfDirectory( cFileSpec, cAttr )
      IF ::UploadFile( hb_FNameDir( cFileSpec ) + aFile[ F_NAME ] )
         cStr += e"\r\n" + aFile[ F_NAME ]
      ENDIF
   NEXT

   /* QUESTION: Shouldn't this return an array?
                Why emulate a platform specific and ill-defined format? */
   RETURN SubStr( cStr, Len( e"\r\n" ) + 1 )

METHOD UploadFile( cLocalFile, cRemoteFile ) CLASS TIPClientFTP

   ::bEof := .F.
   ::oUrl:cFile := hb_defaultValue( cRemoteFile, hb_FNameNameExt( cLocalFile ) )

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

METHOD DownloadFile( cLocalFile, cRemoteFile ) CLASS TIPClientFTP

   ::bEof := .F.
   ::oUrl:cFile := hb_defaultValue( cRemoteFile, hb_FNameNameExt( cLocalFile ) )

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

// Return total file size for <cFileSpec>
METHOD FileSize( cFileSpec ) CLASS TIPClientFTP

   LOCAL aFile
   LOCAL nSize := 0

   FOR EACH aFile IN ::ListFiles( cFileSpec )
      nSize += aFile[ F_SIZE ]
   NEXT

   RETURN nSize

/* Listing formats (from libcurl)
   https://github.com/curl/curl/blob/master/lib/ftplistparser.c
   Unix version 1: drwxr-xr-x 1 user01 ftp  512 Jan 29 23:32 prog
   Unix version 2: drwxr-xr-x 1 user01 ftp  512 Jan 29 1997  prog
   Unix version 3: drwxr-xr-x 1      1   1  512 Jan 29 23:32 prog
   Unix symlink  : lrwxr-xr-x 1 user01 ftp  512 Jan 29 23:32 prog -> prog2000
   DOS style/IIS : 01-29-97 11:32PM <DIR> prog
   DOS style/IIS : 01-29-97 11:32PM    512 prog
   DOS style/IIS : 01-29-2010 11:32PM <DIR> prog
 */

// Parse the :list() string into a hb_vfDirectory() compatible 2-dim array
METHOD ListFiles( cFileSpec ) CLASS TIPClientFTP

   LOCAL aMonth := { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" }

   LOCAL aList, aFile, cEntry, nStart, nEnd
   LOCAL cYear, cMonth, cDay, cTime

   LOCAL cList := ::list( cFileSpec )

   IF Empty( cList )
      RETURN {}
   ENDIF

   aList := hb_ATokens( cList, .T. )

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

         IF Val( StrTran( aFile[ F_ATTR ], "-" ) ) == 0

            // continue with Unix format

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
            cMonth        := StrZero( hb_AScan( aMonth, SubStr( cEntry, nStart, nEnd - nStart ), , , .T. ), 2 )
            nStart        := nEnd

            // Day
            DO WHILE SubStr( cEntry, ++nStart, 1 ) == " "
            ENDDO
            nEnd          := hb_At( " ", cEntry, nStart )
            cDay          := SubStr( cEntry, nStart, nEnd - nStart )
            nStart        := nEnd

            // Year
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

            aFile[ F_DATE ] := hb_SToD( cYear + cMonth + cDay )
            aFile[ F_TIME ] := cTime

         ELSE

            // DOS style/IIS format

            aFile[ F_LEN + 1 ] := 0
            aFile[ F_LEN + 2 ] := aFile[ F_LEN + 3 ] := aFile[ F_ATTR ] := ""

            aFile[ F_DATE ] := hb_CToD( aFile[ F_ATTR ], "mm-dd-yy" )

            // # time
            DO WHILE SubStr( cEntry, ++nStart, 1 ) == " "
            ENDDO
            nEnd   := hb_At( " ", cEntry, nStart )
            cTime  := SubStr( cEntry, nStart, nEnd - nStart )
            nStart := nEnd

            aFile[ F_TIME ] := Left( TString( Secs( Left( cTime, 5 ) ) + iif( Right( cTime, 2 ) == "PM", 43200, 0 ) ), 5 )

            // file size
            DO WHILE SubStr( cEntry, ++nStart, 1 ) == " "
            ENDDO
            nEnd            := hb_At( " ", cEntry, nStart )
            aFile[ F_SIZE ] := Val( SubStr( cEntry, nStart, nEnd - nStart ) )
            nStart          := nEnd

         ENDIF

         // file name
         DO WHILE SubStr( cEntry, ++nStart, 1 ) == " "
         ENDDO

         aFile[ F_NAME ] := SubStr( cEntry, nStart )

         cEntry := aFile
      ENDIF
   NEXT

   RETURN aList
