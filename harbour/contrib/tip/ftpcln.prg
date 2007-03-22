/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 *
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

#include "hbclass.ch"
#include "tip.ch"
#include "common.ch"

/**
* Inet service manager: ftp
*/

CLASS tIPClientFTP FROM tIPClient
   DATA nDataPort
   DATA cDataServer
   DATA bUsePasv
   DATA RegBytes
   DATA RegPasv
   // Socket opened in response to a port command
   DATA SocketControl

   METHOD New()
   METHOD Open()
   METHOD Read( nLen )
   METHOD Write( nLen )
   METHOD Close()
   METHOD TransferStart()
   METHOD Commit()

   METHOD GetReply()
   METHOD Pasv()
   METHOD TypeI()
   METHOD TypeA()
   METHOD List()
   METHOD Cwd()
   METHOD Dele()
   //METHOD Port()
   //METHOD SendPort()
   METHOD Retr()
   METHOD Stor()
   METHOD Quit()
   METHOD ScanLength()
   METHOD ReadAuxPort()
	method mget()
	// Method bellow contributed by  Rafa Carmona
	
   METHOD LS( cSpec ) 
   METHOD Rename( cFrom, cTo )
   // new method for file upload
   METHOD UpLoadFile( cLocalFile, cRemoteFile )
   // new method to download file
   METHOD DownLoadFile( cLocalFile, cRemoteFile ) 
   // new method to create an directory on ftp server
   METHOD MKD( cPath )
ENDCLASS


METHOD New(lTrace) CLASS tIPClientFTP
   local cFile :="ftp"
   local n := 0

   ::nDefaultPort := 21
   ::nConnTimeout := 3000
   ::bUsePasv := .T.
   ::nAccessMode := TIP_RW  // a read-write protocol
   ::lTrace :=lTrace
   if ::ltrace
      if !file("ftp.log")
         ::nHandle := fcreate("ftp.log")
      else
         while file(cFile+alltrim(str(n,2))+".log")
           n++
         enddo
         ::nHandle := fcreate(cFile+alltrim(str(n,2))+".log")
      endif        
   endif

   // precompilation of regex for better prestations
   ::RegBytes := HB_RegexComp( "\(([0-9]+)[ )a-zA-Z]" )
   ::RegPasv :=  HB_RegexComp( ;
      "([0-9]*) *, *([0-9]*) *, *([0-9]*) *, *([0-9]*) *, *([0-9]*) *, *([0-9]*)" )
RETURN Self


METHOD Open() CLASS tIPClientFTP

   IF Len( ::oUrl:cUserid ) == 0 .or. Len( ::oUrl:cPassword ) == 0
      RETURN .F.
   ENDIF

   IF .not. ::super:Open()
      RETURN .F.
   ENDIF

   InetSetTimeout( ::SocketCon, ::nConnTimeout )
   IF ::GetReply()
      ::InetSendall( ::SocketCon, "USER " + ::oUrl:cUserid + ::cCRLF )
      IF ::GetReply()
         ::InetSendall( ::SocketCon, "PASS " + ::oUrl:cPassword + ::cCRLF )
         // set binary by default
         IF ::GetReply() .and. ::TypeI()
            RETURN .T.
         ENDIF
      ENDIF
   ENDIF
RETURN .F.

METHOD GetReply() CLASS tIPClientFTP
   LOCAL nLen
   LOCAL cRep

   ::cReply := ::InetRecvLine( ::SocketCon, @nLen, 128 )
   
   cRep := ::cReply
   
   IF cRep == NIL
      RETURN .F.
   ENDIF

   // now, if the reply has a '-' as fourth character, we need to proceed...
   DO WHILE .not. Empty(cRep) .and. cRep[4] == '-'
      ::cReply := ::InetRecvLine( ::SocketCon, @nLen, 128 )
      cRep := IIf(ValType(::cReply) == "C", ::cReply, "")
   ENDDO

   // 4 and 5 are error codes
   IF ::InetErrorCode( ::SocketCon ) != 0 .or. ::cReply[1] >= '4'
      RETURN .F.
   ENDIF
RETURN .T.

METHOD Pasv() CLASS tIPClientFTP
   LOCAL aRep

   ::InetSendall( ::SocketCon, "PASV" + ::cCRLF )
   IF .not. ::GetReply()
      RETURN .F.
   ENDIF
   aRep := HB_Regex( ::RegPasv, ::cReply )

   IF Empty(aRep)
      RETURN .F.
   ENDIF

   ::cDataServer := aRep[2] + "." + aRep[3] + "." + aRep[4] + "." + aRep[5]
   ::nDataPort := Val(aRep[6]) *256 + Val( aRep[7] )

RETURN .T.


METHOD Close() CLASS tIPClientFTP
   InetSetTimeOut( ::SocketCon, ::nConnTimeout )
   if ::ltrace
      fClose(::nHandle)
   endif

   ::Quit()
RETURN ::super:Close()


METHOD Quit() CLASS tIPClientFTP
   ::InetSendall( ::SocketCon, "QUIT" + ::cCRLF )
RETURN ::GetReply()


METHOD TypeI() CLASS tIPClientFTP
   ::InetSendall( ::SocketCon, "TYPE I" + ::cCRLF )
RETURN ::GetReply()


METHOD TypeA() CLASS tIPClientFTP
   ::InetSendall( ::SocketCon, "TYPE A" + ::cCRLF )
RETURN ::GetReply()


METHOD CWD( cPath ) CLASS tIPClientFTP
   ::InetSendall( ::SocketCon, "CWD " + cPath + ::cCRLF )
RETURN ::GetReply()

METHOD DELE( cPath ) CLASS tIPClientFTP
   ::InetSendall( ::SocketCon, "DELE " + cPath + ::cCRLF )
RETURN ::GetReply()


// scan last reply for an hint of length
METHOD ScanLength() CLASS tIPClientFTP
   LOCAL aBytes
   aBytes := HB_Regex( ::RegBytes, ::cReply )
   IF .not. Empty(aBytes)
      ::nLength = Val( aBytes[2] )
   ENDIF
RETURN .T.


METHOD TransferStart() CLASS tIPClientFTP
   LOCAL skt
   ::SocketControl := ::SocketCon

   IF ::bUsePasv
      skt := InetConnectIP( ::cDataServer, ::nDataPort )
      IF skt != NIL .and. ::InetErrorCode( skt ) == 0
         // Get the start message from the control connection
         IF .not. ::GetReply()
            InetClose( skt )
            RETURN .F.
         ENDIF

         InetSetTimeout( skt, ::nConnTimeout )
         ::SocketCon := skt
      ENDIF
   /*ELSE
      ::SocketCon := InetAccept( ::SocketPortServer )*/
   ENDIF

RETURN .T.



METHOD Commit() CLASS tIPClientFTP
   InetClose( ::SocketCon )
   ::SocketCon := ::SocketControl
   ::bInitialized := .F.
   IF .not. ::GetReply()
      RETURN .F.
   ENDIF

   // error code?
   IF ::cReply[1] == "5"
      RETURN .F.
   ENDIF
/*
   IF ::GetReply() .and. ::cReply[1] != "5"
      RETURN .T.
   ENDIF*/
RETURN .F.


METHOD List(cSpec) CLASS tIPClientFTP
   LOCAL cStr
   
	IF cSpec=nil
		cSpec:=''
	else
		cSpec:=' '+cSpec
	ENDIF
   IF ::bUsePasv
      IF .not. ::Pasv()
         //::bUsePasv := .F.
         RETURN .F.
      ENDIF
   ENDIF

/*   IF .not. ::bUsePasv
      IF .not. ::Port()
         RETURN .F.
      ENDIF
   ENDIF
*/
   ::InetSendAll( ::SocketCon, "LIST"+cSpec + ::cCRLF )
   cStr := ::ReadAuxPort()
   ::bEof := .f.
RETURN cStr

METHOD ReadAuxPort(cLocalFile) CLASS tIPClientFTP
   LOCAL cRet, cList := "",nFile:=0

   IF .not. ::TransferStart()
      RETURN NIL
   END
   IF !empty(cLocalFile)
		nFile:=fcreate(cLocalFile)
   ENDIF
   cRet := ::super:Read( 512 )
   WHILE cRet != NIL .and. len( cRet ) > 0
		IF nFile>0
			fwrite(nFile,cRet)
		else
      		cList += cRet
		ENDIF
      cRet := ::super:Read( 512 )
   END

   InetClose( ::SocketCon )
   ::SocketCon := ::SocketControl
   IF ::GetReply()
	IF nFile>0
		fclose(nFile)
		return(.t.)
	ENDIF
    RETURN cList
   ENDIF
RETURN NIL



METHOD Stor( cFile ) CLASS tIPClientFTP

   LOCAL nTimeout

   IF ::bUsePasv
      IF .not. ::Pasv()
         //::bUsePasv := .F.
         RETURN .F.
      ENDIF
   ENDIF

   ::InetSendall( ::SocketCon, "STOR " + cFile+ ::cCRLF )

RETURN ::TransferStart()

/*
METHOD Port() CLASS tIPClientFTP
   LOCAL nPort := 16000

   ::SocketPortServer := InetCreate( ::nConnTimeout )
   DO WHILE nPort < 24000
      InetServer( nPort, ::SocketPortServer )
      IF ::InetErrorCode( ::SocketPortServer ) == 0
         RETURN ::SendPort()
      ENDIF
      nPort ++
   ENDDO
   ::SocketPortServer := NIL

RETURN .F.

METHOD SendPort() CLASS tIPClientFTP
   LOCAL cAddr
   LOCAL cPort, nPort

   cAddr := InetAddress( ::SocketPortServer )
   cAddr := StrTran( cAddr, ".", "," )
   nPort := InetPort( ::SocketPortServer )
   cPort := "," + AllTrim( Str ( Int( nPort / 256 ) ) ) +  "," + AllTrim( Str ( nPort % 256 ) )

   ? "PORT " + cAddr + cPort
   ::InetSendall( ::SocketCom, "PORT " + cAddr + cPort  + ::cCRLF )
RETURN ::GetReply()
*/

METHOD Read( nLen ) CLASS tIPClientFTP
   LOCAL cRet

   IF .not. ::bInitialized

      IF .not. Empty( ::oUrl:cPath )

         IF .not. ::CWD( ::oUrl:cPath )

            ::bEof = .T.  // no data for this transaction
            RETURN .F.

         ENDIF

      ENDIF

      IF Empty( ::oUrl:cFile )

         RETURN ::List()

      ENDIF

      IF .not. ::Retr( ::oUrl:cFile )

         ::bEof = .T.  // no data for this transaction
         RETURN .F.

      ENDIF

      // now channel is open
      ::bInitialized := .T.

   ENDIF

   cRet := ::super:Read( nLen )

   IF cRet == NIL

      ::Commit()
      ::bEof := .T.

   ENDIF

RETURN cRet

*
* FTP transfer wants commit only at end.
*
METHOD Write( cData, nLen ) CLASS tIPClientFTP

   IF .not. ::bInitialized

      IF Empty( ::oUrl:cFile )

         RETURN -1

      ENDIF

      IF .not. Empty( ::oUrl:cPath )

         IF .not. ::CWD( ::oUrl:cPath )
            RETURN -1
         ENDIF

      ENDIF

      IF .not. ::Stor( ::oUrl:cFile )
         RETURN -1
      ENDIF

      // now channel is open
      ::bInitialized := .T.
   ENDIF

RETURN ::super:Write( cData, nLen, .F. )

METHOD Retr( cFile,cLocalFile ) CLASS tIPClientFTP
   LOCAL nTimeout,cRet

   IF ::bUsePasv
      IF .not. ::Pasv()
         //::bUsePasv := .F.
         RETURN .F.
      ENDIF
   ENDIF

   ::InetSendAll( ::SocketCon, "RETR " + cFile+ ::cCRLF )

   IF ::TransferStart()
      RETURN .T.
   ENDIF

RETURN .F.

METHOD MGET( cSpec,cLocalPath ) CLASS tIPClientFTP

   LOCAL cStr,cfile,aFiles

   IF cSpec == nil
      cSpec := ''
	ENDIF
   IF cLocalPath=nil
      cLocalPath:=''
   ENDIF
   IF ::bUsePasv
      IF .not. ::Pasv()
         //::bUsePasv := .F.
         RETURN .F.
      ENDIF
   ENDIF

   ::InetSendAll( ::SocketCon, "NLST "+cSpec + ::cCRLF )
   cStr := ::ReadAuxPort()

   IF !empty(cStr)
      aFiles:=hb_atokens(strtran(cStr,chr(13),''),chr(10))
      FOR each cFile in aFiles
         ::downloadfile( cLocalPath+trim(cFile), trim(cFile) )
      NEXT

   ENDIF

	RETURN cStr

METHOD UpLoadFile( cLocalFile, cRemoteFile ) CLASS tIPClientFTP

   LOCAL cFileData
   LOCAL lRet,nRet
   LOCAL cPath := ""
   LOCAL cFile := ""
   Local cExt  := ""
   
   HB_FNameSplit( cLocalFile, @cPath, @cFile,@cExt  ) 
   
   DEFAULT cRemoteFile to cFile+cExt 
   
   cFileData := MemoRead(cLocalFile)   
   ::bEof := .F.   
   ::oUrl:cFile := cRemoteFile

   nRet := ::Write( cFileData )
   
   IF nRet == -1
      lRet := .F.         
   ELSE
      lRet := .T.
   ENDIF
     
   ::Commit() // Close Passive conection of previus file
   
RETURN lRet   

METHOD LS( cSpec ) CLASS tIPClientFTP

    LOCAL cStr,cfile,x,y

    IF cSpec == nil
       cSpec := ''
     ENDIF

    IF ::bUsePasv
    
       IF .not. ::Pasv()
       
          //::bUsePasv := .F.
          RETURN .F.
          
       ENDIF
       
    ENDIF

    ::InetSendAll( ::SocketCon, "NLST "+cSpec + ::cCRLF )
    cStr := ::ReadAuxPort()

RETURN cStr

/*Rename a traves del ftp */

METHOD Rename( cFrom, cTo ) CLASS tIPClientFTP
    Local lResult  := .F.

    ::InetSendAll( ::SocketCon, "RNFR "+ cFrom + ::cCRLF )
    
    IF ::GetReply()
    
       ::InetSendAll( ::SocketCon, "RNTO "+ cTo + ::cCRLF )
       lResult := ::GetReply()
       
    ENDIF

Return lResult

METHOD DownLoadFile( cLocalFile, cRemoteFile ) CLASS tIPClientFTP
   LOCAL cFileData
   LOCAL lRet,xRet 

   Local nHandle
   LOCAL cPath := ""
   LOCAL cFile := ""
   Local cExt  := ""
   
   HB_FNameSplit( cLocalFile, @cPath, @cFile,@cExt  ) 

   
   DEFAULT cRemoteFile to cFile+cExt 

   ::bEof := .F.   
   ::oUrl:cFile := cRemoteFile

   xRet := ::Read()

   IF ISLOGICAL( xRet )

      IF !xRet

         lRet := .F.

      ENDIF

   ELSEIF ISCHARACTER( xRet )

      IF EMPTY( XRET )

         lRet := .F.

      ELSE

         nHandle := FCREATE( cLocalFile )
         FWRITE( nHandle, xRet )
         FCLOSE( nHandle )
         lRet := .T.

      ENDIF

   ENDIF
     
   ::Commit() // Close Passive conection of previus file
   
RETURN lRet   

// Create a new folder
METHOD MKD( cPath ) CLASS tIPClientFTP
   ::InetSendall( ::SocketCon, "MKD " + cPath + ::cCRLF )
RETURN ::GetReply()
