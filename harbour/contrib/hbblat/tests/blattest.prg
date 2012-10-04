/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HBBLAT sample test file
 *
 * Copyright 2007-2009 Francesco Saverio Giudice <info@fsgiudice.com>
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


#include "hbblat.ch"

#define ADDRESS_FROM   "yourname@domain.com"         // put here your address from
#define ADDRESS_TO     "hbblat_test@fsgiudice.com"   // this mail can be used for tests
//#define ADDRESS_CC     "another@domain.com"

#define SERVER_SMTP    "your.stmpserver.com"         // put your smtp server here

PROCEDURE Main()

   //LOCAL cCmd
   LOCAL nRet
   LOCAL oBlat := HBBlat():New()

   ?
   ? "HBBlat test"

   oBlat:cFrom                   := ADDRESS_FROM
   oBlat:cTo                     := ADDRESS_TO
   // oBlat:cUserAUTH               := "myaccount@mydomain.org"
   // oBlat:cPasswordAUTH           := "mypassword"
   // oBlat:cHostname               := "mail.anydomain.com"
   // oBlat:cCC                     := ADDRESS_CC
   // oBlat:cCCFile                 := "f_cc.txt"
   // oBlat:cBCC                    := "info@fsgiudice.com"
   // oBlat:cBCCFile                := "f_bcc.txt"
   // oBlat:cBodyFile               := "c.bin"
   oBlat:cBody                   := e"Body part\n\rEnd Body"
   oBlat:cServerSMTP             := SERVER_SMTP
   oBlat:cSubject                := "Test from Blat"
   // oBlat:lSuppressSubject        := .T.
   // oBlat:cSubjectFile            := "f_subjct.txt"
   // oBlat:lToUndiscloseRecipients := .T.
   oBlat:cPostScriptumFile       := "f_ps.txt"
   oBlat:lRequestDisposition     := .T.          // does not work ???
   oBlat:lRequestReturnReceipt   := .T.

   oBlat:cAttachTextFiles         := "f_subjct.txt"
   oBlat:aAttachTextFiles         := { "f_ps.txt", "blattest.prg", "blatcmd.prg" }
   oBlat:cAttachListTextFile      := "f_listtx.txt"

   oBlat:cLogFile                 := "log.txt"
   oBlat:lLogTimestamp            := .T.
   oBlat:lDebug                   := .T.
   oBlat:lLogOverwrite            := .T.

   // oBlat:lSuperDebug              := .T.     // This display internal checking

   ? "Checking options ..."
   //oBlat:Check()
   ?
   ? "Command .........: ", oBlat:GetCommand()  // Not necessary - this show complete command line sent to blat

   ? "Sending mail ..."
   ? "Return Value ....: ", nRet := oBlat:Send()

   ? "Error String ....: ", oBlat:ErrorString()

   // Blat error
   ? "Blat Error ......: ", oBlat:BlatError()
   ? "Blat Error String: ", oBlat:BlatErrorString()

   ?
   ? iif( nRet == 0, "mail sent correctly!", "mail NOT sent" )
   ?

   RETURN
