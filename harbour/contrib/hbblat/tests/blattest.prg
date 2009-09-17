/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HBBLAT sample test file
 *
 * Copyright 2007-2009 Francesco Saverio Giudice <info@fsgiudice.com>
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


#include "common.ch"
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

   WITH OBJECT oBlat
      :cFrom                   := ADDRESS_FROM
      :cTo                     := ADDRESS_TO
      //:cUserAUTH               := "myaccount@mydomain.org"
      //:cPasswordAUTH           := "mypassword"
      //:cHostname               := "mail.anydomain.com"
      //:cCC                     := ADDRESS_CC
      //:cCCFile                 := "f_cc.txt"
      //:cBCC                    := "info@fsgiudice.com"
      //:cBCCFile                := "f_bcc.txt"
      //:cBodyFile               := "c.bat"
      :cBody                   := e"Body part\n\rEnd Body"
      :cServerSMTP             := SERVER_SMTP
      :cSubject                := "Test from Blat"
      //:lSuppressSubject        := TRUE
      //:cSubjectFile            := "f_subjct.txt"
      //:lToUndiscloseRecipients := TRUE
      :cPostScriptumFile       := "f_ps.txt"
      :lRequestDisposition     := TRUE          // does not work ???
      :lRequestReturnReceipt   := TRUE

      :cAttachTextFiles         := "f_subjct.txt"
      :aAttachTextFiles         := { "f_ps.txt", "blattest.prg", "blatcmd.prg" }
      :cAttachListTextFile      := "f_listtx.txt"

      :cLogFile                 := "log.txt"
      :lLogTimestamp            := TRUE
      :lDebug                   := TRUE
      :lLogOverwrite            := TRUE

      //:lSuperDebug              := TRUE     // This display internal checking

   END

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
   ? IIF( nRet == 0, "mail sent correctly!", "mail NOT sent" )
   ?

RETURN

//-----------------------------------------
