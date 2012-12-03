/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * BLAT wrapper library interface code.
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

/*
  Code Description

  2    The server actively denied our connection.
       The mail server doesn't like the sender name.

  1    Unable to open SMTP socket
       SMTP get line did not return 220
       command unable to write to socket
       Server does not like To: address
       Mail server error accepting message data.

  0    OK
  1    File name (message text) not given
  1    Bad argument given
  2    File (message text) does not exist
  3    Error reading the file (message text) or attached file
  4    File (message text) not of type FILE_TYPE_DISK
  5    Error Reading File (message text)

  12   -server or -f options not specified and not found in registry
  13   Error opening temporary file in temp directory
*/

#ifndef _HBBLAT_CH
#define _HBBLAT_CH

/* Numeric Errors */

#define BLAT_ERR_UNKNONW                               -1

#define BLAT_SUCCESS                                   0
#define BLAT_ERR_MESSAGE_NOT_ACCEPTED                  1

#define BLAT_ERR_MISSING_FROM                          1000
#define BLAT_ERR_MISSING_TO                            1001
#define BLAT_ERR_MISSING_TOFILE                        1002
#define BLAT_ERR_MISSING_BODY                          1003
#define BLAT_ERR_MISSING_BODYFILE                      1004
#define BLAT_ERR_MISSING_SERVERSMTP                    1005
#define BLAT_ERR_MISSING_SUBJECTFILE                   1006
#define BLAT_ERR_MISSING_CCFILE                        1007
#define BLAT_ERR_MISSING_BCCFILE                       1008
#define BLAT_ERR_MISSING_PSFILE                        1009
#define BLAT_ERR_MISSING_ATTACHLISTBINFILE             1010
#define BLAT_ERR_MISSING_ATTACHLISTTEXTFILE            1011
#define BLAT_ERR_MISSING_ATTACHLISTEMBEDDEDFILE        1012
#define BLAT_ERR_MISSING_ALTERNATETEXTFILE             1013
#define BLAT_ERR_MISSING_SIGNATUREFILE                 1014
#define BLAT_ERR_MISSING_TAGFILE                       1015
#define BLAT_ERR_WRONG_DSN                             1016

#define BLAT_ERR_LOGICAL_EXPECTED                      2000
#define BLAT_ERR_STRING_EXPECTED                       2001

/* Text Errors */

#define BLAT_TEXT_ERROR                                "Blat Error"

#define BLAT_TEXT_ERR_UNKNOWN                          "Unknown error"

#define BLAT_TEXT_SUCCESS                              "Ok"
#define BLAT_TEXT_ERR_MESSAGE_NOT_ACCEPTED             "Message not accepted by server"

#define BLAT_TEXT_ERR_MISSING_FROM                     "Missing From: parameter"
#define BLAT_TEXT_ERR_MISSING_TO                       "Missing To: parameter"
#define BLAT_TEXT_ERR_MISSING_TOFILE                   "ToFile file not found"
#define BLAT_TEXT_ERR_MISSING_BODY                     "Missing message body"
#define BLAT_TEXT_ERR_MISSING_BODYFILE                 "Message body file not found"
#define BLAT_TEXT_ERR_MISSING_SERVERSMTP               "Missing SMTP server"
#define BLAT_TEXT_ERR_MISSING_SUBJECTFILE              "Subject file not found"
#define BLAT_TEXT_ERR_MISSING_CCFILE                   "CC file not found"
#define BLAT_TEXT_ERR_MISSING_BCCFILE                  "BCC file not found"
#define BLAT_TEXT_ERR_MISSING_PSFILE                   "PS file not found"
#define BLAT_TEXT_ERR_MISSING_ATTACHLISTBINFILE        "Attach List Bin file not found"
#define BLAT_TEXT_ERR_MISSING_ATTACHLISTTEXTFILE       "Attach List Text file not found"
#define BLAT_TEXT_ERR_MISSING_ATTACHLISTEMBEDDEDFILE   "Attach List Embedded file not found"
#define BLAT_TEXT_ERR_MISSING_ALTERNATETEXTFILE        "Alternate Text file not found"
#define BLAT_TEXT_ERR_MISSING_SIGNATUREFILE            "Signature file not found"
#define BLAT_TEXT_ERR_MISSING_TAGFILE                  "Tag file not found"
#define BLAT_TEXT_ERR_WRONG_DSN                        "Wrong DSN values (admitted: n, s, f, d)"

#define BLAT_TEXT_ERR_LOGICAL_EXPECTED                 "Logical var expected"
#define BLAT_TEXT_ERR_STRING_EXPECTED                  "String var expected"

/* Blat Errors */

#define BLAT_ERR_CANT_MALLOC                           4001  // "Malloc failed (possibly out of memory)."
#define BLAT_ERR_SENDING_DATA                          4002  // "Error sending data."
#define BLAT_ERR_INITIALIZING                          4003  // "Error initializing gensock.dll."
#define BLAT_ERR_VER_NOT_SUPPORTED                     4004  // "Version not supported."
#define BLAT_ERR_EINVAL                                4005  // "The winsock version specified by gensock is not supported by this winsock.dll."
#define BLAT_ERR_SYS_NOT_READY                         4006  // "Network not ready."
#define BLAT_ERR_CANT_RESOLVE_HOSTNAME                 4007  // "Can't resolve (mailserver) hostname."
#define BLAT_ERR_CANT_GET_SOCKET                       4008  // "Can't create a socket (too many simultaneous links?)"
#define BLAT_ERR_READING_SOCKET                        4009  // "Error reading socket."
#define BLAT_ERR_NOT_A_SOCKET                          4010  // "Not a socket."
#define BLAT_ERR_BUSY                                  4011  // "Busy."
#define BLAT_ERR_CLOSING                               4012  // "Error closing socket."
#define BLAT_WAIT_A_BIT                                4013  // "Wait a bit (possible timeout)."
#define BLAT_ERR_CANT_RESOLVE_SERVICE                  4014  // "Can't resolve service."
#define BLAT_ERR_CANT_CONNECT                          4015  // "Can't connect to mailserver (timed out if winsock.dll error 10060)"
#define BLAT_ERR_NOT_CONNECTED                         4016  // "Connection to mailserver was dropped."
#define BLAT_ERR_CONNECTION_REFUSED                    4017  // "Mail server refused connection."

#define BLAT_ERR_NO_ERROR_CODE                         -5000      /* this is returned by misbehaving stacks that
                                                                   * fail, but don't set an error code
                                                                   */
#define BLAT_TRY_INFINITE_KEY                          -1
#define BLAT_TRY_INFINITE_VALUE                        "INFINITE"

#endif /* _HBBLAT_CH */
