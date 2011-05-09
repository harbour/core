/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   Test CT3 function CSETARGERR()
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
 *
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


#include "../ct.ch"


procedure main

local cRet, olderr

 ctinit()

 qout ("Begin test of CSETARGERR()")
 qout ("")

 qout ("")
 qout ("Local error handler: ")

 olderr := errorblock ({|oerr|myerrhandler(oerr)})

 // standard behaviour on argument error
 qout ("")
 qout ("Standard behaviour")
 qout ("  Call to addascii (5789676,1,2,.T.):")
 cRet := addascii (5789676,1,2,.T.)
 qout ("  return value was", cRet)
 qout ("")
 qout ("  Call to charadd ('AA',.F.):")
 cRet := charadd ("AA",.F.)
 qout ("  return value was", cRet, "<Press any key>")
 qout ("")
 inkey (0)

 // CT_ARGERR_WHOCARES on argument error
 qout ("")
 qout ("CT_ARGERR_WHOCARES behaviour")
 CSETARGERR (CT_ARGERR_WHOCARES)
 qout ("  Call to addascii (5789676,1,2,.T.):")
 cRet := addascii (5789676,1,2,.T.)
 qout ("  return value was", cRet)
 qout ("")
 qout ("  Call to charadd ('AA',.F.):")
 cRet := charadd ("AA",.F.)
 qout ("  return value was", cRet, "<Press any key>")
 qout ("")
 inkey (0)

 // CT_ARGERR_WARNING on argument error
 qout ("")
 qout ("CT_ARGERR_WARNING behaviour")
 CSETARGERR (CT_ARGERR_WARNING)
 qout ("  Call to addascii (5789676,1,2,.T.):")
 cRet := addascii (5789676,1,2,.T.)
 qout ("  return value was", cRet)
 qout ("")
 qout ("  Call to charadd ('AA',.F.):")
 cRet := charadd ("AA",.F.)
 qout ("  return value was", cRet, "<Press any key>")
 qout ("")
 inkey (0)

 // CT_ARGERR_ERROR on argument error
 qout ("")
 qout ("CT_ARGERR_ERROR behaviour")
 CSETARGERR (CT_ARGERR_ERROR)
 qout ("  Call to addascii (5789676,1,2,.T.):")
 cRet := addascii (5789676,1,2,.T.)
 qout ("  return value was", cRet)
 qout ("")
 qout ("  Call to charadd ('AA',.F.):")
 cRet := charadd ("AA",.F.)
 qout ("  return value was", cRet, "<Press any key>")
 qout ("")
 inkey (0)

 // CT_ARGERR_CATASTROPHIC on argument error
 qout ("")
 qout ("CT_ARGERR_CATASTROPHIC behaviour")
 CSETARGERR (CT_ARGERR_CATASTROPHIC)
 qout ("  Call to addascii (5789676,1,2,.T.):")
 cRet := addascii (5789676,1,2,.T.)
 qout ("  return value was", cRet)
 qout ("")
 qout ("  Call to charadd ('AA',.F.):")
 cRet := charadd ("AA",.F.)
 qout ("  return value was", cRet, "<Press any key>")
 qout ("")
 inkey (0)

 qout ("")
 qout ("Standard error handler: ")
 errorblock (olderr)

 // standard behaviour on argument error
 qout ("")
 qout ("Standard behaviour")
 qout ("  Call to charadd ('AA',.F.):")
 cRet := charadd ("AA",.F.)
 qout ("  return value was", cRet, "<Press any key>")
 qout ("")
 inkey (0)

 // CT_ARGERR_WHOCARES on argument error
 qout ("")
 qout ("CT_ARGERR_WHOCARES behaviour")
 CSETARGERR (CT_ARGERR_WHOCARES)
 qout ("  Call to charadd ('AA',.F.):")
 cRet := charadd ("AA",.F.)
 qout ("  return value was", cRet, "<Press any key>")
 qout ("")
 inkey (0)

 // CT_ARGERR_WARNING on argument error
 qout ("")
 qout ("CT_ARGERR_WARNING behaviour")
 CSETARGERR (CT_ARGERR_WARNING)
 qout ("  Call to charadd ('AA',.F.):")
 cRet := charadd ("AA",.F.)
 qout ("  return value was", cRet, "<Press any key>")
 qout ("")
 inkey (0)

 // CT_ARGERR_ERROR on argument error
 qout ("")
 qout ("CT_ARGERR_ERROR behaviour")
 CSETARGERR (CT_ARGERR_ERROR)
 qout ("  Call to charadd ('AA',.F.):")
 cRet := charadd ("AA",.F.)
 qout ("  return value was", cRet, "<Press any key>")
 qout ("")
 inkey (0)

 // CT_ARGERR_CATASTROPHIC on argument error
 qout ("")
 qout ("CT_ARGERR_CATASTROPHIC behaviour")
 CSETARGERR (CT_ARGERR_CATASTROPHIC)
 qout ("  Call to charadd ('AA',.F.):")
 cRet := charadd ("AA",.F.)
 qout ("  return value was", cRet, "<Press any key>")
 qout ("")
 inkey (0)

 qout ("End test of CSETARGERR()")

 ctexit()

return


function myerrhandler (oerr)

local ni, nDigit

  memvar Input

  qout ("    Error handler called:")
  qout ("      err:severity.....:",oerr:severity)
  qout ("      err:subSystem....:",oerr:subSystem)
  qout ("      err:operation....:",oerr:operation)
  qout ("      len(err:args)....:",len(oerr:args))
  for ni := 1 to len (oerr:args)
    qout ("          err:args["+alltrim(str(ni))+"]..:",oerr:args[ni])
  next ni
  qout ("      err:genCode......:",oerr:genCode)
  qout ("      err:subCode......:",oerr:subCode)
  qout ("      err:osCode.......:",oerr:osCode)
  qout ("      err:filename.....:",oerr:filename)
  qout ("      err:tries........:",oerr:tries)
  qout ("      err:cargo........:",oerr:cargo)
  qout ("      err:canDefault...:",oerr:canDefault)
  qout ("      err:canRetry.....:",oerr:canRetry)
  qout ("      err:canSubstitute:",oerr:canSubstitute)
  qout()

  if oerr:canSubstitute

    private Input := ""

    qout ("    Error handler can substitute return value, so please")
    ACCEPT "    type in return value <Return for default>: " TO Input

    if empty (Input)
      qout ("    You have chosen the default return value. Ok, this should ")
      qout ("    be now problem, since the last digit of err:subCode indicates")
      qout ("    the type of the return value:")
      qout ("      0 is NIL,    1 is String,    2 is Integer,")
      qout ("      3 is Float,  4 is Boolean,   5 is Date")
      qout ("      6 is Block,  7 is Array,    8 is Object")
      qout ("      9 is unknown")
      nDigit := int (oerr:subCode%10)
      qout ("    Here it's a "+alltrim(str(nDigit))+", so I return a ")
      do case
        case nDigit == 0
          qqout ("NIL.")
          Input := NIL

        case nDigit == 1
          qqout ("String.")
          Input := ""

        case nDigit == 2
          qqout ("Integer.")
          Input := 0

        case nDigit == 3
          qqout ("Float.")
          Input := 0.0

        case nDigit == 4
          qqout ("Boolean.")
          Input := .F.

        case nDigit == 5
          qqout ("Date.")
          Input := ctod ("")

        case nDigit == 6
          qqout ("Block.")
          Input := {||NIL}

        case nDigit == 7
          qqout ("Array.")
          Input := {}

        case nDigit == 8
          qqout ("Object.")
          Input := GetNew()

        case nDigit == 9
          qqout ("<don't know, NIL would be best.")
          Input := NIL

      endcase

    endif

    return Input

  endif

  if oerr:canDefault
    qout ("    Subsystem can set the default value itself, so this error")
    qout ("    is only informative.")
  endif

return .F.
