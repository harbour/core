
/*
 * $Id$
 */

/*
 * Copyright 2001 Patrick Mast <harbour@patrick.be.kz>
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


//----------------------------------------------------------------------
Function TestTPerPro()
LOCAL cScript, oPP

oPP:=TPreProcessor():New("c:\harbour\include")
oPP:TranslateFile( "Test.scr", .t.,"ppo",.f.)
oPP:End()

Alert("Please, see test.ppo for preprocessed result")

RETURN NIL






//------------------------------------------------------------------------
#include "hbclass.ch"

#DEFINE CRLF HB_OsNewLine()


CLASS TPreProcessor

   DATA cIncludePath,cPreProcesses

   METHOD New( cIncludePath )
   METHOD End()

   METHOD AddRule( cRule )
   METHOD AddIncludepath( cIncludePath )
   METHOD SetIncludepath( cIncludePath )
   METHOD TranslateLine( cCode )
   METHOD TranslateFile( cFile, lWritePPO )
 //METHOD TranslateStr( cStr )


ENDCLASS




METHOD New( cIncludePath ) CLASS TPreprocessor

   __PP_Init( cIncludePath )

   ::cIncludepath:=cIncludePath

return Self




METHOD End() CLASS TPreprocessor

   __PP_Free()

return Self



METHOD SetIncludepath( cIncludePath ) CLASS TPreprocessor

   __PP_PATH( cIncludePath, .t. )

   ::cIncludepath:=cIncludePath

return Self




METHOD AddIncludepath( cIncludePath ) CLASS TPreprocessor

   __PP_PATH( cIncludePath, .f. )

   ::cIncludepath:=::cIncludePath+";"+cIncludePath

return Self





METHOD AddRule( cRule ) CLASS TPreprocessor

   local lResult

   if SubStr( LTrim( cRule ), 1, 1 ) != "#"
      Alert( "error on rule definition" )
      return nil
   endif

   lResult = __PPAddRule( cRule )

return lresult




METHOD TranslateLine( cCode ) CLASS TPreprocessor

   local cResult

   cResult = __PreProcess( cCode )

return cResult




METHOD TranslateFile( cFile, lWritePPO, cPPOExt, lWasteNoSpace ) CLASS TPreprocessor

   local cStr, cByte, hPPO, cExt, cPP
   local cResult :="", cCode := "", cByteWas:=""
   local n:=1, f:=1, nPuntComma:=1

   if lWritePPO = NIL
      lWritePPO := .f.
   endif

   if cPPOExt = NIL
      cPPOExt := ".ppo"
   else
      cPPOExt := "." + StrTran( cPPOExt, ".", "" )
   endif

   if lWasteNoSpace = NIL
      lWasteNoSpace := .f.
   endif

   cStr := MemoRead( cFile )

   while n<Len(cStr)

      cCode:=""
      nPuntComma:=1
      while n<=Len(cStr)
         cByte:=SubStr(cStr,f++,1)
         if cByte=Chr(9)
            loop
         endif
         n++
         if cByte = Chr(13)
            f++
            if cByteWas == ";"
               cCode:=SubStr(cCode ,1 ,Len( cCode )-1)
               nPuntComma++
               loop
            endif
            exit
         endif
         cByteWas:=cByte
         cCode+=cByte
      enddo


      IF ("#INCLUDE"  $Upper(cCode)) .OR.;
         ("#XCOMMAND" $Upper(cCode)) .OR.;
         ("#TRANSLATE"$Upper(cCode))
         __ppAddRule( cCode )
      ELSE
         cPP := __PreProcess( cCode )
      ENDIF

      if lWasteNoSpace
         if !Empty( cPP )
            cResult+= LTrim( cPP ) + Replicate(CRLF,nPuntComma)
         endif
      else
         cResult+=cPP + Replicate(CRLF,nPuntComma)
      endif

   enddo

   if lWritePPO

      cExt := SubStr( cFile, RAt( '.', cFile ) )
      if! ( cExt == '' )
         hPPO := FCreate( StrTran( cFile, cExt, cPPOExt ) )
      else
         hPPO := FCreate( cFile + cPPOExt )
      endif
      if hPPO == -1
         Alert( "ERROR! creating '"+ cFile +"' , O/S Error: " + Str( FError(), 2 ) )
      endif

      if FWrite(hPPO, cResult) == len(cResult)
         Fclose(hPPO)
      else
         Alert("ERROR! writing '"+ cFile +"'")
         Fclose(hPPO)
      endif

   endif

return cResult