/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compatibility calls.
 *
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
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

#define HB_CLS_NOTOBJECT  /* avoid definition of method: INIT */

#include "hbclass.ch"

#include "common.ch"
#include "error.ch"

#define EG_OLEEXCEPTION 1001

#xcommand TRY                => BEGIN SEQUENCE WITH s_bBreak
#xcommand CATCH [<!oError!>] => RECOVER [USING <oError>] <-oError->
#xcommand FINALLY            => ALWAYS

STATIC s_bBreak := { | oError | Break( oError ) }

STATIC PROCEDURE Throw( oError )
   LOCAL lError := Eval( ErrorBlock(), oError )
   IF ! ISLOGICAL( lError ) .OR. lError
      __ErrInHandler()
   ENDIF
   Break( oError )
   RETURN

CREATE CLASS TOLEAUTO FROM WIN_OLEAUTO
   /* TODO: Implement compatibility to the required extent */
   VAR cClassName
   METHOD New( xOle, cIID )
   METHOD hObj( xOle )
ENDCLASS

METHOD hObj( xOle ) CLASS TOLEAUTO

   IF PCount() > 0 .AND. xOle != NIL
      IF ISNUMBER( xOle )
         xOle := win_N2P( xOle )
      ENDIF
      IF hb_isPointer( xOle )
         ::__hObj := xOle
      ENDIF
   ENDIF

   RETURN ::__hObj

METHOD New( xOle, cClass ) CLASS TOLEAUTO
   LOCAL hOle
   LOCAL oError

   IF ISNUMBER( xOle )
      xOle := win_N2P( xOle )
   ENDIF

   IF hb_isPointer( xOle )
      ::__hObj := xOle
      IF ISCHARACTER( cClass )
         ::cClassName := cClass
      ELSE
         ::cClassName := hb_ntos( win_P2N( xOle ) )
      ENDIF
   ELSEIF ISCHARACTER( xOle )
      hOle := __OleCreateObject( xOle )
      IF Empty( hOle )
         ::__hObj := hOle
         ::cClassName := xOle
      ELSE
         oError := ErrorNew()
         oError:Args          := hb_AParams()
         oError:CanDefault    := .F.
         oError:CanRetry      := .F.
         oError:CanSubstitute := .T.
         oError:Description   := win_OleErrorText()
         oError:GenCode       := EG_OLEEXCEPTION
         oError:Operation     := ProcName()
         oError:Severity      := ES_ERROR
         oError:SubCode       := -1
         oError:SubSystem     := "TOleAuto"

         RETURN Throw( oError )
      ENDIF
   ENDIF

   RETURN Self

FUNCTION CreateObject( xOle, cClass )
   RETURN TOleAuto():New( xOle, cClass )

FUNCTION GetActiveObject( xOle, cClass )
   LOCAL o := TOleAuto():New()
   LOCAL hOle
   LOCAL oError

   IF ISNUMBER( xOle )
      xOle := win_N2P( xOle )
   ENDIF

   IF hb_isPointer( xOle )
      o:__hObj := xOle
      IF ISCHARACTER( cClass )
         o:cClassName := cClass
      ELSE
         o:cClassName := hb_ntos( win_P2N( xOle ) )
      ENDIF
   ELSEIF ISCHARACTER( xOle )
      hOle := __OleGetActiveObject( xOle )
      IF ! Empty( hOle )
         o:__hObj := hOle
         o:cClassName := xOle
      ELSE
         oError := ErrorNew()
         oError:Args          := hb_AParams()
         oError:CanDefault    := .F.
         oError:CanRetry      := .F.
         oError:CanSubstitute := .T.
         oError:Description   := win_OleErrorText()
         oError:GenCode       := EG_OLEEXCEPTION
         oError:Operation     := ProcName()
         oError:Severity      := ES_ERROR
         oError:SubCode       := -1
         oError:SubSystem     := "TOleAuto"

         RETURN Throw( oError )
      ENDIF
   ENDIF

   RETURN o

FUNCTION CreateOLEObject( ... )
   RETURN win_P2N( __OleCreateObject( ... ) )

CREATE CLASS Win32Prn FROM WIN_PRN
ENDCLASS

CREATE CLASS Win32Bmp FROM WIN_BMP
ENDCLASS
