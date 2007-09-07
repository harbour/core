/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    Undocumented CA-Cl*pper function used to validate
 *    instance variable type in assign messages.
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

/* NOTE: In CA-Cl*pper 5.2 the 6th and 7th arguments are a minimum and 
         maximum limit for a (numerical) value. In CA-Cl*pper 5.3 and 
         in Harbour there is no 7th argument and 6th is a generic 
         validation codeblock. In other words, in case of _eInstVar() 
         Harbour is compatible with CA-Cl*pper 5.3, not with 5.2. */

/* NOTE: In CA-Cl*pper 5.2/5.3 the cMethod argument seems to be ignored. */

FUNCTION _eInstVar( oVar, cMethod, xValue, cType, nSubCode, bValid )

   LOCAL oError

   IF VALTYPE( xValue ) != cType .OR. ;
      ( bValid != NIL .AND. !EVAL( bValid, oVar, xValue ) )
      oError := ErrorNew()
      oError:description := HB_LANGERRMSG( 1 )
      oError:gencode := 1
      oError:severity := 2
      oError:cansubstitute := .T.
      oError:subsystem := oVar:classname
      oError:operation := cMethod
      oError:subcode := nSubCode
      oError:args := { xValue }
      xValue := EVAL( ERRORBLOCK(), oError )
      IF VALTYPE( xValue ) != cType
         __errInHandler()
      ENDIF
   ENDIF

   RETURN xValue
