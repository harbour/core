/*
 * Harbour Project source code:
 * xhb compatibility functions
 *
 * Copyright 2007 Viktor Szakats (vszakats.net/harbour)
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

#include "hbclass.ch"

ANNOUNCE XHB_LIB


/* It's a hack which directly manipulates class definition.
 * It should not be used in user code. [druzus]
 */
#xcommand OPTIONAL INHERIT <!className!> =>                 ;
            IF hb_IsFunction( <"className"> )              ;;
               AAdd( oClass:asSuper, @__<className>() )    ;;
            ENDIF


INIT PROCEDURE xhb_Init()

   /* Add calls to do initial settings to Harbour to be more compatible with xhb. */

   ASSOCIATE CLASS _Character    WITH TYPE Character
   ASSOCIATE CLASS _Numeric      WITH TYPE Numeric
   ASSOCIATE CLASS _Array        WITH TYPE Array
   ASSOCIATE CLASS _Hash         WITH TYPE Hash
   ASSOCIATE CLASS _Block        WITH TYPE Block

   RETURN

CREATE CLASS Character FUNCTION _Character

   OPTIONAL INHERIT HBCharacter
   OPERATOR "[]" FUNCTION xhb_Index()
   OPERATOR "+"  FUNCTION xhb_Plus()
   OPERATOR "-"  FUNCTION xhb_Minus()
   OPERATOR "*"  FUNCTION xhb_Mult()
   OPERATOR "/"  FUNCTION xhb_Div()
   OPERATOR "%"  FUNCTION xhb_Mod()
   OPERATOR "^"  FUNCTION xhb_Pow()
   OPERATOR "++" FUNCTION xhb_Inc()
   OPERATOR "--" FUNCTION xhb_Dec()

   OPERATOR "==" FUNCTION xhb_EEqual()
   OPERATOR "="  FUNCTION xhb_Equal()
   OPERATOR "<>" FUNCTION xhb_NotEqual()
   OPERATOR "<"  FUNCTION xhb_Less()
   OPERATOR "<=" FUNCTION xhb_LessEq()
   OPERATOR ">"  FUNCTION xhb_Greater()
   OPERATOR ">=" FUNCTION xhb_GreaterEq()

ENDCLASS

CREATE CLASS Numeric FUNCTION _Numeric

   OPTIONAL INHERIT HBNumeric
   OPERATOR "+"  FUNCTION xhb_Plus()
   OPERATOR "-"  FUNCTION xhb_Minus()
   OPERATOR "*"  FUNCTION xhb_Mult()
   OPERATOR "/"  FUNCTION xhb_Div()
   OPERATOR "%"  FUNCTION xhb_Mod()
   OPERATOR "^"  FUNCTION xhb_Pow()
   OPERATOR "++" FUNCTION xhb_Inc()
   OPERATOR "--" FUNCTION xhb_Dec()

   OPERATOR "==" FUNCTION xhb_EEqual()
   OPERATOR "="  FUNCTION xhb_Equal()
   OPERATOR "<>" FUNCTION xhb_NotEqual()
   OPERATOR "<"  FUNCTION xhb_Less()
   OPERATOR "<=" FUNCTION xhb_LessEq()
   OPERATOR ">"  FUNCTION xhb_Greater()
   OPERATOR ">=" FUNCTION xhb_GreaterEq()

ENDCLASS

CREATE CLASS Array FUNCTION _Array

   OPTIONAL INHERIT HBArray
   OPERATOR "[]" FUNCTION xhb_Index()
   OPERATOR "$$" FUNCTION xhb_Include()

ENDCLASS

CREATE CLASS Hash INHERIT __HBHash FUNCTION _Hash

   OPTIONAL INHERIT HBHash
   ON ERROR FUNCTION xhb_HashError()
   OPERATOR "+"  FUNCTION xhb_Plus()
   OPERATOR "-"  FUNCTION xhb_Minus()
   OPERATOR "$$" FUNCTION xhb_Include()
   OPERATOR "="  FUNCTION xhb_Equal()
   OPERATOR "<>" FUNCTION xhb_NotEqual()

ENDCLASS

CREATE CLASS Block FUNCTION _Block

   OPTIONAL INHERIT HBBlock
   OPERATOR "==" FUNCTION xhb_EEqual()

ENDCLASS
