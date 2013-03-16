/*
 * Harbour Project source code:
 * Xbase++ operator emulation
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
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

#include "hbclass.ch"

ANNOUNCE XPP_OP_OVERLOAD

/* It's a hack which directly manipulates class definition.
 * It should not be used in user code. [druzus]
 */
#xcommand OPTIONAL INHERIT <!className!> =>                 ;
            IF hb_IsFunction( <"className"> )              ;;
               AAdd( oClass:asSuper, @__<className>() )    ;;
            ENDIF

INIT PROCEDURE xpp_Init()

   ASSOCIATE CLASS xpp_op_Character WITH TYPE Character
   ASSOCIATE CLASS xpp_op_Array     WITH TYPE Array

   RETURN

CREATE CLASS Character FUNCTION xpp_op_Character

   OPTIONAL INHERIT HBCharacter
   OPERATOR "[]" FUNCTION xpp_Index()

ENDCLASS

CREATE CLASS Array FUNCTION xpp_op_Array

   OPTIONAL INHERIT HBArray
   OPERATOR "$$" FUNCTION xpp_Include()

ENDCLASS
