/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Base Object from wich all object finally inherit
 *
 * Copyright 2000 JfL&RaC <jfl@mafact.com>, <rac@mafact.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/* WARNING: Can not use the preprocessor */

#include "common.ch"
#include "hboo.ch"

FUNCTION TObject()
   STATIC s_oClass
   LOCAL nScope := 1
  
   IF s_oClass == NIL
      s_oClass := TClass():New( "TObject", {} )
  
      s_oClass:AddInline( "CLASSNAME"      , {| Self | __OBJGETCLSNAME( Self )     }, nScope )
      s_oClass:AddInline( "CLASSH"         , {| Self | __CLASSH( Self )            }, nScope )
      s_oClass:AddInline( "CLASSSEL"       , {| Self | __CLASSSEL( Self:CLASSH() ) }, nScope )
  
      /*s_oClass:AddInline( "EVAL"           , {| Self | __EVAL( Self )             }, nScope ) */
      /*s_oClass:AddInline( "ISDERIVEDFROM"  , {| Self, xPar1 | __ObjDerivedFrom( Self, xPar1 ) }, nScope ) */
  
      /* Those one exist within Class(y), so we will probably try to implement it               */
  
      /*s_oClass:AddInline( "INIT"           , {| Self | Self                       }, nScope ) */
      /*s_oClass:AddInline( "MSGNOTFOUND"    , {| Self |                            }, nScope ) */
                                                  
      /*s_oClass:AddInline( "ISKINDOF"       , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "asString"       , {| Self | ::class:name + " object"   }, nScope ) */
      /*s_oClass:AddInline( "asExpStr"       , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "basicSize"      , {| Self | Len( Self )                }, nScope ) */
      /*s_oClass:AddInline( "become"         , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "isEqual"        , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "isScalar"       , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "copy"           , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "deepCopy"       , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "deferred"       , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "exec"           , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "error           , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "hash"           , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "null"           , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "size"           , {| Self | Len( Self )                }, nScope ) */
      /*s_oClass:AddInline( "protectErr"     , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "hiddenErr"      , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "msgNotFound"    , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "readOnlyErr"    , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "wrongClass"     , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "badMethod"      , {| Self |                            }, nScope ) */

      /* this one exit within VO and is Auto Called when object ran out of scope */
  
      /*s_oClass:AddInline( "Axit"           , {| Self |  }, nScope ) */
  
      s_oClass:Create()

   ENDIF
  
   RETURN s_oClass:Instance()

