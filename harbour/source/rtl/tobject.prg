/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Base Object from wich all object finally inherit
 *
 * Copyright 2000 JfL&RaC  <jfl@mafact.com>, <rac@mafact.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * This is work in progress ... To be continued
 *
 * See doc/license.txt for licensing terms.
 *
 */

/* Harbour Class TObject !  Warning ... can not use the preprocessor */

#include "common.ch"
#include "hboo.ch"

function TObject()
  static oClass
  local nScope := 1

  if oClass == nil
     oClass := TClass():New("TObject", {} )

     oClass:AddInline("CLASSNAME"      , {|Self | __OBJGETCLSNAME( SELF )     }, nScope )
     oClass:AddInline("CLASSH"         , {|Self | __CLASSH( SELF )            }, nScope )
     oClass:AddInline("CLASSSEL"       , {|Self | __CLASSSEL( SELF:CLASSH() ) }, nScope )

     /*oClass:AddInline("EVAL"           ,{|Self | __EVAL( SELF )             }, nScope ) */
     /*oClass:AddInline("ISDERIVEDFROM"  ,{|Self, xPar1 | ObjDerivedFrom( SELF,xPar1 ) }, nScope ) */

     /* Those one exist within class(y), so we will probably try to implement it                */

     /*oClass:AddInline("INIT"           ,{|Self | Self                       }, nScope ) */
     /*oClass:AddInline("MSGNOTFOUND"    ,{|Self |                            }, nScope ) */

     /*oClass:AddInline("ISKINDOF"       ,{|Self |                            }, nScope ) */
     /*oClass:AddInline("asString"       ,{|Self | ::class:name + " object"   }, nScope ) */
     /*oClass:AddInline("asExpStr"       ,{|Self |                            }, nScope ) */
     /*oClass:AddInline("basicSize"      ,{|Self | LEN( self )                }, nScope ) */
     /*oClass:AddInline("become"         ,{|Self |                            }, nScope ) */
     /*oClass:AddInline("isEqual"        ,{|Self |                            }, nScope ) */
     /*oClass:AddInline("isScalar"       ,{|Self |                            }, nScope ) */
     /*oClass:AddInline("copy"           ,{|Self |                            }, nScope ) */
     /*oClass:AddInline("deepCopy"       ,{|Self |                            }, nScope ) */
     /*oClass:AddInline("deferred"       ,{|Self |                            }, nScope ) */
     /*oClass:AddInline("exec"           ,{|Self |                            }, nScope ) */
     /*oClass:AddInline("error           ,{|Self |                            }, nScope ) */
     /*oClass:AddInline("hash"           ,{|Self |                            }, nScope ) */
     /*oClass:AddInline("null"           ,{|Self |                            }, nScope ) */
     /*oClass:AddInline("size"           ,{|Self | LEN( self )                }, nScope ) */
     /*oClass:AddInline("protectErr"     ,{|Self |                            }, nScope ) */
     /*oClass:AddInline("hiddenErr"      ,{|Self |                            }, nScope ) */
     /*oClass:AddInline("msgNotFound"    ,{|Self |                            }, nScope ) */
     /*oClass:AddInline("readOnlyErr"    ,{|Self |                            }, nScope ) */
     /*oClass:AddInline("wrongClass"     ,{|Self |                            }, nScope ) */
     /*oClass:AddInline("badMethod"      ,{|Self |                            }, nScope ) */


     /* this one exit within VO and is Auto Called when object ran out of scope */

     /*oClass:AddInline("Axit"       ,{|Self |  }, nScope ) */


     oClass:Create()
  endif

return oClass:Instance()

