/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for Class commands
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
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
 * Copyright 2000 Brian Hays <bhays@abacuslaw.com>
 *    Documentation for the commands
 *
 * See doc/license.txt for licensing terms.
 *
 */

#ifndef HB_CLASS_CH_
#define HB_CLASS_CH_

#xcommand CLASS <ClassName> [ <frm: FROM, INHERIT> <SuperClass> ] => ;
   function <ClassName>() ;;
      static oClass ;;
      if oClass == nil ;;
         oClass := TClass():New( <(ClassName)> [,<(SuperClass)>] ) ;;
     #undef _CLASS_NAME_ ;;
     #define _CLASS_NAME_ <ClassName> ;;
     #translate CLSMETH <ClassName> <MethodName>() => @<ClassName>_<MethodName>() ;
     [ ; #translate Super : => ::<SuperClass>: ] ;
     [ ; extern <SuperClass> ]


/* Note the use of commas ',' on the following rule to avoid their call
   if there are no AS ... or INIT clauses specified. As we just use
   those methods first parameter, the second one supplied acts as a dummy one */

#xcommand DATA <DataName1> [,<DataNameN>] [ AS <type> ] [ INIT <uValue> ] => ;
   [ oClass:SetType( <(type)> ) ; ][ oClass:SetInit( <uValue> ) ; ] ;
     oClass:AddData( <(DataName1)> ) ;
   [; oClass:AddData( <(DataNameN)> ) ] ;
   [; oClass:SetInit(,<uValue>) ] [ ; oClass:SetType(,<(type)>) ]

/* Note the use of commas ',' on the following rule to avoid their call
   if there are no AS ... or INIT clauses specified. As we just use
   those methods first parameter, the second one supplied acts as a dummy one */

#xcommand CLASSDATA <DataName1> [,<DataNameN>] [ AS <type> ] [ INIT <uValue> ] => ;
   [ oClass:SetType( <(type)> ) ; ][ oClass:SetInit( <uValue> ) ; ] ;
     oClass:AddClassData( <(DataName1)> ) ;
   [; oClass:AddClassData( <(DataNameN)> ) ] ;
   [; oClass:SetInit(,<uValue>) ] [ ; oClass:SetType(,<(type)>) ]

#xcommand METHOD <MethodName>( [<params,...>] ) [ CONSTRUCTOR ] => ;
   oClass:AddMethod( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>() )

#xcommand METHOD <MethodName>( [<params,...>] ) BLOCK <CodeBlock> => ;
   oClass:AddInline( <(MethodName)>, <CodeBlock> )

#xcommand METHOD <MethodName>( [<params,...>] ) EXTERN <FuncName>( [<params,...>] ) => ;
   oClass:AddMethod( <(MethodName)>, @<FuncName>() )

#xcommand METHOD <MethodName>( [<params,...>] ) INLINE <Code,...> => ;
   oClass:AddInline( <(MethodName)>, {|Self [,<params>] | <Code> } )

#xcommand METHOD <MethodName>( [<params,...>] ) VIRTUAL => ;
   oClass:AddVirtual( <(MethodName)> )

#xcommand METHOD <MethodName>( [<params,...>] ) SETGET => ;
   oClass:AddMethod( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>() ) ;;
   oClass:AddMethod( "_" + <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>() )

#xcommand METHOD <MethodName>( [<param>] ) OPERATOR <op> => ;
   oClass:AddMethod( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>() ) ;;
   oClass:AddInline( <(op)>, {|Self [,<param>] | ::<MethodName>( [<param>] ) } )

#xcommand MESSAGE <MessageName> METHOD <MethodName>( [<params,...>] ) => ;
   oClass:AddMethod( <(MessageName)>, CLSMETH _CLASS_NAME_ <MethodName>() )

#xcommand MESSAGE <MessageName>() METHOD <MethodName>( [<params,...>] ) => ;
   oClass:AddMethod( <(MessageName)>, CLSMETH _CLASS_NAME_ <MethodName>() )

#xcommand ERROR HANDLER <MethodName>( [<params,...>] ) => ;
   oClass:SetOnError( CLSMETH _CLASS_NAME_ <MethodName>() )

#xcommand ON ERROR <MethodName>( [<params,...>] ) => ;
   oClass:SetOnError( CLSMETH _CLASS_NAME_ <MethodName>() )

#xcommand ENDCLASS => oClass:Create() ;;
                      endif ;;
                      return oClass:Instance()

#xcommand METHOD <MethodName>( [<params,...>] ) CLASS <ClassName> => ;
          static function <ClassName>_<MethodName>( [<params>] ) ;;
          local Self := QSelf()

#endif /* HB_CLASS_CH_ */
