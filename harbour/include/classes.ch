/*
 * $Id$
 */

/*
   Harbour classes commands

   Copyright(C) 1999 by Antonio Linares.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: alinares@fivetech.com
*/

#ifndef _CLASSES_CH
#define _CLASSES_CH

#xcommand CLASS <ClassName> [ <frm: FROM, INHERIT> <SuperClass> ] => ;
   function <ClassName>() ;;
      static oClass ;;
      if oClass == nil ;;
         oClass = TClass():New( <(ClassName)> [,<(SuperClass)>] ) ;;
     #define _CLASS_NAME_ <ClassName> ;;
     #translate CLSMETH <ClassName> <MethodName>() => @<ClassName>_<MethodName>() ;
     [ ; #translate Super : => ::<SuperClass>: ] ;
     [ ; extern <SuperClass> ]

#xcommand DATA <DataName1> [,<DataNameN>] => ;
   oClass:AddData( <(DataName1)> ) [; oClass:AddData( <(DataNameN)> ) ]

#xcommand CLASSDATA <DataName1> [,<DataNameN>] => ;
   oClass:AddClassData( <(DataName1)> ) [; oClass:AddClassData( <(DataNameN)> ) ]

#xcommand METHOD <MethodName>( [<params,...>] ) [ CONSTRUCTOR ] => ;
   oClass:AddMethod( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>() )

#xcommand METHOD <MethodName>( [<params,...>] ) INLINE <Code,...> => ;
   oClass:AddInline( <(MethodName)>, {|Self [,<params>] | <Code> } )

#xcommand METHOD <MethodName>( [<params,...>] ) VIRTUAL => ;
   oClass:AddVirtual( <(MethodName)> )

#xcommand METHOD <MethodName>( [<params,...>] ) SETGET => ;
   oClass:AddMethod( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>() ) ;;
   oClass:AddMethod( "_" + <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>() )

#xcommand MESSAGE <MessageName> METHOD <MethodName>( [<params,...>] ) => ;
   oClass:AddMethod( <(MessageName)>, CLSMETH _CLASS_NAME_ <MethodName>() )

#xcommand MESSAGE <MessageName>() METHOD <MethodName>( [<params,...>] ) => ;
   oClass:AddMethod( <(MessageName)>, CLSMETH _CLASS_NAME_ <MethodName>() )

#xcommand ENDCLASS => oClass:Create() ;;
                      endif ;;
                      return oClass:Instance()

#xcommand METHOD <MethodName>( [<params,...>] ) CLASS <ClassName> => ;
          static function <ClassName>_<MethodName>( [<params>] ) ;;
          local Self := QSelf()

#endif /* _CLASSES_CH */
