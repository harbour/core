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
 * Copyright 2000 JF Lefebvre <jfl@mafact.com> and RA Cuylen <rac@mafact.com>
 *    Support for Class(y), TopClass and Visual Object compatibility
 *    Support for MI (multiple inheritance),
 *    Scoping (Protect, Hidden and Readonly),
 *    Delegating, DATA Shared
 *    Support of 10 Chars limits
 *
 * Copyright 2000 Brian Hays <bhays@abacuslaw.com>
 *    Documentation for the commands
 *
 * See doc/license.txt for licensing terms.
 *
 */

#ifndef HB_CLASS_CH_
#define HB_CLASS_CH_

#include "hbsetup.ch"
#include "hboo.ch"

#xtranslate HBCLSCHOICE( <export>, <protect>, <hidde> ) => iif( <export>, HB_OO_CLSTP_EXPORTED , iif( <protect>, HB_OO_CLSTP_PROTECTED, iif( <hidde>, HB_OO_CLSTP_HIDDEN, nScope) ) )

#xtranslate CREATE CLASS => CLASS

#ifndef HB_SHORTNAMES

#xcommand CLASS <ClassName> [ <frm: FROM, INHERIT> <SuperClass1> [,<SuperClassN>] ] [<static: STATIC>] => ;
   <static> function <ClassName>() ;;
      static s_oClass ;;
      local nScope := HB_OO_CLSTP_EXPORTED ;;
      if s_oClass == NIL ;;
         s_oClass := TClass():New( <(ClassName)>, __CLS_PARAM([ <(SuperClass1)> ] [ ,<(SuperClassN)> ] ) ) ;;
     #undef _CLASS_NAME_ ;;
     #define _CLASS_NAME_ <ClassName> ;;
     #translate CLSMETH <ClassName> <MethodName>() => @<ClassName>_<MethodName>() ;
     [ ; #translate Super( <SuperClassN> ) : => ::<SuperClassN>: ] ;
     [ ; #translate Super( <SuperClass1> ) : => ::<SuperClass1>: ] ;
     [ ; #translate Super() : => ::<SuperClass1>: ] ;
     [ ; #translate Super : => ::<SuperClass1>: ] ;
     [ ; REQUEST <SuperClass1> ] [ ,<SuperClassN> ]

#else

#xcommand CLASS <ClassName> [ <frm: FROM, INHERIT> <SuperClass1> [,<SuperClassN>] ] [<static: STATIC>] => ;
   <static> function <ClassName>() ;;
      static s_oClass ;;
      local nScope := HB_OO_CLSTP_EXPORTED ;;
      if s_oClass == NIL ;;
         s_oClass := TClass():New( <(ClassName)>, __CLS_PARAM([ <(SuperClass1)> ] [ ,<(SuperClassN)> ] ) ) ;;
     #undef _CLASS_NAME_ ;;
     #define _CLASS_NAME_ <ClassName> ;;
     #translate CLSMETH <ClassName> <MethodName>() => @<MethodName>() ;
     [ ; #translate Super( <SuperClassN> ) : => ::<SuperClassN>: ] ;
     [ ; #translate Super( <SuperClass1> ) : => ::<SuperClass1>: ] ;
     [ ; #translate Super() : => ::<SuperClass1>: ] ;
     [ ; #translate Super : => ::<SuperClass1>: ] ;
     [ ; REQUEST <SuperClass1> ] [ ,<SuperClassN> ]

#endif /* HB_SHORTNAMES */

/* VO compatibility */
#xtranslate  ( <name>{ [<p,...>] }        =>  ( <name>():New( <p> )
#xtranslate := <name>{ [<p,...>] }        => := <name>():New( <p> )
#xtranslate  = <name>{ [<p,...>] }        =>  = <name>():New( <p> )

// This rule is generating preprocess matching errors.
// #xtranslate  , <name>{ [<p,...>] }        =>  , <name>():New( <p> )

#xtranslate    EXPORTED:       =>      nScope := HB_OO_CLSTP_EXPORTED
#xtranslate    VISIBLE:        =>      nScope := HB_OO_CLSTP_EXPORTED
#xtranslate    HIDDEN:         =>      nScope := HB_OO_CLSTP_HIDDEN
#xtranslate    PROTECTED:      =>      nScope := HB_OO_CLSTP_PROTECTED

#xcommand DATA <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<ro: READONLY, RO>] => ;
   s_oClass:AddMultiData( <(type)>, <uValue>, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), \{<(DataNames)>\} ) ;

#xcommand VAR <DataNames,...> [ TYPE <type> ] [ ASSIGN <uValue> ] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<ro: READONLY, RO>] => ;
   s_oClass:AddMultiData( <(type)>, <uValue>, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), \{<(DataNames)>\} ) ;

#xcommand VAR <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<ro: READONLY, RO>] => ;
   s_oClass:AddMultiData( <(type)>, <uValue>, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), \{<(DataNames)>\} ) ;

#xcommand VAR <DataName> IN <SuperClass> => ;
   s_oClass:AddInline( <(DataName)>, {|Self| Self:<SuperClass>:<DataName> }, HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY ) ;;
   s_oClass:AddInline( "_" + <(DataName)>, {|Self, param| Self:<SuperClass>:<DataName> := param }, HB_OO_CLSTP_EXPORTED )

#xcommand VAR <DataName> IS <SprDataName> IN <SuperClass> => ;
   s_oClass:AddInline( <(DataName)>, {|Self| Self:<SuperClass>:<SprDataName> }, HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY ) ;;
   s_oClass:AddInline( "_" + <(DataName)>, {|Self, param| Self:<SuperClass>:<SprDataName> := param }, HB_OO_CLSTP_EXPORTED )

#xcommand VAR <DataName1> IS <DataName2> => ;
   s_oClass:AddInline( <(DataName1)>, {|Self| Self:<DataName2> }, HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY ) ;;
   s_oClass:AddInline( "_" + <(DataName1)>, {|Self, param| Self:<DataName2> := param }, HB_OO_CLSTP_EXPORTED )

#xcommand VAR <DataName1> IS <DataName2> TO <oObject> => ;
   s_oClass:AddInline( <(DataName1)>, {|Self| Self:<oObject>:<DataName2> }, HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY ) ;;
   s_oClass:AddInline( "_" + <(DataName1)>, {|Self, param| Self:<oObject>:<DataName2> := param }, HB_OO_CLSTP_EXPORTED )

#xcommand EXPORT <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [<ro: READONLY, RO>] => ;
   s_oClass:AddMultiData( <(type)>, <uValue>, HB_OO_CLSTP_EXPORTED + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), \{<(DataNames)>\} ) ;

#xcommand EXPORT <DataNames,...> [ TYPE <type> ] [ ASSIGN <uValue> ] [<ro: READONLY, RO>] => ;
   s_oClass:AddMultiData( <(type)>, <uValue>, HB_OO_CLSTP_EXPORTED + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), \{<(DataNames)>\} ) ;

#xcommand PROTECT <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [<ro: READONLY, RO>] => ;
   s_oClass:AddMultiData( <(type)>, <uValue>, HB_OO_CLSTP_PROTECTED + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), \{<(DataNames)>\} ) ;

#xcommand PROTECT <DataNames,...> [ TYPE <type> ] [ ASSIGN <uValue> ] [<ro: READONLY, RO>] => ;
   s_oClass:AddMultiData( <(type)>, <uValue>, HB_OO_CLSTP_PROTECTED + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), \{<(DataNames)>\} ) ;

#xcommand HIDDE <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [<ro: READONLY, RO>] => ;
   s_oClass:AddMultiData( <(type)>, <uValue>, HB_OO_CLSTP_HIDDEN + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), \{<(DataNames)>\} ) ;

#xcommand HIDDE <DataNames,...> [ TYPE <type> ] [ ASSIGN <uValue> ] [<ro: READONLY, RO>] => ;
   s_oClass:AddMultiData( <(type)>, <uValue>, HB_OO_CLSTP_HIDDEN + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), \{<(DataNames)>\} ) ;

#xcommand CLASSDATA <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<ro: READONLY, RO>] [<share: SHARED>] => ;
   s_oClass:AddMultiClsData(<(type)>, <uValue>, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ) + HB_OO_CLSTP_SHARED, \{<(DataNames)>\} ) ;

#xtranslate CLASS VAR => CLASSVAR

#xcommand CLASSVAR <DataNames,...> [ TYPE <type> ] [ ASSIGN <uValue> ] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<ro: READONLY, RO>] [<share: SHARED>] => ;
   s_oClass:AddMultiClsData(<(type)>, <uValue>, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ) + iif( <.share.>, HB_OO_CLSTP_SHARED, 0 ), \{<(DataNames)>\} ) ;

#xcommand CLASSVAR <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<ro: READONLY, RO>] [<share: SHARED>] => ;
   s_oClass:AddMultiClsData(<(type)>, <uValue>, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ) + iif( <.share.>, HB_OO_CLSTP_SHARED, 0 ), \{<(DataNames)>\} ) ;

#xtranslate CLASS METHOD => CLASSMETHOD

#xcommand CLASSMETHOD <MethodName> [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<share: SHARED>] => ;
   s_oClass:AddClsMthds( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.share.>, HB_OO_CLSTP_SHARED, 0 ) )

#xcommand CLASSMETHOD <MethodName>() [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<share: SHARED>] => ;
   s_oClass:AddClsMthds( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.share.>, HB_OO_CLSTP_SHARED, 0 ) )

#xcommand CLASSMETHOD <MethodName>( [<params,...>] ) [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<share: SHARED>] => ;
   s_oClass:AddClsMthds( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.share.>, HB_OO_CLSTP_SHARED, 0 ) )

#xcommand CONSTRUCTOR <Name>( [<params,...>] ) => METHOD <Name>( [<params,...>] ) CONSTRUCTOR

//Oups to verify that the word new is not reserved...
//#xcommand CONSTRUCTOR New( [<params,...>] ) => METHOD New( [<params,...>] ) CONSTRUCTOR

#xcommand METHOD <MethodName> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
   s_oClass:AddMethod( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) )

#xcommand METHOD <MethodName>( [<params,...>] ) [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
   s_oClass:AddMethod( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) )

#xcommand METHOD <MethodName> BLOCK <CodeBlock> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
   s_oClass:AddInline( <(MethodName)>, <CodeBlock>, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) )

#xcommand METHOD <MethodName>( [<params,...>] ) BLOCK <CodeBlock> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
   s_oClass:AddInline( <(MethodName)>, <CodeBlock>, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) )

#xcommand METHOD <MethodName>( [<params,...>] ) EXTERN <FuncName>( [<params,...>] ) [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
   s_oClass:AddMethod( <(MethodName)>, @<FuncName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) )

#xcommand METHOD <MethodName> INLINE <Code,...> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
   s_oClass:AddInline( <(MethodName)>, {|Self | <Code> }, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) )

#xcommand METHOD <MethodName>( [<params,...>] ) INLINE <Code,...> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
   s_oClass:AddInline( <(MethodName)>, {|Self [,<params>] | <Code> }, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) )

#xcommand METHOD <MethodName> INLINE [Local <v>,] <Code,...> [<other>] => ;
          METHOD <MethodName> BLOCK {|Self [,<v>] | <Code> } [<other>]

#xcommand METHOD <MethodName>( [<params,...>] ) INLINE [Local <v>,] <Code,...> [<other>] => ;
          METHOD <MethodName> BLOCK {|Self [,<params>] [,<v>] | <Code> } [<other>]

#xcommand METHOD <MethodName> DEFERRED => ;
   s_oClass:AddVirtual( <(MethodName)> )

#xcommand METHOD <MethodName>( [<params,...>] ) DEFERRED => ;
   s_oClass:AddVirtual( <(MethodName)> )

#xcommand METHOD <MethodName>( [<params,...>] ) VIRTUAL => ;
   s_oClass:AddVirtual( <(MethodName)> )

#xcommand METHOD <MethodName>( [<params>] ) OPERATOR <op> [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
   s_oClass:AddMethod( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) ) ;;
   s_oClass:AddInline( <(op)>, {|Self [,<params>] | Self:<MethodName>( [<params>] ) }, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) )

#xcommand MESSAGE <MessageName> METHOD <MethodName> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
   s_oClass:AddMethod( <(MessageName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) )

#xcommand MESSAGE <MessageName> METHOD <MethodName>( [<params,...>] ) [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>]=> ;
   s_oClass:AddMethod( <(MessageName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) )

#xcommand MESSAGE <MessageName>( [<params,...>] ) METHOD <MethodName> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
   s_oClass:AddMethod( <(MessageName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) )

#xcommand MESSAGE <MessageName>( [<params,...>] ) METHOD <MethodName>( [<dummy,...>] ) [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
   s_oClass:AddMethod( <(MessageName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) )

#xcommand MESSAGE <MessageName> IN <SuperClass> => ;
   s_oClass:AddInline( <(MessageName)>, {|Self| Self:<SuperClass>:<MessageName>() } )

#xcommand MESSAGE <MessageName>( [<params,...>] ) IN <SuperClass> => ;
   s_oClass:AddInline( <(MessageName)>, {|Self [,<params>]| Self:<SuperClass>:<MessageName>( [<params>] ) } )

#xcommand MESSAGE <MessageName> IS <SprMethodName> IN <SuperClass> => ;
   s_oClass:AddInline( <(MessageName)>, {|Self| Self:<SuperClass>:<SprMethodName>() } )

#xcommand MESSAGE <MessageName> IS <SprMethodName>( [<params,...>] ) IN <SuperClass> => ;
   s_oClass:AddInline( <(MessageName)>, {|Self [,<params>]| Self:<SuperClass>:<SprMethodName>( [<params>] ) } )

#xcommand MESSAGE <MessageName>( [<params,...>] ) IS <SprMethodName> IN <SuperClass> => ;
   s_oClass:AddInline( <(MessageName)>, {|Self [,<params>]| Self:<SuperClass>:<SprMethodName>( [<params>] ) } )

#xcommand MESSAGE <MessageName>( [<params,...>] ) IS <SprMethodName>( [<dummy,...>] ) IN <SuperClass> => ;
   s_oClass:AddInline( <(MessageName)>, {|Self [,<params>]| Self:<SuperClass>:<SprMethodName>( [<params>] ) } )

#xcommand MESSAGE <MessageName> IS <MethodName> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
   s_oClass:AddMethod( <(MessageName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) )

#xcommand MESSAGE <MessageName> IS <MethodName>( [<params,...>] ) [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>]=> ;
   s_oClass:AddMethod( <(MessageName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) )

#xcommand MESSAGE <MessageName>( [<params,...>] ) IS <MethodName> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
   s_oClass:AddMethod( <(MessageName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) )

#xcommand MESSAGE <MessageName>( [<params,...>] ) IS <MethodName>( [<dummy,...>] ) [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
   s_oClass:AddMethod( <(MessageName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) )

#xcommand MESSAGE <MessageName> TO <oObject> =>;
   s_oClass:AddInline( <(MessageName)>, {|Self| Self:<oObject>:<MessageName> } )

#xcommand MESSAGE <MessageName>( [<params,...>] ) TO <oObject> =>;
   s_oClass:AddInline( <(MessageName)>, {|Self [,<params>]| Self:<oObject>:<MessageName>( [<params>] ) } )

#xcommand DELEGATE <MessageName> TO <oObject> =>;
   s_oClass:AddInline( <(MessageName)>, {|Self| Self:<oObject>:<MessageName> } )

#xcommand DELEGATE <MessageName>( [<params,...>] ) TO <oObject> =>;
   s_oClass:AddInline( <(MessageName)>, {|Self [,<params>]| Self:<oObject>:<MessageName>( [<params>] ) } )

#xcommand METHOD <MethodName>( [<params,...>] ) SETGET => ;
   s_oClass:AddMethod( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY ) ;;
   s_oClass:AddMethod( "_" + <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>() )

#xcommand ACCESS <AccessName> => ;
   s_oClass:AddMethod( <(AccessName)>, CLSMETH _CLASS_NAME_ <AccessName>(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY )

#xcommand ACCESS <AccessName> INLINE [Local <v>,] <code,...> => ;
   s_oClass:AddInline( <(AccessName)>, {|Self [,<v>] | <code> }, HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY )

#xcommand ACCESS <AccessName> DEFERRED => ;
   s_oClass:AddVirtual( <(AccessName)> )

#xcommand ASSIGN <AssignName>( [<params,...>] ) => ;
   s_oClass:AddMethod( "_" + <(AssignName)>, CLSMETH _CLASS_NAME_ _<AssignName>(), HB_OO_CLSTP_EXPORTED )

#xcommand ASSIGN <AssignName>( [<params,...>] ) INLINE [Local <v>,] <Code,...> => ;
   s_oClass:AddInline( "_" + <(AssignName)>, {|Self [,<params>] [,<v>] | <Code> }, HB_OO_CLSTP_EXPORTED )

#xcommand ERROR HANDLER <MethodName>( [<params,...>] ) => ;
   s_oClass:SetOnError( CLSMETH _CLASS_NAME_ <MethodName>() )

#xcommand ON ERROR <MethodName>( [<params,...>] ) => ;
   s_oClass:SetOnError( CLSMETH _CLASS_NAME_ <MethodName>() )

#xtranslate END CLASS => ENDCLASS

#xcommand ENDCLASS => ;; // Here we will add a inline message to ::Class. RaC&JfL
                      s_oClass:Create() ;;
                      endif ;;
                      return s_oClass:Instance()

#xtranslate :Super( <SuperClass> ) : => :<SuperClass>:
#xtranslate :Super() : => :Super:
#xtranslate :Super() => :Super

#ifndef HB_SHORTNAMES

#xcommand METHOD <MethodName>( [<params,...>] ) CLASS <ClassName> => ;
          static function <ClassName>_<MethodName>( [<params>] ) ;;
          local Self := QSelf()

#xcommand ACCESS <AccessName>() CLASS <ClassName> => ;
          static function <ClassName>_<AccessName>() ;;
          local Self := QSelf()

#xcommand ASSIGN <AssignName>( [<params,...>] ) CLASS <ClassName> => ;
          static function <ClassName>__<AssignName>( [<params>] ) ;;
          local Self := QSelf()
#else

#xcommand METHOD <MethodName>( [<params,...>] ) CLASS <ClassName> => ;
          static function <MethodName>( [<params>] ) ;;
          local Self := QSelf()

#xcommand ACCESS <AccessName>() CLASS <ClassName> => ;
          static function <AccessName>() ;;
          local Self := QSelf()

#xcommand ASSIGN <AssignName>( [<params,...>] ) CLASS <ClassName> => ;
          static function _<AssignName>( [<params>] ) ;;
          local Self := QSelf()

#endif /* HB_SHORTNAMES */

#endif /* HB_CLASS_CH_ */