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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2000 ( ->07/2000 ) JF. Lefebvre <jfl@mafact.com> & RA. Cuylen <rac@mafact.com>
 *    Support for Class(y), TopClass and Visual Object compatibility
 *    Support for MI (multiple inheritance),

 * Copyright 2000-2001 ( 08/2000-> ) JF. Lefebvre <jfl@mafact.com>
 *    Scoping (Protect, Hidden and Readonly),
 *    Delegating, DATA Shared
 *    Support of 10 Chars limits
 *
 * See doc/license.txt for licensing terms.
 *
 */

#ifndef HB_CLASS_CH_
#define HB_CLASS_CH_

#include "hbsetup.ch"
#include "hboo.ch"

/* You can actually define one or all the syntax, they do not collide each other */
/* There is some difference with their original form and I hope I will have enough */
/* time to document it <g> */
/* This is work in progress ... */
/* FWOBJECT AND CLASSY compatibility are the base of this work */
/* VO is just here as I like it's way of */
/* instanciating object but there is only a very few VO keywords here :-( */
/* TOPCLASS is better implemented because I like the way some Classy command */
/* are simplified */
/* There is also a big common block extending in fact each of the four base syntax */
/* it seem actually impossible to completely separate it without creating */
/* four differents include file (what I would not see in fact ) */

/* There is also two compatibility define you can use */
/* HB_CLS_NOTOBJECT wich IF DEFINED, disable the auto inherit of tobject */
/* (wich in fact also disable the classy compatibility :new(...) => :Init(...)  */
/* HB_CLS_NOAUTOINIT wich disable the (VO like) AutoInit for Logical and Numeric */
/* when not specifically initiated */
/* These two are disabled by default */
/* So Each class _inherit_ of tObject by default and */
/*    Each type logical or numerical is initiated to .F. and 0 by default */

/* #define HB_CLS_NOTOBJECT  */ /* Should be included in some compatibility include files as needed */
/* #define HB_CLS_NOAUTOINIT */ /* Idem */
/* #define HB_CLS_ALLOWCLASS */ /* Work in progress, don't define it now */
/* #define HB_CLS_ENFORCERO FLAG to disable Write access to RO VAR outside */
/*         of Constructors /!\ Could be related to some incompatibility */

DECLARE TClass ;
        New( cName AS String, OPTIONAL SuperParams ) AS CLASS TClass ;
        Create() AS Object ;
        Instance() AS Object ;
        AddClsMthds( cName AS String, @MethodName(), nScope AS Numeric, n2 AS Numeric, n3 AS Numeric ) ;
        AddMultiClsData( cType AS String, uVal, nScope AS Numeric, aDatas AS Array OF String ) ;
        AddMultiData( cType AS String, uVal, nScope AS Numeric, aDatas AS Array OF String, x AS LOGICAL, lPer AS LOGICAL ) ;
        AddMethod( cName AS String, @MethodName(), nScope AS Numeric, lPersistent AS LOGICAL ) ;
        AddInLine( cName AS String, bBlock AS CodeBlock, nScope AS Numeric, lPersistent AS LOGICAL ) ;
        AddVirtual( cName AS String )

#xtranslate __ERR([<msg,...>]) => #error [<msg>]
#xtranslate )() => )

#ifdef HB_CLS_NOTOBJECT
 #define __HB_CLS_PAR  __CLS_PAR00
#else
 #define __HB_CLS_PAR  __CLS_PARAM
#endif

#ifdef HB_CLS_NOAUTOINIT
 #define __HB_CLS_NOINI .T.
#else
 #define __HB_CLS_NOINI .F.
#endif

#ifndef HB_CLS_FWO
#ifndef HB_CLS_CSY
#ifndef HB_CLS_VO
#ifndef HB_CLS_TOP

 /* IF NOTHING DECIDED BY THE PROGRAMER USE ALL */
#define HB_CLS_FWO
#define HB_CLS_CSY
#define HB_CLS_VO
#define HB_CLS_TOP

#endif
#endif
#endif
#endif

#xtranslate HBCLSCHOICE( <export>, <protect>, <hidde> ) => iif( <export>, HB_OO_CLSTP_EXPORTED , iif( <protect>, HB_OO_CLSTP_PROTECTED, iif( <hidde>, HB_OO_CLSTP_HIDDEN, nScope) ) )

/* CLASSY SYNTAX */
#IFDEF HB_CLS_CSY
#xtranslate CREATE CLASS => CLASS
#endif

#ifndef HB_SHORTNAMES

#xtranslate DECLMETH <ClassName> <MethodName> => <ClassName>_<MethodName>

#xcommand CLASS <ClassName> [METACLASS <metaClass>] [ <frm: FROM, INHERIT> <SuperClass1> [,<SuperClassN>] ] [<static: STATIC>] => ;
   _HB_CLASS <ClassName> ;;
   <static> function <ClassName>() ;;
      static s_oClass ;;
      local MetaClass,nScope := HB_OO_CLSTP_EXPORTED ;;
      if s_oClass == NIL ;;
         s_oClass := IIF(<.metaClass.>, <(metaClass)> ,TClass():new( <(ClassName)> , __HB_CLS_PAR ( [ <(SuperClass1)> ] [ ,<(SuperClassN)> ] ) ) ) ;;
     #undef  _CLASS_NAME_ ;;
     #define _CLASS_NAME_ <ClassName> ;;
     #undef  _CLASS_MODE_ ;;
     #define _CLASS_MODE_ _CLASS_DECLARATION_ ;;
     #xtranslate CLSMETH <ClassName> <MethodName> => @<ClassName>_<MethodName> ;;
     #xtranslate DECLCLASS <ClassName> => ;;
     [ ; #translate Super( <SuperClassN> ) : => ::<SuperClassN>: ] ;
     [ ; #translate Super( <SuperClass1> ) : => ::<SuperClass1>: ] ;
     [ ; #translate Super() : => ::<SuperClass1>: ] ;
     [ ; #translate Super : => ::<SuperClass1>: ] ;
     [ ; #translate ::Super : => ::<SuperClass1>: ] ;
     [ ; REQUEST <SuperClass1> ] [ ,<SuperClassN> ]

#else

#xcommand CLASS <ClassName> [METACLASS <metaClass>] [ <frm: FROM, INHERIT> <SuperClass1> [,<SuperClassN>] ] [<static: STATIC>] => ;
   _HB_CLASS <ClassName> ;;
   <static> function <ClassName>() ;;
      static s_oClass  ;;
      local MetaClass,nScope := HB_OO_CLSTP_EXPORTED ;;
      if s_oClass == NIL ;;
         s_oClass := IIF(<.metaClass.>,<(metaClass)>,TClass():new(<(ClassName)>, __HB_CLS_PAR ( [ <(SuperClass1)> ] [ ,<(SuperClassN)> ] ) ) ) ;;
     #undef  _CLASS_NAME_ ;;
     #define _CLASS_NAME_ <ClassName> ;;
     #undef  _CLASS_MODE_ ;;
     #define _CLASS_MODE_ _CLASS_DECLARATION_ ;;
     #translate CLSMETH <ClassName> <MethodName>() => @<MethodName> ;
     [ ; #translate Super( <SuperClassN> ) : => ::<SuperClassN>: ] ;
     [ ; #translate Super( <SuperClass1> ) : => ::<SuperClass1>: ] ;
     [ ; #translate Super() : => ::<SuperClass1>: ] ;
     [ ; #translate Super : => ::<SuperClass1>: ] ;
     [ ; #translate ::Super : => ::<SuperClass1>: ] ;
     [ ; REQUEST <SuperClass1> ] [ ,<SuperClassN> ]

#endif /* HB_SHORTNAMES */

/* Disable the message :Class */
#ifndef HB_CLS_ALLOWCLASS
/* CLASSY SYNTAX */
#IFDEF HB_CLS_CSY
#xtranslate  :CLASS  =>
#xtranslate  :CLASS: => :
#endif
#endif

/* CLASSY SYNTAX */
#IFDEF HB_CLS_CSY

#xcommand VAR <DataNames,...> [ TYPE <type> ] [ ASSIGN <uValue> ] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   s_oClass:AddMultiData( <(type)>, <uValue>, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), {<(DataNames)>}, __HB_CLS_NOINI, <.persistent.> ) ;

#xcommand VAR <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   s_oClass:AddMultiData( <(type)>, <uValue>, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), {<(DataNames)>}, __HB_CLS_NOINI, <.persistent.> ) ;

#xcommand VAR <DataName> [ AS <type> ] IN <SuperClass> => ;
   _HB_MEMBER {[AS <type>] <DataName>} ;;
   s_oClass:AddInline( <(DataName)>, {|Self| Self:<SuperClass>:<DataName> }, HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY ) ;;
   s_oClass:AddInline( "_" + <(DataName)>, {|Self, param| Self:<SuperClass>:<DataName> := param }, HB_OO_CLSTP_EXPORTED )

#xcommand VAR <DataName> [ AS <type> ] IS <SprDataName> IN <SuperClass> => ;
   _HB_MEMBER {[AS <type>] <DataName>} ;;
   s_oClass:AddInline( <(DataName)>, {|Self| Self:<SuperClass>:<SprDataName> }, HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY ) ;;
   s_oClass:AddInline( "_" + <(DataName)>, {|Self, param| Self:<SuperClass>:<SprDataName> := param }, HB_OO_CLSTP_EXPORTED )

#xcommand VAR <DataName1> [ AS <type> ] IS <DataName2> => ;
   _HB_MEMBER {[AS <type>] <DataName1>} ;;
   s_oClass:AddInline( <(DataName1)>, {|Self| Self:<DataName2> }, HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY ) ;;
   s_oClass:AddInline( "_" + <(DataName1)>, {|Self, param| Self:<DataName2> := param }, HB_OO_CLSTP_EXPORTED )

#xcommand VAR <DataName1> IS <DataName2> TO <oObject> => ;
   s_oClass:AddInline( <(DataName1)>, {|Self| Self:<oObject>:<DataName2> }, HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY ) ;;
   s_oClass:AddInline( "_" + <(DataName1)>, {|Self, param| Self:<oObject>:<DataName2> := param }, HB_OO_CLSTP_EXPORTED )

#xtranslate    EXPORTED:       =>      nScope := HB_OO_CLSTP_EXPORTED
#xtranslate    EXPORT:         =>      nScope := HB_OO_CLSTP_EXPORTED
#xtranslate    VISIBLE:        =>      nScope := HB_OO_CLSTP_EXPORTED
#xtranslate    HIDDEN:         =>      nScope := HB_OO_CLSTP_HIDDEN
#xtranslate    PROTECTED:      =>      nScope := HB_OO_CLSTP_PROTECTED

#xtranslate CLASS VAR => CLASSVAR
#xtranslate CLASS METHOD => CLASSMETHOD

#xcommand METHOD <MethodName> [ AS <type> ] DEFERRED => ;
   _HB_MEMBER <MethodName>() [ AS <type> ];;
   s_oClass:AddVirtual( <(MethodName)> )

#endif


/* VO SYNTAX */
#ifdef HB_CLS_VO

#xtranslate  ( <!name!>{ [<p,...>] }        =>  ( <name>():New( <p> )
#xtranslate  = <!name!>{ [<p,...>] }        =>  = <name>():New( <p> )
#xtranslate  , <!name!>{ [<p,...>] }        =>  , <name>():New( <p> )

#xcommand EXPORT <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   s_oClass:AddMultiData( <(type)>, <uValue>, HB_OO_CLSTP_EXPORTED + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), {<(DataNames)>}, __HB_CLS_NOINI, <.persistent.> ) ;

#xcommand EXPORT <DataNames,...> [ TYPE <type> ] [ ASSIGN <uValue> ] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   s_oClass:AddMultiData( <(type)>, <uValue>, HB_OO_CLSTP_EXPORTED + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), {<(DataNames)>}, __HB_CLS_NOINI, <.persistent.> ) ;

#xcommand PROTECT <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   s_oClass:AddMultiData( <(type)>, <uValue>, HB_OO_CLSTP_PROTECTED + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), {<(DataNames)>}, __HB_CLS_NOINI, <.persistent.> ) ;

#xcommand PROTECT <DataNames,...> [ TYPE <type> ] [ ASSIGN <uValue> ] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   s_oClass:AddMultiData( <(type)>, <uValue>, HB_OO_CLSTP_PROTECTED + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), {<(DataNames)>}, __HB_CLS_NOINI, <.persistent.> ) ;

#xcommand HIDDE <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   s_oClass:AddMultiData( <(type)>, <uValue>, HB_OO_CLSTP_HIDDEN + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), {<(DataNames)>}, __HB_CLS_NOINI, <.persistent.> ) ;

#xcommand HIDDE <DataNames,...> [ TYPE <type> ] [ ASSIGN <uValue> ] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   s_oClass:AddMultiData( <(type)>, <uValue>, HB_OO_CLSTP_HIDDEN + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), {<(DataNames)>}, __HB_CLS_NOINI, <.persistent.> ) ;

#ENDIF


#xcommand CLASSVAR <DataNames,...> [ TYPE <type> ] [ ASSIGN <uValue> ] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<ro: READONLY, RO>] [<share: SHARED>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   s_oClass:AddMultiClsData(<(type)>, <uValue>, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ) + iif( <.share.>, HB_OO_CLSTP_SHARED, 0 ), {<(DataNames)>}, __HB_CLS_NOINI ) ;

#xcommand CLASSVAR <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<ro: READONLY, RO>] [<share: SHARED>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   s_oClass:AddMultiClsData(<(type)>, <uValue>, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ) + iif( <.share.>, HB_OO_CLSTP_SHARED, 0 ), {<(DataNames)>}, __HB_CLS_NOINI ) ;


/* FWOBJECT SYNTAX */
#ifdef HB_CLS_FWO

#xcommand DATA <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   s_oClass:AddMultiData( <(type)>, <uValue>, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), {<(DataNames)>}, __HB_CLS_NOINI, <.persistent.> ) ;


#xcommand CLASSDATA <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<ro: READONLY, RO>] [<share: SHARED>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   s_oClass:AddMultiClsData(<(type)>, <uValue>, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ) + HB_OO_CLSTP_SHARED, {<(DataNames)>}, __HB_CLS_NOINI ) ;

#endif

#xcommand CLASSMETHOD <MethodName> [ AS <type> ] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<share: SHARED>] => ;
   _HB_MEMBER <MethodName>() [ AS <type> ];;
   s_oClass:AddClsMthds( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.share.>, HB_OO_CLSTP_SHARED, 0 ) )

#xcommand CONSTRUCTOR <Name> => METHOD <Name> CONSTRUCTOR

#ifdef STRICT_OO
  #xcommand METHOD <MethodName> [ <ctor: CONSTRUCTOR> ] [ AS <type> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [_CLASS_DECLARATION_] ;
    [<persistent: PERSISTENT, PROPERTY>] => ;
    _HB_MEMBER <MethodName>() [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
    #xcommand METHOD <MethodName> [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>;;
    s_oClass:AddMethod( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ),;
                        <.persistent.> )
#else
  #xcommand METHOD <MethodName> [ <ctor: CONSTRUCTOR> ] [ AS <type> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [_CLASS_DECLARATION_] ;
    [<persistent: PERSISTENT, PROPERTY>] => ;
    _HB_MEMBER <MethodName>() [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
    #xcommand METHOD <MethodName> [([<anyParams,...>])] [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>([<anyParams>]);;
    s_oClass:AddMethod( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ),;
    <.persistent.> )

  #xcommand METHOD <MethodName>([<params,...>]) [ <ctor: CONSTRUCTOR> ] [ AS <type> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [_CLASS_DECLARATION_] ;
    [<persistent: PERSISTENT, PROPERTY>] => ;
    _HB_MEMBER <MethodName>([<params>]) [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
    #xcommand METHOD <MethodName> [([<anyParams,...>])] [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>([<anyParams>]);;
    s_oClass:AddMethod( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ),;
                        <.persistent.> )
#endif

#xcommand METHOD <MethodName> [ AS <type> ] BLOCK <CodeBlock> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER <MethodName>() [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
   s_oClass:AddInline( <(MethodName)>, <CodeBlock>, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ),;
                       <.persistent.> )

#xcommand METHOD <MethodName> [ AS <type> ] EXTERN <FuncName> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER <MethodName>() [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
   s_oClass:AddMethod( <(MethodName)>, @<FuncName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ),;
   <.persistent.> )

#xcommand METHOD <MethodName> [ AS <type> ] INLINE <Code,...> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER <MethodName>() [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
   s_oClass:AddInline( <(MethodName)>, {|Self | <Code> }, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ),;
                       <.persistent.> )

/* Must have secondary version with params because params are used in the block */
#xcommand METHOD <MethodName>( [<params,...>] ) [ AS <type> ] INLINE <Code,...> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER <MethodName>([<params>]) [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
   s_oClass:AddInline( <(MethodName)>, {|Self [,<params>] | <Code> }, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ),;
                       <.persistent.> )

#xcommand METHOD <MethodName> [ AS <type> ] INLINE [Local <v>,] <Code,...> [<other>] => ;
          METHOD <MethodName> [ AS <type> ] BLOCK {|Self [,<v>] | <Code> } [<other>]

/* Must have secondary version with params becuase params are used in the block */
#xcommand METHOD <MethodName>( [<params,...>] ) [ AS <type> ] INLINE [Local <v>,] <Code,...> [<other>] => ;
          METHOD <MethodName> [ AS <type> ] BLOCK {|Self [,<params>] [,<v>] | <Code> } [<other>]


#xcommand METHOD <MethodName> [ AS <type> ] VIRTUAL => ;
   _HB_MEMBER <MethodName>() [ AS <type> ];;
   s_oClass:AddVirtual( <(MethodName)> )

#ifdef STRICT_OO
  #xcommand METHOD <MethodName> [ AS <type> ] OPERATOR <op> [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
    _HB_MEMBER <MethodName>()  [ AS <type> ];;
     #xcommand METHOD <MethodName> [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>;;
    s_oClass:AddMethod( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) ) ;;
    s_oClass:AddInline( <(op)>, {|Self [,<params>] | Self:<MethodName>( [<params>] ) }, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) )
#else
  #xcommand METHOD <MethodName> [ AS <type> ] OPERATOR <op> [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
    _HB_MEMBER <MethodName>()  [ AS <type> ];;
    #xcommand METHOD <MethodName> [([<anyParams,...>])] [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>([<anyParams>]);;
    s_oClass:AddMethod( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) ) ;;
    s_oClass:AddInline( <(op)>, {|Self [,<params>] | Self:<MethodName>( [<params>] ) }, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) )

  #xcommand METHOD <MethodName>( [<params,...>] ) [ AS <type> ] OPERATOR <op> [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
    _HB_MEMBER <MethodName>([<params>])  [ AS <type> ];;
    #xcommand METHOD <MethodName> [([<anyParams,...>])] [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>([<anyParams>]);;
    s_oClass:AddMethod( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) ) ;;
    s_oClass:AddInline( <(op)>, {|Self [,<params>] | Self:<MethodName>( [<params>] ) }, HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) )
#endif

#ifdef STRICT_OO
  #xcommand MESSAGE <MessageName> [ AS <type> ] METHOD <MethodName> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
     _HB_MEMBER <MessageName>() [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
     #xcommand METHOD <MethodName> [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>;;
     s_oClass:AddMethod( <(MessageName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) )
#else
  #xcommand MESSAGE <MessageName> [ AS <type> ] METHOD <MethodName> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
     _HB_MEMBER <MessageName>() [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
    #xcommand METHOD <MethodName> [([<anyParams,...>])] [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>([<anyParams>]);;
     s_oClass:AddMethod( <(MessageName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) )

  #xcommand MESSAGE <MessageName>([<MsgParams,...>]) [ AS <type> ] METHOD <MethodName> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
     _HB_MEMBER <MessageName>([<MsgParams>]) [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
    #xcommand METHOD <MethodName> [([<anyParams,...>])] [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>([<anyParams>]);;
     s_oClass:AddMethod( <(MessageName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) )

  #xcommand MESSAGE <MessageName> [ AS <type> ] METHOD <MethodName>([<MtdParams,...>]) [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
     _HB_MEMBER <MessageName>([<MtdParams>]) [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
    #xcommand METHOD <MethodName> [([<anyParams,...>])] [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>([<anyParams>]);;
     s_oClass:AddMethod( <(MessageName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) )

  #xcommand MESSAGE <MessageName>([<MsgParams,...>]) [ AS <type> ] METHOD <MethodName>([<MtdParams,...>]) [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] => ;
     _HB_MEMBER <MessageName>([<MtdParams>]) [<-MsgParams->] [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
    #xcommand METHOD <MethodName> [([<anyParams,...>])] [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>([<anyParams>]);;
     s_oClass:AddMethod( <(MessageName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HBCLSCHOICE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) )
#endif

#xcommand MESSAGE <MessageName> [ AS <type> ] IN <SuperClass> => ;
   _HB_MEMBER <MessageName>() [ AS <type> ];;
   s_oClass:AddInline( <(MessageName)>, {|Self| Self:<SuperClass>:<MessageName>() } )

/* Must have secondary version with params becuase params are used in the block */
#xcommand MESSAGE <MessageName>( [<params,...>] ) [ AS <type> ] IN <SuperClass> => ;
   _HB_MEMBER <MessageName>([<params>]) [ AS <type> ];;
   s_oClass:AddInline( <(MessageName)>, {|Self [,<params>]| Self:<SuperClass>:<MessageName>( [<params>] ) } )

#xcommand MESSAGE <MessageName> [ AS <type> ] IS <SprMethodName> IN <SuperClass> => ;
   _HB_MEMBER <MessageName>() [ AS <type> ];;
   s_oClass:AddInline( <(MessageName)>, {|Self| Self:<SuperClass>:<SprMethodName>() } )

/* Must have secondary version with params becuase params are used in the block */
#xcommand MESSAGE <MessageName> [ AS <type> ] IS <SprMethodName>( [<params,...>] ) IN <SuperClass> => ;
   _HB_MEMBER <MessageName>() [ AS <type> ];;
   s_oClass:AddInline( <(MessageName)>, {|Self [,<params>]| Self:<SuperClass>:<SprMethodName>( [<params>] ) } )

/* Must have secondary version with params becuase params are used in the block */
#xcommand MESSAGE <MessageName>( [<params,...>] ) [ AS <type> ] IS <SprMethodName> IN <SuperClass> => ;
   _HB_MEMBER <MessageName>([<params>]) [ AS <type> ];;
   s_oClass:AddInline( <(MessageName)>, {|Self [,<params>]| Self:<SuperClass>:<SprMethodName>( [<params>] ) } )

/* Must have secondary version with params becuase params are used in the block */
#xcommand MESSAGE <MessageName>( [<params,...>] ) [ AS <type> ] IS <SprMethodName>( [<dummy,...>] ) IN <SuperClass> => ;
   _HB_MEMBER <MessageName>([<params>]) [ AS <type> ];;
   s_oClass:AddInline( <(MessageName)>, {|Self [,<params>]| Self:<SuperClass>:<SprMethodName>( [<params>] ) } )

#xcommand MESSAGE <MessageName> [ AS <type> ] IS <MethodName> [<more,...>] => MESSAGE <MessageName> [ AS <type> ] METHOD <MethodName> [<more>]

#xcommand MESSAGE <MessageName> [ AS <type> ] TO <oObject> =>;
   _HB_MEMBER <MessageName>() [ AS <type> ];;
   s_oClass:AddInline( <(MessageName)>, {|Self| Self:<oObject>:<MessageName> } )

/* Must have secondary version with params becuase params are used in the block */
#xcommand MESSAGE <MessageName>( [<params,...>] ) [ AS <type> ] TO <oObject> =>;
   _HB_MEMBER <MessageName>([<params>]) [ AS <type> ];;
   s_oClass:AddInline( <(MessageName)>, {|Self [,<params>]| Self:<oObject>:<MessageName>( [<params>] ) } )

#xcommand DELEGATE <MessageName> [ AS <type> ] TO <oObject> =>;
   _HB_MEMBER <MessageName>() [ AS <type> ];;
   s_oClass:AddInline( <(MessageName)>, {|Self| Self:<oObject>:<MessageName> } )

/* Must have secondary version with params becuase params are used in the block */
#xcommand DELEGATE <MessageName>( [<params,...>] ) [ AS <type> ] TO <oObject> =>;
   _HB_MEMBER <MessageName>([<params>]) [ AS <type> ];;
   s_oClass:AddInline( <(MessageName)>, {|Self [,<params>]| Self:<oObject>:<MessageName>( [<params>] ) } )

#ifdef STRICT_OO
  #xcommand METHOD <MethodName> [ AS <type> ] SETGET => ;
    _HB_MEMBER <MethodName>() [ AS <type> ];;
    _HB_MEMBER _<MethodName>() [ AS <type> ];;
    #xcommand METHOD <MethodName> [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>;;
    s_oClass:AddMethod( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY ) ;;
    s_oClass:AddMethod( "_" + <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>() )
#else
  #xcommand METHOD <MethodName> [ AS <type> ] SETGET => ;
    _HB_MEMBER <MethodName>() [ AS <type> ];;
    _HB_MEMBER _<MethodName>() [ AS <type> ];;
    #xcommand METHOD <MethodName> [([<anyParams,...>])] [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>([<anyParams>]);;
    s_oClass:AddMethod( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY ) ;;
    s_oClass:AddMethod( "_" + <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>() )

  #xcommand METHOD <MethodName>([<params,...>]) [ AS <type> ] SETGET => ;
    _HB_MEMBER <MethodName>([<params>]) [ AS <type> ];;
    _HB_MEMBER _<MethodName>([<params>]) [ AS <type> ];;
    #xcommand METHOD <MethodName> [([<anyParams,...>])] [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>([<anyParams>]);;
    s_oClass:AddMethod( <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY ) ;;
    s_oClass:AddMethod( "_" + <(MethodName)>, CLSMETH _CLASS_NAME_ <MethodName>() )
#endif

#ifdef STRICT_OO
  #xcommand ACCESS <AccessName> [ AS <type> ] => ;
    _HB_MEMBER <AccessName>() [ AS <type> ];;
    #xcommand METHOD <AccessName> [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <AccessName>;;
    s_oClass:AddMethod( <(AccessName)>, CLSMETH _CLASS_NAME_ <AccessName>(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY )
#else
  #xcommand ACCESS <AccessName> [ AS <type> ] => ;
    _HB_MEMBER <AccessName>() [ AS <type> ];;
    #xcommand METHOD <AccessName> [([<anyParams,...>])] [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <AccessName>([<anyParams>]);;
    s_oClass:AddMethod( <(AccessName)>, CLSMETH _CLASS_NAME_ <AccessName>(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY )

  #xcommand ACCESS <AccessName>([<params,...>]) [ AS <type> ] => ;
    _HB_MEMBER <AccessName>([<params>]) [ AS <type> ];;
    #xcommand METHOD <AccessName> [([<anyParams,...>])] [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <AccessName>([<anyParams>]);;
    s_oClass:AddMethod( <(AccessName)>, CLSMETH _CLASS_NAME_ <AccessName>(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY )
#endif

#xcommand ACCESS <AccessName> [ AS <type> ] INLINE [Local <v>,] <code,...> => ;
   _HB_MEMBER <AccessName>() [ AS <type> ];;
   s_oClass:AddInline( <(AccessName)>, {|Self [,<v>] | <code> }, HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY )

#xcommand ACCESS <AccessName> [ AS <type> ] DEFERRED => ;
   _HB_MEMBER <AccessName>() [ AS <type> ];;
   s_oClass:AddVirtual( <(AccessName)> )

#ifdef STRICT_OO
  #xcommand ASSIGN <AssignName> [ AS <type> ] => ;
    _HB_MEMBER _<AssignName>() [ AS <type> ];;
    #xcommand METHOD <AssignName> [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <AssignName>);;
    s_oClass:AddMethod( "_" + <(AssignName)>, CLSMETH _CLASS_NAME_ _<AssignName>(), HB_OO_CLSTP_EXPORTED )
#else
  #xcommand ASSIGN <AssignName> [ AS <type> ] => ;
    _HB_MEMBER _<AssignName>() [ AS <type> ];;
    #xcommand METHOD <AssignName> [([<anyParams,...>])] [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <AssignName>([<anyParams>]);;
    s_oClass:AddMethod( "_" + <(AssignName)>, CLSMETH _CLASS_NAME_ _<AssignName>(), HB_OO_CLSTP_EXPORTED )

  #xcommand ASSIGN <AssignName>([<params,...>]) [ AS <type> ] => ;
    _HB_MEMBER _<AssignName>([<params>]) [ AS <type> ];;
    #xcommand METHOD <AssignName> [([<anyParams,...>])] [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <AssignName>([<anyParams>]);;
    s_oClass:AddMethod( "_" + <(AssignName)>, CLSMETH _CLASS_NAME_ _<AssignName>(), HB_OO_CLSTP_EXPORTED )
#endif

#xcommand ASSIGN <AssignName>( [<params,...>] ) [ AS <type> ] INLINE [Local <v>,] <Code,...> => ;
   _HB_MEMBER _<AssignName>([<params>]) [ AS <type> ];;
   s_oClass:AddInline( "_" + <(AssignName)>, {|Self [,<params>] [,<v>] | <Code> }, HB_OO_CLSTP_EXPORTED )

#xcommand ON ERROR <MethodName> => ERROR HANDLER <MethodName>;

#ifdef STRICT_OO
   #xcommand ERROR HANDLER <MethodName> => ;
     _HB_MEMBER <MethodName>;;
     #xcommand METHOD <MethodName>                    [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>;;
     s_oClass:SetOnError( CLSMETH _CLASS_NAME_ <MethodName>() )
#else
   #xcommand ERROR HANDLER <MethodName> => ;
     _HB_MEMBER <MethodName>();;
    #xcommand METHOD <MethodName> [([<anyParams,...>])] [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>([<anyParams>]);;
     s_oClass:SetOnError( CLSMETH _CLASS_NAME_ <MethodName>() )

   #xcommand ERROR HANDLER <MethodName>([<params,...>]) => ;
     _HB_MEMBER <MethodName>([<params>]);;
    #xcommand METHOD <MethodName> [([<anyParams,...>])] [DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>([<anyParams>]);;
     s_oClass:SetOnError( CLSMETH _CLASS_NAME_ <MethodName>() )
#endif

#xtranslate END CLASS => ENDCLASS

#ifdef HB_CLS_ALLOWCLASS
#xcommand ENDCLASS => ;;
                       s_oClass:Create(MetaClass) ;;
                       MetaClass:InitClass();;
                      endif ;;
                      return s_oClass:Instance() AS CLASS _CLASS_NAME_ ;;
                      #undef  _CLASS_MODE_ ;;
                      #define _CLASS_MODE_ _CLASS_IMPLEMENTATION_
#else
#xcommand ENDCLASS => ;;
                      s_oClass:Create() ;;
                      endif ;;
                      return s_oClass:Instance() AS CLASS _CLASS_NAME_ ;;
                      #undef  _CLASS_MODE_ ;;
                      #define _CLASS_MODE_ _CLASS_IMPLEMENTATION_
#endif

#xtranslate :Super( <SuperClass> ) : => :<SuperClass>:
#xtranslate :Super() : => :Super:
#xtranslate :Super() => :Super


#ifndef HB_SHORTNAMES

#xcommand METHOD <MethodName>                   => METHOD <MethodName>                       _CLASS_MODE_

#ifdef HB_CLS_NO_OO_ERR

   #xcommand METHOD <MethodName> CLASS <ClassName> => METHOD <MethodName>     CLASS <ClassName> _CLASS_IMPLEMENTATION_

   #xcommand METHOD <MethodName>                      _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>
   #xcommand METHOD <MethodName> CLASS <ClassName>    _CLASS_IMPLEMENTATION_ => DECLARED METHOD <ClassName> <MethodName>
#else

   #xcommand METHOD <MethodName> CLASS <ClassName> => METHOD <MethodName> DECLCLASS <ClassName> _CLASS_IMPLEMENTATION_

   //#define STRICT_OO
   #ifdef STRICT_OO
      #xcommand METHOD <MethodName>                   _CLASS_IMPLEMENTATION_ => __ERR(Method <"MethodName"> not declared or declaration mismatch in class: _CLASS_NAME_) ; function <MethodName> ; local self := QSelf()
      #xcommand METHOD <MethodName> CLASS <ClassName> _CLASS_IMPLEMENTATION_ => #error Method <"MethodName"> not declared or declaration mismatch in class: <ClassName> ; function <MethodName> ; local self := QSelf()
   #else
      #xcommand METHOD <MethodName>                   _CLASS_IMPLEMENTATION_ => __ERR(Method <"MethodName"> not declared in class: _CLASS_NAME_) ; function <MethodName> ; local self := QSelf()
      #xcommand METHOD <MethodName> CLASS <ClassName> _CLASS_IMPLEMENTATION_ => #error Method <"MethodName"> not declared in class: <ClassName> ; function <MethodName> ; local self := QSelf()
   #endif
#endif

#xcommand METHOD <MethodName> DECLCLASS <ClassName> _CLASS_IMPLEMENTATION_ => #error Class <"ClassName"> not declared for method: <MethodName> ; function <MethodName> ; local self := QSelf()

#xcommand DECLARED METHOD <ClassName> <MethodName> => ;
          static function DECLMETH <ClassName> <MethodName> ;;
          local Self AS CLASS <ClassName> := QSelf() AS CLASS <ClassName>

#xcommand ACCESS <AccessName> CLASS <ClassName> => ;
          static function <ClassName>_<AccessName> ;;
          local Self AS CLASS <ClassName> := QSelf() AS CLASS <ClassName>

#xcommand ASSIGN <AssignName> CLASS <ClassName> => ;
          static function <ClassName>__<AssignName> ;;
          local Self AS CLASS <ClassName> := QSelf() AS CLASS <ClassName>
#else

#xcommand DECLARED METHOD <ClassName> <MethodName>=> ;
          static function <MethodName> ;;
          local Self AS CLASS <ClassName> := QSelf() AS CLASS <ClassName>

#xcommand ACCESS <AccessName> CLASS <ClassName> => ;
          static function <AccessName> ;;
          local Self AS CLASS <ClassName> := QSelf() AS CLASS <ClassName>

#xcommand ASSIGN <AssignName> CLASS <ClassName> => ;
          static function _<AssignName> ;;
          local Self AS CLASS <ClassName> := QSelf() AS CLASS <ClassName>

#endif /* HB_SHORTNAMES */

#endif /* HB_CLASS_CH_ */