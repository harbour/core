/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for Class commands
 *
 * Copyright 1999 Antonio Linares <alinares@fivetechsoft.com>
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *    most of rules rewritten
 *
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2000 ( ->07/2000 ) JF. Lefebvre <jfl@mafact.com> & RA. Cuylen <rac@mafact.com>
 *    Support for Class(y), TopClass and Visual Object compatibility
 *    Support for MI (multiple inheritance),

 * Copyright 2000-2001 ( 08/2000-> ) JF. Lefebvre <jfl@mafact.com>
 *    Scoping (Protect, Hidden and Readonly),
 *    Delegating, DATA Shared
 *    Support of 10 Chars limits
 *
 * See COPYING.txt for licensing terms.
 *
 */

#ifndef HB_CLASS_CH_
#define HB_CLASS_CH_

#include "hboo.ch"

/* You can actually define one or all the syntax, they do not collide each other */
/* There is some difference with their original form and I hope I will have enough */
/* time to document it <g> */
/* This is work in progress ... */
/* FWOBJECT AND CLASSY compatibility are the base of this work */
/* VO is just here as I like it's way of */
/* instantiating object but there is only a very few VO keywords here :-( */
/* TOPCLASS is better implemented because I like the way some Classy command */
/* are simplified */
/* There is also a big common block extending in fact each of the four base syntax */
/* it seem actually impossible to completely separate it without creating */
/* four different include files (what I would not see in fact ) */

/* There is also two compatibility define you can use */
/* HB_CLS_NOTOBJECT which IF DEFINED, disable the auto inherit of HBObject */
/* (which in fact also disable the classy compatibility :new(...) => :Init(...)  */
/* HB_CLS_NOAUTOINIT which disable the (VO like) AutoInit for Logical and Numeric */
/* when not specifically initiated */
/* These two are disabled by default */
/* So Each class _inherit_ of HBObject by default and */
/*    Each type logical or numerical is initiated to .F. and 0 by default */

/* #define HB_CLS_NOTOBJECT     */ /* Should be included in some compatibility include files as needed */
/* #define HB_CLS_NOAUTOINIT    */ /* Idem */
/* #define HB_CLS_NO_DECORATION */ /* disable adding <class>_ prefix to method function names */
/* #define HB_CLS_NO_PARAMS_ERR */ /* disable parameters validation in method declaration and implementation */
/* #define HB_CLS_NO_OO_ERR     */ /* disable all code validation */


#ifndef HB_CLS_FWO
#ifndef HB_CLS_CSY
#ifndef HB_CLS_VO
#ifndef HB_CLS_TOP

/* IF NOTHING DECIDED BY THE PROGRAMMER USE ALL */
#define HB_CLS_FWO
#define HB_CLS_CSY
#define HB_CLS_VO
#define HB_CLS_TOP
#define HB_CLS_XPP

#endif
#endif
#endif
#endif


/* Disable method decoration when Harbour compiled strict compatibility mode.
   In strict mode, PP doesn't support identifier concatenation, which
   would be needed for method decoration. */
#ifdef HB_CLP_STRICT
   #ifndef HB_CLS_NO_DECORATION
      #define HB_CLS_NO_DECORATION
   #endif
   #ifndef HB_CLS_PARAM_LIST
      #define HB_CLS_PARAM_LIST
   #endif
#endif

/* disable strict parameters validation in method declaration and
   implementation when warning level (-w?) is not 3 or higher */
#if __pragma( WARNINGLEVEL ) < 3
   #ifndef HB_CLS_NO_PARAMS_ERR
      #define HB_CLS_NO_PARAMS_ERR
   #endif
#endif

/* should we use <ClassName>_ prefix for real method names? */
#ifdef HB_CLS_NO_DECORATION
   #xtranslate __HB_CLS_MTHNAME <ClassName> <MethodName> => <MethodName>
#else
   #xtranslate __HB_CLS_MTHNAME <ClassName> <MethodName> => <ClassName>_<MethodName>
#endif

/* parameters list passed throw - it's Harbour extension */
#ifndef HB_CLS_PARAM_LIST
   #define HB_CLS_PARAM_LIST ...
#endif

/* should we use _HB_CLASS/_HB_MEMBER declarations? */
#ifdef HB_CLS_NO_DECLARATIONS
   #xcommand _HB_CLASS  <name>        =>
   #xcommand _HB_CLASS  <name> <name> =>
   #xcommand _HB_MEMBER <name>        =>
   #xcommand DECLARE    <*decl*>      =>
   #xtranslate AS <type>              =>
   #xtranslate AS CLASS <name>        =>
#endif

/* should we inherit from HBObject class by default ? */
#ifdef HB_CLS_NOTOBJECT
   #xtranslate __HB_CLS_PAR([<cls,...>])  => { <cls> }
#else
   #xtranslate __HB_CLS_PAR([<cls,...>])  => iif( <.cls.>, { <cls> }, { @HBObject() } )
#endif

/* Should we initialize typed instance variables? */
#ifdef HB_CLS_NOAUTOINIT
   #define __HB_CLS_NOINI .T.
#else
   #define __HB_CLS_NOINI .F.
#endif

/* Should we generate compile error when method declaration has different parameters? */
#ifdef HB_CLS_NO_PARAMS_ERR
   #xtranslate __HB_CLS_PARAMS(<Method>)     => __HB_CLS_ASID(<Method>)
#else
   #xtranslate __HB_CLS_PARAMS(<Method>)     => <Method>
#endif

/* Should we disable compile errors for undeclared methods? */
#ifdef HB_CLS_NO_OO_ERR
   #xtranslate __HB_CLS_ERR([<msg,...>]) =>
   #xtranslate __HB_CLS_WARN([<msg,...>]) =>
#else
   #xtranslate __HB_CLS_ERR([<msg,...>]) => ;#error [ <msg>] ; #line
   #xtranslate __HB_CLS_WARN([<msg,...>]) => ;#warning [ <msg>] ; #line
#endif

#xtranslate __HB_CLS_VARERR(<var>) => __HB_CLS_ERR( Invalid instance variable name \<<var>> )

DECLARE HBClass ;
        New( cName AS String, OPTIONAL SuperParams ) AS CLASS HBClass ;
        Create() AS Object ;
        Instance() AS Object ;
        AddClsMethod( cName AS String, @MethodName(), nScope AS Numeric, n2 AS Numeric, n3 AS Numeric ) ;
        AddMultiClsData( cType AS String, uVal, nScope AS Numeric, aDatas AS Array OF String ) ;
        AddMultiData( cType AS String, uVal, nScope AS Numeric, aDatas AS Array OF String, x AS LOGICAL, lPer AS LOGICAL ) ;
        AddMethod( cName AS String, @MethodName(), nScope AS Numeric ) ;
        AddInLine( cName AS String, bBlock AS CodeBlock, nScope AS Numeric ) ;
        AddVirtual( cName AS String )

/*
 * Class(y) like non virtual send operator but instead of using early
 * bindings it casts object to class in which current method were defined.
 */
#translate @:<MessageName>([<MsgParams,...>]) => ;
                                ::realclass:<MessageName>([ <MsgParams>])

/* Indirect super casting translation */
#xtranslate :Super( <SuperClass> ): => :<SuperClass>:


#xtranslate __HB_CLS_OPT(<a>,<b>) =>  <a>
#xtranslate __HB_CLS_OPT(<a>)     =>  <a>

#xtranslate __HB_CLS_ASSTRING( <FuncName> )                 => <(FuncName)>
#xtranslate __HB_CLS_ASSTRING( <FuncName>([<params,...>]) ) => <(FuncName)>
#xtranslate __HB_CLS_ASFUNC( <FuncName> )                   => <FuncName>()
#xtranslate __HB_CLS_ASFUNC( <FuncName>([<params,...>]) )   => <FuncName>([ <params>])
#xtranslate __HB_CLS_ASID( <FuncName> )                     => <FuncName>
#xtranslate __HB_CLS_ASID( <FuncName>([<params,...>]) )     => <FuncName>
#xtranslate __HB_CLS_ASARGS( <FuncName> )                   =>
#xtranslate __HB_CLS_ASARGS( <FuncName>([<Args,...>]) )     => [ <Args>]
#xtranslate __HB_CLS_ASARGSOPT( <FuncName> )                =>
#xtranslate __HB_CLS_ASARGSOPT( <FuncName>([<Args,...>]) )  => [, <Args>]
#xtranslate __HB_CLS_ISVAR( <var> )                         => __HB_CLS_VARERR(<var>)
#xtranslate __HB_CLS_ISVAR( <!var!> )                       =>
#xcommand __HB_CLS_CHECKVAR( <param1> [,<paramN>] )         => __HB_CLS_ISVAR( <param1> ) [;__HB_CLS_ISVAR( <paramN> )]

/* #xtranslate __HB_CLS_SCOPE( <export>, <protect>, <hidde> ) => ;
      iif( <export>, HB_OO_CLSTP_EXPORTED , ;
      iif( <protect>, HB_OO_CLSTP_PROTECTED, ;
      iif( <hidde>, HB_OO_CLSTP_HIDDEN, nScope ) ) ) */
#xtranslate __HB_CLS_SCOPE( <x,...> )        => ) __HB_CLS_ERR( Can not use multiple scope qualifiers! )
#xtranslate __HB_CLS_SCOPE( .T., .F., .F. )  => HB_OO_CLSTP_EXPORTED
#xtranslate __HB_CLS_SCOPE( .F., .T., .F. )  => HB_OO_CLSTP_PROTECTED
#xtranslate __HB_CLS_SCOPE( .F., .F., .T. )  => HB_OO_CLSTP_HIDDEN
#xtranslate __HB_CLS_SCOPE( .F., .F., .F. )  => nScope /* Default */


#xcommand CLASS <ClassName> [METACLASS <metaClass>] ;
             [ <frm: FROM, INHERIT> <SuperClass1> [,<SuperClassN>] ] ;
             [ <modulfriend: MODULE FRIENDLY> ] ;
             [ <static: STATIC> ] [ FUNCTION <FuncName> ] => ;
   _HB_CLASS <ClassName> <FuncName> ;;
   <static> function __HB_CLS_OPT( [<FuncName>,] <ClassName> ) ( HB_CLS_PARAM_LIST ) ;;
      STATIC s_oClass ;;
      LOCAL nScope, oClass, oInstance ;;
      IF s_oClass == NIL .AND. __clsLockDef( @s_oClass ) ;;
         BEGIN SEQUENCE ;;
            nScope := HB_OO_CLSTP_EXPORTED ; HB_SYMBOL_UNUSED( nScope ) ;;
            oClass  := iif( <.metaClass.>, <(metaClass)>, HBClass():new( <(ClassName)>, __HB_CLS_PAR( [ @<SuperClass1>() ] [ , @<SuperClassN>() ] ), @__HB_CLS_OPT([__HB_CLS_ASID(<FuncName>),] <ClassName>)() [, <.modulfriend.> ] ) ) ;;
   #undef  _CLASS_NAME_ ; #define _CLASS_NAME_ <ClassName> ;;
   #undef  _CLASS_MODE_ ; #define _CLASS_MODE_ _CLASS_DECLARATION_ ;
   [ ; #translate Super( <SuperClassN> ): => ::<SuperClassN>: ] ;
   [ ; #translate Super( <SuperClass1> ): => ::<SuperClass1>: ] ;
   [ ; #translate Super(): => ::<SuperClass1>: ] ;
   [ ; #translate Super: => ::<SuperClass1>: ] ;
   [ ; #translate ::Super : => ::<SuperClass1>: ]

#xcommand ENDCLASS [<lck: LOCK, LOCKED>] => ;
            oClass:Create() ; [<-lck-> __clsLock( oClass:hClass ) ] ;;
         ALWAYS ;;
            __clsUnlockDef( @s_oClass, oClass ) ;;
         END SEQUENCE ;;
         oInstance := oClass:Instance() ;;
         IF __objHasMsg( oInstance, "InitClass" ) ;;
            oInstance:InitClass( HB_CLS_PARAM_LIST ) ;;
         END ;;
         RETURN oInstance ;;
      END ;;
      RETURN s_oClass:Instance() AS CLASS _CLASS_NAME_ ;;
   #undef  _CLASS_MODE_ ; #define _CLASS_MODE_ _CLASS_IMPLEMENTATION_


#xcommand DECLARED METHOD <type: FUNCTION, PROCEDURE> <MethodName> CLASS <ClassName> => ;
      static <type> __HB_CLS_MTHNAME <ClassName> <MethodName> ;;
      local Self AS CLASS <ClassName> := QSelf() AS CLASS <ClassName>

#xcommand __HB_CLS_DECLARE_METHOD <MethodName> <ClassName> => ;
      #xcommand METHOD \<type: FUNCTION, PROCEDURE> <MethodName> CLASS <ClassName> _CLASS_IMPLEMENTATION_ => ;
         DECLARED METHOD \<type> <MethodName> CLASS <ClassName>

#xcommand __HB_CLS_DECLARE_METHOD <!MethodName!> <ClassName> => ;
   #xcommand METHOD \<type: FUNCTION, PROCEDURE> <MethodName> CLASS <ClassName> _CLASS_IMPLEMENTATION_ => ;
         DECLARED METHOD \<type> <MethodName> CLASS <ClassName> ;;
   #xcommand METHOD \<type: FUNCTION, PROCEDURE> <MethodName>(\[ \<xparams,...>] ) CLASS <ClassName> _CLASS_IMPLEMENTATION_ => ;
         DECLARED METHOD \<type> <MethodName>(\[ \<xparams>] ) CLASS <ClassName>

#xcommand METHOD <type: FUNCTION, PROCEDURE> <MethodName> CLASS <ClassName> _CLASS_IMPLEMENTATION_ => ;
   __HB_CLS_WARN( Method \<<MethodName>> not declared or declaration mismatch in class \<<ClassName>> ) ;;
   DECLARED METHOD <type> <MethodName> CLASS <ClassName>

#xcommand METHOD <MethodName> [ <ctor: CONSTRUCTOR> ] [ AS <type> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] [_CLASS_DECLARATION_] => ;
   _HB_MEMBER __HB_CLS_ASFUNC(<MethodName>) [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
   __HB_CLS_DECLARE_METHOD __HB_CLS_PARAMS(<MethodName>) _CLASS_NAME_ ;;
   oClass:AddMethod( __HB_CLS_ASSTRING(<MethodName>), @__HB_CLS_ASID( __HB_CLS_MTHNAME _CLASS_NAME_ <MethodName> )(), __HB_CLS_SCOPE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + iif( <.persistent.>, HB_OO_CLSTP_PERSIST, 0 ) + iif( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) )

#xcommand ACCESS <AccessName> [ AS <type> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] [_CLASS_DECLARATION_] => ;
   METHOD <AccessName> [ AS <type> ] <export> <protect> <hidde> <persistent> <sync> _CLASS_DECLARATION_

#xcommand ASSIGN <AssignName> [ AS <type> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] [_CLASS_DECLARATION_] => ;
   METHOD _<AssignName> [ AS <type> ] <export> <protect> <hidde> <persistent> <sync> _CLASS_DECLARATION_

#xcommand METHOD <type: FUNCTION, PROCEDURE> <MethodName> [CLASS <ClassName>] => ;
   METHOD <type> <MethodName> CLASS __HB_CLS_OPT([<ClassName>,] _CLASS_NAME_) _CLASS_MODE_
#xcommand METHOD <MethodName> CLASS <ClassName> => METHOD FUNCTION  <MethodName> CLASS <ClassName> _CLASS_MODE_
#xcommand ACCESS <AccessName> CLASS <ClassName> => METHOD FUNCTION  <AccessName> CLASS <ClassName> _CLASS_MODE_
#xcommand ASSIGN <AssignName> CLASS <ClassName> => METHOD FUNCTION _<AssignName> CLASS <ClassName> _CLASS_MODE_

#xcommand METHOD <MethodName> _CLASS_IMPLEMENTATION_ => ;
   METHOD FUNCTION <MethodName> CLASS _CLASS_NAME_
#xcommand METHOD <MethodName> => METHOD <MethodName> _CLASS_MODE_

/* For backward compatibility */
#xcommand PROCEDURE <MethodName> CLASS <ClassName> => METHOD PROCEDURE <MethodName> CLASS <ClassName> _CLASS_MODE_


/* special method(s) */
#xcommand CONSTRUCTOR <Name>        => METHOD <Name> CONSTRUCTOR
#xcommand DESTRUCTOR <MethodName>   => ;
   _HB_MEMBER __HB_CLS_ASFUNC(<MethodName>);;
   __HB_CLS_DECLARE_METHOD __HB_CLS_PARAMS(<MethodName>) _CLASS_NAME_ ;;
   oClass:SetDestructor( @__HB_CLS_ASID( __HB_CLS_MTHNAME _CLASS_NAME_ <MethodName> )() )
#xcommand DESTRUCTOR FUNCTION <FuncName> => ;
   oClass:SetDestructor( @__HB_CLS_ASID( <FuncName> )() )

#xcommand ON ERROR <MethodName>     => ERROR HANDLER <MethodName>
#xcommand ERROR HANDLER <MethodName> => ;
   _HB_MEMBER __HB_CLS_ASFUNC(<MethodName>);;
   __HB_CLS_DECLARE_METHOD __HB_CLS_PARAMS(<MethodName>) _CLASS_NAME_ ;;
   oClass:SetOnError( @__HB_CLS_ASID( __HB_CLS_MTHNAME _CLASS_NAME_ <MethodName> )() )
#xcommand ON ERROR FUNCTION <FuncName> => ;
   oClass:SetOnError( @__HB_CLS_ASID( <FuncName> )() )

/* Friend function/class definitions */
#xcommand FRIEND CLASS <ClassName1> [, <ClassNameN> ] => ;
   oClass:AddFriendClass( @__HB_CLS_ASID(<ClassName1>)() [, @__HB_CLS_ASID(<ClassNameN>)() ] )

#xcommand FRIEND FUNCTION <FuncName1> [, <FuncNameN> ] => ;
   oClass:AddFriendFunc( @__HB_CLS_ASID(<FuncName1>)() [, @__HB_CLS_ASID(<FuncNameN>)() ] )

/* Operator overloading */
#xcommand OPERATOR <op> [<arg: ARG, ARGS> <Args,...>] [LOCAL <Locals,...>] INLINE <Code,...> [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<sync: SYNC>] => ;
   oClass:AddInline( <(op)>, {|Self [,<Args>] [,<Locals>]| HB_SYMBOL_UNUSED(Self), <Code>}, __HB_CLS_SCOPE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) )

#xcommand METHOD <MethodName> [ AS <type> ] OPERATOR <op> [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<sync: SYNC>] => ;
   _HB_MEMBER __HB_CLS_ASFUNC(<MethodName>) [ AS <type> ];;
   __HB_CLS_DECLARE_METHOD __HB_CLS_PARAMS(<MethodName>) _CLASS_NAME_ ;;
   oClass:AddMethod( <(op)>, @__HB_CLS_ASID( __HB_CLS_MTHNAME _CLASS_NAME_ <MethodName> )(), __HB_CLS_SCOPE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) )

#xcommand OPERATOR <op> FUNCTION <FuncName> [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<sync: SYNC>] => ;
   oClass:AddMethod( <(op)>, @__HB_CLS_ASID( <FuncName> )(), __HB_CLS_SCOPE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) )

/* Set/Get Method */
#xcommand METHOD <MethodName> [ AS <type> ] SETGET [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<ro: READONLY, RO>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   _HB_MEMBER __HB_CLS_ASFUNC(<MethodName>) [ AS <type> ];;
   _HB_MEMBER __HB_CLS_ASFUNC(_<MethodName>) [ AS <type> ];;
   __HB_CLS_DECLARE_METHOD __HB_CLS_PARAMS(<MethodName>) _CLASS_NAME_ ;;
   oClass:AddMethod( __HB_CLS_ASSTRING(<MethodName>), @__HB_CLS_ASID( __HB_CLS_MTHNAME _CLASS_NAME_ <MethodName> )(), __HB_CLS_SCOPE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.persistent.>, HB_OO_CLSTP_PERSIST, 0 ) + iif( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ) ;;
   oClass:AddMethod( __HB_CLS_ASSTRING(_<MethodName>), @__HB_CLS_ASID( __HB_CLS_MTHNAME _CLASS_NAME_ <MethodName> )(), __HB_CLS_SCOPE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ) + iif( <.persistent.>, HB_OO_CLSTP_PERSIST, 0 ) + iif( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) )

/* Message definitions */

/* Virtual / Deferred Methods */
#xcommand MESSAGE <MessageName> [ AS <type> ] VIRTUAL => ;
   _HB_MEMBER __HB_CLS_ASFUNC(<MessageName>) ;;
   oClass:AddVirtual( __HB_CLS_ASSTRING(<MessageName>) )

#xcommand MESSAGE <MessageName> [ AS <type> ] DEFERRED => ;
   MESSAGE <MessageName> [ AS <type> ] VIRTUAL

#xcommand MESSAGE <MessageName> [ AS <type> ] IS DEFERRED => ;
   MESSAGE <MessageName> [ AS <type> ] VIRTUAL

#xcommand MESSAGE <MessageName> [ AS <type> ] METHOD <MethodName> [<ctor: CONSTRUCTOR>] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   _HB_MEMBER __HB_CLS_ASFUNC(<MessageName>) [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
   __HB_CLS_DECLARE_METHOD __HB_CLS_PARAMS(<MethodName>) _CLASS_NAME_ ;;
   oClass:AddMethod( __HB_CLS_ASSTRING(<MessageName>), @__HB_CLS_ASID( __HB_CLS_MTHNAME _CLASS_NAME_ <MethodName> )(), __HB_CLS_SCOPE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + iif( <.persistent.>, HB_OO_CLSTP_PERSIST, 0 ) + iif( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) )

#xcommand MESSAGE <MessageName> [ AS <type> ] EXTERN <FuncName> [<ctor: CONSTRUCTOR>] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   _HB_MEMBER __HB_CLS_ASFUNC(<MessageName>) [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
   oClass:AddMethod( __HB_CLS_ASSTRING(<MessageName>), @__HB_CLS_ASID( <FuncName> )(), __HB_CLS_SCOPE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + iif( <.persistent.>, HB_OO_CLSTP_PERSIST, 0 ) + iif( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) )

#xcommand MESSAGE <MessageName> [ AS <type> ] BLOCK <CodeBlock> [<ctor: CONSTRUCTOR>] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   _HB_MEMBER __HB_CLS_ASFUNC(<MessageName>) [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
   oClass:AddInline( __HB_CLS_ASSTRING(<MessageName>), <CodeBlock>, __HB_CLS_SCOPE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + iif( <.persistent.>, HB_OO_CLSTP_PERSIST, 0 ) + iif( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) )

#xcommand MESSAGE <MessageName> [ AS <type> ] [LOCAL <Locals,...>] INLINE <Code,...> [<ctor: CONSTRUCTOR>] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   MESSAGE <MessageName> [ AS <type> ] BLOCK {|Self __HB_CLS_ASARGSOPT(<MessageName>) [,<Locals>]| HB_SYMBOL_UNUSED(Self), <Code>} <ctor> <export> <protect> <hidde> <persistent> <sync>

#xcommand MESSAGE <MessageName> [ AS <type> ] <arg: ARG, ARGS> <Args,...> [LOCAL <Locals,...>] INLINE <Code,...> [<ctor: CONSTRUCTOR>] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   MESSAGE __HB_CLS_ASID(<MessageName>)(<Args>) [ AS <type> ] [LOCAL <Locals>] INLINE <Code> <ctor> <export> <protect> <hidde> <persistent> <sync>

#xcommand MESSAGE <MessageName> [ AS <type> ] IS <AltMsgName> [<ctor: CONSTRUCTOR>] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   MESSAGE <MessageName> [ AS <type> ] INLINE Self:<AltMsgName> <ctor> <export> <protect> <hidde> <persistent> <sync>

#xcommand MESSAGE <MessageName> [ AS <type> ] TO <oObject> [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   MESSAGE <MessageName> [ AS <type> ] INLINE Self:<oObject>:<MessageName> <export> <protect> <hidde> <persistent> <sync>

#xcommand MESSAGE <MessageName> [ AS <type> ] IN <SuperClass> [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   MESSAGE <MessageName> [ AS <type> ] INLINE Self:<SuperClass>:<MessageName> <export> <protect> <hidde> <persistent> <sync>

#xcommand MESSAGE <MessageName> [ AS <type> ] IS <AltMsgName> TO <oObject> [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   MESSAGE <MessageName> [ AS <type> ] INLINE Self:<oObject>:__HB_CLS_ASID(<AltMsgName>)(__HB_CLS_ASARGS(<MessageName>)) <export> <protect> <hidde> <persistent> <sync>

#xcommand MESSAGE <MessageName> [ AS <type> ] IS <SprMethodName> IN <SuperClass> [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   MESSAGE <MessageName> [ AS <type> ] INLINE Self:<SuperClass>:__HB_CLS_ASID(<SprMethodName>)(__HB_CLS_ASARGS(<MessageName>)) <export> <protect> <hidde> <persistent> <sync>

#xcommand DELEGATE <MessageName> [ AS <type> ] TO <oObject> [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] =>;
   MESSAGE <MessageName> [ AS <type> ] TO <oObject> <export> <protect> <hidde> <persistent> <sync>

#xcommand ACCESS <MessageName> [ AS <type> ] [LOCAL <Locals,...>] INLINE <Code,...> [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   MESSAGE <MessageName> [ AS <type> ] [LOCAL <Locals>] INLINE <Code> <export> <protect> <hidde> <persistent> <sync>

#xcommand ASSIGN <MessageName> [ AS <type> ] [LOCAL <Locals,...>] INLINE <Code,...> [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   MESSAGE _<MessageName> [ AS <type> ] [LOCAL <Locals>] INLINE <Code> <export> <protect> <hidde> <persistent> <sync>

#xcommand ACCESS <MessageName> [ AS <type> ] <mth: METHOD, IS> <MethodName> [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   MESSAGE <MessageName> [ AS <type> ] <mth> <MethodName> <export> <protect> <hidde> <persistent> <sync>

#xcommand ASSIGN <MessageName> [ AS <type> ] <mth: METHOD, IS> <MethodName> [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   MESSAGE _<MessageName> [ AS <type> ] <mth> <MethodName> <export> <protect> <hidde> <persistent> <sync>

/*
 * These definitions are for backward compatibility only.
 * They are using METHOD keyword instead of MESSAGE
 * or only single local parameter in INLINED blocks.
 * If possible please do not use them and update your
 * code to use supported syntax. [druzus]
 */
#xcommand METHOD <MessageName> [ AS <type> ] VIRTUAL => ;
   MESSAGE <MessageName> [ AS <type> ] VIRTUAL
#xcommand METHOD <MessageName> [ AS <type> ] DEFERRED => ;
   MESSAGE <MessageName> [ AS <type> ] VIRTUAL
#xcommand METHOD <MessageName> [ AS <type> ] EXTERN <FuncName> [<ctor: CONSTRUCTOR>] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   MESSAGE <MessageName> [ AS <type> ] EXTERN <FuncName> <ctor> <export> <protect> <hidde> <persistent> <sync>
#xcommand METHOD <MessageName> [ AS <type> ] INLINE <Code,...> [LOCAL <Locals,...>] [<ctor: CONSTRUCTOR>] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   MESSAGE <MessageName> [ AS <type> ] [LOCAL <Locals>] INLINE <Code> <ctor> <export> <protect> <hidde> <persistent> <sync>
#xcommand METHOD <MessageName> [ AS <type> ] BLOCK <CodeBlock> [<ctor: CONSTRUCTOR>] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   MESSAGE <MessageName> [ AS <type> ] BLOCK <CodeBlock> <ctor> <export> <protect> <hidde> <persistent> <sync>
#xcommand METHOD <MessageName> [ AS <type> ] INLINE [LOCAL <v>] <Code,...> [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   MESSAGE <MessageName> [ AS <type> ] [LOCAL <v>] INLINE <Code> <export> <protect> <hidde> <persistent> <sync>
#xcommand ACCESS <MessageName> [ AS <type> ] INLINE LOCAL <v> <Code,...> [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   MESSAGE <MessageName> [ AS <type> ] LOCAL <v> INLINE <Code> <export> <protect> <hidde> <persistent> <sync>
#xcommand ASSIGN <MessageName> [ AS <type> ] INLINE LOCAL <v> <Code,...> [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   MESSAGE _<MessageName> [ AS <type> ] LOCAL <v> INLINE <Code> <export> <protect> <hidde> <persistent> <sync>


/* Definitions of instance variables */

/* CLASSY SYNTAX */
#ifdef HB_CLS_CSY

   #xcommand CREATE CLASS <ClassName> [<*more*>] => CLASS <ClassName> <more>
   #xcommand END CLASS [<*more*>]   => ENDCLASS <more>
   #xcommand CLASS VAR <*more*>     => CLASSVAR <more>
   #xcommand CLASS METHOD <*more*>  => CLASSMETHOD <more>

   /* Disable the message :Class */
   #xtranslate  :CLASS  =>
   #xtranslate  :CLASS: => :

   #xtranslate _HB_MEMBER {AS Int      => _HB_MEMBER {AS Numeric
   #xtranslate _HB_MEMBER {AS Integer  => _HB_MEMBER {AS Numeric
   #xtranslate _HB_MEMBER {AS Num      => _HB_MEMBER {AS Numeric
   #xtranslate _HB_MEMBER {AS Char     => _HB_MEMBER {AS Character
   #xtranslate _HB_MEMBER {AS Block    => _HB_MEMBER {AS CodeBlock

   #xcommand EXPORTED:   =>    nScope := HB_OO_CLSTP_EXPORTED ; HB_SYMBOL_UNUSED( nScope )
   #xcommand EXPORT:     =>    nScope := HB_OO_CLSTP_EXPORTED ; HB_SYMBOL_UNUSED( nScope )
   #xcommand VISIBLE:    =>    nScope := HB_OO_CLSTP_EXPORTED ; HB_SYMBOL_UNUSED( nScope )
   #xcommand HIDDEN:     =>    nScope := HB_OO_CLSTP_HIDDEN   ; HB_SYMBOL_UNUSED( nScope )
   #xcommand PROTECTED:  =>    nScope := HB_OO_CLSTP_PROTECTED; HB_SYMBOL_UNUSED( nScope )


   /* Classy compatibility... Added By JF Lefebvre (mafact) 2006/11/07 */
   #xcommand METHOD <MethodName> [ AS <type> ] INLINE [Local <v>,] <Code,...> [<other>] => ;
             MESSAGE <MethodName> [ AS <type> ] BLOCK {|Self [,<v>] | HB_SYMBOL_UNUSED(Self), <Code>} [ <other>]

   #xcommand METHOD <MethodName>( [<params,...>] ) [ AS <type> ] INLINE [Local <v>,] <Code,...> [<other>] => ;
             MESSAGE <MethodName> [ AS <type> ] BLOCK {|Self [, <params>] [, <v>] | HB_SYMBOL_UNUSED(Self), <Code> } [ <other>]


   /* These definitions are not Class(y) compatible - I'm leaving them as is now */

   #xcommand VAR <DataNames,...> [ <tp: TYPE, AS> <type> ] [ <as: ASSIGN, INIT> <uValue> ] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<ro: READONLY, RO>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
      __HB_CLS_CHECKVAR(<DataNames>);;
      _HB_MEMBER {[ AS <type>] <DataNames> } ;;
      oClass:AddMultiData( <(type)>, <uValue>, __HB_CLS_SCOPE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ) + iif( <.persistent.>, HB_OO_CLSTP_PERSIST, 0 ) + iif( <.sync.>, HB_OO_CLSTP_SYNC, 0 ), {<(DataNames)>}, __HB_CLS_NOINI )

   #xcommand VAR <DataName> [ AS <type> ] IN <SuperClass> => ;
      __HB_CLS_CHECKVAR(<DataName>);;
      _HB_MEMBER {[ AS <type>] <DataName> } ;;
      oClass:AddInline( <(DataName)>, {|Self| Self:<SuperClass>:<DataName> }, HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY ) ;;
      oClass:AddInline( "_" + <(DataName)>, {|Self, param| Self:<SuperClass>:<DataName> := param }, HB_OO_CLSTP_EXPORTED )

   #xcommand VAR <DataName> [ AS <type> ] IS <SprDataName> IN <SuperClass> => ;
      __HB_CLS_CHECKVAR(<DataName>);;
      _HB_MEMBER {[ AS <type>] <DataName> } ;;
      oClass:AddInline( <(DataName)>, {|Self| Self:<SuperClass>:<SprDataName> }, HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY ) ;;
      oClass:AddInline( "_" + <(DataName)>, {|Self, param| Self:<SuperClass>:<SprDataName> := param }, HB_OO_CLSTP_EXPORTED )

   #xcommand VAR <DataName1> [ AS <type> ] IS <DataName2> => ;
      __HB_CLS_CHECKVAR(<DataName1>);;
      _HB_MEMBER {[ AS <type>] <DataName1> } ;;
      oClass:AddInline( <(DataName1)>, {|Self| Self:<DataName2> }, HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY ) ;;
      oClass:AddInline( "_" + <(DataName1)>, {|Self, param| Self:<DataName2> := param }, HB_OO_CLSTP_EXPORTED )

   #xcommand VAR <DataName1> [ AS <type> ] IS <DataName2> TO <oObject> => ;
      __HB_CLS_CHECKVAR(<DataName1>);;
      _HB_MEMBER {[ AS <type>] <DataName1> } ;;
      oClass:AddInline( <(DataName1)>, {|Self| Self:<oObject>:<DataName2> }, HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY ) ;;
      oClass:AddInline( "_" + <(DataName1)>, {|Self, param| Self:<oObject>:<DataName2> := param }, HB_OO_CLSTP_EXPORTED )

#endif /* HB_CLS_CSY */

/* FWOBJECT SYNTAX */
#ifdef HB_CLS_FWO

   #xcommand DATA <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [ <export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<ro: READONLY, RO>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
      __HB_CLS_CHECKVAR(<DataNames>);;
      _HB_MEMBER {[ AS <type>] <DataNames> } ;;
      oClass:AddMultiData( <(type)>, <uValue>, __HB_CLS_SCOPE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ) + iif( <.persistent.>, HB_OO_CLSTP_PERSIST, 0 ) + iif( <.sync.>, HB_OO_CLSTP_SYNC, 0 ), {<(DataNames)>}, __HB_CLS_NOINI )

   /* Warning! For backward compatibility this CLASSDATA ignores the
      SHARED clause and always create shared class variables */
   #xcommand CLASSDATA <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<ro: READONLY, RO>] [<share: SHARED>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
      _HB_MEMBER {[ AS <type>] <DataNames> } ;;
   oClass:AddMultiClsData( <(type)>, <uValue>, __HB_CLS_SCOPE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ) + iif( /* <.share.> */ .T., HB_OO_CLSTP_SHARED, 0 ) + iif( <.persistent.>, HB_OO_CLSTP_PERSIST, 0 ) + iif( <.sync.>, HB_OO_CLSTP_SYNC, 0 ), {<(DataNames)>}, __HB_CLS_NOINI )

#endif /* HB_CLS_FWO */

/* VO SYNTAX */
#ifdef HB_CLS_VO

#xtranslate  ( <!name!>{ [<p,...>] }        =>  ( <name>():New( <p> )
#xtranslate  , <!name!>{ [<p,...>] }        =>  , <name>():New( <p> )
#xtranslate  = <!name!>{ [<p,...>] }        =>  = <name>():New( <p> )
#xtranslate := <!name!>{ [<p,...>] }        => := <name>():New( <p> )

#xcommand EXPORT <!DataName1!> [, <!DataNameN!>] [ <tp: TYPE, AS> <type> ] [ <as: ASSIGN, INIT> <uValue> ] [<ro: READONLY, RO>] [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[ AS <type>] <DataName1> [, <DataNameN>] } ;;
   oClass:AddMultiData( <(type)>, <uValue>, HB_OO_CLSTP_EXPORTED + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ) + iif( <.persistent.>, HB_OO_CLSTP_PERSIST, 0 ), {<(DataName1)> [, <(DataNameN)>]}, __HB_CLS_NOINI )

#xcommand PROTECT <!DataName1!> [, <!DataNameN!>] [ <tp: TYPE, AS> <type> ] [ <as: ASSIGN, INIT> <uValue> ] [<ro: READONLY, RO>] [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[ AS <type>] <DataName1> [, <DataNameN>] } ;;
   oClass:AddMultiData( <(type)>, <uValue>, HB_OO_CLSTP_PROTECTED + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ) + iif( <.persistent.>, HB_OO_CLSTP_PERSIST, 0 ), {<(DataName1)> [, <(DataNameN)>]}, __HB_CLS_NOINI )

#xcommand HIDDEN <!DataName1!> [, <!DataNameN!>] [ <tp: TYPE, AS> <type> ] [ <as: ASSIGN, INIT> <uValue> ] [<ro: READONLY, RO>] [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[ AS <type>] <DataName1> [, <DataNameN>] } ;;
   oClass:AddMultiData( <(type)>, <uValue>, HB_OO_CLSTP_HIDDEN + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ) + iif( <.persistent.>, HB_OO_CLSTP_PERSIST, 0 ), {<(DataName1)> [, <(DataNameN)>]}, __HB_CLS_NOINI )

#endif /* HB_CLS_VO */

/* Xbase++ syntax */
#ifdef HB_CLS_XPP

   #xcommand SYNC METHOD <MethodName> [<decl,...>] => METHOD <MethodName> [<decl>] SYNC
   #xcommand SYNC CLASS METHOD <MethodName> [<decl,...>] => CLASSMETHOD <MethodName> [<decl>] SYNC

   #xcommand METHOD <!MethodName1!>[([<params,...>])], <!MethodName2!>[([<params,...>])] [, <!MethodNameN!>[([<params,...>])]] => ;
             METHOD <MethodName1> [ ; METHOD <MethodName2> ] [ ; METHOD <MethodNameN> ]
   #xcommand SYNC METHOD <!MethodName1!>[([<params,...>])], <!MethodName2!>[([<params,...>])] [, <!MethodNameN!>[([<params,...>])]] => ;
             SYNC METHOD <MethodName1> [ ; SYNC METHOD <MethodName2> ] [ ; SYNC METHOD <MethodNameN> ]

   #xcommand METHOD <!className!>:<!methodName!>[([<params,...>])] => ;
             METHOD <methodName>( <params> ) CLASS <className>

#endif /* HB_CLS_XPP */

/* Class datas and messages - we do not support real class messages yet */
#xcommand CLASSMETHOD <MethodName> [ AS <type> ] [<ctor: CONSTRUCTOR>] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<share: SHARED>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   _HB_MEMBER __HB_CLS_ASFUNC(<MethodName>) [ AS <type> ];;
   __HB_CLS_DECLARE_METHOD __HB_CLS_PARAMS(<MethodName>) _CLASS_NAME_ ;;
   oClass:AddClsMethod( __HB_CLS_ASSTRING(<MethodName>), @__HB_CLS_ASID( __HB_CLS_MTHNAME _CLASS_NAME_ <MethodName> )(), __HB_CLS_SCOPE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.share.>, HB_OO_CLSTP_SHARED, 0 ) + iif( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + iif( <.persistent.>, HB_OO_CLSTP_PERSIST, 0 ) + iif( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) )

#xcommand CLASSVAR <!DataName1!> [, <!DataNameN!>] [ <tp: TYPE, AS> <type> ] [ <as: ASSIGN, INIT> <uValue> ] [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<ro: READONLY, RO>] [<share: SHARED>] [<persistent: PERSISTENT, PROPERTY>] [<sync: SYNC>] => ;
   _HB_MEMBER {[ AS <type>] <DataName1> [, <DataNameN>] } ;;
   oClass:AddMultiClsData( <(type)>, <uValue>, __HB_CLS_SCOPE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ) + iif( <.share.>, HB_OO_CLSTP_SHARED, 0 ) + iif( <.persistent.>, HB_OO_CLSTP_PERSIST, 0 ) + iif( <.sync.>, HB_OO_CLSTP_SYNC, 0 ), {<(DataName1)> [, <(DataNameN)>]}, __HB_CLS_NOINI )


/* Scalar classes support */
#command ASSOCIATE CLASS <ClassFuncName> WITH TYPE <type: ;
   ARRAY, BLOCK, CHARACTER, DATE, HASH, LOGICAL, NIL, NUMERIC, SYMBOL, TIMESTAMP, POINTER> => ;
      __clsAssocType( __clsInstSuper( @<ClassFuncName>() ), #<type> )

#command ENABLE TYPE CLASS <type: ;
   ARRAY, BLOCK, CHARACTER, DATE, HASH, LOGICAL, NIL, NUMERIC, SYMBOL, TIMESTAMP, POINTER> ;
   [, <typeN: ;
   ARRAY, BLOCK, CHARACTER, DATE, HASH, LOGICAL, NIL, NUMERIC, SYMBOL, TIMESTAMP, POINTER>] => ;
      REQUEST HB<type> [, HB<typeN>]

#command ENABLE TYPE CLASS ALL => ;
      REQUEST HBArray, HBBlock, HBCharacter, HBDate, HBHash, ;
              HBLogical, HBNil, HBNumeric, HBSymbol, HBTimeStamp, HBPointer

#endif /* HB_CLASS_CH_ */
