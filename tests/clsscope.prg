/*
 * Harbour Project source code:
 *    demonstration/test code for class method scoping
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#command ? => OutStd( hb_eol() )
#xtranslate QOut( <x,...> ) => OutStd( hb_eol(), <x> )

#include "hbclass.ch"

#xcommand MAKE_TEST(<obj>,<v>)                        ;
        =>  begin sequence                            ;;
               ? "      hVar:= ", <obj>:hVar:=<v>     ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      hVar   ", <obj>:hVar          ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      hrVar:=", <obj>:hrVar:=<v>    ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      hrVar  ", <obj>:hrVar         ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      pVar:= ", <obj>:pVar:=<v>     ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      pVar   ", <obj>:pVar          ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      prVar:=", <obj>:prVar:=<v>    ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      prVar  ", <obj>:prVar         ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      eVar:= ", <obj>:eVar:=<v>     ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      eVar   ", <obj>:eVar          ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      erVar:=", <obj>:erVar:=<v>    ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      erVar  ", <obj>:erVar         ;;
            end sequence


#xcommand MAKE_TESTBLOCK(<fn>,<o>,<v>)                                     ;
        =>  begin sequence                                                 ;;
               <fn>({| self | qout("      hVar:= ", ::hVar:=<v>)},<o>)     ;;
            end sequence                                                   ;;
            begin sequence                                                 ;;
               <fn>({| self | qout("      hrVar:=", ::hrVar:=<v>)},<o>)    ;;
            end sequence                                                   ;;
            begin sequence                                                 ;;
               <fn>({| self | qout("      pVar:= ", ::pVar:=<v>)},<o>)     ;;
            end sequence                                                   ;;
            begin sequence                                                 ;;
               <fn>({| self | qout("      prVar:=", ::prVar:=<v>)},<o>)    ;;
            end sequence                                                   ;;
            begin sequence                                                 ;;
               <fn>({| self | qout("      eVar:= ", ::eVar:=<v>)},<o>)     ;;
            end sequence                                                   ;;
            begin sequence                                                 ;;
               <fn>({| self | qout("      erVar:=", ::erVar:=<v>)},<o>)    ;;
            end sequence


#xcommand MAKE_TESTINLINE(<obj>,<mPref>)              ;
        =>  begin sequence                            ;;
               ? "      hVar:= ", <obj>:<mPref>hVar   ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      hrVar:=", <obj>:<mPref>hrVar  ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      pVar:= ", <obj>:<mPref>pVar   ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      prVar:=", <obj>:<mPref>prVar  ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      eVar:= ", <obj>:<mPref>eVar   ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      erVar:=", <obj>:<mPref>erVar  ;;
            end sequence

/* Clipper preprocessor does not support concatenation
   We need separate definitions for MAKE_TESTINLINE */

#xcommand MAKE_TESTINLINE0(<obj>[,<mPref>])           ;
        =>  begin sequence                            ;;
               ? "      hVar:= ", <obj>:b0hVar        ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      hrVar:=", <obj>:b0hrVar       ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      pVar:= ", <obj>:b0pVar        ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      prVar:=", <obj>:b0prVar       ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      eVar:= ", <obj>:b0eVar        ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      erVar:=", <obj>:b0erVar       ;;
            end sequence

#xcommand MAKE_TESTINLINE1(<obj>[,<mPref>])           ;
        =>  begin sequence                            ;;
               ? "      hVar:= ", <obj>:b1hVar        ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      hrVar:=", <obj>:b1hrVar       ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      pVar:= ", <obj>:b1pVar        ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      prVar:=", <obj>:b1prVar       ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      eVar:= ", <obj>:b1eVar        ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      erVar:=", <obj>:b1erVar       ;;
            end sequence

#xcommand MAKE_TESTINLINE2(<obj>[,<mPref>])           ;
        =>  begin sequence                            ;;
               ? "      hVar:= ", <obj>:b2hVar        ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      hrVar:=", <obj>:b2hrVar       ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      pVar:= ", <obj>:b2pVar        ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      prVar:=", <obj>:b2prVar       ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      eVar:= ", <obj>:b2eVar        ;;
            end sequence                              ;;
            begin sequence                            ;;
               ? "      erVar:=", <obj>:b2erVar       ;;
            end sequence



proc main()
   local cbErr, self, o

   ? DATE(), TIME(), VERSION(), OS()
   ?

   o := clsX():new()
   self := cls2():new()
   cbErr := errorBlock( {| oErr | errHandler( oErr ) } )

   /***************************************************************/
   ? "Class Access:"
   self:TEST1()
   ?
   ? "Parent Class Access:"
   self:TEST0()
   ?
   ? "Subclass Access:"
   self:TEST2()
   ?
   ? "Subclass Super Access:"
   self:TEST3()
   ?
   ? "Foreign Class Access:"
   o:TESTX(self)
   ?
   ? "Extern Access:"
   MAKE_TEST(self,"extr")
   ?

   /***************************************************************/
   ?
   ? "Inline Class Access:"
   MAKE_TESTINLINE1(self,b1)
   ?
   ? "Inline Parent Class Access:"
   MAKE_TESTINLINE0(self,b0)
   ?
   ? "Inline Subclass Access:"
   MAKE_TESTINLINE2(self,b2)
   ?

   /***************************************************************/
   ?
   ? "Access from class method by"
   ? "eval() with block created in class method:"
   self:CBINT1()
   ?
   ? "Access from subclass method by"
   ? "eval() with block created in subclass method:"
   self:CBINT2()
   ?
   ? "Access from parent class method by"
   ? "eval() with block created in parent class method:"
   self:CBINT0()
   ?
   ? "Access from foreign class method by"
   ? "eval() with block created in foreign class method:"
   o:CBINTX(self)

   /***************************************************************/
   ?
   ? "Access from function executed from class method by"
   ? "eval() block created in class method:"
   self:CBEVL1()
   ?
   ? "Access from function executed from subclass method by"
   ? "eval() with block created in subclass method:"
   self:CBEVL2()
   ?
   ? "Access from function executed from parent class method by"
   ? "eval() with block created in parent class method:"
   self:CBEVL0()

   /***************************************************************/
   ?
   ? "Access from foreign object method executed from class method by"
   ? "eval() block created in class method:"
   self:CBFEVL1(o)
   ?
   ? "Access from foreign object method executed from subclass method by"
   ? "eval() with block created in subclass method:"
   self:CBFEVL2(o)
   ?
   ? "Access from foreign object method executed from parent class method by"
   ? "eval() with block created in parent class method:"
   self:CBFEVL0(o)

   /***************************************************************/
   ?
   ? "External block executed from class method"
   MAKE_TESTBLOCK(self:cbeval1,self,::classname()+":"+procname())
   ?
   ? "External block executed from subclass method"
   MAKE_TESTBLOCK(self:cbeval2,self,::classname()+":"+procname())
   ?
   ? "External block executed from parent class method"
   MAKE_TESTBLOCK(self:cbeval0,self,::classname()+":"+procname())
   ?
   ? "External block access"
   MAKE_TESTBLOCK(eval,self,::classname()+":"+procname())

   /***************************************************************/
   ?
   ? "External block executed from function called from class method"
   MAKE_TESTBLOCK(self:cbeval1,self,::classname()+":"+procname())
   ?
   ? "External block executed from function called from subclass method"
   MAKE_TESTBLOCK(self:cbeval2,self,::classname()+":"+procname())
   ?
   ? "External block executed from function called from parent class method"
   MAKE_TESTBLOCK(self:cbeval0,self,::classname()+":"+procname())
   ?
   /***************************************************************/

   errorBlock(cbErr)
   return


static function errHandler( oErr )
   ? "[ Error:", hb_ntos( oErr:gencode ), "/", hb_ntos( oErr:subcode ), ;
                 oErr:description, oErr:operation, "]"
   break( oErr )
   RETURN NIL


STATIC PROCEDURE EXTEVAL( cb, o )
   EXTEVAL1( cb, o )
   RETURN

STATIC PROCEDURE EXTEVAL1( cb, o )
   EXTEVAL2( cb, o )
   RETURN

STATIC PROCEDURE EXTEVAL2( cb, o )
   EXTEVAL3( cb, o )
   RETURN

STATIC PROCEDURE EXTEVAL3( cb, o )
   EVAL( cb, o )
   RETURN





CREATE CLASS CLSX
EXPORTED:
   METHOD   TESTX
   METHOD   CBINTX
   METHOD   CBEVALX
   METHOD   CBXEVALX
ENDCLASS

METHOD TESTX( o )
   MAKE_TEST( o, "clsx" )
   RETURN self

METHOD CBINTX( o )
   MAKE_TESTBLOCK(eval,o,::classname()+":"+procname())
   RETURN self

METHOD CBEVALX( cb, o )
   EVAL( cb, o )
   RETURN self

METHOD CBXEVALX( cb, o )
   EXTEVAL( cb, o )
   RETURN self


CREATE CLASS CLS0
EXPORTED:
   METHOD   TEST0
   METHOD   CBINT0
   METHOD   CBEVL0
   METHOD   CBFEVL0
   METHOD   CBEVAL0
   METHOD   CBXEVAL0
   MESSAGE  b0hVar   INLINE ::hVar  := ::classname()+":"+procname()
   MESSAGE  b0hrVar  INLINE ::hrVar := ::classname()+":"+procname()
   MESSAGE  b0pVar   INLINE ::pVar  := ::classname()+":"+procname()
   MESSAGE  b0prVar  INLINE ::prVar := ::classname()+":"+procname()
   MESSAGE  b0eVar   INLINE ::eVar  := ::classname()+":"+procname()
   MESSAGE  b0erVar  INLINE ::erVar := ::classname()+":"+procname()
ENDCLASS

METHOD TEST0
   MAKE_TEST(self,"cls0")
   RETURN self

METHOD CBINT0
   MAKE_TESTBLOCK(eval,self,::classname()+":"+procname())
   RETURN self

METHOD CBEVL0()
   MAKE_TESTBLOCK(exteval,self,::classname()+":"+procname())
   RETURN self

METHOD CBFEVL0(o)
   MAKE_TESTBLOCK(o:cbevalx,self,::classname()+":"+procname())
   RETURN self

METHOD CBEVAL0( cb, o )
   EVAL( cb, o )
   RETURN self

METHOD CBXEVAL0( cb, o )
   EXTEVAL( cb, o )
   RETURN self



CREATE CLASS CLS1 FROM CLS0
HIDDEN:
   VAR      hVar
   VAR      hrVar READONLY
PROTECTED:
   VAR      pVar
   VAR      prVar READONLY
EXPORTED:
   VAR      eVar
   VAR      erVar READONLY

   METHOD   TEST1
   METHOD   CBINT1
   METHOD   CBEVL1
   METHOD   CBFEVL1
   METHOD   CBEVAL1
   METHOD   CBXEVAL1

   MESSAGE  b1hVar   INLINE ::hVar  := ::classname()+":"+procname()
   MESSAGE  b1hrVar  INLINE ::hrVar := ::classname()+":"+procname()
   MESSAGE  b1pVar   INLINE ::pVar  := ::classname()+":"+procname()
   MESSAGE  b1prVar  INLINE ::prVar := ::classname()+":"+procname()
   MESSAGE  b1eVar   INLINE ::eVar  := ::classname()+":"+procname()
   MESSAGE  b1erVar  INLINE ::erVar := ::classname()+":"+procname()
ENDCLASS


METHOD TEST1
   MAKE_TEST(self,"cls1")
   RETURN self

METHOD CBINT1
   MAKE_TESTBLOCK(eval,self,::classname()+":"+procname())
   RETURN self

METHOD CBEVL1()
   MAKE_TESTBLOCK(exteval,self,::classname()+":"+procname())
   RETURN self

METHOD CBFEVL1(o)
   MAKE_TESTBLOCK(o:cbevalx,self,::classname()+":"+procname())
   RETURN self

METHOD CBEVAL1( cb, o )
   EVAL( cb, o )
   RETURN self

METHOD CBXEVAL1( cb, o )
   EXTEVAL( cb, o )
   RETURN self



CREATE CLASS CLS2 FROM CLS1
EXPORTED:
   METHOD   TEST2
   METHOD   TEST3
   METHOD   CBINT2
   METHOD   CBEVL2
   METHOD   CBFEVL2
   METHOD   CBEVAL2
   METHOD   CBXEVAL2

   MESSAGE  b2hVar   INLINE ::hVar  := ::classname()+":"+procname()
   MESSAGE  b2hrVar  INLINE ::hrVar := ::classname()+":"+procname()
   MESSAGE  b2pVar   INLINE ::pVar  := ::classname()+":"+procname()
   MESSAGE  b2prVar  INLINE ::prVar := ::classname()+":"+procname()
   MESSAGE  b2eVar   INLINE ::eVar  := ::classname()+":"+procname()
   MESSAGE  b2erVar  INLINE ::erVar := ::classname()+":"+procname()
ENDCLASS

METHOD TEST2
   MAKE_TEST(self,"cls2")
   RETURN self

METHOD TEST3
   LOCAL oSuper := self:super
   MAKE_TEST(oSuper,"supr")
   RETURN self

METHOD CBINT2
   MAKE_TESTBLOCK(eval,self,::classname()+":"+procname())
   RETURN self

METHOD CBEVL2()
   MAKE_TESTBLOCK(exteval,self,::classname()+":"+procname())
   RETURN self

METHOD CBFEVL2(o)
   MAKE_TESTBLOCK(o:cbevalx,self,::classname()+":"+procname())
   RETURN self

METHOD CBEVAL2( cb, o )
   EVAL( cb, o )
   RETURN self

METHOD CBXEVAL2( cb, o )
   EXTEVAL( cb, o )
   RETURN self




/*

*************************************************************************
  Below are results Harbour when compiled with -DHB_CLASSY_BLOCK_SCOPE
  They are exactly the same as from Class(y).
  Tested with:
   2006-09-16 23:20 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
=========================================================================

 2006-09-16 23:17:33 Harbour Alpha build 46.2 Intl. (Flex) Linux 2.6.15 i686

 Class Access:
       hVar:=  cls1
       hVar    cls1
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       hrVar   NIL
       pVar:=  cls1
       pVar    cls1
       prVar:= cls1
       prVar   cls1
       eVar:=  cls1
       eVar    cls1
       erVar:= cls1
       erVar   cls1

 Parent Class Access:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:HRVAR ]
       pVar:=  cls0
       pVar    cls0
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       prVar   cls1
       eVar:=  cls0
       eVar    cls0
       erVar:= cls0
       erVar   cls0

 Subclass Access:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:HRVAR ]
       pVar:=  cls2
       pVar    cls2
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       prVar   cls1
       eVar:=  cls2
       eVar    cls2
       erVar:= cls2
       erVar   cls2

 Subclass Super Access:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:HRVAR ]
       pVar:=  supr
       pVar    supr
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       prVar   cls1
       eVar:=  supr
       eVar    supr
       erVar:= supr
       erVar   supr

 Foreign Class Access:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:HRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:_PVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:PVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:PRVAR ]
       eVar:=  clsx
       eVar    clsx
 [ Error: 13 / 42 Scope violation (protected) CLS2:_ERVAR ]
       erVar   supr

 Extern Access:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:HRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:_PVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:PVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:PRVAR ]
       eVar:=  extr
       eVar    extr
 [ Error: 13 / 42 Scope violation (protected) CLS2:_ERVAR ]
       erVar   supr


 Inline Class Access:
       hVar:=  CLS2:(b)CLS1
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CLS1
       prVar:= CLS2:(b)CLS1
       eVar:=  CLS2:(b)CLS1
       erVar:= CLS2:(b)CLS1

 Inline Parent Class Access:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CLS0
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CLS0
       erVar:= CLS2:(b)CLS0

 Inline Subclass Access:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CLS2
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CLS2
       erVar:= CLS2:(b)CLS2


 Access from class method by
 eval() with block created in class method:
       hVar:=  CLS2:(b)CBINT1
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBINT1
       prVar:= CLS2:(b)CBINT1
       eVar:=  CLS2:(b)CBINT1
       erVar:= CLS2:(b)CBINT1

 Access from subclass method by
 eval() with block created in subclass method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBINT2
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBINT2
       erVar:= CLS2:(b)CBINT2

 Access from parent class method by
 eval() with block created in parent class method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBINT0
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBINT0
       erVar:= CLS2:(b)CBINT0

 Access from foreign class method by
 eval() with block created in foreign class method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:_PVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBINTX
 [ Error: 13 / 42 Scope violation (protected) CLS2:_ERVAR ]

 Access from function executed from class method by
 eval() block created in class method:
       hVar:=  CLS2:(b)CBEVL1
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBEVL1
       prVar:= CLS2:(b)CBEVL1
       eVar:=  CLS2:(b)CBEVL1
       erVar:= CLS2:(b)CBEVL1

 Access from function executed from subclass method by
 eval() with block created in subclass method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBEVL2
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBEVL2
       erVar:= CLS2:(b)CBEVL2

 Access from function executed from parent class method by
 eval() with block created in parent class method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBEVL0
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBEVL0
       erVar:= CLS2:(b)CBEVL0

 Access from foreign object method executed from class method by
 eval() block created in class method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:_PVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBFEVL1
 [ Error: 13 / 42 Scope violation (protected) CLS2:_ERVAR ]

 Access from foreign object method executed from subclass method by
 eval() with block created in subclass method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:_PVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBFEVL2
 [ Error: 13 / 42 Scope violation (protected) CLS2:_ERVAR ]

 Access from foreign object method executed from parent class method by
 eval() with block created in parent class method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:_PVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBFEVL0
 [ Error: 13 / 42 Scope violation (protected) CLS2:_ERVAR ]

 External block executed from class method
       hVar:=  CLS2:(b)MAIN
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)MAIN
       prVar:= CLS2:(b)MAIN
       eVar:=  CLS2:(b)MAIN
       erVar:= CLS2:(b)MAIN

 External block executed from subclass method
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)MAIN
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)MAIN
       erVar:= CLS2:(b)MAIN

 External block executed from parent class method
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)MAIN
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)MAIN
       erVar:= CLS2:(b)MAIN

 External block access
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:_PVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)MAIN
 [ Error: 13 / 42 Scope violation (protected) CLS2:_ERVAR ]

 External block executed from function called from class method
       hVar:=  CLS2:(b)MAIN
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)MAIN
       prVar:= CLS2:(b)MAIN
       eVar:=  CLS2:(b)MAIN
       erVar:= CLS2:(b)MAIN

 External block executed from function called from subclass method
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)MAIN
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)MAIN
       erVar:= CLS2:(b)MAIN

 External block executed from function called from parent class method
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)MAIN
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)MAIN
       erVar:= CLS2:(b)MAIN


*************************************************************************
  And now Harbour results when compiled with -DHB_REAL_BLOCK_SCOPE.
  Tested with:
   2006-09-16 23:20 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
=========================================================================

 2006-09-16 23:18:59 Harbour Alpha build 46.2 Intl. (Flex) Linux 2.6.15 i686

 Class Access:
       hVar:=  cls1
       hVar    cls1
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       hrVar   NIL
       pVar:=  cls1
       pVar    cls1
       prVar:= cls1
       prVar   cls1
       eVar:=  cls1
       eVar    cls1
       erVar:= cls1
       erVar   cls1

 Parent Class Access:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:HRVAR ]
       pVar:=  cls0
       pVar    cls0
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       prVar   cls1
       eVar:=  cls0
       eVar    cls0
       erVar:= cls0
       erVar   cls0

 Subclass Access:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:HRVAR ]
       pVar:=  cls2
       pVar    cls2
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       prVar   cls1
       eVar:=  cls2
       eVar    cls2
       erVar:= cls2
       erVar   cls2

 Subclass Super Access:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:HRVAR ]
       pVar:=  supr
       pVar    supr
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       prVar   cls1
       eVar:=  supr
       eVar    supr
       erVar:= supr
       erVar   supr

 Foreign Class Access:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:HRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:_PVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:PVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:PRVAR ]
       eVar:=  clsx
       eVar    clsx
 [ Error: 13 / 42 Scope violation (protected) CLS2:_ERVAR ]
       erVar   supr

 Extern Access:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:HRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:_PVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:PVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:PRVAR ]
       eVar:=  extr
       eVar    extr
 [ Error: 13 / 42 Scope violation (protected) CLS2:_ERVAR ]
       erVar   supr


 Inline Class Access:
       hVar:=  CLS2:(b)CLS1
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CLS1
       prVar:= CLS2:(b)CLS1
       eVar:=  CLS2:(b)CLS1
       erVar:= CLS2:(b)CLS1

 Inline Parent Class Access:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CLS0
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CLS0
       erVar:= CLS2:(b)CLS0

 Inline Subclass Access:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CLS2
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CLS2
       erVar:= CLS2:(b)CLS2


 Access from class method by
 eval() with block created in class method:
       hVar:=  CLS2:(b)CBINT1
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBINT1
       prVar:= CLS2:(b)CBINT1
       eVar:=  CLS2:(b)CBINT1
       erVar:= CLS2:(b)CBINT1

 Access from subclass method by
 eval() with block created in subclass method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBINT2
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBINT2
       erVar:= CLS2:(b)CBINT2

 Access from parent class method by
 eval() with block created in parent class method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBINT0
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBINT0
       erVar:= CLS2:(b)CBINT0

 Access from foreign class method by
 eval() with block created in foreign class method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:_PVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBINTX
 [ Error: 13 / 42 Scope violation (protected) CLS2:_ERVAR ]

 Access from function executed from class method by
 eval() block created in class method:
       hVar:=  CLS2:(b)CBEVL1
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBEVL1
       prVar:= CLS2:(b)CBEVL1
       eVar:=  CLS2:(b)CBEVL1
       erVar:= CLS2:(b)CBEVL1

 Access from function executed from subclass method by
 eval() with block created in subclass method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBEVL2
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBEVL2
       erVar:= CLS2:(b)CBEVL2

 Access from function executed from parent class method by
 eval() with block created in parent class method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBEVL0
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBEVL0
       erVar:= CLS2:(b)CBEVL0

 Access from foreign object method executed from class method by
 eval() block created in class method:
       hVar:=  CLS2:(b)CBFEVL1
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBFEVL1
       prVar:= CLS2:(b)CBFEVL1
       eVar:=  CLS2:(b)CBFEVL1
       erVar:= CLS2:(b)CBFEVL1

 Access from foreign object method executed from subclass method by
 eval() with block created in subclass method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBFEVL2
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBFEVL2
       erVar:= CLS2:(b)CBFEVL2

 Access from foreign object method executed from parent class method by
 eval() with block created in parent class method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBFEVL0
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBFEVL0
       erVar:= CLS2:(b)CBFEVL0

 External block executed from class method
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:_PVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)MAIN
 [ Error: 13 / 42 Scope violation (protected) CLS2:_ERVAR ]

 External block executed from subclass method
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:_PVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)MAIN
 [ Error: 13 / 42 Scope violation (protected) CLS2:_ERVAR ]

 External block executed from parent class method
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:_PVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)MAIN
 [ Error: 13 / 42 Scope violation (protected) CLS2:_ERVAR ]

 External block access
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:_PVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)MAIN
 [ Error: 13 / 42 Scope violation (protected) CLS2:_ERVAR ]

 External block executed from function called from class method
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:_PVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)MAIN
 [ Error: 13 / 42 Scope violation (protected) CLS2:_ERVAR ]

 External block executed from function called from subclass method
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:_PVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)MAIN
 [ Error: 13 / 42 Scope violation (protected) CLS2:_ERVAR ]

 External block executed from function called from parent class method
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:_PVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)MAIN
 [ Error: 13 / 42 Scope violation (protected) CLS2:_ERVAR ]

*/
