/*
 * Demonstration/test code for class method scoping
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
               <fn>({| self | QOut("      hVar:= ", ::hVar:=<v>)},<o>)     ;;
            end sequence                                                   ;;
            begin sequence                                                 ;;
               <fn>({| self | QOut("      hrVar:=", ::hrVar:=<v>)},<o>)    ;;
            end sequence                                                   ;;
            begin sequence                                                 ;;
               <fn>({| self | QOut("      pVar:= ", ::pVar:=<v>)},<o>)     ;;
            end sequence                                                   ;;
            begin sequence                                                 ;;
               <fn>({| self | QOut("      prVar:=", ::prVar:=<v>)},<o>)    ;;
            end sequence                                                   ;;
            begin sequence                                                 ;;
               <fn>({| self | QOut("      eVar:= ", ::eVar:=<v>)},<o>)     ;;
            end sequence                                                   ;;
            begin sequence                                                 ;;
               <fn>({| self | QOut("      erVar:=", ::erVar:=<v>)},<o>)    ;;
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


procedure main()

   local cbErr, self, o

   ? OS(), Version(), Date(), Time()
   ?

   o := clsX():new()
   self := cls2():new()
   cbErr := ErrorBlock( {| oErr | errHandler( oErr ) } )

   /* --- */
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
   o:TESTX( self )
   ?
   ? "Extern Access:"
   MAKE_TEST( self, "extr" )
   ?

   /* --- */
   ?
   ? "Inline Class Access:"
   MAKE_TESTINLINE1( self, b1 )
   ?
   ? "Inline Parent Class Access:"
   MAKE_TESTINLINE0( self, b0 )
   ?
   ? "Inline Subclass Access:"
   MAKE_TESTINLINE2( self, b2 )
   ?

   /* --- */
   ?
   ? "Access from class method by"
   ? "Eval() with block created in class method:"
   self:CBINT1()
   ?
   ? "Access from subclass method by"
   ? "Eval() with block created in subclass method:"
   self:CBINT2()
   ?
   ? "Access from parent class method by"
   ? "Eval() with block created in parent class method:"
   self:CBINT0()
   ?
   ? "Access from foreign class method by"
   ? "Eval() with block created in foreign class method:"
   o:CBINTX( self )

   /* --- */
   ?
   ? "Access from function executed from class method by"
   ? "Eval() block created in class method:"
   self:CBEVL1()
   ?
   ? "Access from function executed from subclass method by"
   ? "Eval() with block created in subclass method:"
   self:CBEVL2()
   ?
   ? "Access from function executed from parent class method by"
   ? "Eval() with block created in parent class method:"
   self:CBEVL0()

   /* --- */
   ?
   ? "Access from foreign object method executed from class method by"
   ? "Eval() block created in class method:"
   self:CBFEVL1( o )
   ?
   ? "Access from foreign object method executed from subclass method by"
   ? "Eval() with block created in subclass method:"
   self:CBFEVL2( o )
   ?
   ? "Access from foreign object method executed from parent class method by"
   ? "Eval() with block created in parent class method:"
   self:CBFEVL0( o )

   /* --- */
   ?
   ? "External block executed from class method"
   MAKE_TESTBLOCK( self:cbeval1, self, ::classname() + ":" + ProcName() )
   ?
   ? "External block executed from subclass method"
   MAKE_TESTBLOCK( self:cbeval2, self, ::classname() + ":" + ProcName() )
   ?
   ? "External block executed from parent class method"
   MAKE_TESTBLOCK( self:cbeval0, self, ::classname() + ":" + ProcName() )
   ?
   ? "External block access"
   MAKE_TESTBLOCK( eval, self, ::classname() + ":" + ProcName() )

   /* --- */
   ?
   ? "External block executed from function called from class method"
   MAKE_TESTBLOCK( self:cbeval1, self, ::classname() + ":" + ProcName() )
   ?
   ? "External block executed from function called from subclass method"
   MAKE_TESTBLOCK( self:cbeval2, self, ::classname() + ":" + ProcName() )
   ?
   ? "External block executed from function called from parent class method"
   MAKE_TESTBLOCK( self:cbeval0, self, ::classname() + ":" + ProcName() )
   ?
   /* --- */

   ErrorBlock( cbErr )
   return


static procedure errHandler( oErr )
   ? "[ Error:", hb_ntos( oErr:gencode ), "/", hb_ntos( oErr:subcode ), ;
                 oErr:description, oErr:operation, "]"
   Break( oErr )
   return


static procedure EXTEVAL( cb, o )
   EXTEVAL1( cb, o )
   return

static procedure EXTEVAL1( cb, o )
   EXTEVAL2( cb, o )
   return

static procedure EXTEVAL2( cb, o )
   EXTEVAL3( cb, o )
   return

static procedure EXTEVAL3( cb, o )
   Eval( cb, o )
   return





create class CLSX
exported:
   method   TESTX
   method   CBINTX
   method   CBEVALX
   method   CBXEVALX
endclass

method TESTX( o )
   MAKE_TEST( o, "clsx" )
   return self

method CBINTX( o )
   MAKE_TESTBLOCK( eval, o, ::classname() + ":" + ProcName() )
   return self

method CBEVALX( cb, o )
   Eval( cb, o )
   return self

method CBXEVALX( cb, o )
   EXTEVAL( cb, o )
   return self


create class CLS0
exported:
   method   TEST0
   method   CBINT0
   method   CBEVL0
   method   CBFEVL0
   method   CBEVAL0
   method   CBXEVAL0
   message  b0hVar   inline ::hVar  := ::classname() + ":" + ProcName()
   message  b0hrVar  inline ::hrVar := ::classname() + ":" + ProcName()
   message  b0pVar   inline ::pVar  := ::classname() + ":" + ProcName()
   message  b0prVar  inline ::prVar := ::classname() + ":" + ProcName()
   message  b0eVar   inline ::eVar  := ::classname() + ":" + ProcName()
   message  b0erVar  inline ::erVar := ::classname() + ":" + ProcName()
endclass

method TEST0
   MAKE_TEST( self, "cls0" )
   return self

method CBINT0
   MAKE_TESTBLOCK( eval, self, ::classname() + ":" + ProcName() )
   return self

method CBEVL0()
   MAKE_TESTBLOCK( exteval, self, ::classname() + ":" + ProcName() )
   return self

method CBFEVL0(o)
   MAKE_TESTBLOCK( o:cbevalx, self, ::classname() + ":" + ProcName() )
   return self

method CBEVAL0( cb, o )
   Eval( cb, o )
   return self

method CBXEVAL0( cb, o )
   EXTEVAL( cb, o )
   return self



CREATE CLASS CLS1 INHERIT CLS0
HIDDEN:
   VAR      hVar
   VAR      hrVar READONLY
PROTECTED:
   VAR      pVar
   VAR      prVar READONLY
EXPORTED:
   VAR      eVar
   VAR      erVar READONLY

   method   TEST1
   method   CBINT1
   method   CBEVL1
   method   CBFEVL1
   method   CBEVAL1
   method   CBXEVAL1

   MESSAGE  b1hVar   INLINE ::hVar  := ::classname() + ":" + ProcName()
   MESSAGE  b1hrVar  INLINE ::hrVar := ::classname() + ":" + ProcName()
   MESSAGE  b1pVar   INLINE ::pVar  := ::classname() + ":" + ProcName()
   MESSAGE  b1prVar  INLINE ::prVar := ::classname() + ":" + ProcName()
   MESSAGE  b1eVar   INLINE ::eVar  := ::classname() + ":" + ProcName()
   MESSAGE  b1erVar  INLINE ::erVar := ::classname() + ":" + ProcName()
ENDCLASS


method TEST1
   MAKE_TEST( self, "cls1" )
   return self

method CBINT1
   MAKE_TESTBLOCK( eval, self, ::classname() + ":" + ProcName() )
   return self

method CBEVL1()
   MAKE_TESTBLOCK( exteval, self, ::classname() + ":" + ProcName() )
   return self

method CBFEVL1(o)
   MAKE_TESTBLOCK( o:cbevalx, self, ::classname() + ":" + ProcName() )
   return self

method CBEVAL1( cb, o )
   Eval( cb, o )
   return self

method CBXEVAL1( cb, o )
   EXTEVAL( cb, o )
   return self



CREATE CLASS CLS2 INHERIT CLS1
EXPORTED:
   method   TEST2
   method   TEST3
   method   CBINT2
   method   CBEVL2
   method   CBFEVL2
   method   CBEVAL2
   method   CBXEVAL2

   MESSAGE  b2hVar   INLINE ::hVar  := ::classname() + ":" + ProcName()
   MESSAGE  b2hrVar  INLINE ::hrVar := ::classname() + ":" + ProcName()
   MESSAGE  b2pVar   INLINE ::pVar  := ::classname() + ":" + ProcName()
   MESSAGE  b2prVar  INLINE ::prVar := ::classname() + ":" + ProcName()
   MESSAGE  b2eVar   INLINE ::eVar  := ::classname() + ":" + ProcName()
   MESSAGE  b2erVar  INLINE ::erVar := ::classname() + ":" + ProcName()
ENDCLASS

method TEST2
   MAKE_TEST( self, "cls2" )
   return self

method TEST3
   LOCAL oSuper := self:super
   MAKE_TEST( oSuper, "supr" )
   return self

method CBINT2
   MAKE_TESTBLOCK( eval, self, ::classname() + ":" + ProcName() )
   return self

method CBEVL2()
   MAKE_TESTBLOCK( exteval, self, ::classname() + ":" + ProcName() )
   return self

method CBFEVL2(o)
   MAKE_TESTBLOCK( o:cbevalx, self, ::classname() + ":" + ProcName() )
   return self

method CBEVAL2( cb, o )
   Eval( cb, o )
   return self

method CBXEVAL2( cb, o )
   EXTEVAL( cb, o )
   return self




/*
  -------------------------------------------------------------------------
  Below are results Harbour when compiled with -DHB_CLASSY_BLOCK_SCOPE
  They are exactly the same as from Class(y).
  Tested with:
   2006-09-16 23:20 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
  -------------------------------------------------------------------------

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
 Eval() with block created in class method:
       hVar:=  CLS2:(b)CBINT1
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBINT1
       prVar:= CLS2:(b)CBINT1
       eVar:=  CLS2:(b)CBINT1
       erVar:= CLS2:(b)CBINT1

 Access from subclass method by
 Eval() with block created in subclass method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBINT2
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBINT2
       erVar:= CLS2:(b)CBINT2

 Access from parent class method by
 Eval() with block created in parent class method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBINT0
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBINT0
       erVar:= CLS2:(b)CBINT0

 Access from foreign class method by
 Eval() with block created in foreign class method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:_PVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBINTX
 [ Error: 13 / 42 Scope violation (protected) CLS2:_ERVAR ]

 Access from function executed from class method by
 Eval() block created in class method:
       hVar:=  CLS2:(b)CBEVL1
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBEVL1
       prVar:= CLS2:(b)CBEVL1
       eVar:=  CLS2:(b)CBEVL1
       erVar:= CLS2:(b)CBEVL1

 Access from function executed from subclass method by
 Eval() with block created in subclass method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBEVL2
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBEVL2
       erVar:= CLS2:(b)CBEVL2

 Access from function executed from parent class method by
 Eval() with block created in parent class method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBEVL0
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBEVL0
       erVar:= CLS2:(b)CBEVL0

 Access from foreign object method executed from class method by
 Eval() block created in class method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:_PVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBFEVL1
 [ Error: 13 / 42 Scope violation (protected) CLS2:_ERVAR ]

 Access from foreign object method executed from subclass method by
 Eval() with block created in subclass method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:_PVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBFEVL2
 [ Error: 13 / 42 Scope violation (protected) CLS2:_ERVAR ]

 Access from foreign object method executed from parent class method by
 Eval() with block created in parent class method:
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


  -------------------------------------------------------------------------
  And now Harbour results when compiled with -DHB_REAL_BLOCK_SCOPE.
  Tested with:
   2006-09-16 23:20 UTC+0200 Przemyslaw Czerpak (druzus/at/priv.onet.pl)
  -------------------------------------------------------------------------

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
 Eval() with block created in class method:
       hVar:=  CLS2:(b)CBINT1
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBINT1
       prVar:= CLS2:(b)CBINT1
       eVar:=  CLS2:(b)CBINT1
       erVar:= CLS2:(b)CBINT1

 Access from subclass method by
 Eval() with block created in subclass method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBINT2
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBINT2
       erVar:= CLS2:(b)CBINT2

 Access from parent class method by
 Eval() with block created in parent class method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBINT0
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBINT0
       erVar:= CLS2:(b)CBINT0

 Access from foreign class method by
 Eval() with block created in foreign class method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
 [ Error: 13 / 42 Scope violation (protected) CLS2:_PVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBINTX
 [ Error: 13 / 42 Scope violation (protected) CLS2:_ERVAR ]

 Access from function executed from class method by
 Eval() block created in class method:
       hVar:=  CLS2:(b)CBEVL1
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBEVL1
       prVar:= CLS2:(b)CBEVL1
       eVar:=  CLS2:(b)CBEVL1
       erVar:= CLS2:(b)CBEVL1

 Access from function executed from subclass method by
 Eval() with block created in subclass method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBEVL2
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBEVL2
       erVar:= CLS2:(b)CBEVL2

 Access from function executed from parent class method by
 Eval() with block created in parent class method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBEVL0
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBEVL0
       erVar:= CLS2:(b)CBEVL0

 Access from foreign object method executed from class method by
 Eval() block created in class method:
       hVar:=  CLS2:(b)CBFEVL1
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBFEVL1
       prVar:= CLS2:(b)CBFEVL1
       eVar:=  CLS2:(b)CBFEVL1
       erVar:= CLS2:(b)CBFEVL1

 Access from foreign object method executed from subclass method by
 Eval() with block created in subclass method:
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HVAR ]
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_HRVAR ]
       pVar:=  CLS2:(b)CBFEVL2
 [ Error: 13 / 41 Scope violation (hidden) CLS2:_PRVAR ]
       eVar:=  CLS2:(b)CBFEVL2
       erVar:= CLS2:(b)CBFEVL2

 Access from foreign object method executed from parent class method by
 Eval() with block created in parent class method:
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
