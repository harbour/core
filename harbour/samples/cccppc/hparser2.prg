/*
 * $Id$
 */

//*******************************************************************
// hparser2.prg: Hessmark parser(2): a '#'-al kezdìdì sorok elemzÇse.
// 1999, Csisz†r Levente

//*******************************************************************
#include "objgen.ch"

//*******************************************************************
#define _STRICT_PARENT_
#include "token.och"
//#include "mmarker.och"
//#include "rmarker.och"
//#include "maltrset.och"
//#include "rsmmarkr.och"
//#include "ralter.och"
#include "edefdict.och"
#include "defdict.och"
//#include "extrdict.och"
//#include "xtrdict.och"
// #include "freader.och"
//#include "incl.och"
//#include "prserr.och"
//*******************************************************************
// #include "cr_lf.ch"
#include "ctoken.ch"
// #include "prserr.ch"

//*******************************************************************
#include "hparser.ch"

//*******************************************************************
#include "reader.och"
#include "treader.och"
#include "hparser.och"

//*******************************************************************
#xtranslate this:<m> => HPARSER.(this):<m>

//*******************************************************************
function hpr_prsDefine(this,mNameToken)
// A # define elemzìje.
local state,tkId,tkStr
local name,params
local ujsor

#ifdef ID_STRING
   #define ST_D_START             "start"
   #define ST_D_TORZS             "torzs"
   #define ST_D_PARAM_VAGY_TORZS  "param_vagy_torzs"
   #define ST_D_TORZS_START       "torzs_start"
   #define ST_D_PARAM_START       "param_start"
   #define ST_D_PARAM_NEVUTAN     "param_nevutan"
   #define ST_D_PARAM_VESSZOUTAN  "param_vesszoutan"
#else
   #define ST_D_START             1
   #define ST_D_TORZS             2
   #define ST_D_PARAM_VAGY_TORZS  3
   #define ST_D_TORZS_START       4
   #define ST_D_PARAM_START       5
   #define ST_D_PARAM_NEVUTAN     6
   #define ST_D_PARAM_VESSZOUTAN  7
#endif               
   state:=ST_D_START
   this:rds()
   while(this:item!=nil)    
      tkId:=TOKEN.(this:item):id
      tkStr:=TOKEN.(this:item):str
      ujsor:=tkId==TKID_UJSOR .or.;
             tkId==TKID_BOS .or. tkId==TKID_EOS
      if (state==ST_D_START)
         if (ujsor)
            // Hib†s Çs vÇge is van.
            this:errorgen(mNameToken,HPRERR_SDEFINE)
            exit
         elseif (tkId==TKID_URES)
            // Eldobjuk.
            state:=ST_D_START
         elseif (tkId==TKID_NEV)
            // Ez a neve
            name:=tkStr
            state:=ST_D_PARAM_VAGY_TORZS
         else
            // Hib†s.
            this:errorgen(this:item,HPRERR_SDEFINE)
            exit
         endif
      elseif (state==ST_D_PARAM_VAGY_TORZS)
         if (ujsor)
            // A defin°ci¢ Åres.
            this:unrds()
            this:clearParserbuffer()
            this:addDefine(mNameToken,name,params)
            // this:putParserBuffer(this:item)
            exit
         elseif (tkId==TKID_URES)
            // ètmegyÅnk a tîrzshîz.
            this:clearParserbuffer()
            state:=ST_D_TORZS_START
         elseif (tkId==TKID_CHAR .and. tkStr=="(")
            // A paramÇterek.
            state:=ST_D_PARAM_START
         else
            // A tîrzs.
            this:clearParserBuffer()
            this:putParserBuffer(this:item)
            state:=ST_D_TORZS
         endif
      elseif (state==ST_D_PARAM_START)
         // '(' ut†n. NÇv vagy ')' jîhet.
         params:={}
         if (tkId==TKID_URES)
            // Eldobjuk.
         elseif (tkId==TKID_CHAR .and. tkStr==")")
            // Kezdìdik a tîrzs.
            this:clearParserbuffer()
            state:=ST_D_TORZS_START
         elseif (tkId==TKID_NEV)
            // ParamÇter.
            aadd(params,tkStr)
            state:=ST_D_PARAM_NEVUTAN
         else
            // B†rmi m†s, az nem j¢! Ebben benne van az £j sor is!
            // Hib†s Çs vÇge is van.
            this:errorgen(mNameToken,HPRERR_LDEFINE)
            exit
         endif
      elseif (state==ST_D_PARAM_NEVUTAN)
         // ParamÇterlist†ban egy nÇv ut†n.
         // ',' vagy ')' jîhet.
         if (tkId==TKID_URES)
            // Eldobjuk.
         elseif (tkId==TKID_CHAR .and. tkStr==")")
            // Kezdìdik a tîrzs.
            // öres paramÇter lista.
            this:clearParserBuffer()
            state:=ST_D_TORZS_START
         elseif (tkId==TKID_CHAR .and. tkStr==",")
            // Kezdìdik a kîvetkezì paramÇter.
            state:=ST_D_PARAM_VESSZOUTAN
         else
            // B†rmi m†s, az nem j¢! Ebben benne van az £j sor is!
            // Hib†s Çs vÇge is van.
            this:errorgen(mNameToken,HPRERR_PDEFINE)
            exit
         endif
      elseif (state==ST_D_PARAM_VESSZOUTAN)
         // ',' ut†n. Csak nÇv jîhet.
         if (tkId==TKID_URES)
            // Eldobjuk.
         elseif (tkId==TKID_NEV)
            // ParamÇter.
            aadd(params,tkStr)
            state:=ST_D_PARAM_NEVUTAN
         else
            // B†rmi m†s, az nem j¢! Ebben benne van az £j sor is!
            // Hib†s Çs vÇge is van.
            this:errorgen(mNameToken,HPRERR_PDEFINE)
            exit
          endif
      elseif (state==ST_D_TORZS_START)
         // Az Åreseket lenyeljÅk.
         if (tkId==TKID_URES)
            // Eldobjuk.
            this:unputParserBuffer()
         elseif (tkId==TKID_UJSOR .or.;
             tkId==TKID_BOS .or. tkId==TKID_EOS)
            // VÇge a definici¢nak.
            this:unrds()
            this:addDefine(mNameToken,name,params)
            exit
         else
            // B†rmi m†s, indul a tîrzs.
            state:=ST_D_TORZS
          endif
      elseif (state==ST_D_TORZS)
         if (tkId==TKID_UJSOR .or.;
             tkId==TKID_BOS .or. tkId==TKID_EOS)
            // VÇge a definici¢nak.
            this:unrds()
            this:addDefine(mNameToken,name,params)
            exit
         else
            // B†rmi m†s a tîrzs folytat¢dik.
         endif
      else
         ? "HPARSER:parseDefine(): Ismeretlen †llapot: ",state
         errorlevel(1)
         quit
      endif
      this:rds()
   end while
   this:sorNyel()
   
return nil

//*******************************************************************
function hpr_prsUndef(this,mnameToken)
// A # undef elemzìje.

local state

#ifdef ID_STRING
   #define ST_UD_START             "start"
   #define ST_UD_VEGE              "vege"
#else
   #define ST_UD_START             1
   #define ST_UD_VEGE              2
#endif

local tkId,tkStr
local name
local ujsor
               
   state:=ST_UD_START
   this:rds()
   while(this:item!=nil)    
      tkId:=TOKEN.(this:item):id
      tkStr:=TOKEN.(this:item):str
      ujsor:=tkId==TKID_UJSOR .or.;
             tkId==TKID_BOS .or. tkId==TKID_EOS
      if (state==ST_UD_START)
         if (ujsor)
            // Hib†s Çs vÇge is van.
            this:errorgen(mnameToken,HPRERR_SUNDEF)
            exit
         elseif (tkId==TKID_URES)
            // Maradunk.
         elseif (tkId==TKID_NEV)
            // Ez a neve
            name:=tkStr
            state:=ST_UD_VEGE
         else
            // Hib†s.
            this:errorgen(mnameToken,HPRERR_SUNDEF)
            exit
         endif
      elseif (state==ST_UD_VEGE)
         if (ujsor)
            // KÇsz vagyunk.
            this:unrds()
            this:clearParserbuffer()
            // Nem kell hib†t jelezni, ha nincs.
            DEFDICT.(this:defdict):delKey(name)
            exit
         elseif (tkId==TKID_URES)
            // Maradunk.
         else
            // Hib†s
            this:errorgen(this:item,HPRERR_SUNDEF)
            exit
         endif
      else
         ? "HPARSER:parseUndef(): Ismeretlen †llapot: ",state
         errorlevel(1)
         quit
      endif
      this:rds()
   end while
   this:sorNyel()
   
return nil

//*******************************************************************
function hpr_addDefine(this,mnameToken,name,params)
// A tîrzs a parserBuffer-ben van.
// Az mNameToken jelzi, hogy hol volt a defin°ci¢.
local w,prev,errStr

   w:=C.EDEFDICT:onew(name,params,;
                      this:arrayParserBuffer(),;
                      TOKEN.mnameToken:file,;
                      TOKEN.mnameToken:line,;
                      TOKEN.mnameToken:pos)

   this:clearParserBuffer()

   if (nil!=(prev:=DEFDICT.(this:defdict):add(w)))
      // M†r van ilyen makr¢
      errStr:="Previous: "+;
              EDEFDICT.prev:deffilelinepos[1]+"("+;
              toStr(EDEFDICT.prev:deffilelinepos[2])+")"
      this:errorGen(mnameToken,HPRERR_MDUPLICATE,{name,errStr})
   endif

return nil

//*******************************************************************
function hpr_prsIfdef(this,mnameToken,ifdefType)
// A #ifdef/#ifndef  elemzìje.

local state

#ifdef ID_STRING
   #define ST_FD_START             "start"
   #define ST_FD_VEGE              "vege"
#else
   #define ST_FD_START             1
   #define ST_FD_VEGE              2
#endif

local tkId,tkStr
local name
local ujsor
local megvan

   state:=ST_FD_START
   this:rds()
   while(this:item!=nil)    
      tkId:=TOKEN.(this:item):id
      tkStr:=TOKEN.(this:item):str
      ujsor:=tkId==TKID_UJSOR .or.;
             tkId==TKID_BOS .or. tkId==TKID_EOS
      if (state==ST_FD_START)
         if (ujsor)
            // Hib†s Çs vÇge is van.
            this:errorgen(mnameToken,HPRERR_IFDEFNL)
            exit
         elseif (tkId==TKID_URES)
            // Maradunk.
         elseif (tkId==TKID_NEV)
            // Ez a neve
            name:=tkStr
            state:=ST_FD_VEGE
         else
            // Hib†s.
            this:errorgen(this:item,HPRERR_SIFDEF)
            exit
         endif
      elseif (state==ST_FD_VEGE)
         if (ujsor)
            // KÇsz vagyunk.
            this:unrds()
            this:clearParserbuffer()
            if (ifdefType==IFDEFTYPE_NONE)
               // Hamis †gban levì if.
               this:branch:=nil
               aadd(this:ifStack,{IFB_NONEBRANCH,this:branch})
               this:sorNyel()
            else
               megvan:=DEFDICT.(this:defdict):atKey(name)
               this:branch:=if(ifdefType==IFDEFTYPE_IFDEF,megvan!=nil,megvan==nil)
               aadd(this:ifStack,{IFB_IFBRANCH,this:branch})
            endif
            exit
         elseif (tkId==TKID_URES)
            // Maradunk.
         else
            // Hib†s
            this:errorgen(this:item,HPRERR_SIFDEF)
            exit
         endif
      else
         ? "HPARSER:parseIfdef(): Ismeretlen †llapot: ",state
         errorlevel(1)
         quit
      endif
      this:rds()
   end while
   this:sorNyel()
   
return nil

//*******************************************************************
function hpr_prsElse(this,mnameToken)
// A #else  elemzìje.
local w

   if (len(this:ifStack)<=0)
      // Hiba, nincs hozz† if.
      this:errorgen(this:item,HPRERR_ELSE) // Nincs hozz† if
   elseif (this:branch!=nil)
      w:=atail(this:ifStack)
      // A spec-ben tîbb else is lehet, ilyenkor csak megford°tja a 
      // feltÇtelt.
      if (w[1]==IFB_IFBRANCH)
         w[1]:=IFB_ELSEBRANCH
      else
         // Duplik†lt else, A spec-ben ez nem hiba!
         this:incompErrorgen(this:item,HPRERR_ELSE2) // Duplik†lt else
      endif
      this:branch:=w[2]:=!w[2]
   endif
   this:sorNyel()
   
return nil


//*******************************************************************
function hpr_prsEndif(this,mnameToken)
// A #endif  elemzìje.
local w

   if (len(this:ifStack)<=0)
      // Hiba, nincs hozz† if.
      this:errorgen(this:item,HPRERR_ENDIF) // Nincs hozz† if
   else
      adrop(this:ifStack)
      if (len(this:ifStack)<=0)
         this:branch:=.t.
      else
         w:=atail(this:ifStack)
         this:branch:=w[2]
      endif
   endif
   this:sorNyel()
   
return nil

//*******************************************************************



