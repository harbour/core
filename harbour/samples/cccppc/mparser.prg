/*
 * $Id$
 */

//*******************************************************************
// mparser.prg: A mparser oszt†ly implement†ci¢ja.
// 1999, Csisz†r Levente

// A sorokban a makr¢kat (define, command, xcommand, translate,
// xtranslate) helyettes°ti.
// A PARSER-tìl îrîkîl.
// A nÇv Çs a sor (lparser) elemzì †ltal kÇsz°tett tokeneket v†r az 
// inputr¢l. èltal†ban a hparser ut†n van.


/*
   Algoritmus:
      Olvas az inputj†r¢l addig, am°g el nem dînti, hogy 
      helyettes°teni kell vagy nem. Ha igen, akkor a helyettes°tÇs
      eredmÇnyÇt visszarakja az inputj†ra, ha pedig nem
      kellett helyettes°teni, akkor azt kirakja az outputra.
      Ezekut†n mindig nil-t ad olvas†sra.
*/

// #define DEBUG
//*******************************************************************
#include "debug.ch"


//*******************************************************************
#include "objgen.ch"

//*******************************************************************
#define _STRICT_PARENT_
#include "token.och"
#include "tokenst.och"
#include "edefdict.och"
#include "defdict.och"
#include "extrdict.och"
#include "xtrdict.och"
#include "reader.och"
#include "mmarker.och"
#include "tbuffer.och"
// #include "prtree.och"

#include "maltrset.och"
#include "rsmmarkr.och"
// #include "prserr.och"

//*******************************************************************
#include "cr_lf.ch"
#include "ctoken.ch"
// #include "prserr.ch"

//*******************************************************************
// ôsszehasonl°tja a (tkId, tkStr)-t egy karaterrel.
#define eqTkChar(tkId,tkStr,aChar)         ((tkId)==TKID_CHAR .and.;
                                           (tkStr)==(aChar))
                                    
//*******************************************************************
// MegnÇzi, hogy, ha a token TKID_CHAR, akkor a tkStr benne van-e
// az aString-ben.
#define eqTkInCharList(tkId,tkStr,aString) ((tkId)==TKID_CHAR .and.;
                                            (tkStr)$(aString))


#define PVT_IDX    1
#define PVT_NAME   2

//*******************************************************************
#define _MPARSER_PRG_
#define _IMPLEMENT_ONEW_

#include "mparser.och"

//*******************************************************************
// implement oinit(inputReader,name,defdict,errorStream)
implement oinit(inputReader,name,errorStream)
           
   super:oinit(inputReader,name,errorStream)
   // this:errorItem:=nil
   // this:defdict:=defdict
   
return this
   
//*******************************************************************
implement startMakroBuf(item)
   if (item==nil)
      this:makroBuf:={}
   else
      this:makroBuf:={item}
   endif
return nil

//*******************************************************************
implement rdsMakroBuf()
local w

   w:=this:rds()
   if (this:item!=nil)
      aadd(this:makroBuf,this:item)
   endif
return w
 
//*******************************************************************
implement unrdsMakroBuf(n)
local w,m

   w:=this:unrds(n)
   m:=len(this:makroBuf)-w
   asize(this:makroBuf,if(m>0,m,0))
   
return w

//*******************************************************************
implement readItem()
   outerr("MPARSER.o:readItem(): Ez a mñvelet nem h°vhat¢!",crlf())
return super:readItem()

#ifdef OLD
local w,tkId,tkStr
   
   /*
      Olvas a puffer-be:
      - ha nÇv, Çs az szerepel a define sz¢t†rban, akkor
         akkor ind°t egy define elemzìt.
         Ez az elemzì elmenti a parserBuffer-t, majd elvÇgzi
         a define objektum elemzÇsÇt.
   */

   while(nil==(w:=this:getParserBuffer()) .and.;
         nil!=(w:=super:readInput()) .and.;
         TOKEN.w:id==TKID_NEV)
                     
      this:item:=w
      // Ez itt nem jîhet ki!
      outerr("mparser: readItem",crlf())
      this:putParserBuffer(w)
      this:parseFun()
      
   end while
return w
#endif

//*******************************************************************
implement parseFun(edefdict)
/*
 Ez vÇgzi a tÇnyleges elemzÇst.
 A this:item-t elemzi, szÅksÇg esetÇn mÇg olvashat.
 Elemezi az inputon a <nÇv>'('<param1>,...')' paramÇtereket. 
 Az edefdict-nek a <nÇv>-hez tartoz¢ makr¢ defin°ci¢nak kell
 lennie.

 Ret: {sikeres,itemLista}
 Ha sikeres volt, akkor a sikeres==.t., Çs az itemLista a csere
    eredmÇnye.
 Ha nem volt sikeres, akkor a sikeres==.f., Çs az itemLista a
    beolvasott (elìreolvasott) itemek list†ja.
 A parserBufferben csak egy token lehet, ami az item-ben is
 van.
*/

// Mj.: ElemzÇskor elfogadja az Åres paramÇtereket is, ezeket csak a
//      helyettes°tÇskor szñrjÅk ki. Ez azÇrt van °gy, mert Åres
//      paramÇter £gy is lehet Åres, hogy maga a token lista csupa
//      Åres tokenbìl †ll.

local state,tkId,tkStr,ujsor
local tList,i
local success,params,currentParam
local parentStack
#define STF_START  "start"
#define STF_EXPR   "expr"
#define STF_PARENT "parent"
               
   tkId:=TOKEN.(this:item):id
   tkStr:=TOKEN.(this:item):str
   
   if (EDEFDICT.edefdict:params==nil)
      // ParamÇterek nincsenek.
      // Ez csak egy <nÇv> -> <token1>,... t°pus£ helyettes°tÇs.
      this:unputParserBuffer()
      return {.t.,EDEFDICT.edefdict:change()}
   endif

   // Most elemezzÅk a '('<param1>,...')' -t.   
                
   parentStack:={} // Itt vannak a z†r¢jelek.
   state:=STF_START
   success:=.f.
   this:rds()
   while(this:item!=nil)    
      tkId:=TOKEN.(this:item):id
      tkStr:=TOKEN.(this:item):str
      ujsor:=tkId==TKID_UJSOR .or.;
             tkId==TKID_BOS .or. tkId==TKID_EOS .or.;
             (tkId==TKID_CHAR .and. tkStr==";")
      
      if (state==STF_START)
         if (tkId==TKID_URES)
            // Maradunk
            // state:=state
         elseif (tkId==TKID_CHAR .and. tkStr=="(")
            // Kezdìdik az elemzÇs.
            currentParam:={}
            params:={}
            state:=STF_EXPR
         else
            // VÇge. Ide tartozik a sorvÇgjel is.
            exit
         endif
      elseif (state==STF_EXPR)
         if (tkId==TKID_UJSOR .or.;
             tkId==TKID_BOS .or. tkId==TKID_EOS .or.;
             (tkId==TKID_CHAR .and. tkStr==";"))
            // VÇge.
            exit
         elseif (tkId==TKID_URES)
            // Maradunk
            // state:=state
            aadd(currentParam,this:item)
         elseif (tkId==TKID_CHAR .and. tkStr==")")
            // VÇge a paramÇter list†nak.
            aadd(params,currentParam)
            success:=.t.
            exit
         elseif (tkId==TKID_CHAR .and. tkStr==",")
            // ój paramÇter kezdìdik.
            aadd(params,currentParam)
            currentParam:={}
            state:=STF_EXPR
         elseif (tkId==TKID_CHAR .and.;
                 (tkStr=="(" .or. tkStr=="{" .or. tkStr=="["))
            // El kell menni a kîvetkezì csuk¢ig, kîzben figyelni,
            // hogy rendesen z†r¢jelezve van-e.
            aadd(currentParam,this:item)
            aadd(parentStack,thisclass:getCloseParent(tkStr))
            state:=STF_PARENT
         else
            // Ez a paramÇterhez tartozik.
            aadd(currentParam,this:item)
         endif
      elseif (state==STF_PARENT)
         if (ujsor)
            // VÇge.
            exit
         elseif (tkId==TKID_URES)
            // Maradunk
            // state:=state
            aadd(currentParam,this:item)
         elseif (tkId==TKID_CHAR .and.;
                 (tkStr=="(" .or. tkStr=="{" .or. tkStr=="["))
            // El kell menni a kîvetkezì csuk¢ig, kîzben figyelni,
            // hogy rendesen z†r¢jelezve van-e.
            aadd(currentParam,this:item)
            aadd(parentStack,thisclass:getCloseParent(tkStr))
            // Maradunk
         elseif (tkId==TKID_CHAR .and. tkStr==alast(parentStack))
            aadd(currentParam,this:item)
            asize(parentStack,len(parentStack)-1)
            if (len(parentStack)==0)
               state:=STF_EXPR
            endif
         elseif (tkId==TKID_NEV)
            aadd(currentParam,this:item)
         else
            // Ez a paramÇterhez tartozik.
            aadd(currentParam,this:item)
         endif
      else
         ? "MPARSER:parseFun(): Ismeretlen †llapot: ",state
         errorlevel(1)
         quit
      endif
      this:rds()
   end while
   
   if (success .and.;
       len(params)==1 .and.;
       0==ascan(params[1],{|x| !TOKEN.x:id==TKID_URES}))
      asize(params,0)
   endif

   if (success .and. len(params)==len(EDEFDICT.edefdict:params))
      // SikerÅlt az elemzÇs, most meg kell csin†lni a cserÇt.
      for i:=1 to len(params)
         params[i]:=thisclass:trimTokenList(params[i])
         if (empty(params[i]))
            return {.f.,this:arrayParserBuffer()}
         endif
      end for
      #ifdef DEBUG
      outerr(crlf())
      outerr("define change: ",EDEFDICT.edefdict:printStr(),crlf())
      for i:=1 to len(params)
         outerr("params[",i,"]: ")
         aeval(params[i],{|t| outerr(TOKEN.t:getStr())})
         outerr(crlf())
      end for
      #endif
      return {.t.,EDEFDICT.edefdict:change(params)}
   endif
   // Nem sikerÅlt az elemzÇs.
return {.f.,this:arrayParserBuffer()}

//*******************************************************************
// implement printDefDict()
// return DEFDICT.(this:defdict):printStr()

//*******************************************************************
cimplement getCloseParent(aChar)
// Ha az aChar egy nyit¢ z†r¢jel, akkor a a csuk¢ p†rj†t adja, 
// egyÇbkÇnt nil-t.

   if (aChar=="(")
      return ")"
   elseif (aChar=="{")
      return "}"
   elseif (aChar=="[")
      return "]"
   endif
return nil

//*******************************************************************
cimplement trimTokenList(tList)
// A tList elejÇrìl Çs vÇgÇrìl elt†vol°tja az Åres tokeneket.
// Ret: a tList.
local i
 
   while(!empty(tList))
      if (TOKEN.(tList[1]):id!=TKID_URES)
         exit
      endif
      adel(tList,1)
      asize(tList,len(tList)-1)
   end while
   
   for i:=len(tList) to 1 step -1
      if (TOKEN.(tList[i]):id!=TKID_URES)
         asize(tList,i)
         exit
      endif
   end for

return tList

//*******************************************************************
cimplement parse(item,inputReader,name,defdict,edefdict,xtrdict,mi,errorStream,ujsor,trPrsAlg)
/*
 Itt az item csak TOKEN lehet vagy nil.
 Az inputReader-nek kell ind°tania az £j elemzìket.
 
 Az algoritmus:
    MegnÇzi a define sz¢t†rban, ha megvan, akkor ind°t egy
    elemzìt r†, ha az sikeres, akkor visszatÇr, ha nem sikeres, akkor
    visszateszi a beolvasott token sort az inputra, elkezdi ind°tani
    r† az xtranslate makr¢kat. 
    Egy xtranslate makr¢ ind°t†sa:
    
       - Ind°tja a makr¢t, ha sikeres, akkor visszatÇr, ha nem, 
         akkor a token sort visszateszi az inputra.

    A maxim†lis hossz£s†g£ tokensort meg kell ìrizni, Çs
    azt adni teljes sikertelensÇg esetÇn.
    M†sik lehetìsÇg, hogy ravaszkodunk: beiktatunk egy †tmeneti
    puffert az inputunk elÇ (ez az mcontrol!), Çs 
    azt adjuk sikertelensÇg esetÇn.
*/

// Nem teljesen korrekt, mert az elemzìt minden alkalommal legy†rtja.

local othis,w,match,xtrList,xcmList
local i,j
   
   if (nil==item)
      return {item}
   endif
      
   match:={.f.,{item}}
   // if (TOKEN.item:id==TKID_NEV .and.;
   //     nil!=(edefdict:=DEFDICT.(defdict):atKey(TOKEN.item:str)))
   if (edefdict!=nil)
       
      othis:=class:onew(inputReader,name,defdict,xtrdict,errorStream)
      othis:item:=item
      othis:putParserBuffer(item)
      match:=othis:parseFun(edefdict)
      // Az othis inputj†ra visszatett tokeneket itt ki kellene venni!
      // Persze most ilyen nem lehet, mert ebbìl az objektumb¢l senki
      // sem olvas.
      if (match[1])
         // Az illeszÇs sikeres volt.
         return match
      endif
   endif

   // #define XTR_SEQ
   if (trPrsAlg==TRPRA_SEQ)
      // Szekvenci†lis illesztÇs.
      xtrList:=XTRDICT.xtrdict:getExtrList(item)
      if (ujsor)
         xcmList:=XTRDICT.xtrdict:getExtrList(item,.t.)
         xtrList:=aconcatenate(xcmList,xtrList)
         //xtrList:=aconcatenate(xtrList,xcmList)
      endif
      
      // H†tulr¢l elìre haladunk.
      for i:=len(xtrList) to 1 step -1
         // Az elsì maga az item, ezÇrt azt nem kell visszatenni.
         for j:=len(match[2]) to 2 step -1
            READER.inputReader:unread(match[2][j])
         end for
         othis:=class:onew(inputReader,name,defdict,xtrdict,errorStream)
         othis:item:=item
         othis:putParserBuffer(item)
         
         match:=othis:parseXtr(xtrList[i])
      
         // Az othis inputj†ra visszatett tokeneket itt ki kellene venni!
         // Persze most ilyen nem lehet, mert ebbìl az objektumb¢l senki
         // sem olvas.
      
         if (match[1])
            // Az illeszÇs sikeres volt.
            return match
         endif
      end for
   elseif (mi[1]!=0 .or. mi[2]!=0)
      // Fa bej†r†sos illesztÇs.
   
      // Elìszîr az xtranslate-eket illesztjÅk.
      for j:=len(match[2]) to 2 step -1
         READER.inputReader:unread(match[2][j])
      end for
      if (mi[1]!=0)
         othis:=class:onew(inputReader,name,defdict,xtrdict,errorStream)
         othis:item:=item
         othis:putParserBuffer(item)
      
         match:=othis:parseXtrTree(XTRDICT.xtrdict:trdictTree,mi[1],.f.)
   
         // Az othis inputj†ra visszatett tokeneket itt ki kellene venni!
         // Persze most ilyen nem lehet, mert ebbìl az objektumb¢l senki
         // sem olvas.
         
         if (match[1])
            // Az illeszÇs sikeres volt.
            return match
         endif
      endif
      
      if (ujsor .and. mi[2]!=0)
         // Azut†n az xcommand-okat.
         for j:=len(match[2]) to 2 step -1
            READER.inputReader:unread(match[2][j])
         end for
         othis:=class:onew(inputReader,name,defdict,xtrdict,errorStream)
         othis:item:=item
         othis:putParserBuffer(item)
         
         match:=othis:parseXtrTree(XTRDICT.xtrdict:cmdictTree,mi[2],.t.)
         
         // Az othis inputj†ra visszatett tokeneket itt ki kellene venni!
         // Persze most ilyen nem lehet, mert ebbìl az objektumb¢l senki
         // sem olvas.
         
         if (match[1])
            // Az illeszÇs sikeres volt.
            return match
         endif
      endif
   // else
   //    for j:=len(match[2]) to 2 step -1
   //       READER.inputReader:unread(match[2][j])
   //    end for
   endif
return match

//*******************************************************************
static function nextLeftToken(o)
// Az o °gy nÇz ki: {iLeft,leftTokenlist}
// Az iLeft mindig a beolvasand¢ra mutat
local t
   
   while(o[1]<=len(o[2]))
      t:=o[2][o[1]]
      o[1]++
      if (TOKEN.t:id!=TKID_URES)
         return t
      endif
   end while
return nil

//*******************************************************************
static function addMatchParam(paramValues,idx,name,tokenList)
   if (paramValues[1]==PVT_IDX)
      if (len(paramValues[2])<idx)
         asize(paramValues[2],idx)
      endif
      if (paramValues[2][idx]==nil)
         paramValues[2][idx]:={tokenList}
      else
         aadd(paramValues[2][idx],tokenList)
      endif
   else
      aadd(paramValues[2],{name,tokenList})
   endif
return nil

//*******************************************************************
implement parseTokenList(leftTokenList,cmd4,paramValues,toEOL)
/*
 Ez vÇgzi a tÇnyleges elemzÇst. Az input folyamot megpr¢b†lja 
 illeszteni a leftTokeList-re.
 A this:item-t elemzi, szÅksÇg esetÇn mÇg olvashat.
 Amikor h°vjuk, akkor a parserBuffer-ben csak a this:item-nek szabad
 lennie.
 Ha a toEOL nem Åres, akkor csak akkor sikeres az illesztÇs, ha
 a sor vÇgig ment.
 
 Ha a cmd4 nem Åres, akkor a neveket 4 hossz£ra v†gva hasonl°tja
 îssze.

 Ret: 0: Ha nem sikerÅlt az illesztÇs.
         Ekkor a parserBuffer-ben vannak a beolvasott tokenek.

      1: Ha sikerÅlt az illesztÇs, de nem ment elìre. (Pl. egy Åres
         alternat°va.

      2: Ha sikerÅlt az illesztÇs Çs elìre is ment.
*/   

/*
   MegyÅnk vÇgig az extrdict bal oldal†n Çs:
   
      - Ha normal token (nem match token Çs nem maltrset), akkor
        annak illeszkednie kell, ha nem, akkor nem illeszkedett
        jelzÇssel visszatÇr.
        
      - Ha match token, akkor arra ind°tunk egy kÅlîn match 
        token elemzìt. Ez az elemzì megkapja a kîvetkezì tokent.
        
*/

// #define STX_START               "START"
#ifdef OLD
local tkId,tkStr,ujsor
local tList,i
local oLeftTList, leftToken, leftTkId, leftTkStr
local match
local w
local nUres//, utNemUresItem

   oLeftTList:={1,leftTokenList}
 
   // state:=STX_START
   // tokenList:={}
   if (nil==(leftToken:=nextLeftToken(oLeftTList)))
      // öres ==> Nem illeszkedik semmire.
      return .f.      
   endif

   nUres:=0
   // utNemUresItem:=this:item
   
   while(this:item!=nil)
      leftTkId:=TOKEN.leftToken:id
      leftTkStr:=TOKEN.leftToken:str
      tkId:=TOKEN.(this:item):id
      tkStr:=TOKEN.(this:item):str
      ujsor:=tkId==TKID_UJSOR .or.;
             tkId==TKID_BOS .or. tkId==TKID_EOS .or.;
             (tkId==TKID_CHAR .and. tkStr==";")
      
      if (tkId==TKID_URES)
         // Maradunk
         // aadd(tokenList,this:item)
         nUres++
      elseif (ujsor)
         // VÇge. 
         // Illeszkedett, ha a leftToken egy alternat°va, mert r† az
         // Åres is illeszkedik, nem illeszkedett, ha nem az.
         // Ha illeszkedett, akkor a vÇgÇrìl az £jsort Çs az Åreseket
         // vissza kell tenni az inputra.
         if (leftTkId==TKID_MALTERSET)
            this:unrds(nUres+1)
            // this:item:=utNemUresItem
            // Itt nem kell a toEOL-t vizsg†lni.
            return .t.
         endif
         exit
      else
         if (leftTkId==TKID_MALTERSET)
            this:mleftXMRToken(leftToken,cmd4,paramValues,nUres)
            match:=.t.
            this:makroBuf:=nil
         elseif (leftTkId==TKID_REGULAR_MATCH_MARKER)
            match:=this:mleftXRMMToken(leftToken,cmd4)
         elseif (leftTkId==TKID_WILD_MATCH_MARKER)
            match:=this:mleftXWMToken(leftToken)
         elseif (leftTkId==TKID_EXT_EXPR_MATCH_MARKER)
            match:=this:mleftXEEMToken(leftToken)
         elseif (leftTkId==TKID_LIST_MATCH_MARKER)
            match:=this:mleftXLMToken(leftToken,cmd4)
         elseif (leftTkId==TKID_RESTRICTED_MATCH_MARKER)
            match:=this:mleftXRSMMToken(leftToken,cmd4)
         else
            // EgyÇb illeszkedÇs.
            match:=this:mleftXNToken(leftToken,cmd4)
            this:makroBuf:=nil
         endif
         nUres:=0
         if (!match)
            // Az illesztÇs nem sikerÅlt ==> VÇge.
            exit
         endif
         // Az illesztÇs sikerÅlt, az eredmÇny a tokenList-ben van,
         // megyÅnk tov†bb.
         if (this:makroBuf!=nil)
            addMatchParam(paramValues,;
                          MMARKER.leftToken:mNum,;
                          MMARKER.leftToken:getName(),;
                          this:makroBuf)
         endif
         leftToken:=nextLeftToken(oLeftTList)
      endif
      if (nil==leftToken)
         // VÇgig ÇrtÅnk a tokenlist†n.
         // Ha a toEOL nem Åres, akkor meg kell nÇzni, hogy a sor 
         // vÇgÇn vagyunk-e vagy a sor vÇgÇig csak Åresek vannak-e.
         if (!empty(toEOL))
            this:rds()
            while(this:item!=nil)
               tkId:=TOKEN.(this:item):id
               tkStr:=TOKEN.(this:item):str
               ujsor:=tkId==TKID_UJSOR .or.;
                      tkId==TKID_BOS .or. tkId==TKID_EOS .or.;
                      (tkId==TKID_CHAR .and. tkStr==";")
               if (tkId==TKID_URES)
                  // MegyÅnk tov†bb.
                  nUres++
               elseif (ujsor)
                  this:unrds(nUres+1)
                  return .t.
               else
                  return .f.
               endif
               this:rds()
            end while
         endif
         return .t.
      endif
      this:rds()
   end while
   
return .f.
#endif

local oLeftTList,leftToken,retVal,gone

   oLeftTList:={1,leftTokenList}

   if (nil==(leftToken:=nextLeftToken(oLeftTList)))
      // öres ==> Nem illeszkedik semmire.
      return 0
   endif

   gone:=.f.   
   while(0!=(retVal:=matchLeftToken(this,leftToken,cmd4,paramValues,toEOL)))
      if (retVal==2)
         gone:=.t.
      endif
      if (nil==(leftToken:=nextLeftToken(oLeftTList)))
         if (nilLeftToken(this,toEOL))
            return if(gone,2,1)
         endif
         // Nem illeszkedett.
         return 0
      endif
      this:rds()
   end while

   // Az illesztÇs nem sikerÅlt.
return 0

//*******************************************************************
static function nilLeftToken(this,toEOL)
// Akkor kell h°vni, amikor a leftToken nil
local tkId,tkStr,ujsor,nUres
                       
   nUres:=0
   // VÇgig ÇrtÅnk a tokenlist†n.
   // Ha a toEOL nem Åres, akkor meg kell nÇzni, hogy a sor 
   // vÇgÇn vagyunk-e vagy a sor vÇgÇig csak Åresek vannak-e.
   if (!empty(toEOL))
      this:rds()
      while(this:item!=nil)
         tkId:=TOKEN.(this:item):id
         tkStr:=TOKEN.(this:item):str
         ujsor:=tkId==TKID_UJSOR .or.;
                tkId==TKID_BOS .or. tkId==TKID_EOS .or.;
                (tkId==TKID_CHAR .and. tkStr==";")
         if (tkId==TKID_URES)
            // MegyÅnk tov†bb.
            nUres++
         elseif (ujsor)
            this:unrds(nUres+1)
            return .t.
         else
            // Az illesztÇs nem sikerÅlt.
            return .f.
         endif
         this:rds()
      end while
   endif
return .t.

//*******************************************************************
static function matchLeftToken(this,leftToken,cmd4,paramValues,toEOL)
// Illeszt egy matchToken-t az inputra.
// A leftToken-nek nem nil-nek kell lennie.
// Ret: 0: Ha nem sikerÅlt az illesztÇs.
//         Ekkor a parserBuffer-ben vannak a beolvasott tokenek.
//
//      1: Ha sikerÅlt az illesztÇs, de nem ment elìre. (Pl. egy Åres
//         alternat°va.
//
//      2: Ha sikerÅlt az illesztÇs Çs elìre is ment.
local tkId,tkStr,ujsor
local tList,i
local oLeftTList, leftTkId, leftTkStr
local match
local w
local nUres
local gone:=.t.

   nUres:=0
   while(this:item!=nil)
      leftTkId:=TOKEN.leftToken:id
      leftTkStr:=TOKEN.leftToken:str
      tkId:=TOKEN.(this:item):id
      tkStr:=TOKEN.(this:item):str
      ujsor:=tkId==TKID_UJSOR .or.;
             tkId==TKID_BOS .or. tkId==TKID_EOS .or.;
             (tkId==TKID_CHAR .and. tkStr==";")
      
      if (tkId==TKID_URES)
         // Maradunk
         // aadd(tokenList,this:item)
         nUres++
      elseif (ujsor)
         // VÇge. 
         // Illeszkedett, ha a leftToken egy alternat°va, mert r† az
         // Åres is illeszkedik, nem illeszkedett, ha nem az.
         // Ha illeszkedett, akkor a vÇgÇrìl az £jsort Çs az Åreseket
         // vissza kell tenni az inputra.
         if (leftTkId==TKID_MALTERSET)
            this:unrds(nUres+1)
            // this:item:=utNemUresItem
            // Itt nem kell a toEOL-t vizsg†lni.
            // Illeszkedik, de nem ment elìre.
            return 1
         endif
         // Az illesztÇs nem sikerÅlt.
         return 0 //exit
      else
         if (leftTkId==TKID_MALTERSET)
            gone:=this:mleftXMRToken(leftToken,cmd4,paramValues,nUres)
            match:=.t.
            this:makroBuf:=nil
         elseif (leftTkId==TKID_REGULAR_MATCH_MARKER)
            match:=this:mleftXRMMToken(leftToken,cmd4)
         elseif (leftTkId==TKID_WILD_MATCH_MARKER)
            match:=this:mleftXWMToken(leftToken)
         elseif (leftTkId==TKID_EXT_EXPR_MATCH_MARKER)
            match:=this:mleftXEEMToken(leftToken)
         elseif (leftTkId==TKID_LIST_MATCH_MARKER)
            match:=this:mleftXLMToken(leftToken,cmd4)
         elseif (leftTkId==TKID_RESTRICTED_MATCH_MARKER)
            match:=this:mleftXRSMMToken(leftToken,cmd4)
         else
            // EgyÇb illeszkedÇs.
            match:=this:mleftXNToken(leftToken,cmd4)
            this:makroBuf:=nil
         endif
         nUres:=0
         if (!match)
            // Az illesztÇs nem sikerÅlt ==> VÇge.
            return 0 // exit
         endif
         // Az illesztÇs sikerÅlt, az eredmÇny a tokenList-ben van,
         // megyÅnk tov†bb.
         if (this:makroBuf!=nil)
            addMatchParam(paramValues,;
                          MMARKER.leftToken:mNum,;
                          MMARKER.leftToken:getName(),;
                          this:makroBuf)
         endif
         // Tov†bb.
         return if(gone,2,1)
         // leftToken:=nextLeftToken(oLeftTList)
      endif
      this:rds()
   end while

   // Egy olyan sor, aminek nincs sorvÇgjele.
   if (leftTkId==TKID_MALTERSET)
      this:unrds(nUres+1)
      // this:item:=utNemUresItem
      // Itt nem kell a toEOL-t vizsg†lni.
      // Illeszkedik, de nem ment elìre.
      return 1
   endif
   // Az illesztÇs nem sikerÅlt.
return 0 //exit


//*******************************************************************
static function markParamValues(paramValues)
local mark,i

   mark:=array(len(paramValues[2]))
   for i:=1 to len(paramValues[2])
      if (paramValues[2][i]!=nil)
         mark[i]:=len(paramValues[2][i])
      endif
   end for
return mark

//*******************************************************************
static function restoreParamValues(paramValues,mark)
local i

   for i:=1 to len(mark)
      if (mark[i]==nil)
         paramValues[2][i]:=nil
      else
         asize(paramValues[2][i],mark[i])
      endif
   end for
   
   for i:=len(mark)+1 to len(paramValues[2])
      paramValues[2][i]:=nil
   endfor
      
return nil

//*******************************************************************
function parseDictTree(this,dictTree,cmd4,paramValues,toEOL,mi)
/*
   Elemez egy dictTree-t. VÇgigpr¢b†lja a fa minden †g†t.
   ElvÇgzi a helyettes°tÇst °s, hogy folytathassa az elemzÇst, ha
   a helyettes°tÇs nem vÇgezhetì el.
   Ha az mi meg van adva, akkor az mi indexñ node-t¢l indul.
   tree:=nodeList
   node:={token,nodeList[,extrDict]}
   nodeList:={node1,...}
   
   Ret: nil, ha nem sikerÅlt illeszteni, vagy nem lehetett 
             helyettes°teni.
        tokenList: A helyettes°tÇs eredmÇnye, ha sikerÅlt illeszteni
                   Çs sikerÅlt helyettes°teni.
*/
local l,item,markParamValues
local i//,retVal
local extrDict,befejezheto
local mL,mItem,mMarkParamValues
local resultTkList

   l:=TBUFFER.(this:parserBuffer):bItemNumber()
   item:=this:item

   // MegyÅnk a node-okon a legfelsì szinten, ha tal†lunk 
   // illeszkedÇst, akkor a fa tov†bbi rÇszÇt is illesztjÅk, ha
   // nem, akkor visszalÇpÅnk.
   markParamValues:=markParamValues(paramValues)
   for i:=if(empty(mi),1,mi) to len(dictTree)
      if (0!=(/*retVal:=*/matchLeftToken(this,dictTree[i][1],cmd4,paramValues,toEOL)))
         // Illeszkedett
         // MegnÇzzÅk, hogy lehet-e tov†bb menni.
         befejezheto:=len(dictTree[i])>=3 .and. dictTree[i][3]!=nil
         if (len(dictTree[i][2])>0)
            // MegyÅnk lefelÇ a f†ban ezen a node-on.
            if (befejezheto)
               mL:=TBUFFER.(this:parserBuffer):bItemNumber()
               mItem:=this:item
               mMarkParamValues:=markParamValues(paramValues)
            endif
            this:rds()
            if (nil!=(resultTkList:=parseDictTree(this,dictTree[i][2],cmd4,paramValues,toEOL)))
               // SikerÅlt.
               return resultTkList
            endif
            if (befejezheto)
               // Vissza kell lÇpni.
               this:unrds(TBUFFER.(this:parserBuffer):bItemNumber()-mL)
               this:item:=mItem
               restoreParamValues(paramValues,mMarkParamValues)
            endif
         endif

         // MegnÇzzÅk, hogy itt be lehet-e fejezni.
         if (befejezheto)
            // Be lehet fejezni!
            if (nilLeftToken(this,toEOL))
               extrDict:=dictTree[i][3]
               asize(paramValues[2],EXTRDICT.extrdict:numMatchMarkers)
               if (nil!=(resultTkList:=EXTRDICT.extrdict:change(paramValues[2])))
                  return resultTkList
               endif
               // Nem sikerÅlt az elemzÇs.
            endif
            // Vissza kell lÇpni.
         endif
         this:unrds(TBUFFER.(this:parserBuffer):bItemNumber()-l)
         this:item:=item
         restoreParamValues(paramValues,markParamValues)
         // Nem sikerÅlt.
      // elseif (retVal)
      //    // SikerÅlt.
      //    // Ide nem jîhet!
      //    outerr("parseDictTree: retVal==.t.: Ide nem jîhet!",newline())
      //    this:unrds(TBUFFER.(this:parserBuffer):bItemNumber()-l)
      //    this:item:=item
      //    restoreParamValues(paramValues,markParamValues)
      //    return nil
      else
         // Itt vissza kell lÇpni, Çs £jra pr¢b†lkozni.
         this:unrds(TBUFFER.(this:parserBuffer):bItemNumber()-l)
         this:item:=item
         restoreParamValues(paramValues,markParamValues)
      endif
   end while
   // A befejezÇst itt nem kell nÇzni, mert a legfelsì szinten
   // nem lehet befejezni (az az Åres szab†ly lenne), az als¢bb
   // szinteken pedig a h°v¢ nÇzi.
return nil
      
//*******************************************************************
implement parseXtr(extrdict)
/*
 Ez vÇgzi a tÇnyleges elemzÇst.
 A this:item-t elemzi, szÅksÇg esetÇn mÇg olvashat.
 Elemezi az inputon az extrdict tokenjeit. 

 Ret: {sikeres,itemLista}
 Ha sikeres volt, akkor a sikeres==.t., Çs az itemLista a csere
    eredmÇnye.
 Ha nem volt sikeres, akkor a sikeres==.f., Çs az itemLista a
    beolvasott (elìreolvasott) itemek list†ja.
 A parserBufferben csak egy token lehet, ami az item-ben is
 van.
*/   

local paramValues,w
#ifdef DEBUG
local i
#endif

   paramValues:={PVT_IDX,array(EXTRDICT.extrdict:numMatchMarkers)}
   if (0!=this:parseTokenList(EXTRDICT.extrdict:leftSide,;
                           EXTRDICT.extrdict:cmdType==XTRTYPE_COMMAND .or.;
                           EXTRDICT.extrdict:cmdType==XTRTYPE_TRANSLATE,;
                           paramValues,;
                           EXTRDICT.extrdict:cmdType==XTRTYPE_XCOMMAND .or.;
                           EXTRDICT.extrdict:cmdType==XTRTYPE_COMMAND))
      // SikerÅlt az elemzÇs, most meg kell csin†lni a cserÇt.
      #ifdef DEBUG
      outerr(crlf())
      outerr("xtranslate change: ",EXTRDICT.extrdict:printStr(),crlf())
      // for i:=1 to len(tokenList)
      //    outerr("left[",i,"]: ",TOKEN.tokenList[i]:getStr(),crlf())
      // end for
      #endif
      if (nil!=(w:=EXTRDICT.extrdict:change(paramValues[2])))
         return {.t.,w}
      endif
   endif
   // Nem sikerÅlt az elemzÇs.
return {.f.,this:arrayParserBuffer()}
   
//*******************************************************************
implement parseXtrTree(trdictTree,mi,toEOL)
/*
 Ez vÇgzi a tÇnyleges elemzÇst.
 A this:item-t elemzi, szÅksÇg esetÇn mÇg olvashat.
 Elemezi az inputon az extrdict tokenjeit. 
 Az mi indexñ node-t¢l indul.

 Ret: {sikeres,itemLista}
 Ha sikeres volt, akkor a sikeres==.t., Çs az itemLista a csere
    eredmÇnye.
 Ha nem volt sikeres, akkor a sikeres==.f., Çs az itemLista a
    beolvasott (elìreolvasott) itemek list†ja.
 A parserBufferben csak egy token lehet, ami az item-ben is
 van.
*/   

local paramValues,w//,extrDict
#ifdef DEBUG
local i
#endif

   paramValues:={PVT_IDX,{}}
   if (nil!=(w:=parseDictTree(this,trdictTree,;
                          .f.,;
                          paramValues,;
                          toEOL,mi)))
      
      // SikerÅlt az elemzÇs, a cserÇt a parserDict()-nek kell
      // csin†lnia, hogy tov†bb folytathassa az elemzÇst, ha
      // a helyettes°tÇst nem lehet elvÇgezni.
      #ifdef OLD
      asize(paramValues[2],EXTRDICT.extrdict:numMatchMarkers)
      #ifdef DEBUG
      outerr(crlf())
      outerr("xtranslate change: ",EXTRDICT.extrdict:printStr(),crlf())
      // for i:=1 to len(tokenList)
      //    outerr("left[",i,"]: ",TOKEN.tokenList[i]:getStr(),crlf())
      // end for
      #endif
      if (nil!=(w:=EXTRDICT.extrdict:change(paramValues[2])))
         return {.t.,w}
      endif
      #endif
      return {.t.,w}
   endif
   // Nem sikerÅlt az elemzÇs.
return {.f.,this:arrayParserBuffer()}
   
//*******************************************************************
function isMatchNToken(tkId,tkStr,mTkId,mTkStr,cmd4)
// Egy norm†l token illeszkedi-e egy norm†l (bal oldalon lÇvì) 
// tokenre.
// Mj.: A tk Çs az mTk nem cserÇlhetì fel egym†ssal!

   if (tkId==TKID_NEV)
      return mTkId==TKID_NEV .and. compareWNames(tkStr,mTkStr,cmd4)
   elseif (tkId==TKID_STRING)
      return mTkId==TKID_STRING .and. compareWNames(tkStr,mTkStr,cmd4)
   endif
return tkId==mTkId .and. tkStr==mTkStr

//*******************************************************************
implement mleftXNToken(leftToken,cmd4)
/*
   Match left xtranslate normal token.
   Norm†l token (nem match Çs nem malterset) elemzì.
   A this:item-ben levì tokent vizsg†lja, mÇg olvashat. 
   Ha az illesztÇs sikertelen, akkor a plusz beolvasott tokeneket
   visszateszi az inputra Çs a this:item-et vissza†ll°tja.
*/
local tkId, tkStr

   tkId:=TOKEN.(this:item):id
   tkStr:=TOKEN.(this:item):str
   
   if (isMatchNToken(tkId,tkStr,;
                     TOKEN.leftToken:id,TOKEN.leftToken:str,;
                     TOKEN.leftToken:eqType/*cmd4*/))
      return .t.
   endif
return .f.

//*******************************************************************
static function connectParamValues(paramValues,wParamValues)
local i

   if (paramValues[1]==PVT_IDX)
      if (len(paramValues[2])<len(wParamValues[2]))
         asize(paramValues[2],len(wParamValues[2]))
      endif
      for i:=1 to len(wParamValues[2])
         if (wParamValues[2][i]!=nil)
            if (paramValues[2][i]==nil)
               paramValues[2][i]:=wParamValues[2][i]
            else
               aappend(paramValues[2][i],wParamValues[2][i])
            endif
         endif
      end for
   else
      aappend(paramValues[2],wParamValues[2])
   endif
return nil

//*******************************************************************
implement mleftXMRToken(leftToken,cmd4,paramValues,nUres)
// MALTERSET
/*
   Az algoritmus:
   
      MegyÅnk az alternat°v†kon, Çs megpr¢b†ljuk illeszteni.
      Ha nem sikerÅlt, vissza az inputra a beolvasott tokeneket,
      Çs vesszÅk a kîvetkezìt.
      Ha sikerÅlt, akkor elìrìl kezdjÅk vÇgignÇzni az alternat°v†kat.
      Mj.: Ez v†ltozhat
      Ha az alternat°v†k egyike sem illeszkedett, akkor tov†bbmegyÅnk.
      Mj.: Az illesztÇs mindig sikeres, mert az Åres is benne van.
           a lehetìsÇgek kîzîtt, csak ilyenkor az olvasott
           tokent vissza kell tenni az inputra.
*/
local oldParserBuffer
local item,i,leftTokenList
local wParamValues
local gone,iGone

   gone:=.f.
   oldParserBuffer:=this:parserBuffer
   this:unrds()
   this:parserBuffer:=C.TBUFFER:onew()
   this:rds()
   item:=this:item
   leftTokenList:=MALTRSET.leftToken:alterset
   i:=1
   while(i<=len(leftTokenList))
      if (paramValues[1]==PVT_IDX)
         wParamValues:={paramValues[1],array(len(paramValues[2]))}
      else
         wParamValues:={paramValues[1],{}}
      endif
      if (2==(iGone:=this:parseTokenList(leftTokenList[i],cmd4,wParamValues)))
         // SikerÅlt illeszteni.
         // A mostani parserBuffer-t hozz† kell adni a rÇgihez.
         TBUFFER.oldParserBuffer:appendBuffer(this:parserBuffer)
         // A paramÇtereket hozz† kell adni a rÇgihez.
         connectParamValues(paramValues,wParamValues)
         
         // ójra vÇgignÇzzÅk a list†t.
         this:parserBuffer:=C.TBUFFER:onew()
         this:rds()   
         item:=this:item
         i:=1
         nUres:=0
         gone:=.t.
      else
         // A mostani parserBuffert pedig el kell dobni (a tartalm†t
         // visszatenni az inputra), a item-et pedig vissza kell 
         // †ll°tani.
         this:unrds(TBUFFER.(this:parserBuffer):bItemNumber()-1)
         this:item:=item
         i++
      endif
   end while
   // Nem lehet tov†bb menni, de az illesztÇs azÇrt sikeres!
   this:unrds()
   this:parserBuffer:=oldParserBuffer
   // A buffer vÇgÇn levì <space>-kat vissza kell rakni az
   // inputra.
   this:unrds(nUres)
return gone

//*******************************************************************
static function compareWNames(name,trName,cmd4)
// A name (ami az elemzendì sorban van) illeszkedik-e a translate
// bal oldal†n levì token-re.
// Sajnos nem nÇgy hosszan, hanem minimum 4 hosszan kell hasonl°tani,
// Çs nem mindegy, hogy mi illeszkedik mire.

   // Ez kell!!!
   if (len(name)>len(trName))
      return .f.
   endif
   
   if (empty(cmd4) .or. len(name)<4)
      return lower(name)==lower(trName)
   endif
   
//   Mj.: Ez lÇnyegÇben egy '=' (îsszehasonl°t†s), de az '=' 
//        obsoleted.
return (lower(name)==lower(left(trName,len(name))))

//*******************************************************************
static function matchLookForward(tkId,tkStr,forwTkId,forwTkStr,forwT,cmd4)

   if (forwTkId==TKID_RESTRICTED_MATCH_MARKER)
      // A spec nem nÇz elìre ebben az esetben, ezÇrt ez nem kell.
      #ifdef OLD
      // Sajnos ez egyenlìre nem j¢, mert a wordList-ben tokeneknek
      // kellene lennie, hogy a stringeket kÅlîn kezelhessÅk, 
      // ezenk°vñl az ÅreseknÇl meg kellene †llni, etc.
      // De ez egyenlìre nincs kitesztelve.
      // EzÇrt egyenlìre a stringeket kihagyjuk.
      if (tkId==TKID_NEV .or.;
          tkId==TKID_SZAMTOMB .or.;
          ;//tkId==TKID_STRING .or.;
          tkId==TKID_CHAR)
         return 0!=ascan(RSMMARKR.forwT:wordList,;
                         {|x| compareWNames(tkStr,x,RSMMARKR.forwT:eqType/*cmd4*/)})
      endif
      #endif
      return .f.
   elseif (forwTkId==TKID_NEV .or. forwTkID==TKID_STRING)
      // A neveket Çs a stringeket 'case insensitive' m¢don kell 
      // îsszehasonl°tani.
      return tkId==forwTkId .and. compareWNames(tkStr,forwTkStr,RSMMARKR.forwT:eqType/*cmd4*/)
   endif

   // Itt mÇg lehetnek csavar†sok pl. a sz†mokat ÇrtÇkÅk szerint, 
   // vagy a stringeket £gy, hogy nem sz†m°t mivel hat†rolt†k etc.
return tkId==forwTkId .and. tkStr==forwTkStr
   
//*******************************************************************
implement mleftXRMMToken(leftToken,cmd4)
/*
   Regular Match marker.
   Addig megy, am°g egy kifejezÇs tart, vagy el nem Çri a 
   leftToken:nextToken-t.
   Sajnos a specifik†ci¢ nem kîvetkezetes: 
   Pl: Ez nem egy kifejezÇs: 'a*+b', ez viszont igen: 'a*(+b)'

   A cmd4 az elìrenÇzÇshez kell.    
*/
local state

#define STXRMM_START       "start"
#define STXRMM_PARENT      "parent"

local tkId,tkStr,ujsor
local parentStack
// local iForwRead
local clfBuf,clfNumBuf,elvalaszt
local clf
local endToken,endTkId,endTkStr
               
   this:startMakrobuf(this:item)
   clfBuf:={}
   clfNumBuf:={}
        
   // iForwRead:=1
   
   // Itt ki kell szñrni az '='-t, mert azzal kifejezÇs nem kezdìdhet.
   // Kihaszn†ljuk, hogy this:item £jsor Çs Åres nem lehet.
   if (TOKEN.(this:item):id==TKID_CHAR .and.;
       TOKEN.(this:item):classify=='=')
      // Vajon itt kell unrds()?
      this:unrdsMakroBuf()
      return .f.
   endif

   endToken:=MMARKER.leftToken:nextToken
   if (MMARKER.leftToken:nextToken!=nil)
      endTkId:=TOKEN.(MMARKER.leftToken:nextToken):id
      endTkStr:=TOKEN.(MMARKER.leftToken:nextToken):str
   else
      endTkId:=nil
      endTkStr:=nil
   endif
   
   elvalaszt:=nil   
   state:=STXRMM_START
   while(this:item!=nil)
      tkId:=TOKEN.(this:item):id
      tkStr:=TOKEN.(this:item):str
      ujsor:=tkId==TKID_UJSOR .or.;
             tkId==TKID_BOS .or. tkId==TKID_EOS .or.;
             (tkId==TKID_CHAR .and. tkStr==";")

      PDEBUG(outerr("mleftXRMMToken: ","state: ",state, "tkId: ",tkId, "tkStr: ", tkStr,crlf()))
      if (state==STXRMM_START)
         if (ujsor)
            // VÇge!
            exit
         elseif (matchLookForward(tkId,tkStr,endTkId,endTkStr,endToken,cmd4))
         // elseif (tkId==endTkId .and. tkStr==endTkStr)
            // Ez itt problÇm†s, mert neveknÇl, vagy stringeknÇl nem 
            // tudjuk, hogyan kell az îsszehasonl°t†st csin†lni.
            // (case sensitive, neveknÇl nÇgy karakter hosszan is
            // lehet (translate,command)
            // VÇge!
            // Mj.: Nem biztos, hogy j¢ a clBuf-beli visszalÇpÇseket 
            // kezelni kellene
            exit
         elseif (eqTkInCharList(tkId,tkStr,','))
            // VÇge!
            exit
         elseif (eqTkInCharList(tkId,tkStr,')}]'))
            // VÇge!
            exit
         elseif (tkId==TKID_URES)
            // Maradunk.
         elseif (eqTkInCharList(tkId,tkStr,'({['))
            // Elìszîr megnÇzzÅk, hogy jîhet-e itt z†r¢jel.
            aadd(clfBuf,TKCL_PARENT)
            // Amikor kijîn a z†r¢jelbìl, ide be fogja °rni a helyes
            // sz†mot.            
            aadd(clfNumBuf,len(this:makroBuf))
            if (nil!=(elvalaszt:=exprChk(clfBuf)))
               // Nem jîhet.
               exit
            endif
            state:=STXRMM_PARENT
            parentStack:={thisclass:getCloseParent(tkStr)}
         elseif (nil!=(clf:=TOKEN.(this:item):classify))
            // Hozz†vesszÅk a clfBuf-hoz, Çs megnÇzzÅk, hogy
            // jîhet-e.
            if (clf=='++' .and. len(clfBuf)>0 .and. atail(clfBuf)=='++')
               clfNumBuf[len(clfNumBuf)]:=len(this:makroBuf)
            else
               aadd(clfBuf,clf)
               aadd(clfNumBuf,len(this:makroBuf))
               if (nil!=(elvalaszt:=exprChk(clfBuf)))
                  // Nem jîhet. 
                  // Az elvalaszt-n†l van a kifejezÇshat†r.
                  exit
               endif
            endif
         else
            // Ide nem jîhetÅnk, egyenlìre elengedjÅk, b†rmi is van 
            // itt.
         endif
      elseif (state==STXRMM_PARENT)
         // Mj.: A clfNumBuf vÇgÇn a makr¢buf hossz†t azÇrt kell
         //      vezetni, hogy az Åres helyeket a vÇgÇn le tudjuk 
         //      v†gni.
         if (ujsor)
            // VÇge! 
            exit
         elseif (tkId==TKID_URES)
            // Maradunk.
         elseif (eqTkInCharList(tkId,tkStr,'({['))
            aadd(parentStack,thisclass:getCloseParent(tkStr))
            clfNumBuf[len(clfNumBuf)]:=len(this:makroBuf)
         elseif (eqTkInCharList(tkId,tkStr,')}]'))
            // Ha nem illeszkedik a stack-re, akkor nem vesszÅk
            // figyelembe, ha illeszkedik, akkor a stack-rìl 
            // levesszÅk a legfelsì elemet.
            clfNumBuf[len(clfNumBuf)]:=len(this:makroBuf)
            if (atail(parentStack)==tkStr)
               adrop(parentStack)
               if (empty(parentStack))
                  state:=STXRMM_START
               endif
            endif
         else
            // B†rmi m†s, folytat¢dik az elemzÇs.
            clfNumBuf[len(clfNumBuf)]:=len(this:makroBuf)
         endif
      else
         ? "MPARSER:mleftXRMMToken(): Ismeretlen †llapot: ",state
         errorlevel(1)
         quit
      endif
      this:rdsMakroBuf()
      // iForwRead++
   end while
        
   if (elvalaszt==nil)
      // Sor vÇge, vesszì, etc.
      // Ha a clfBuf nem Åres, akkor az utols¢ra lÇpÅnk vissza, ha 
      // Åres, nem illeszkedett!
      if (len(clfBuf)==0)
         this:unrdsMakroBuf(len(this:makroBuf))
         return .f.
      endif
      // MÇg meg kell nÇzni, hogy van-e kifejezÇshat†r, £gy, hogy
      // nem lehet folytatni.
      aadd(clfBuf,"newline")
      aadd(clfNumBuf,len(this:makroBuf))
      if (nil==(elvalaszt:=exprChk(clfBuf)))
         elvalaszt:=len(clfBuf)-1
      endif
   endif   
   // Ilyenkor az clfNumBuf[elv†laszt] azt mondja meg, hogy hova
   // kell visszalÇpnÅnk. Ennyi tokennek kell maradnia a 
   // makroBuf-ban.
   this:unrdsMakroBuf(len(this:makroBuf)-clfnumBuf[elvalaszt])
   // aappend(tokenList,this:makroBuf)
   
return !empty(this:makroBuf)

//*******************************************************************
#ifdef OLD
// Ez egÇsz j¢l mñkîdik, de sokat elront.
// implement mleftXRMMToken(leftToken,tokenList)
// /*
//    Regular Match marker.
//    Addig megy, am°g egy kifejezÇs tart, vagy el nem Çri a 
//    leftToken-ben megadott lez†r¢ tokent.
//    Sajnos a specifik†ci¢ nem kîvetkezetes: 
//    Pl: Ez nem egy kifejezÇs: 'a*+b', ez viszont igen: 'a*(+b)'
//     
// */
// 
// /*
//    A kifejezÇs elemzÇs a kîvetkezìkÇppen megy:
//    
//    Az elemek:
//       <KifejezÇs token>: olyan token, ami meg†llja a helyÇt în†ll¢
//                          kifejezÇskÇnt.
//       <Elìrevetett un†ris oper†tor>:
//                        '&','++','--','+','-','@','.not.','!'
//       <H†travetett un†ris oper†tor>:
//                        '--','++'
//       <bin†ris oper†tor>:
//                       '$','%','*','**','^','+','-','->','.and.','.or.',
//                       '/',':',':=',
//                       '<','<=','<>','!=','#','=','==','>','>='
//                       
//       Speci†lis dolgok:
//         <sz†m>['.'[<sz†m>]]
//         
// 
//       Helyettes°tÇsek: 
//          '.not.' -> '!'
//          '**'    -> '^'
//          
//       MegjegyzÇsek: 
//            
//       - A <sz†m>.and. kifejezÇsben az elsì '.' a .and.-hoz 
//         tartozik, mert a .and. foglalt nÇv. Ugyanez vonatkozik 
//         a '.or.','.not.','.t.','.f.'-re. A kis Çs a nagybetñket
//         nem kÅlînbîzteti meg.
//            
//       - A kÇt '&' jel ugyan£gy megjegyzÇs, mint a '//'
//       
//       - Egy oper†tornak tekinti a kîvetkezì tokenek tetsz. 
//         kombin†ci¢j†t: 
// 
//         '&','++','--','@','$','->','.and.','.or.','.not.','!',':=',
//         '<','<=','<>','!=','#','=','==','>','>=','.'
//          
//         Mj.: A fenti oper†torokat un†ris oper†tornak tekinti.
//             
// 
//         Mj2.: A fentiek kîzÅl a kîvetkezìk maradtak ki:
//               '+','-','*','**','^','/','%'
//         
//       - Speci†lisan kezelt tokenek:
//         '+': Csak bin†ris lehet, elìjel nem lehet.
//         '-': Bin†ris Çs elìjel lehet.
//         '*','**', '^', '/','%': csak bin†ris oper†torok lehetnek.
//            Mj.: A '*','/'-nek van egy speci†lis esete l†sd 'Speci†lis
//                 esetek'.
//            
//       - Speci†lis esetek:
//         <b†rmi> '*' '/' <nÇv> : Ezt elfogadja egy kifejezÇsnek, de
//         pl.; a '*' '/' '*' '/'-t m†r nem. 
//         Mj.: A <nÇv> helyÇn nem †llhat semmit m†s, mÇg £j sor sem!
//         
//       - Az illesztÇst Çs az xtranslate elemzÇst is tokenesen 
//         csin†lja (argh...) Pl.: ez a kÇt xtranslate parancs kÅlînbîzik
//         egym†st¢l:
//         
//         #xtranslate HUHU <a> :=  <b> => let(@<a>,<b>)
//         #xtranslate HUHU <a> : = <b> => let(@<a>,<b>)
// 
//         Mj.: A m†sodikat el sem fogadja 'hi†nyz¢ =>' hibajelzÇssel.
// 
//  
//         
//    LehetsÇges, hogy a kifejezÇshat†rok oldal†r¢l jobban meg lehet fogni
//    a dolgot:
//    
//    <bin†ris oper†tor>:='*'|'**'|'^'|'/'|'%'
//    
//    <Z†r¢jelezett kifejezÇs>: '()', '{}','[]' kîzîtt levì tetsz karakter
//                            sorozat. Az z†r¢jeleken belÅl soha nincs 
//                            kifejezÇshat†r.
// 
//    <p†r nÇlkÅli csuk¢ z†r¢jel>: ')', '}',']' karater, amihez nincs 
//                                 megfelelì nyit¢ z†r¢jel.
//    
//    Ekkor a kifejezÇs hat†rok:
//    
//    <sz†m vagy nÇv> <kifejezÇshat†r> <sz†m vagy nÇv>
//    '+' <kifejezÇshat†r> <Åres> '+'
//    '+' <kifejezÇshat†r> <bin†ris oper†tor>
//    '-' <kifejezÇshat†r> <Åres> '+'
//    '-' <kifejezÇshat†r> <bin†ris oper†tor>
//    '-' <kifejezÇshat†r> <Åres> '-' <Åres> '-'
//    <Z†r¢jelezett kifejezÇs> <kifejezÇshat†r> <nÇv vagy sz†m>
//    <kifejezÇshat†r> <p†r nÇlkÅli z†r¢jel>
//    <bin†ris oper†tor> <kifejezÇshat†r> <bin†ris oper†tor>
//       KivÇtel: '*' '/' <nÇv> // Itt nincs kifejezÇs hat†r.
//    <bin†ris oper†tor> <kifejezÇshat†r> '+'
//    <bin†ris oper†tor> <kifejezÇshat†r> <nem '-',<sz†m>,<nÇv>,<nyit¢ z†r¢jel>.>
//    <kifejezÇshat†r> ','
//    
//    Speci†lis esetek: 
//    
//    
//    '~': Ezt tîrli (?)
//    '`': Ezt string hat†rol¢nak (") tekinti.
//    '.','|': Ezeket nem tekinti hat†rol¢nak.
//    '&&': Ez egysoros megjegyzÇs, olyan, mint a '//'
//          
//    B†rmilyen egyÇb esetben nincs kifejezÇs hat†r.
// 
// */
// 
// local state
// 
// #define isTkIdNevSzam(tkId)  ((tkId)==TKID_NEV .or.;
//                               (tkid)==TKID_SZAMTOMB)
//                             
// #define isTkIdLiteral(tkId)  (isTkIdNevSzam(tkId) .or.;
//                              (tkid)==TKID_STRING)
//                             
// #define isTkClassify(tkId,token,clsfy)  ((tkId)==TKID_CHAR .and.;
//                                          TOKEN.(token):classify==(clsfy))
//                             
// #define STXRMM_XSTART      "xstart"
// #define STXRMM_START       "start"
// #define STXRMM_LITERAL     "literal"
// #define STXRMM_PLUS        "plus"
// #define STXRMM_MINUS       "minus"
// #define STXRMM_MINUS2      "minus2"
// #define STXRMM_PARENT      "parent"
// #define STXRMM_CSILLAG     "csillag"
// #define STXRMM_CSILLAGPER  "csillagper"
// #define STXRMM_BINARY      "binary"
//            
// local tkId,tkStr,ujsor
// local parentStack
// local iForwRead
// local clfBuf
//                
//    iForwRead:=1
//    
//    this:startMakrobuf(this:item)
//    state:=STXRMM_XSTART
//    clfBuf:={}
//    
//    while(this:item!=nil)
//       tkId:=TOKEN.(this:item):id
//       tkStr:=TOKEN.(this:item):str
//       ujsor:=tkId==TKID_UJSOR .or.;
//              tkId==TKID_BOS .or. tkId==TKID_EOS .or.;
//              (tkId==TKID_CHAR .and. tkStr==";")
// 
//       PDEBUG(outerr("state: ",state, "tkId: ",tkId, "tkStr: ", tkStr,crlf()))
//       if (state==STXRMM_XSTART)
//          if (ujsor)
//             // VÇge!
//             exit
//          elseif (tkId==TKID_URES)
//             // Maradunk.
//          elseif (isTkClassify(tkId,this:item,'='))
//             // VÇge!
//             exit
//          else
//             iForwRead:=0
//             this:unrdsMakroBuf()
//             state:=STXRMM_START
//          endif
//       elseif (state==STXRMM_START)
//          if (ujsor)
//             // VÇge!
//             exit
//          elseif (tkId==TKID_URES)
//             // Maradunk.
//          elseif (isTkIdLiteral(tkId))
//             iForwRead:=0
//             state:=STXRMM_LITERAL
//          elseif (eqTkInCharList(tkId,tkStr,'({['))
//             iForwRead:=0
//             state:=STXRMM_PARENT
//             parentStack:={thisclass:getCloseParent(tkStr)}
//          elseif (eqTkInCharList(tkId,tkStr,')}]'))
//             // VÇge (!)
//             exit
//          elseif (eqTkChar(tkId,tkStr,','))
//             // VÇge (!)
//             exit
//          elseif (isTkClassify(tkId,this:item,'!'))
//             // Ezut†n soha nincs kifejezÇs hat†r.
//             iForwRead:=0
//             // Maradunk.
//          elseif (isTkClassify(tkId,this:item,'@'))
//             // Ezut†n soha nincs kifejezÇs hat†r.
//             iForwRead:=0
//             // Maradunk.
//          elseif (isTkClassify(tkId,this:item,'%'))
//             // Hasonl¢ a '/'-hez Çs a '*'-hoz, de nincs kivÇtel.
//             // Ut†na jîhet: '&', '- nev', '- sz†m', 'nev','szam',
//             //              'z†r¢jel', '.t.'
//             iForwRead:=0
//             // state:=STXRMM_PERCENT
//             state:=STXRMM_BINARY
//          elseif (isTkClassify(tkId,this:item,'^'))
//             // Hasonl¢ a '/'-hez Çs a '*'-hoz, de nincs kivÇtel.
//             // Ut†na jîhet: '&', '- nev', '- sz†m', 'nev','szam',
//             //              'z†r¢jel', '.t.'
//             iForwRead:=0
//             // state:=STXRMM_POW
//             state:=STXRMM_BINARY
//          elseif (isTkClassify(tkId,this:item,'&'))
//             // Mindent elfogad.
//             iForwRead:=0
//          elseif (eqTkChar(tkId,tkStr,'*'))
//             // A '*' '/' <name> speci†lis eset kezelÇse.
//             iForwRead:=0
//             state:=STXRMM_CSILLAG
//             PDEBUG(outerr("start->csillag"+crlf()))
//          elseif (eqTkChar(tkId,tkStr,'-'))
//             iForwRead:=0
//             state:=STXRMM_MINUS
//          elseif (eqTkChar(tkId,tkStr,'+'))
//             iForwRead:=0
//             state:=STXRMM_PLUS
//          elseif (isTkClassify(tkId,this:item,'='))
//             // Mivel nem ez az elsì karakter, b†rmit el lehet 
//             // fogadni.
//             iForwRead:=0
//             // Maradunk.
//          /*
//          elseif (isTkClassify(tkId,this:item,'/'))
//          elseif (isTkClassify(tkId,this:item,'++'))
//          elseif (tkId==TKID_NEV)
//          elseif (tkId==TKID_SZAMTOMB)
//          elseif (tkId==TKID_STRING)
//          */
//          else
//             // B†rmi m†s, maradunk.
//             iForwRead:=0
//          endif
//       elseif (state==STXRMM_LITERAL)
//          if (ujsor)
//             // VÇge!
//             exit
//          elseif (tkId==TKID_URES)
//             // Maradunk.
//          elseif (isTkIdLiteral(tkId))
//             // VÇge
//             exit
//          else
//             // B†rmi m†s: £jra fel kell dolgozni.
//             iForwRead--
//             this:unrdsMakroBuf()
//             state:=STXRMM_START
//          endif
//       elseif (state==STXRMM_PLUS)
//          if (ujsor)
//             // VÇge!
//             exit
//          elseif (tkId==TKID_URES)
//             // Maradunk.
//          elseif (eqTkChar(tkId,tkStr,'+'))
//             // VÇge.
//             exit
//          elseif (thisclass:isTkBinaryOp(tkId,tkStr))
//             // *, **, ^,/,%
//             // VÇge.
//             exit
//          else
//             // B†rmi m†s: £jra fel kell
//             // dolgozni.
//             iForwRead--
//             this:unrdsMakroBuf()
//             state:=STXRMM_START
//          endif
//       elseif (state==STXRMM_MINUS)
//          if (ujsor)
//             // VÇge!
//             exit
//          elseif (tkId==TKID_URES)
//             // Maradunk.
//          elseif (eqTkChar(tkId,tkStr,'+'))
//             // VÇge.
//             exit
//          elseif (thisclass:isTkBinaryOp(tkId,tkStr))
//             // *, **, ^,/,%
//             // VÇge.
//             exit
//          elseif (eqTkChar(tkId,tkStr,'-'))
//             state:=STXRMM_MINUS2
//          else
//             // B†rmi m†s: £jra fel kell dolgozni.
//             iForwRead--
//             this:unrdsMakroBuf()
//             state:=STXRMM_START
//          endif
//       elseif (state==STXRMM_MINUS2)
//          if (ujsor)
//             // VÇge! 
//             exit
//          elseif (tkId==TKID_URES)
//             // Maradunk.
//          elseif (isTkIdNevSzam(tkId)) // A string itt nem j¢!
//             // Elmegy.
//             iForwRead:=0
//             state:=STXRMM_LITERAL
//          else
//             // B†rmi m†s: kifejezÇshat†r az elsì minusz ut†n!
//             exit
//          endif
//       elseif (state==STXRMM_PARENT)
//          if (ujsor)
//             // VÇge! 
//             exit
//          elseif (tkId==TKID_URES)
//             // Maradunk.
//          elseif (eqTkInCharList(tkId,tkStr,'({['))
//             iForwRead:=0
//             aadd(parentStack,thisclass:getCloseParent(tkStr))
//          elseif (eqTkInCharList(tkId,tkStr,')}]'))
//             // Ha nem illeszkedik a stack-re, akkor nem vesszÅk
//             // figyelembe, ha illeszkedik, akkor a stack-rìl 
//             // levesszÅk a legfelsì elemet.
//             iForwRead:=0
//             if (atail(parentStack)==tkStr)
//                adrop(parentStack)
//                if (empty(parentStack))
//                   state:=STXRMM_START
//                endif
//             endif
//          else
//             // B†rmi m†s, folytat¢dik az elemzÇs.
//             iForwRead:=0
//          endif
//       elseif (state==STXRMM_CSILLAG)
//          if (ujsor)
//             // VÇge! 
//             exit
//          elseif (tkId==TKID_URES)
//             // Maradunk.
//          elseif (eqTkChar(tkId,tkStr,'/'))
//             state:=STXRMM_CSILLAGPER
//             PDEBUG(outerr("csillag->csillagper"+crlf()))
//          else
//             // Az aktu†lis item-et a binaris oper†torok elemzìjÇvel 
//             // kell elemeztetni.
//             iForwRead--
//             this:unrdsMakroBuf()
//             state:=STXRMM_BINARY
//          endif
//       elseif (state==STXRMM_CSILLAGPER)
//          if (ujsor)
//             // VÇge! 
//             exit
//          elseif (tkId==TKID_URES)
//             // Maradunk.
//          elseif (tkId==TKID_NEV)
//             // Ok. ElkerÅltÅk a kifejezÇshat†rt.
//             // Gyors°tunk, egybìl a liter†lhoz megyÅnk.
//             iForwRead:=0
//             state:=STXRMM_LITERAL
//             PDEBUG(outerr("csillagper->literal"+crlf()))
//          else
//             // A '*' Çs a '/' kîzîtt van a kifejezÇshat†r.
//             exit
//          endif
//       elseif (state==STXRMM_BINARY)
//          if (ujsor)
//             // VÇge! 
//             exit
//          elseif (tkId==TKID_URES)
//             // Maradunk.
//          elseif (tkId==TKID_NEV)
//             // Ok. ElkerÅltÅk a kifejezÇshat†rt.
//             iForwRead:=0
//             state:=STXRMM_LITERAL
//          else
//             // A '*' Çs a '/' kîzîtt van a kifejezÇshat†r.
//             exit
//          endif
//       else
//          ? "MPARSER:mleftXRMMToken(): Ismeretlen †llapot: ",state
//          errorlevel(1)
//          quit
//       endif
//       this:rdsMakroBuf()
//       iForwRead++
//    end while
//         
//    this:unrdsMakroBuf(iForwRead)
//    aappend(tokenList,this:makroBuf)
//    
// return !empty(this:makroBuf)
#endif

//*******************************************************************
implement mleftXWMToken(leftToken)
/*
   Wild matchmarker.
   
   Egy '()' kîzÇ z†rt sorozat, vagy a leghosszabb space mentes
   sorozat.
   Mj.: Ha '('-al kezdìdik, Çs nincs lez†rva, akkor is '()'-nek veszi.
   Mj2.: A tîbbi z†r¢jelet nem veszi figyelembe.
   Mj3.: Nem nÇz elìre.
    
*/
local tkId,tkStr,ujsor
               
   this:startMakrobuf(this:item)
   
   while(this:item!=nil)
      tkId:=TOKEN.(this:item):id
      tkStr:=TOKEN.(this:item):str
      ujsor:=tkId==TKID_UJSOR .or.;
             tkId==TKID_BOS .or. tkId==TKID_EOS .or.;
             (tkId==TKID_CHAR .and. tkStr==";")

      PDEBUG(outerr("mleftXWMToken: "+"tkId: ",tkId, "tkStr: ", tkStr,crlf()))
      if (ujsor)
         // VÇge!
         this:unrdsMakroBuf()
         exit
      endif
      this:rdsMakroBuf()
   end while
        
return !empty(this:makroBuf)


//*******************************************************************
implement mleftXEEMToken(leftToken)
/*
   Extended Expression match marker.
   
   Egy '()' kîzÇ z†rt sorozat, vagy a leghosszabb space mentes
   sorozat.
   Mj.: Ha '('-al kezdìdik, Çs nincs lez†rva, akkor is '()'-nek veszi.
   Mj2.: A tîbbi z†r¢jelet nem veszi figyelembe.
   Mj3.: Nem nÇz elìre.
    
*/
local state

#define STXEEM_START       "start"
#define STXEEM_PARENT      "parent"
#define STXEEM_SOROZAT     "sorozat"

local tkId,tkStr,ujsor
local numParent
               
   this:startMakrobuf(this:item)
   
   state:=STXEEM_START
   while(this:item!=nil)
      tkId:=TOKEN.(this:item):id
      tkStr:=TOKEN.(this:item):str
      ujsor:=tkId==TKID_UJSOR .or.;
             tkId==TKID_BOS .or. tkId==TKID_EOS .or.;
             (tkId==TKID_CHAR .and. tkStr==";")

      PDEBUG(outerr("mleftXEEMToken: ","state: ",state, "tkId: ",tkId, "tkStr: ", tkStr,crlf()))
      if (state==STXEEM_START)
         if (ujsor)
            // VÇge!
            this:unrdsMakroBuf()
            exit
         elseif (tkId==TKID_URES)
           // Lenyeli
         elseif (eqTkInCharList(tkId,tkStr,'('))
            // Z†r¢jelezett m¢d.
            numParent:=1
            state:=STXEEM_PARENT
         else
            // Space nÇlkÅli sorozat m¢d
            state:=STXEEM_SOROZAT
         endif
      elseif (state==STXEEM_PARENT)
         if (ujsor)
            // VÇge! 
            this:unrdsMakroBuf()
            exit
         elseif (eqTkInCharList(tkId,tkStr,'('))
            numParent++
         elseif (eqTkInCharList(tkId,tkStr,')'))
            numParent--
            if (numParent<=0)
               exit
            endif
         else
            // B†rmi m†s, folytat¢dik az elemzÇs.
            // Ebben benne van az Åres is.
         endif
      elseif (state==STXEEM_SOROZAT)
         if (ujsor)
            // VÇge! 
            this:unrdsMakroBuf()
            exit
         elseif (tkId==TKID_URES)
            // VÇge! 
            this:unrdsMakroBuf()
            exit
         else
            // B†rmi m†s, folytat¢dik az elemzÇs.
         endif
      else
         ? "MPARSER:mleftXEEMToken(): Ismeretlen †llapot: ",state
         errorlevel(1)
         quit
      endif
      this:rdsMakroBuf()
   end while
        
return !empty(this:makroBuf)

//*******************************************************************
implement mleftXLMToken(leftToken,cmd4)
// List match marker
// Az Åres paramÇtert (,,) megengedi.
local params:={}
local iForwRead
local tkId,tkStr,ujsor


   this:mleftXRMMToken(leftToken,cmd4)
   this:rds()
   aadd(params,this:makroBuf)
             
   iForwRead:=1
   
   while(this:item!=nil)
      tkId:=TOKEN.(this:item):id
      tkStr:=TOKEN.(this:item):str
      ujsor:=tkId==TKID_UJSOR .or.;
             tkId==TKID_BOS .or. tkId==TKID_EOS .or.;
             (tkId==TKID_CHAR .and. tkStr==";")
   
      if (ujsor)
         // VÇge!
         exit
      elseif (tkId==TKID_URES)
         // Maradunk.
      elseif (eqTkChar(tkId,tkStr,','))
         this:rds()
         if (this:mleftXRMMToken(leftToken,cmd4))
            aadd(params,this:makroBuf)
         else
            aadd(params,{})
         endif
         iForwRead:=0
      else
         // B†rmi m†s, vÇge.
         exit
      endif
      iForwRead++
      this:rds()
   end while
   
   this:unrds(iForwRead)
   if (len(params)==1 .and. len(params[1])==0)
      return .f.
   endif
   this:makroBuf:=params
return .t.

//*******************************************************************
implement mleftXRSMMToken(leftToken,cmd4)
/*
   Restricted match marker.
   
   Csan nevekre, sz†mokra Çs karakterekre illesztÅnk.
*/
local tkId, tkStr,wl,i

   this:startMakrobuf(this:item)
   
   tkId:=TOKEN.(this:item):id
   tkStr:=TOKEN.(this:item):str
   
   if !(tkId==TKID_NEV .or.;
       tkId==TKID_SZAMTOMB .or.;
       ;//tkId==TKID_STRING .or.;
       tkId==TKID_CHAR)
      return .f.
   endif
   
   wl:=RSMMARKR.leftToken:wordList
   if (empty(wl))
      return .f.
   endif
   
   for i:=1 to len(wl)
      if (empty(wl[i]))
         this:unrdsMakrobuf()
         return .t.
      endif
      if (compareWNames(tkStr,wl[i],TOKEN.leftToken:eqType/*cmd4*/))
         return .t.
      endif
   end for
return .f.

//*******************************************************************
/*
cimplement isTkBinaryOp(tkId,tkStr)
// Ez a nem tokeniz†lt v†ltozat.

   if (!tkId==TKID_CHAR)
      return .f.
   endif
   
return tkStr$"*^/%"
*/   
//*******************************************************************


