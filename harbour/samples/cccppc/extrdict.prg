/*
 * $Id$
 */

//*******************************************************************
// extrdict.prg: #xtranslate sz¢t†r elem
// 1999, Csisz†r Levente

//*******************************************************************
/*
   A helyettes°tÇs £gy megy, hogy minden match marker kap egy 
   sorsz†mot. A change fÅggvÇny egy olyan tîmbît v†r, ahol 
   az i. elem, az i. match markerre illeszkedett token list†k 
   list†ja. (Egy match markerre tîbbszîr is lehet illeszteni, ha '[]'
   kîzîtt volt.)
   
   Ezekut†n a csere £gy megy, hogy megyÅnk a jobb oldalon,
   ha nem '[]' kîzîtti result markert tal†lunk, akkor 
   behelyettes°tjÅk a hozz† tartoz¢ token list†t. (Ekkor pontosan egy
   ilyen kell, hogy legyen)
   
   Ha '[]' kîzîtti tokeneket annyiszor °rjuk ki, amennyi a hozz† 
   tartoz¢ result markere csere list†inak a maxim†lis hossza.
   
*/

//*******************************************************************
#include "ctoken.ch"
#include "objgen.ch"

//*******************************************************************
#include "token.och"
#include "mmarker.och"
#include "rmarker.och"
#include "maltrset.och"
#include "ralter.och"

//*******************************************************************

#define _EXTRDICT_PRG_
#define _IMPLEMENT_ONEW_

#include "extrdict.och"


//*******************************************************************
#define EQTYPE_ALLLEN  nil
#define EQTYPE_4LEN    1

//*******************************************************************

implement oinit(cmdType,leftSide,rightSide,deffile,defline,defpos)
local eqType

   super:oinit()
   this:cmdType   :=cmdType
   this:leftSide  :=leftSide
   this:rightSide :=rightSide
   this:deffilelinepos   :={deffile,defline,defpos}

   calcSorszam(this)
   setNextToken(this)
   
   
   if (this:cmdType==XTRTYPE_XTRANSLATE)
      eqType:=EQTYPE_ALLLEN
   elseif (this:cmdType==XTRTYPE_XCOMMAND)
      eqType:=EQTYPE_ALLLEN
   elseif (this:cmdType==XTRTYPE_TRANSLATE)
      eqType:=EQTYPE_4LEN
   elseif (this:cmdType==XTRTYPE_COMMAND)
      eqType:=EQTYPE_4LEN
   else
      eqType:=EQTYPE_ALLLEN
   endif
  
   evalLTokenList(this:leftSide,{|x| TOKEN.x:eqType:=eqType})
   
return this   

//*******************************************************************
static function evalLTokenList(tokenList,block)
// VÇgigmegy egy olyan tokenlist†n, ami a bal oldalon †llhat.
local i

   // Mj.: Blokkokat nem lehet egym†sba skatuly†zni.
   for i:=1 to len(tokenList)
      if (TOKEN.tokenList[i]:id==TKID_MALTERSET)
         aeval(MALTRSET.tokenList[i]:alterset,;
               {|x| evalLTokenList(x,block)})
      else
         eval(block,tokenList[i])
      endif
   end for

return nil

//*******************************************************************
static function x2SetNextToken(prevToken,nextToken)
// A prevToken-be be†ll°tja a nextToken-t, ha a a prevToken
// match marker. Ha a prevToken egy malterset, akkor minden
// elemÇre (az elemek token list†k) megh°vja az xSetNextToken()-t.
// Mj.: Egyenlìre nem figyeli, hogy a nextToken normal token-e (nem 
//      match marker Çs nem malterset).
static idMatchMarkers:={;
   TKID_REGULAR_MATCH_MARKER     ,;
   TKID_WILD_MATCH_MARKER        ,;
   TKID_EXT_EXPR_MATCH_MARKER    ,;
   TKID_LIST_MATCH_MARKER        ,;
   TKID_RESTRICTED_MATCH_MARKER  ;
} 
local i,alterset

   if (TOKEN.prevToken:id==TKID_MALTERSET)
      alterset:=MALTRSET.prevToken:alterset
      for i:=1 to len(alterset)
         xSetNextToken(alterset[i],nextToken)
      end for
   elseif (C.MMARKER:isMatchMarker(prevToken) .and. nextToken!=nil)
      MMARKER.prevToken:nextToken:=nextToken
   endif
return nil

//*******************************************************************
static function xSetNextToken(tokenList,nextToken)
// Az îsszes match markerben be†ll°tja a nextToken-t, ha van.
local i
local prevToken

   prevToken:=nil
   for i:=1 to len(tokenList)
      if (TOKEN.(tokenList[i]):id==TKID_URES)
         // Kihagyjuk
      else
         if (prevToken!=nil)
            x2SetNextToken(prevToken,tokenList[i])
         endif
         prevToken:=tokenList[i]
      endif
   end for
   if (prevToken!=nil)
      x2SetNextToken(prevToken,nextToken)
   endif
   
return nil

//*******************************************************************
static function setNextToken(this)
/*
   Az îsszes match markerben be†ll°tja a nextToken-t, ha van.
*/
   xSetNextToken(this:leftSide)   
return nil

//*******************************************************************
static function xCollectMatchMarkers(tokenList,matchMarkers)
static idMatchMarkers:={;
   TKID_REGULAR_MATCH_MARKER     ,;
   TKID_WILD_MATCH_MARKER        ,;
   TKID_EXT_EXPR_MATCH_MARKER    ,;
   TKID_LIST_MATCH_MARKER        ,;
   TKID_RESTRICTED_MATCH_MARKER  ;
} 
local i, j, alterset

   for i:=1 to len(tokenList)
      if (TOKEN.tokenList[i]:id==TKID_MALTERSET)
         alterset:=MALTRSET.tokenList[i]:alterset
         for j:=1 to len(alterset)
            xCollectMatchMarkers(alterset[j],matchMarkers)
         end for
      elseif (C.MMARKER:isMatchMarker(tokenList[i]))
         aadd(matchMarkers,tokenList[i])
      endif
   end for

return nil

//*******************************************************************
static function xSetResultMarkers(tokenList,matchMarkers,undefRMarkers)
// A tokenList-ben szereplì result markerekben be†ll°tja a 
// sorsz†mot a matchMarkers-ben szereplì match markereknek
// megfelelìen.

local i, j, alterset

   for i:=1 to len(tokenList)
      if (TOKEN.tokenList[i]:id==TKID_RALTER)
         xSetResultMarkers(RALTER.tokenList[i]:tokenList,;
                           matchMarkers,undefRMarkers)
      elseif (C.RMARKER:isResultMarker(tokenList[i]))
         if (!RMARKER.tokenList[i]:setMMIdxByMM(matchMarkers))
            // Nincs ilyen match token.
            aadd(undefRMarkers,tokenList[i])
         endif
      endif
   end for
return nil

//*******************************************************************
static function calcSorszam(this)
/*
   - ôsszeszedi a match markereket.
   - Minden match markerbe be°rja a sorsz†m†t.
   - Minden result marker-be be°rja a hozz† tartoz¢ match
     marker sorsz†m†t.
   - Kisz†molja h†ny match marker van.
*/
local matchMarkers,i

   matchMarkers:={}
   xCollectMatchMarkers(this:leftSide,matchMarkers)
   
   for i:=1 to len(matchMarkers)
      MMARKER.matchMarkers[i]:mNum:=i
   end for

   this:numMatchMarkers:=len(matchMarkers)
   
   this:undefRMarkers:={}
   xSetResultMarkers(this:rightSide,matchMarkers,this:undefRMarkers)
   
return nil

//*******************************************************************
implement printStr()
local i
local str

   if (this:cmdType==XTRTYPE_XTRANSLATE)
      str:="xtranslate"
   elseif (this:cmdType==XTRTYPE_XCOMMAND)
      str:="xcommand"
   elseif (this:cmdType==XTRTYPE_TRANSLATE)
      str:="translate"
   elseif (this:cmdType==XTRTYPE_COMMAND)
      str:="command"
   else
      str:="xtranslate"
   endif
   
   str:="#"+str+" "

   for i:=1 to len(this:leftSide)
      str:=str+TOKEN.(this:leftSide[i]):getStr()
   end for
   
   str+=" => "
   
   for i:=1 to len(this:rightSide)
      str+=TOKEN.(this:rightSide[i]):getStr()
   end for
   
return str   

//*******************************************************************
static function iChange(iLevel,tokenList,paramValues,result)
/*
 A result-ba beleteszi a tokenList alternat°va helyettes°tÇsÇt az 
 iLevel szinten.
 Mj. Ha a tokenList nem egy alternat°va, akkor ez nem alkalmazhat¢(!)
 
 A helyettes°tendìk a paramValues-ban vannak.
 A tokenList-ben nem lehet RALTER token.
 
 Ret: .t.: volt helyettes°tÇs, .f. nem volt.
*/

local i,rLen,success

   rLen:=len(result)
   success:=.f.
   for i:=1 to len(tokenList)
      if (TOKEN.tokenList[i]:id==TKID_RALTER)
         // Hiba!
         return .f.
      elseif (C.RMARKER:isResultMarker(tokenList[i]))
         if (2==RMARKER.tokenList[i]:changeByMMList(paramValues,result,iLevel))
            success:=.t.
         endif
      elseif (TOKEN.(tokenList[i]):id==TKID_URES)
         aadd(result,TOKEN.(tokenList[i]):copyToken(TKID_URES," "))
         // outstd("Hopp2!")
      else
         aadd(result,tokenList[i])
      endif
   end for
   
   if (!success)
      asize(result,rLen)
   endif
   
return success

//*******************************************************************
implement change(paramValues)
// paramvalues:={<list of tokenList>,...}
/*
 A csere £gy megy, hogy megyÅnk a jobb oldalon,
 ha nem '[]' kîzîtti result markert tal†lunk, akkor behelyettes°tjÅk
 a hozz† tartoz¢ token list†t. 
 
 Ha '[]' kîzîtti tokeneket annyiszor °rjuk ki, amennyi a hozz† 
 tartoz¢ result markere csere list†inak a maxim†lis hossza.
 
 Ret: A csere eredmÇnye (egy token lista) vagy nil, ha nem lehetett 
      cserÇlni.
*/
local i,j
local result:={}
local tokenList,alterTl

   tokenList:=this:rightSide
   for i:=1 to len(tokenList)
      if (TOKEN.tokenList[i]:id==TKID_RALTER)
         alterTl:=RALTER.tokenList[i]:tokenList
         j:=1
         while(iChange(j,alterTl,paramValues,result))
            j++
         end while
      elseif (C.RMARKER:isResultMarker(tokenList[i]))
         if (2!=RMARKER.tokenList[i]:changeByMMList(paramValues,result,1,.t.))
            return nil
         endif
      elseif (TOKEN.(tokenList[i]):id==TKID_URES)
         aadd(result,TOKEN.(tokenList[i]):copyToken(TKID_URES," "))
         // outstd("Hopp!")
      else
         aadd(result,tokenList[i])
      endif
   end for
return result

//*******************************************************************

