/*
 * $Id$
 */

//*******************************************************************
// rmarker.prg: az RMARKER oszt ly implement ci¢ja.
// 1999, Csisz r Levente

//*******************************************************************
#include "ctoken.ch"

//*******************************************************************
#include "objgen.ch"

//*******************************************************************
#include "mmarker.och"
#include "token.och"
#include "tkstr.och"

//*******************************************************************
#define _RMARKER_PRG_
#define _IMPLEMENT_ONEW_

#include "rmarker.och"

//*******************************************************************
implement oinit(id,str,file,line,pos)
   super:oinit(id,str,file,line,pos)
   this:mmNum  :=nil
   this:mmTkId :=nil
return this

//*******************************************************************
implement getName()
return this:str

//*******************************************************************
implement setMMIdxByMM(matchMarkers)
// A matchMarkers-ben keres egy ugyanolyan nev– marker-t, mint 
// amilyen saj t maga, ‚s az mmNum-ot be ll¡tja a sorsz m ra,
// ‚s kit”lti a mmTkId-t.
// Egyenl“re case insensitive.
// Ret: .t., ha sikerlt, .f., ha nem.
local name,i

   name:=lower(this:getName())
   
   for i:=1 to len(matchMarkers)
      if (lower(MMARKER.matchMarkers[i]:getName())==name)
         this:mmNum:=MMARKER.matchMarkers[i]:mNum
         this:mmTkId:=MMARKER.matchMarkers[i]:id
         return .t.
      endif
   end for
return .f.

//*******************************************************************
cimplement isResultMarker(aToken)
// Meg llp¡tja, hogy az aToken egy result marker-e.
// Ret: .t., ha igezn, .f., ha nem.
static idResultMarkers:={;
   TKID_DUMB_STR_RESULT_MARKER   ,;
   TKID_REGULAR_RESULT_MARKER    ,;
   TKID_STRINGIFY_RESULT_MARKER  ,;
   TKID_SMART_STR_RESULT_MARKER  ,;
   TKID_BLOCKIFY_RESULT_MARKER   ,;
   TKID_LOGIFY_RESULT_MARKER     ;
} 

return 0!=ascan(idResultMarkers,TOKEN.aToken:id)   

//*******************************************************************
static function stringifyTokenList(tokenList)
local str,i

   str:=""
   for i:=1 to len(tokenList)
      str+=TOKEN.tokenList[i]:str
   end for
return stringifyStr(alltrim(str))

#ifdef OLD   
   if ('"' $ str)
      if ("'" $ str)
         // Ez akkor is ¡gy marad, ha van ']' az str-ben a spec 
         // szerint.
         // Itt azt lehetne csin lni, hogy sz‚tv gjuk a "'" 
         // karakterekn‚l a stringet ‚s az ¡gy kapott stringeket 
         // ”sszeadjuk, ‚s az eg‚sz kifejez‚st z r¢jelbe tesszk.
         str:="["+str+"]"
      else
         str:="'"+str+"'"
      endif
   else
      str:='"'+str+'"'
   endif
   
return str
#endif

//*******************************************************************
static function mkTkStrFromToken(t,str)
return C.TKSTR:copyFromToken(t,TKID_STRING,;
                             substr(str,2,len(str)-2),;
                             left(str,1),;
                             right(str,1))

//*******************************************************************
static function rmListChange(this,result,tl,addBlock,addEmptyBlock)
local i
#ifdef SPEEDY
// Ez nem jelent jelent“s gyorsul st.
// Az aadd() az ami meglehet“sen lass£, helyettes¡teni kellene, egy
// olyannal, ami el“re lefoglal valamennyi helyet.
local comma

   comma:=C.TOKEN:copyFromToken(if(!empty(result),atail(result),this),;
                                TKID_CHAR,",")
#endif
   if (this:mmTkId==TKID_LIST_MATCH_MARKER)
      for i:=1 to len(tl)
         if (i>=2)
            #ifdef SPEEDY
            aadd(result,comma)
            #else
            aadd(result,C.TOKEN:copyFromToken(atail(result),TKID_CHAR,","))
            #endif
         endif
         if (empty(tl[i]))
            if (addEmptyBlock!=nil)
               eval(addEmptyBlock,tl[i],i)
            endif
         else
            eval(addBlock,tl[i],i)
         endif
      end for
   elseif (!empty(tl))
      eval(addBlock,tl,0)
   endif
return nil

//*******************************************************************
implement changeByMMList(paramValues,result,iLevel,oneLevel) 
/*
 A result-ba beleteszi a this  ltal meghat rozott helyettes¡t‚st
 a paramValues-b“l az iLevel szinten. Ha a paramValues-ban az
 iLevel szinten nincs helyettes¡tend“, de van utols¢, akkor az 
 utols¢t helyettes¡ti, ‚s ezt resnek tekinti.
 Ha a oneLevel nem res, ‚s a paramValue-ban egyn‚l t”bb szint van
 a this-hez tartoz¢ result markerhez, akkor nem v‚gzi el a 
 helyettes¡t‚st.

 Ha a oneLevel nem res, akkor az 'res' helyettes¡t‚sekn‚l 2-t
 ad ‚s nem 1-et.
  
 Ret: 
    0: nem sikerlt a helyettes¡t‚s, 
    1: csak 'res' helyettes¡t‚s volt, (Nem volt illesztett match
       marker, de reset helyettes¡tett pl. dumb stringify result 
       marker, logify result marker.
    2: volt helyettes¡t‚s.
    
 Probl‚ma: Ha a match marker egy list_match_marker, akkor a 
 stringify ‚s a blockify markerek elemenk‚nt csin lj k
 a konverzi¢t. (Ez jelenleg nincs imlement lva.)
 
 Mj.: A paramValues-ban egy szinten egy tokenLista van, ami az 
      adott szinten a matchMarkerre illesztett tokeneket adja
      meg, kiv‚tel a list match marker, mert ott ilyen tokenList k
      list ja van, ami a list math markerre illesztett list kat
      tartalmazza.
*/
local tl,retVal,str,wtl

   if (len(paramValues)<this:mmNum)
      tl:={}
   else
      tl:=paramValues[this:mmNum]
   endif
   if (tl==nil .or. /*len(tl)<iLevel*/len(tl)==0)
      if (this:id==TKID_DUMB_STR_RESULT_MARKER)
         // aadd(result,C.TKSTR:copyFromToken(tl[1],TKID_STRING,"",'"','"'))
         aadd(result,mkTkStrFromToken(this,'""'))
         return if(!empty(oneLevel),2,1)
      elseif (this:id==TKID_LOGIFY_RESULT_MARKER)
         // Amikor tokeniz l s lesz, akkor a .t. ‚s a .f. egy 
         // token lesz.
         aadd(result,C.TOKEN:copyFromToken(this,TKID_CHAR,".F."))
         return if(!empty(oneLevel),2,1)
      endif
      return if(!empty(oneLevel),2,1)
   endif
   
   if (!empty(oneLevel) .and. len(tl)>1)
      return 0
   endif
   
   if (len(tl)<iLevel)
      tl:=atail(tl)
      retVal:=1
   else
      tl:=tl[iLevel]
      retVal:=2
   endif
   
   if (this:id==TKID_DUMB_STR_RESULT_MARKER)
      /*
       Itt probl‚m k l‚pnek fel, mert ez a result marker speci lisan
       viselkedik: 
       1. Ha a helyettes¡tend“ stringben " ‚s ' egyar nt el“fodul,
          akkor a spec hib s (ki kell pr¢b lni). Ezt nem felt‚tlenl
          ugyan£gy kell implement lni. Ez az ”sszes stringify
          markerre vonatkozik.
      */
      if (!empty(tl))
             
         wtl:={}
         rmListChange(this,wtl,tl,{|x| aappend(wtl,x)})
         aadd(result,mkTkStrFromToken(wtl[1],stringifyTokenList(wtl)))
         // rmListChange(this,result,tl,;
         //    {|x| aadd(result,mkTkStrFromToken(x[1],stringifyTokenList(x)))})
         // str:=stringifyTokenList(tl)
         // aadd(result,C.TKSTR:copyFromToken(tl[1],TKID_STRING,;
         //                                   substr(str,2,len(str)-2),;
         //                                   left(str,1),;
         //                                   right(str,1))
      endif
   elseif (this:id==TKID_REGULAR_RESULT_MARKER)
      rmListChange(this,result,tl,{|x|aappend(result,x)})
   elseif (this:id==TKID_STRINGIFY_RESULT_MARKER)
      // rmListChange(this,result,tl,;
      //    {|x|aadd(result,C.TOKEN:copyFromToken(x[1],TKID_STRING,stringifyTokenList(x)))})
      // A DOS K™RNYEZETFšGG§EN (pfuj) tesz space-t az elemek el‚ vagy m”g‚.
      // Mi nem tesznk sehova.
      rmListChange(this,result,tl,;
         {|x,i|aadd(result,mkTkStrFromToken(x[1],stringifyTokenList(x)))})
   elseif (this:id==TKID_SMART_STR_RESULT_MARKER)
      // rmListChange(this,result,tl,;
      //    {|x|if(TOKEN.x[1]:id==TKID_CHAR .and. TOKEN.x[1]:str=="(",;
      //           aappend(result,x),;
      //           aadd(result,C.TOKEN:copyFromToken(x[1],TKID_STRING,stringifyTokenList(x))))})
      rmListChange(this,result,tl,;
         {|x|if(TOKEN.x[1]:id==TKID_CHAR .and. TOKEN.x[1]:str=="(",;
                aappend(result,x),;
                aadd(result,mkTkStrFromToken(x[1],stringifyTokenList(x))))})
   elseif (this:id==TKID_BLOCKIFY_RESULT_MARKER)
      rmListChange(this,result,tl,;
         {|x,i|;
              if(i!=0 .and. i!=1,;
                 aadd(result,C.TOKEN:copyFromToken(x[1],TKID_URES," ")),;
                 nil),;
              aadd(result,C.TOKEN:copyFromToken(x[1],TKID_CHAR,"{")),;
              aadd(result,C.TOKEN:copyFromToken(x[1],TKID_CHAR,"|")),;
              aadd(result,C.TOKEN:copyFromToken(x[1],TKID_CHAR,"|")),;
              aappend(result,x),;
              aadd(result,C.TOKEN:copyFromToken(x[1],TKID_CHAR,"}"))})
         /* Nem szabad az res blokkokat begener lni, mert ez egy 
            hiba a DOS verzi¢ban.
         {|x,i|;
              if(i!=0 .and. i!=1,;
                 aadd(result,C.TOKEN:copyFromToken(this,TKID_URES," ")),;
                 nil),;
              aadd(result,C.TOKEN:copyFromToken(this,TKID_CHAR,"{")),;
              aadd(result,C.TOKEN:copyFromToken(this,TKID_CHAR,"|")),;
              aadd(result,C.TOKEN:copyFromToken(this,TKID_CHAR,"|")),;
              aadd(result,C.TOKEN:copyFromToken(this,TKID_CHAR,"}"))})
         */
              
   elseif (this:id==TKID_LOGIFY_RESULT_MARKER)
      // A list match markereket is egyben kell csin lni.
      if (!empty(tl))
         aadd(result,C.TOKEN:copyFromToken(;
            if(this:mmTkId==TKID_LIST_MATCH_MARKER,tl[1][1],tl[1]),;
            TKID_CHAR,".T."))
      else
         aadd(result,C.TOKEN:copyFromToken(this,TKID_CHAR,".F"))
      endif
   endif
return retVal

//*******************************************************************


