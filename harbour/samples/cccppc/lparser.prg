/*
 * $Id$
 */

//*******************************************************************
// lparser.prg: Az lparser oszt ly implement ci¢ja.
// 1999, Csisz r Levente
/*
   Sor elemz“. A TREADER-t“l ”r”k”l.
   Ez az elemz“ vonja ”ssze a folytat¢ sorokat.
   Sajnos a hlye spec. miatt £gy nem lehet meg¡rni, hogy 
   folyamatosan elemezzen, mert a spec. a sor”sszevon sok ut n 
   az res sorokat az ”sszevon s eredm‚nyek‚nt l‚trej”tt sor EL 
   teszi, ¡gy be kell olvasni az eg‚sz sort, hogy ezt mi is 
   megtehessk.
   —gy m–k”dik, hogy beolvas egy teljes sort, majd megn‚zi hogy
   kell-e ”sszevonni. Ha nem, akkor abb¢l ad, m¡g el nem fogy,
   ha igen, akkor a v‚g‚hez hozz olvassa a k”vetkez“ sort, etc.
   Amikor token-t ad, akkor vagy res, vagy az olvasott sor m‚g be
   nem olvasott r‚sz‚t t rolja a sorv‚gjellel egytt. 
*/

#include "objgen.ch"

//*******************************************************************
#include "tbuffer.och"
#include "creader.och"
#include "token.och"

//*******************************************************************
#include "cr_lf.ch"
#include "ctoken.ch"

//*******************************************************************
#define _LPARSER_PRG_
#define _IMPLEMENT_ONEW_

#include "lparser.och"

//*******************************************************************
#define isNullChar(t)  ((t)!=nil .and.;
                        TOKEN.(t):id==TKID_CHAR .and.;
                        TOKEN.(t):str=="")

//*******************************************************************
implement readItem()
// A nullChar-okat eldobja.
local w

   while(.t.)
      if (nil==(w:=this:getParserBuffer()))
         this:readLine()
         w:=this:getParserBuffer()
      endif
      if (!isNullChar(w))
         exit
      endif
   end while
   
return w

//*******************************************************************
implement readInput()
// A #line-okat itt kell lejjebb sz ll¡tani.
// Az ut nuk j”v“ res sorokat t”r”lni kell, a sor sz m ‚rt‚k‚t
// pedig a t”r”lt sorok sz m val megn”velni.
local t,tkId
local bos,line,iLine,i

   while(nil!=(t:=super:readInput()))
      tkId:=TOKEN.t:id
      if (tkId==TKID_BOS .and.;
          left(TOKEN.t:str,5)=="#line" .and.;
          TOKEN.t:line>1)
         // Lejjebb sz ll¡t s indul
         bos:=t
         line:={}
         iLine:=TOKEN.t:line
         while(nil!=(t:=super:readInput()))
            if (TOKEN.t:id==TKID_URES)
               aadd(line,t)
            elseif (TOKEN.t:id==TKID_UJSOR)
               line:={}
               iLine++
            else
               super:unreadInput(t)
               for i:=len(line) to 1 step -1
                  super:unreadInput(line[i])
               end for
               TOKEN.bos:str:=makeHSLineStr(TOKEN.bos:file,iLine) 
               return bos
            endif
         end while
      endif
      if !(tkId==TKID_PPCOMMENT .or. tkId==TKID_CSCOMMENT)
         exit
      endif
   end while
return t

//*******************************************************************
#define ST_START                    0
// #define ST_URES                     1
#define ST_PVESSZO                  2
#define ST_PVESSZO_URES             3
#define ST_PVESSZO_UJSOR            4
#define ST_PVESSZO_UJSOR_URES       5

implement readLine()
// Amikor h¡vj k a parserBuffer-nek resnek kell lennie.
// Beolvas egy teljes sort, az egy soros megjegyz‚seket eldobja, 
// a t”bb sorosakat feldolgozza, a folytat¢ sorokat ”sszevonja.
// A folytat¢ sorokat £gy vonja ”ssze, hogy annyi £j sort rak a sor
// el‚, amennyit sort ”sszevont.
// Mj.: Az reseket nem vonja ”ssze.
local tkId,t,i
local state, numUres,nSor:={}
               
   state:=ST_START
   this:rds()
   while(this:item!=nil)    
      tkId:=TOKEN.(this:item):id
      /* A PPCOMMENT-et ‚s a CSCOMMENT-et m r a readinput-ban eldobjuk.
      if (tkId==TKID_CSCOMMENT .or. tkId==TKID_PPCOMMENT)
         // Eldobjuk.
         this:unputParserBuffer()
         this:item:=nil
      else
      */
      if (tkId==TKID_PCCOMMENT)
         // A puffer elej‚re annyi £j sort kell besz£rni, mint
         // ah ny itt van.
         for i:=1 to xnumat(CHAR_LF,TOKEN.(this:item):str)
            this:ungetParserBuffer(;
               TOKEN.(this:item):copyToken(TKID_UJSOR,guessedEOL()))
         end for                 
         // Maga a megjegyz‚s nem kell, eldobjuk.
         this:unputParserBuffer()
         // A hely‚re egy 'semmi' karaktert rakunk, mert ezt pl ';' 
         // ut n ‚rt‚kesnek kell tekinteni.
         this:putParserBuffer(TOKEN.(this:item):copyToken(TKID_CHAR,""))
         this:unrds()
      elseif (state==ST_START)
         if (tkId==TKID_UJSOR .or. tkId==TKID_BOS .or. tkId==TKID_EOS)
            exit
         // elseif (tkId==TKID_URES)
         //    state:=ST_URES
         elseif (tkId==TKID_CHAR .and. TOKEN.(this:item):str==";")
            state:=ST_PVESSZO
         endif
      elseif (state==ST_PVESSZO)
         if (tkId==TKID_UJSOR .or. tkId==TKID_BOS .or. tkId==TKID_EOS)
            // Az £j sort ‚s a pontosvessz“t el kell dobni.
            aadd(nSor,this:item)
            this:unputParserBuffer()
            this:unputParserBuffer()
            state:=ST_PVESSZO_UJSOR
         elseif (tkId==TKID_URES)
            // Ezt el kell tenni, ‚s ha egy £j sor j”tt, akkor el 
            // kell dobni.
            numUres:=1
            state:=ST_PVESSZO_URES
         else
            // Pontosvessz“ ut n valami ‚rt‚kes. Vissza a startba.
            // Mj.: Ide tartozik a pontosvessz“ ut ni pontosvessz“ is.
            state:=ST_START
         endif
      elseif (state==ST_PVESSZO_URES)
         if (tkId==TKID_UJSOR .or. tkId==TKID_BOS .or. tkId==TKID_EOS)
            // Az £j sort eldobjuk
            aadd(nSor,this:item)
            this:unputParserBuffer()
            // šreseket eldobjuk.

            while(numUres>0)
               this:unputParserBuffer()
               numUres--
            end while

            // A pontosvessz“t eldobjuk.
            this:unputParserBuffer()
            state:=ST_PVESSZO_UJSOR
            
         elseif (tkId==TKID_URES)
            // Ezt el kell tenni, ‚s ha egy £j sor j”tt, akkor el 
            // kell dobni.
            numUres++
         else
            // Pontosvessz“ ut n valami ‚rt‚kes. Vissza a startba.
            // Mj.: Ide tartozik a pontosvessz“ ut ni pontosvessz“ is.
            state:=ST_START
         endif
      elseif (state==ST_PVESSZO_UJSOR)
         // Ez a folytat¢ sor.
         if (tkId==TKID_UJSOR .or. tkId==TKID_BOS .or. tkId==TKID_EOS)
            exit
         elseif (tkId==TKID_URES)
            // Ezt egy darab space-ra kell helyettes¡teni.
            t:=TOKEN.(this:item):copyToken(TKID_URES,space(1))
            this:unputParserBuffer()
            this:putParserBuffer(t)
            this:item:=t
            state:=ST_PVESSZO_UJSOR_URES
         elseif (tkId==TKID_CHAR .and. TOKEN.(this:item):str==";")
            state:=ST_PVESSZO
         else
            state:=ST_START
         endif
      elseif (state==ST_PVESSZO_UJSOR_URES)
         if (tkId==TKID_UJSOR .or. tkId==TKID_BOS .or. tkId==TKID_EOS)
            exit
         elseif (tkId==TKID_URES)
            // Ezt el kell dobni.
            this:unputParserBuffer()
         else
            this:unrds()
            state:=ST_START
         endif   
         // if (tkId==TKID_CHAR .and. TOKEN.(this:item):str==";")
         //    state:=ST_PVESSZO
         // else
         //    state:=ST_START
         // endif
      else
         ? "LPARSER:readline(): Ismeretlen  llapot: ",state
         errorlevel(1)
         quit
      endif
      this:rds()
   end while
   aeval(nSor,{|x| this:ungetParserBuffer(x)})
return nil

#ifdef OLD
   if (2<=(n:=TBUFFER.(this:parserBuffer):bItemNumber))
      utolsoUres:=;
         TKID_URES==tkId(TBUFFER.(this:parserBuffer):getBItem(n-1))
   else
      utolsoUres:=.f.
   endif

   while(this:item!=nil)    
      tkId:=TOKEN.(this:item):id
      if (tkId==TKID_UJSOR .or. tkId==TKID_BOS .or. tkId==TKID_EOS)
         // Itt kell elint‚zni a sor ”sszevon sokat. Sort akkor kell
         // ”sszevonni, ha a pufferben (az resek ‚s az £j sor kiv‚tel‚vel)
         // az utols¢ token egy ';'.
         // Ekkor a ';'-t t”r”ljk, ‚s ha ut na res j”n (majd), akkor azt
         // egy ' '-re helyettes¡tjk (el‚g fura specifik ci¢, nemdeb r? ;)
         // Itt kell elint‚zni a sor ”sszevon sokat.
         exit
      endif
      if (tkId==TKID_PCCOMMENT)
         // A puffer elej‚re annyi £j sort kell besz£rni, mint
         // ah ny itt van.
         for i:=1 to xnumat(CHAR_LF,TOKEN.(this:item):str)
            this:ungetParserBuffer(;
               TOKEN.(this:item):copyToken(TKID_UJSOR,guessedEOL()))
         end for                 
         // Maga a megjegyz‚s nem kell, eldobjuk.
         this:unputParserBuffer()
         this:item:=nil
      elseif (tkId==TKID_URES .and. utolsoUres)
         // Az ez el“tti is res, a kett“t ”ssze kell vonni.
         n:=TBUFFER.(this:parserBuffer):bItemNumber
         w:=TBUFFER.(this:parserBuffer):getBItem(n-1)
         // ™sszevonjuk.
         t:=TOKEN.w:copyToken(TKID_URES,;
                              TOKEN.w:str+TOKEN.(this:item):str)
         this:unputParserBuffer()
         this:unputParserBuffer()
         this:putParserBuffer(t)
      endif
      if (this:item!=nil)
         utolsoUres:=tkId==TKID_URES
      endif
      this:rds()
   end while
return nil
#endif

//*******************************************************************

