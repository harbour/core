/*
 * $Id$
 */

//*******************************************************************
// mcontrol.prg: Az MCONTROL oszt ly implement ci¢ja.
// 1999, Csisz r Levente

//*******************************************************************
// #define DEBUG
#include "debug.ch"
#include "ctoken.ch"

//*******************************************************************
#include "objgen.ch"

//*******************************************************************
#include "token.och"
#include "mparser.och"
#include "tbuffer.och"
#include "defdict.och"
#include "xtrdict.och"

//*******************************************************************
#define _MCONTROL_PRG_
#define _IMPLEMENT_ONEW_

#include "mcontrol.och"

//*******************************************************************
implement oinit(inputReader,name,defdict,xtrdict,errorStream,trPrsAlg)
           
   super:oinit(inputReader,name,errorStream)
   
   this:defdict:=defdict
   this:xtrdict:=xtrdict
   this:ujsor:=.t.
   this:trPrsAlg:=trPrsAlg
   
return this
   
//*******************************************************************
#ifdef HATULROL_ELORE

//*******************************************************************
// H tulr¢l el“re helyettes¡t‚s.
#error "Ez m r nem m–k”dik, csak az algoritmus megmutat s ra van itt."
//*******************************************************************
implement readItem()
/*
   - Egy  £j mcontrol az £j mparser inputja, ¡gy oldja meg a 
     rekurzi¢t.
     
   - Sikertelen helyettes¡t‚sn‚l az eg‚sz megvizsg lt token
     sorozat mehet az outputra.
     
   - Sikeres helyettes¡t‚sn‚l az £j token sorozat visszamegy az 
     inputra.

   - Nem ismeri az xcommand-ot.
*/
local item,w,newMControl,nmtbuf
local i

   while(nil!=(item:=this:readInput()) .and.;
         (TOKEN.item:id==TKID_NEV .or.;
          TOKEN.item:id==TKID_CHAR .or.;
          TOKEN.item:id==TKID_STRING .or.;
          TOKEN.item:id==TKID_SZAMTOMB))
         
      newMControl:=thisclass:onew(this:inputReader,;
                                  this:name,;
                                  this:defdict,;
                                  this:xtrdict,;
                                  this:errorStream)
      if (nil==(w:=C.MPARSER:parse(item,;
                         newMControl,;
                         this:name,;
                         this:defdict,;
                         this:xtrdict,;
                         this:errorStream)))
         // Nincs ilyen makr¢
         exit
      endif

      // A newMControl-ban lev“ puffert r¡teni kell, mert most a 
      // newMControl-t t”r”lni fogjuk. Ez‚rt puffer tartalm t 
      // visszatesszk az inputra (sikeress‚g eset‚n), ‚s kitesszk
      // az outputra (sikertelens‚g eset‚n)
      // Mj.: ltal ban resnek kell lennie, egyenl“re nem sikerlt
      //      olyan teszt fil‚t csin lni, amiben a newMControl 
      //      puffer‚ben lett volna valami.

      // PDEBUG(outerr("megvan!",crlf()))      
      if (w[1])
         // Sikeres. Az eredm‚nyt vissza kell tenni az inputra.

         // El“sz”r a newMControl-ban lev“ puffer megy.
         nmtbuf:=CTHIS.newMControl:arrayTBuffer()
         for i:=len(nmtbuf) to 1 step -1
            PDEBUG(outerr("newMControl vissza az inputra: "+TOKEN.(nmtbuf[i]):getStr(),crlf()))
            this:unreadInput(nmtbuf[i])
         end for

         // Azut n maga az eredm‚ny.
         w:=w[2]
         for i:=len(w) to 1 step -1
            this:unreadInput(w[i])
         end for
      else
         // A newMControl-ban lev“ puffert is elkldhetjk, mert
         // az m r elemezve volt.
         nmtbuf:=CTHIS.newMControl:arrayTBuffer()
         for i:=len(nmtbuf) to 1 step -1
            PDEBUG(outerr("newMControl az outputra: "+TOKEN.(nmtbuf[i]):getStr(),crlf()))
            this:unread(nmtbuf[i])
         end for

         // Sikertelen, az eredm‚ny megy az outputra.
         w:=w[2]
         for i:=len(w) to 1 step -1
            this:unread(w[i])
         end for

         // Most a tbuffer-b“l kell olvasnunk.
         if (nil!=(item:=this:readTBuffer()))
            exit
         endif
      endif
   end while
   
return item
//*******************************************************************

#else

//*******************************************************************
static function findTreeMatch(dictTree,tkId,tkStr)
local i,leftToken,id

   for i:=1 to len(dictTree)
      leftToken:=dictTree[i][1]
      id:=TOKEN.leftToken:id
      if !(id==TKID_NEV .or.;
          id==TKID_CHAR .or.;
          id==TKID_STRING .or.;
          id==TKID_SZAMTOMB)
         return i
      endif
      if (isMatchNToken(tkId,tkStr,;
                       TOKEN.leftToken:id,TOKEN.leftToken:str,;
                       TOKEN.leftToken:eqType))
         return i
      endif
   end for
return 0
//*******************************************************************
// El“r“l h tra helyettes¡t‚s.
//*******************************************************************
implement readItem()
/*
 El“r”l h tra helyettes¡t‚s.
 
 - Nincs szks‚g arra, hogy az mcontrol legyen az £j mparser
   inputja.
   
 - Sikertelen helyettes¡t‚sn‚l csak egy tokent mehetnk el“re.
 
 - Sikeres helyettes¡t‚sn‚l a sor elej‚ig kell visszamenni.
   Ebb“l k”vetkezik, hogy csak akkor ad token-t, ha a teljes sort
   beolvasta ‚s azon m r nem lehet helyettes¡t‚st v‚gezni.
   
 - Ez ismeri az xcommand-ot is.
*/

local item,w,i,edefdict,mi,mehet

   // Ha a tbuffer-ben van valami, akkor abb¢l adunk.
   if (nil!=(item:=this:readTBuffer()))
      return item
   endif
   
   // A tbuffer res, £j sor van.
   this:ujsor:=.t.
   mi:=array(2)

   while(nil!=(item:=this:readInput()) .and.;
         !(TOKEN.item:id==TKID_UJSOR .or.;
          TOKEN.item:id==TKID_BOS   .or.;
          TOKEN.item:id==TKID_EOS   .or.;
          (TOKEN.item:id==TKID_CHAR .and. TOKEN.item:str==";")))
          
      if (TOKEN.item:id==TKID_NEV .or.;
          TOKEN.item:id==TKID_CHAR .or.;
          TOKEN.item:id==TKID_STRING .or.;
          TOKEN.item:id==TKID_SZAMTOMB)
         // Helyettes¡tnk.
         mehet:=.f.
         if (TOKEN.item:id==TKID_NEV)
             if (nil!=(edefdict:=DEFDICT.(this:defdict):atKey(TOKEN.item:str)))
                mehet:=.t.
             endif
         else
            edefdict:=nil
         endif
         if (this:trPrsAlg==TRPRA_TREE)
            if (0!=(mi[1]:=findTreeMatch(;
                              XTRDICT.(this:xtrdict):trdictTree,;
                              TOKEN.item:id,;
                              TOKEN.item:str)))
               mehet:=.t.
            endif
            if (this:ujsor)
               if (0!=(mi[2]:=findTreeMatch(;
                                 XTRDICT.(this:xtrdict):cmdictTree,;
                                 TOKEN.item:id,;
                                 TOKEN.item:str)))
                  mehet:=.t.
               endif
            else
               mi[2]:=0
            endif
            // mi:=nil
         else
            mi[1]:=0
            mi[2]:=0
         endif
         if (mehet)
            if (nil==(w:=C.MPARSER:parse(item,;
                                         this:inputReader,;
                                         this:name,;
                                         this:defdict,;
                                         edefdict,;
                                         this:xtrdict,;
                                         mi,;
                                         this:errorStream,;
                                         this:ujsor,;
                                         this:trPrsAlg)))
               // Nincs ilyen makr¢.
               // Az item a tbuffer-be.
               TBUFFER.(this:tBuffer):put(item)
               this:ujsor:=.f.
            elseif (w[1])
               // Sikeres.
               // Az eredm‚ny ‚s a tbuffer vissza az inputra ‚s megint sor
               // elej‚n vagyunk.
               
               // El“sz”r az eredm‚ny.
               w:=w[2]
               for i:=len(w) to 1 step -1
                  this:unreadInput(w[i])
               end for
               
               // Azut n a tbuffer
               for i:=TBUFFER.(this:tBuffer):bItemNumber() to 1 step -1
                  PDEBUG(outerr("tBuffer vissza az inputra: "+;
                                TOKEN.(TBUFFER.(this:tBuffer):getBItem(i)):getStr(),newline()))
                  this:unreadInput(TBUFFER.(this:tBuffer):getBItem(i))
               end for
               TBUFFER.(this:tBuffer):clear()
               this:ujsor:=.t.
            else
               // Sikertelen.
               // Az eredm‚ny els“ item-je az output-ra megy, a t”bbi
               // az inputra.
            
               w:=w[2]
               for i:=len(w) to 2 step -1
                  this:unreadInput(w[i])
               end for
               TBUFFER.(this:tBuffer):put(w[1])
               this:ujsor:=.f.
            endif
         else
           // ttesszk.
            TBUFFER.(this:tBuffer):put(item)
         endif
      else
         // ttesszk.
         TBUFFER.(this:tBuffer):put(item)
      endif
   end while
   if (item!=nil)
      TBUFFER.(this:tBuffer):put(item)
   endif
return TBUFFER.(this:tBuffer):get()
   
#ifdef OLD
   while(nil!=(item:=this:readInput()) .and.;
         (TOKEN.item:id==TKID_NEV .or.;
          TOKEN.item:id==TKID_CHAR .or.;
          TOKEN.item:id==TKID_STRING .or.;
          TOKEN.item:id==TKID_SZAMTOMB))
         
      // newMControl:=thisclass:onew(this:inputReader,;
      //                             this:name,;
      //                             this:defdict,;
      //                             this:xtrdict,;
      //                             this:errorStream)
      if (nil==(w:=C.MPARSER:parse(item,;
                         this:inputReader,;
                         this:name,;
                         this:defdict,;
                         this:xtrdict,;
                         this:errorStream,this:ujsor)))
         // Nincs ilyen makr¢
         exit
      endif

      // PDEBUG(outerr("megvan!",crlf()))      
      if (w[1])
         // Sikeres.
         
         w:=w[2]
         for i:=len(w) to 1 step -1
            this:unreadInput(w[i])
         end for
      else

         // Sikertelen, az eredm‚ny els“ tokenje megy az outputra,
         // a t”bbi az inputra.
         w:=w[2]
         for i:=len(w) to 2 step -1
            this:unreadInput(w[i])
         end for
         
         item:=w[1]
         exit
         #ifdef OLD
         if (len(w)>1)
            this:unread(w[1])
         endif
         
         // Most a tbuffer-b“l kell olvasnunk.
         if (nil!=(item:=this:readTBuffer()))
            exit
         endif
         #endif
      endif
   end while
   if (item!=nil)
      if (!TOKEN.item:id==TKID_URES)
         this:ujsor:=TOKEN.item:id==TKID_UJSOR .or.;
                     TOKEN.item:id==TKID_BOS   .or.;
                     TOKEN.item:id==TKID_EOS   .or.;
                     (TOKEN.item:id==TKID_CHAR .and. TOKEN.item:str==";")
      endif
   endif
return item
#endif

//*******************************************************************
#endif

//*******************************************************************



