/*
 * $Id$
 */

//*******************************************************************
// nparser.prg: Az nparser oszt†ly implement†ci¢ja.
// 1999, Csisz†r Levente

// NÇv elemzì. A karakter folyamot tokeniz†lja.
// A tokenek: nÇv, string, sz†m, megjegyzÇs, etc.

//*******************************************************************
#define NPRERRGROUP       "hparser"

#define NPRERR_ENDPCCOMMENT   {NPRERRGROUP,"endpccomment"}
#define NPRERR_ENDSTRING      {NPRERRGROUP,"endstring"}

//*******************************************************************
// Ezek tulajdonkÇppen nem kellenek, senki nem haszn†lja.
#define CTID_PLUSPLUS    "plusplus"
#define CTID_MINUSMINUS  "minusminus"
#define CTID_ARROW       "arrow"
#define CTID_AND         "and"
#define CTID_OR          "or"
#define CTID_NOT         "not"
#define CTID_LET         "let"
#define CTID_AD          "ad"
#define CTID_TRUE        "true"
#define CTID_FALSE       "false"
#define CTID_PERPER      "perper"
#define CTID_PERSTAR     "perstar"

#define CTID_EQEQ        "eqeq"
#define CTID_PLUSEQ      "pluseq"
#define CTID_MINUSEQ     "minuseq"
#define CTID_MULEQ       "muleq"
#define CTID_DIVEQ       "diveq"
#define CTID_MODEQ       "modeq"

#define CTID_LESSEQ      "lesseq"
#define CTID_GREATEQ     "greateq"
#define CTID_NOTEQ1      "noteq1"
#define CTID_NOTEQ2      "noteq2"
                         
//*******************************************************************
#include "objgen.ch"

//*******************************************************************
#include "tbuffer.och"
#include "creader.och"
#include "token.och"
#include "tokenst.och"
#include "tkstr.och"
#include "prserr.och"

//*******************************************************************
#include "cr_lf.ch"
#include "ctoken.ch"
#include "error.ch"

//*******************************************************************
#define _NPARSER_PRG_
#define _IMPLEMENT_ONEW_

#include "nparser.och"

//*******************************************************************
// ôsszehasonl°tja a this:item-et egy karaterrel.
#define eqItemChar(aChar) (valtype(this:item)=="C" .and. ;
                           this:item==(aChar))

#define isItemEol()   (valtype(this:item)=="A" .and. this:item[1]==CTKID_EOL)

//*******************************************************************
cimplement oinitclass()
local i,errStr,err

   superclass:oinitclass()
   class:whitespacechar  :=""
   for i:=1 to 32
      if (i!=10 .and. i!=13)
         class:whitespacechar := class:whitespacechar+chr(i)
      endif
   end for
   /* class:specchar    :=;
      CTK_CR         +;
      CTK_LF         +;
      CTK_IDEZ       +;
      CTK_MACS       +;
      CTK_PER        +;
      CTK_PVESSZO */
      
   class:startnamechar:="abcdefghijklmnopqrtsuvwxyz"+;
                        "ABCDEFGHIJKLMNOPQRTSUVWXYZ"+;
                        "_"
                    
   class:numberchar   :="0123456789"

   class:namechar     :=class:startnamechar+;
                        class:numberchar

   class:tokens:={}         
   class:setTokens()       // Be†llitjuk a tokeneket
   if (nil!=(errStr:=class:calcTokenPTree()))  // ElkÇsz°tjÅk az elemzì f†t.
      err:=errorNew()
      err:cargo:=class:tokens
      err:description:=errStr
      err:filename:=''
      err:severity:=ES_ERROR
      err:operation:="calcTokenPTree "+errStr
      err:subsystem:="nparser"
      err:subCode:=1
      eval(errorblock(),err)
   endif

   C.PRSERR:registerError(NPRERR_ENDPCCOMMENT  ,;
      "Unterminated /* */ comment")
   C.PRSERR:registerError(NPRERR_ENDSTRING     ,;
      "Unterminated string")
return class

//*******************************************************************
cimplement isWhitespacechar(c)
return valtype(c)=="C" .and. c$class:whitespacechar
   
//*******************************************************************
// cimplement isSpecchar(c)
// return valtype(c)=="C" .and. c$class:specchar   

//*******************************************************************
cimplement isStartnamechar(c)
return valtype(c)=="C" .and. c$class:startnamechar
   
//*******************************************************************
cimplement isNumberchar(c)
return valtype(c)=="C" .and. c$class:numberchar
   
//*******************************************************************
cimplement isNamechar(c)
return valtype(c)=="C" .and. c$class:namechar
   
//*******************************************************************
cimplement setTokens()
   aadd(class:tokens, {CTID_PLUSPLUS   ,"++"       ,nil})
   aadd(class:tokens, {CTID_MINUSMINUS ,"--"       ,nil})
   aadd(class:tokens, {CTID_AD         ,"**"       ,"^"})
   aadd(class:tokens, {CTID_ARROW      ,"->"       ,nil})
   aadd(class:tokens, {CTID_LET        ,":="       ,nil})
   aadd(class:tokens, {CTID_LESSEQ     ,"<="       ,nil})
   aadd(class:tokens, {CTID_NOTEQ1     ,"<>"       ,nil})
   aadd(class:tokens, {CTID_NOTEQ2     ,"!="       ,"<>"})
   aadd(class:tokens, {CTID_EQEQ       ,"=="       ,nil})
   aadd(class:tokens, {CTID_GREATEQ    ,">="       ,nil})
   aadd(class:tokens, {CTID_PLUSEQ     ,"+="       ,nil})
   aadd(class:tokens, {CTID_MINUSEQ    ,"-="       ,nil})
   aadd(class:tokens, {CTID_MULEQ      ,"*="       ,nil})
   aadd(class:tokens, {CTID_DIVEQ      ,"/="       ,nil})
   aadd(class:tokens, {CTID_MODEQ      ,"%="       ,nil})
   

   aadd(class:tokens, {CTID_AND        ,".and."    ,".AND."})
   aadd(class:tokens, {CTID_OR         ,".or."     ,".OR."})
   aadd(class:tokens, {CTID_NOT        ,".not."    ,"!"})
   aadd(class:tokens, {CTID_TRUE       ,".t."      ,".T."})
   aadd(class:tokens, {CTID_FALSE      ,".f."      ,".F."})


   
   // aadd(class:tokens, {CTID_PERPER     ,"//"        })
   // aadd(class:tokens, {CTID_PERSTAR    ,"/*"        })
return nil
   
//*******************************************************************
static function addTPTree(tPTree,ctId,ctStr,allStr,params)
local c,i,node

   if (len(ctStr)==0)
      // Itt a vÇge.
      node:={nil,ctId,allStr,params}
      if (len(tPTree)==0)
         aadd(tPTree,node)
         return nil
      endif
      if (tpTree[1]==nil)
         // M†r van egy termin†lisunk ugyanilyen tartalommal.
         return "addTPTree: M†r van ilyen token a f†ban: '"+allStr+"'"
      endif
      aunget(tPTree,node)
      return nil
   endif
   c:=left(ctStr,1)
   for i:=1 to len(tPTree)
      if (tPTree[i][1]==c)
         return addTPTree(tPTree[i][2],ctId,substr(ctStr,2),allStr,params)
      endif      
   end for
   // Hozz† kell venni.
   node:={c,{}}
   aadd(tPTree,node)
return addTPTree(node[2],ctId,substr(ctStr,2),allStr,params)

//*******************************************************************
cimplement calcTokenPTree()
// KÇsz°t egy elemzì f†t a tokenek elemzÇsÇhez a class:tokenPTree-be.
/*
   Az elemzì fa:
   <root>:=<node>
   <node>:={ [<terminal>,] <inner branch1>, <inner branch2>, ... }

   <inner branch>:={karakter,<node>}

   <terminal>:={nil,<id>,fullStr,params}
   
   Karakter: A i. szinten a token i. karaktere.
   id:       A token id-je.
   fullStr:  A token, mint string.
*/
local i, errStr,wStr

   class:tokenptree:={}
   
   errStr:=""
   for i:=1 to len(class:tokens)
      if (nil!=(wStr:=addTPTree(class:tokenptree,;
                                class:tokens[i][1],;
                                class:tokens[i][2],;
                                class:tokens[i][2],;
                                class:tokens[i][3])))
         errStr:=errStr+wStr+crlf()
      endif
   end for
return if (len(errStr)==0,nil,errStr)

//*******************************************************************
implement oinit(inputReader,name,errorStream)
           
   super:oinit(inputReader,name,errorStream)
   
   // this:item:=nil
   // this:buf:=C.TBUFFER:onew()
   this:soreleje:=.t.
   
return this
   
//*******************************************************************

//*******************************************************************
implement readItem()
   
   this:tokenPos:={CREADER.(this:inputReader):getFile(),;
                   CREADER.(this:inputReader):getLine(),;
                   CREADER.(this:inputReader):getPos()}
//   TBUFFER.(this:buf):clear()
//   this:rds()
   super:readItem()
return this:parse()

//*******************************************************************
implement makeToken(tkId,str)

return C.TOKEN:onew(tkId,str,;
                    this:tokenPos[1],;
                    this:tokenPos[2],;
                    this:tokenPos[3])
                   
//*******************************************************************
implement makeTokenSt(tkId,stArray)
// Az stArray szerkezete: {ctkId,filename,line,pos,deep}

return C.TOKENST:onew(tkId,"",;
                      stArray[2],;
                      stArray[3],;
                      stArray[4],;
                      stArray[5])
                   
//*******************************************************************
implement makeTokenLSt(tkId,stArray)
// Az stArray szerkezete: {ctkId,filename,line,pos,deep}

local str

   if (stArray[3]>1 .or. stArray[5]>1)
      str:=makeHSLineStr(stArray[2],stArray[3]) 
      
      // str:="#line "+toStr(stArray[3])+" "+;
      //      '"'+stArray[2]+'"'+guessedEol()
   else
      str:=""
   endif
        
return C.TOKENST:onew(tkId,str,;
                      stArray[2],;
                      stArray[3],;
                      stArray[4],;
                      stArray[5])
                   
//*******************************************************************
implement makeErrToken(tkId,str,errCode)
local t

   t:=C.TOKEN:onew(tkId,str,;
                   this:tokenPos[1],;
                   this:tokenPos[2],;
                   this:tokenPos[3])
   TOKEN.t:error:=errCode
return t
                
//*******************************************************************
#define LASTSTOP_NRDS  1
#define LASTSTOP_ITEM  2
#define LASTSTOP_ID    3
#define LASTSTOP_STR   4

#define N_LASTSTOP     4

//*******************************************************************
static function newLastStop(nrds,item,id,str)
local o

   o:=array(N_LASTSTOP)

   o[LASTSTOP_NRDS ]:=nrds
   o[LASTSTOP_ITEM ]:=item
   o[LASTSTOP_ID   ]:=id
   o[LASTSTOP_STR  ]:=str
   
return o

//*******************************************************************
static function lststUnrds(lastStop,this,nrds)
   this:unrds(nrds-lastStop[LASTSTOP_NRDS])
   this:item:=lastStop[LASTSTOP_ITEM]
return nil

//*******************************************************************
implement parseTPTree()
// Az input folyamr¢l Çrkezì karaktereket elemzi. A this:item-nek
// karakternek kell lennie, olvas, ha szÅksÇges.
// A betñket 'case insensitive'-kÇnt hasonl°tja îssze.
local tptree,nRds,cItem:=this:item
local str,lastStop,i

   str:=this:item

   tptree:=thisclass:tokenptree
   nRds:=0
   lastStop:=newLastStop(0,this:item,nil,nil)
   while(.t.)
      if (0==(i:=ascan(tptree,{|x| x[1]==lower(this:item)})))
         // Nincs tov†bb, viszont a lastStop-ban van olyan token,
         // ami illeszkedik ennek az elejÇhez.
         exit
      endif
      
      // Van ilyen karakter, megyÅnk lejjebb a f†ban
      tptree:=tptree[i][2]
      // Elìszîr megnÇzzÅk, hogy a vÇgÇre ÇrtÅnk-e.
      if (len(tptree)==0)
         // Hiba, vagy Åres a fa, vissza a lastStop-hoz.
         exit
      endif
      if (tptree[1][1]==nil)
         // Itt meg lehet †llni, pl. ha a kîvetkezì keresÇs nem hoz
         // eredmÇnyt.
         if (len(tptree)==1)
            // Nem lehet tov†bb menni, de visszalÇpni sem kell.
            // id:=tptree[1][2]
            if (tptree[1][4]!=nil)
               // Incompatibility
               // Helyettes°tÅnk.
               // outerr("parseTPTree: "+tptree[1][4],newline())
               return tptree[1][4]
            else
               return str
            endif
         endif
         // Lehet mÇg tov†bb, de ha elakad, akkor ez j¢!
         lastStop[LASTSTOP_NRDS ]:=nRds
         lastStop[LASTSTOP_ITEM ]:=this:item
         lastStop[LASTSTOP_ID   ]:=tptree[1][2]
         if (tptree[1][4]!=nil)
            // Incompatibility
            // Helyettes°tÅnk.
            lastStop[LASTSTOP_STR  ]:=tptree[1][4]
         else
            lastStop[LASTSTOP_STR  ]:=str
         endif
      endif

      // Olvasunk.      
      this:rds()
      nRds++
      if (valtype(this:item)!="C")
         exit
      endif
      str+=this:item
   end while

   lststUnrds(lastStop,this,nrds)
   
return lastStop[LASTSTOP_STR] // Ez nil, ha nem †ll°tottuk be!

//*******************************************************************
implement parse()
// Ez vÇgzi a tÇnyleges elemzÇst, rekurz°van is lehet h°vni.
// A this:item-t elemzi, szÅksÇg esetÇn mÇg olvashat.
// 
local wSoreleje:=this:soreleje
local perItem,crItem,wStr

   if (this:item==nil)
      return nil
   endif
   if (!valtype(this:item)=="C")
      if (this:item[1]==CTKID_PRINTLINE)
         return this:makeToken(TKID_PRINTLINE,;
                        "#line "+toStr(CREADER.(this:inputReader):getLine())+" "+;
                        '"'+CREADER.(this:inputReader):getFile()+'"'+;
                        guessedEol())
      elseif (this:item[1]==CTKID_BOS)
         this:soreleje:=.t.
         // outerr("bos",newline())
         return this:makeTokenLSt(TKID_BOS,this:item)
      elseif (this:item[1]==CTKID_EOS)
         if (!this:soreleje)
            // outerr("eos+newline",newline())
            this:unread(this:makeTokenSt(TKID_EOS,this:item))
            this:soreleje:=.t.
            return this:makeToken(TKID_UJSOR,guessedEol())
         endif
         // outerr("eos",newline())
         return this:makeTokenSt(TKID_EOS,this:item)
      elseif (this:item[1]==CTKID_EOL)
         // outerr("eol",newline())
         this:soreleje:=.t.
         return this:makeToken(TKID_UJSOR,this:item[2])
      endif
   else
      // outerr("egyeb",newline())
      this:soreleje:=.f.
      if (thisclass:isWhitespacechar(this:item))
         this:rds()
         while(thisclass:isWhitespacechar(this:item))
            this:rds()
         end while
         this:unrds()
         this:soreleje:=wSoreleje
         return this:makeToken(TKID_URES,this:strParserBuffer())
      endif
      if (this:item==CTK_CSILLAG .and. wSoreleje)
         return this:parseLineComment(TKID_CSCOMMENT)
      elseif (nil!=(wStr:=this:parseTPTree()))
         // outerr("nparser: "+wStr,newline())
         return this:makeToken(TKID_CHAR,wStr)
      elseif (this:item==CTK_PER)
         perItem:=this:item
         this:rds()
         if (eqItemChar(CTK_PER))
            // '//'-es megjegyzÇs.
            return this:parseLineComment(TKID_PPCOMMENT)
         elseif (eqItemChar(CTK_CSILLAG))
            // '/*'-os megjegyzÇs
            return this:parsePcComment()
         else
            // Sima '/' karakter
            this:unrds()
            return this:makeToken(TKID_CHAR,perItem)
         endif
      elseif (this:item==CTK_ET)
         perItem:=this:item
         this:rds()
         if (eqItemChar(CTK_ET))
            // '&&'-es megjegyzÇs.
            return this:parseLineComment(TKID_PPCOMMENT)
         else
            // Sima '&' karakter
            this:unrds()
            return this:makeToken(TKID_CHAR,perItem)
         endif
      elseif (this:item==CTK_IDEZ .or. this:item==CTK_FIDEZ)
         // Felsìvesszìs string.
         return this:parseString(CTK_IDEZ)
      elseif (this:item==CTK_MACS)
         // Macskakîrmîs string.
         return this:parseString(CTK_MACS)
      elseif (thisclass:isStartnamechar(this:item))
         // NÇv. (Betñvel vagy al†h£z†ssal kezdìdik, Çs betñvel, 
         // al†h£z†ssal vagy sz†mmal folytat¢dik).
         return this:parseName()
      elseif (thisclass:isNumberchar(this:item) .or. eqItemChar("."))
         // Sz†m.
         return this:parseNumberArray()
      // elseif (item==CTK_PVESSZO)
      //    return this:makeToken(TKID_CHAR,item)
      else
         return this:makeToken(TKID_CHAR,this:item)
      endif
   endif
return nil
   

//*******************************************************************
implement parseLineComment(tkId)
// '//' vagy '*' megjegyzÇs

   this:rds()
   while(this:item!=nil)
      // if (eqItemChar(CHAR_LF) .or. eqItemChar(CHAR_CR_LF))
      if (isItemEol())
         this:unrds()
         exit
      elseif (valtype(this:item)!="C")
         if (this:item[1]==CTKID_EOS .or. this:item[1]==CTKID_BOS)
            this:unrds()
            exit
         endif
      endif
      this:rds()
   end while
   // this:soreleje:=.t.
return this:makeToken(tkId,this:strParserBuffer())

//*******************************************************************
implement parsePcComment()
// '/*'-os megjegyzÇs
// Mj.: Egy ilyen sorban: "/* huhu */ * hehe" a "* hehe" nem sz†m°t
//      megjegyzÇsnek, mert a '*' nem a sor elejÇn van.

   this:rds()
   while(this:item!=nil)
      if (eqItemChar(CTK_CSILLAG))
         this:rds()
         if (eqItemChar(CTK_PER))
            exit
         endif
         this:unrds()
      elseif (valtype(this:item)!="C")
         if (this:item[1]==CTKID_EOS .or. this:item[1]==CTKID_BOS)
            this:unrds()
            // Befejezetlen '/*' megjegyzÇs.
            return this:makeErrToken(TKID_PCCOMMENT,this:strParserBuffer(),;
                                     NPRERR_ENDPCCOMMENT)
         endif
      endif
      this:rds()
   end while

return this:makeToken(TKID_PCCOMMENT,this:strEolParserBuffer())

//*******************************************************************
implement strEolParserBuffer()
// Megadja a parserBuffer tartalm†t stringkÇnt.
// A nem stringeket az eol kivÇtelÇvel eldobja.
local i,str:="",w

   for i:=1 to TBUFFER.(this:parserBuffer):bItemNumber
      w:=TBUFFER.(this:parserBuffer):getBItem(i)
      if (valtype(w)=="C")
         str+=w
      elseif (w[1]==CTKID_EOL)
         str+=w[2]
      endif
   end for
return str

//*******************************************************************
static function makeStrToken(this,errCode,str,kezdo,zaro)
   if (errCode!=nil)
      this:addError(C.PRSERR:onew(;
         errCode,;
         kezdo+str+zaro,;
         this:tokenPos[1],;
         this:tokenPos[2],;
         this:tokenPos[3]))
   endif
   
return C.TKSTR:onew(TKID_STRING,str,;
                    this:tokenPos[1],;
                    this:tokenPos[2],;
                    this:tokenPos[3],;
                    kezdo,zaro)
                    
//*******************************************************************
implement parseString(hatarolo)
// Felsìvesszìs Çs macskakîrmîs string.
local str

   this:rds()
   while(this:item!=nil)
      if (eqItemChar(hatarolo))
         exit
      // elseif (eqItemChar(CHAR_LF) .or. eqItemChar(CHAR_CR_LF))
      elseif (isItemEol())
         this:unrds()
         // Befejezetlen string.
         str:=this:strParserBuffer()
         return makeStrToken(this,NPRERR_ENDSTRING,substr(str,2),left(str,1),"")
            
      elseif (valtype(this:item)!="C")
         if (this:item[1]==CTKID_EOS .or. this:item[1]==CTKID_BOS)
            this:unrds()
            // Befejezetlen string.
            str:=this:strParserBuffer()
            return makeStrToken(this,NPRERR_ENDSTRING,substr(str,2),left(str,1),"")
         endif
      endif
      this:rds()
   end while

   str:=this:strParserBuffer()
return makeStrToken(this,nil,substr(str,2,len(str)-2),left(str,1),right(str,1))
// return this:makeToken(TKID_STRING,this:strParserBuffer())

//*******************************************************************
implement parseName()
// NÇv.

   this:rds()
   while(thisclass:isNamechar(this:item))
      this:rds()
   end while
   this:unrds()
   // outstd("nev: '"+this:strParserBuffer()+"'"+guessedEol())
return this:makeToken(TKID_NEV,this:strParserBuffer())

//*******************************************************************
// #define isTkPont(token) (TOKEN.(token):id==TKID_CHAR .and.;
//                          TOKEN.(token):str==".")
                         
//*******************************************************************
implement parseNumberArray()
// Sz†m sor. 
// Akkor kell h°vni, ha a this:item sz†m vagy pont.
/*
   LehetsÇges alakok: 
   
   <Sz†m>
   <Sz†m>'.'<Sz†m>
   '.'<Sz†m>
   
   Mj.: Teh†t <Sz†m> '.' nem lehet!
*/
local state
#define STPNA_START     "start"
#define STPNA_NUM       "num"
#define STPNA_NUMPONT   "numpont"
#define STPNA_PONT      "pont"
#define STPNA_PONTNUM   "pontnum"

local tkId

   // this:rds()
   state:=STPNA_START
   tkId:=TKID_SZAMTOMB
   while(nil!=this:item)
      if (state==STPNA_START)
         if (thisclass:isNumberchar(this:item))
            state:=STPNA_NUM
         elseif (eqItemChar("."))
            state:=STPNA_PONT
         else
            exit
         endif
      elseif (state==STPNA_NUM)
         if (thisclass:isNumberchar(this:item))
            // Maradunk.
         elseif (eqItemChar("."))
            state:=STPNA_NUMPONT
         else
            // Sz†m ut†n nem sz†m vagy pont.
            exit
         endif
      elseif (state==STPNA_NUMPONT)
         if (thisclass:isNumberchar(this:item))
            state:=STPNA_PONTNUM
         else
            // Sz†m Çs pont ut†n nem sz†m, az item-et vissza kell 
            // tenni, Çs egy sz†mtîmb tokent adni.
            this:unrds() // Ez kell!!!
            exit
         endif
      elseif (state==STPNA_PONT)
         if (thisclass:isNumberchar(this:item))
            state:=STPNA_PONTNUM
         else
            // Pont ut†n nem sz†m, az item-et vissza kell tenni, Çs
            // egy pont tokent adni.
            tkId:=TKID_CHAR
            exit
         endif
      elseif (state==STPNA_PONTNUM)
         if (thisclass:isNumberchar(this:item))
            // Maradunk.
         else
            exit
         endif
      else
         ? "NPARSER:parseNumberArray(): Ismeretlen †llapot: ",state
         errorlevel(1)
         quit
      endif
      this:rds()
   end while
   this:unrds()
return this:makeToken(tkId,this:strParserBuffer())

//*******************************************************************

