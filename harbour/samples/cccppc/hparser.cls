//
// $Id$
//

//*******************************************************************
// hparser.cls: Hessmark parser: a '#'-al kezd“d“ sorok elemz‚se.
// 1999, Csisz r Levente

// A PARSER-t“l ”r”k”l.
// Ez az elemz“ v‚gzi el a '#'-al kezd“d“ sorok elemz‚s‚t.
// A n‚v ‚s a sor (lparser) elemz“  ltal k‚sz¡tett tokeneket v r az 
// inputr¢l.

// A #-os sorokat feldolgozza, a t”bbit  tengedi.

class HPARSER PARSER
   
   attrib soreleje
   attrib defdict
   attrib xtrdict
   attrib ifStack
   attrib branch   // .t., ha egy #if igaz  g n vagyunk, vagy nem
                   // vagyunk if-ben, .f., ha egy #if hamis  g n 
                   // vagyunk, nil, ha egy if hamis  g n belli 
                   // if-en vagyunk.
                   
   attrib inclObj  // Egy objektum, amiben vezetjk az include 
                   // fil‚ket.
      
   attrib lastEos  // Az utols¢ eos token.
   
   rmethod readItem()
   // Olvas egy elemet. 
   // Ret: Az elem, ha OK, nil, ha v‚ge van a streamnek, vagy ha
   //      hiba volt.

   // nmethod addBuf(anItem)
   // Hozz ad egy elemet a pufferhez. Az elem lehet speci lis is.

   // nmethod getStr()
   // Megadja a buf tartalm t stringk‚nt.
   // A nem stringeket eldobja.
   
   // nmethod rds()
   // Olvas egy karatert a readInput()-al a buf-ba.
   // Az olvasott ‚rt‚ket a buf-ba is beleteszi, ha az nem az eof.
   
   // nmethod unrds(n)
   // A buf utols¢ n elem‚t visszateszi az inputra. Az item-et t”rli.
   // Azt adja vissza, hogy h ny elemet sikerlt visszatenni.
   // Ha az n nil, akkor egyet tesz vissza.
      
   nmethod parseLine()
   // Ez v‚gzi a t‚nyleges elemz‚st, rekurz¡van is lehet h¡vni.
   // A this:item-t elemzi, szks‚g eset‚n m‚g olvashat.
   
   nmethod parseFalseLine()
   // Egy #if hamis  g n v‚gzi az elemz‚st.
   // A this:item-t elemzi, szks‚g eset‚n m‚g olvashat.
   
   nmethod parseDefine{hpr_prsDefine}(mnameToken)
   // # define elemz“.
   
   nmethod parseUndef{hpr_prsUndef}(mnameToken)
   // # undef elemz“.
   
   nmethod parseTranslate(mNameToken)
   // # xtranslate, etc elemz“.
   
   nmethod parseIfdef{hpr_prsIfdef}(mnameToken,ifdefType)
   
   nmethod parseElse{hpr_prsElse}(mnameToken)

   nmethod parseEndif{hpr_prsEndif}(mnameToken)

   nmethod parseInclude(mnameToken)
   
   nmethod parseLTranslate(mnameToken)
   // A # xtranslate bal oldal nak az elemz“je.
   
   nmethod parseRTranslate(mnameToken)
   // A # xtranslate jobb oldal nak az elemz“je.
   
   nmethod addDefine{hpr_addDefine}(mnameToken,name,params)
   // Felvesz a #define sz¢t rba egy elemet name n‚ven.
   // A defin¡ci¢ t”rzse a parserBuffer-ben van.
   // Az mnameToken jelzi, hogy hol volt a defin¡ci¢.
   // Ha a n‚v m r l‚tezik, akkor az errorGen-el legy rt egy
   // hiba tokent a az mnameToken-b“l.
   
   nmethod addXTranslate(mnameToken,leftSide,rightSide)
   // nmethod addXTranslate{hpr_addXTranslate}(mnameToken,leftSide,rightSide)
   // Felvesz a #xtranslate sz¢t rba egy elemet.
   // Az mnameToken jelzi, hogy hol volt a defin¡ci¢.
   
   nmethod sorNyel(kiirBlock)
   // T”rli a parserBuffer-t, majd a sor v‚g‚ig lenyeli az item-eket.
   // A sorv‚gjelet a parserpuffer-ben hagyja.
   // Ha a kiirBlock egy block, akkor a token stringeket  tadja neki.
   
   nmethod errorgen(token,errorCode,params)
   // Kiad egy hibazenetet. Az zenet helye a token-ben specifik lt
   // hely lesz.
   // A params lehet nil is.

   nmethod incompErrorgen(token,errorcode,params)
   // Ez egy olyan hiba, amit csak akkor kell kiadni, ha pontosan
   // ut nozni akarjuk a specifik ci¢t. M s sz¢val itt a spec. rossz
   // ‚s mi csin ln nk/csin ljuk j¢l. Egy logikai ‚rt‚kkel t‚r 
   // vissza, ha igaz, akkor a hibazenet ki van adva, ha hamis, 
   // akkor a hibazetet nem adta ki (lehet folytatni).
   // A param‚terez‚se megegyezik az errorgen()-el.

   nmethod chkEndOFFile()
   // Elv‚gzi a szks‚ges ellen“rz‚seket a fil‚ v‚g‚n.
   // (Lez ratlan #if, etc.)
   
   rmethod oinit(inputReader,name,defDict,xtrDict,inclObj,errorStream)
   
metaclass
   
   rmethod onew(inputReader,name,defDict,xtrDict,inclObj,errorStream)


   rmethod oinitclass()

