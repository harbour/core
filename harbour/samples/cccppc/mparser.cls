//
// $Id$
//

//*******************************************************************
// mparser.cls: A sorokban a makr¢kat (define, command, xcommand, 
//              translate, xtranslate) helyettes¡ti.
// 1999, Csisz r Levente

// A PARSER-t“l ”r”k”l.
// A n‚v ‚s a sor (lparser) elemz“  ltal k‚sz¡tett tokeneket v r az 
// inputr¢l. ltal ban a hparser ut n van.

class MPARSER PARSER
   
   attrib soreleje
   // attrib defdict
          
   attrib makroBuf // Itt tartja azokat a tokeneket, amiket egy
                   // makr¢ v ltoz¢hoz rendel elemz‚skor.
   
   nmethod startMakroBuf(item)
   // Inicializ lja a makroBuf-t. Ha az item nem nil, akkor beteszi
   // els“ elemnek.
   
   nmethod rdsMakroBuf()
   // Ugyanaz, mint az rds(), csak a makro buf-ban is vezeti.
   
   nmethod unrdsMakroBuf(n)
   // Ugyanaz, mint az unrds(n), csak a makro buf-ban is vezeti.
   
   rmethod readItem()
   // Olvas egy elemet. 
   // Ret: Az elem, ha OK, nil, ha v‚ge van a streamnek, vagy ha
   //      hiba volt.

   nmethod parseFun(edefdict)
   // Ez v‚gzi a t‚nyleges elemz‚st.
   // A this:item-t elemzi, szks‚g eset‚n m‚g olvashat.
   // Elemezi az inputon a <n‚v>'('<param1>,...')' param‚tereket. 
   // Az edefdict-nek a <n‚v>-hez tartoz¢ makr¢ defin¡ci¢nak kell
   // lennie.
   
   // Ret: {sikeres,itemLista}
   // Ha sikeres volt, akkor a sikeres==.t., ‚s az itemLista a csere
   //    eredm‚nye.
   // Ha nem volt sikeres, akkor a sikeres==.f., ‚s az itemLista a
   //    beolvasott (el“reolvasott) itemek list ja.
   // A parserBufferben csak egy token lehet, ami az item-ben is
   // van.
   
   // nmethod printDefDict()

   nmethod parseTokenList(leftTokenList,cmd4,paramValues,toEOL)
   
   nmethod parseXtr(extrdict)
   // Ez v‚gzi a t‚nyleges elemz‚st.
   // A this:item-t elemzi, szks‚g eset‚n m‚g olvashat.
   // Elemezi az inputon az extrdict tokenjeit. 
   
   // Ret: {sikeres,itemLista}
   // Ha sikeres volt, akkor a sikeres==.t., ‚s az itemLista a csere
   //    eredm‚nye.
   // Ha nem volt sikeres, akkor a sikeres==.f., ‚s az itemLista a
   //    beolvasott (el“reolvasott) itemek list ja.
   // A parserBufferben csak egy token lehet, ami az item-ben is
   // van.
   
   nmethod parseXtrTree(trdictTree,mi,toEOL)
   // Ez v‚gzi a t‚nyleges elemz‚st.
   // A this:item-t elemzi, szks‚g eset‚n m‚g olvashat.
   // Elemezi az inputon az extrdict tokenjeit. 
   
   // Ret: {sikeres,itemLista}
   // Ha sikeres volt, akkor a sikeres==.t., ‚s az itemLista a csere
   //    eredm‚nye.
   // Ha nem volt sikeres, akkor a sikeres==.f., ‚s az itemLista a
   //    beolvasott (el“reolvasott) itemek list ja.
   // A parserBufferben csak egy token lehet, ami az item-ben is
   // van.
   
   nmethod mleftXNToken(leftToken,cmd4)
   // Match left xtranslate normal token.
   
   nmethod mleftXMRToken(leftToken,cmd4,paramValues,nUres)
   // Match left xtranslate malterset token
   
   nmethod mleftXRMMToken(leftToken,cmd4)
   // Match left xtranslate regular match marker

   nmethod mleftXWMToken(leftToken)
   // Match left xtranslate wild match marker
   
   nmethod mleftXEEMToken(leftToken)
   // Match left xtranslate extended expression match marker
   
   nmethod mleftXLMToken(leftToken,cmd4)
   // Match left xtranslate list match marker
   
   nmethod mleftXRSMMToken(leftToken,cmd4)
   // Match left xtranslate restricted match marker

   // rmethod oinit(inputReader,name,defDict)
   rmethod oinit(inputReader,name,errorStream)

   
metaclass
   
   // rmethod onew(inputReader,name,defDict)
   rmethod onew(inputReader,name,errorStream)

   nmethod getCloseParent(aChar)
   // Ha az aChar egy nyit¢ z r¢jel, akkor a a csuk¢ p rj t adja, 
   // egy‚bk‚nt nil-t.

   nmethod trimTokenList(tList)
   // A tList elej‚r“l ‚s v‚g‚r“l elt vol¡tja az res tokeneket.
   // Ret: a tList.


   nmethod parse(item,inputReader,name,defdict,edefdict,xtrdict,mi,errorStream,ujsor,trPrsAlg)
   // Elemzi ‚s az item-el kezd“d“ ‚s az inputReader  ltal defini lt
   // folyammal folytat¢d¢ folyamot, ‚s ha tal l benne az itemmel 
   // kezd“d“ makr¢t, akkor megadja, hogy azt mire kell cser‚lni.
   // Ret: {sikeres,itemLista}
   // Ha sikeres volt, akkor a sikeres==.t., ‚s az itemLista a csere
   //    eredm‚nye.
   // Ha nem volt sikeres, akkor a sikeres==.f., ‚s az itemLista a
   //    beolvasott (el“reolvasott) itemek list ja.
   
   
   // Mj.: Jelenleg az item csak nil vagy TOKEN lehet.


   // nmethod isTkBinaryOp(tkId,tkStr)

