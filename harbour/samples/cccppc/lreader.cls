//
// $Id$
//

//*******************************************************************
// lreader.cls: L nc reader.
// 1999, Csisz r Levente

// Ez a reader readerek stackj‚n keresztl olvas, £gy, hogy olvas a 
// legfels“b“l, ha az el‚rte az EOF-ot, akkor kld egy EOS (End Of 
// Stream)-et, eldobja a stack tetej‚n lev“ reader-t, ‚s folytatja 
// az olvas st a stack tetej‚n maradt reader-el.
// Amikor £j stream-et kezd, akkor kld egy BOS (Begin Of Stream)
// elemet.
// Az EOS ‚s a BOF ‚rt‚ke param‚terezhet“, s“t el is hagyhat¢ak.

// Mj.: A stackb“l t”r”lt reader-ekre kiadja a destruct()-ot.
//      Egy lreader destruct-ja az ”sszes a stack-j‚ben lev“ reader
//      destructj t jelenti.
// Mj2.: Az errorStream-et nem  ll¡tja, arr¢l, hogy a hib kat
//       megkapjuk neknk kell gondoskodni.

// A neve mindig a stack legfels“ eleme.


class LREADER CREADER

   attrib readerStack  // A stack, amiben a reader-ek vannak.
                       // Egy elem {reader,needBOS} alak£.
                       // A needBOS .t., ha kell BOS-t kldeni
                       // .f., ha nem.

   attrib BOSItem      // A BOS elem. B rmilyen objektum vagy 
                       // ‚rt‚k lehet (karakter is.)
                       // Ha nil, akkor nem kld ilyet.
      
   attrib EOSItem      // Az EOS elem. B rmilyen objektum vagy 
                       // ‚rt‚k lehet (karakter is.)
                       // Ha nil, akkor nem kld ilyet.
   rmethod readItem()
   // Olvas egy elemet. 
   // Ret: Az elem, ha OK, nil, ha v‚ge van a streamnek, vagy ha
   //      hiba volt.
   
   nmethod pushReader(aReader)
   // Az itt megadott readert beteszi a stack-be. A legk”zelebbi
   // olvas s ebb“l fog olvasni.
   
   nmethod topReader()
   // Megadja a stack tetej‚n lev“ readert. Ha nincs ret nil
   
   nmethod popReader()
   // Visszaadja a stack tetej‚n lev“ readert, ‚s a stack-r“l t”rli.
   // Nem adja ki r  a destruct()-ot.
   
   nmethod dropReader()
   // A stackr“l t”rli a tetej‚n lev“ readert.
   // Kiadja r  a destruct()-ot.
   
   nmethod isEmpty()
   // Igaz, ha a stack res, egy‚bk‚nt hamis.
   
   nmethod getBOSItem()
   // Ad egy BOS itemet.
   
   nmethod getEOSItem()
   // Ad egy EOS itemet.
   
   rmethod getFile()
   rmethod getLine()
   rmethod getPos()

   rmethod oinit(BOSItem,EOSItem,name,errorStream)
   // Inicializ lja az objektumot.
   
metaclass

   rmethod onew(BOSItem,EOSItem,name,errorStream)



