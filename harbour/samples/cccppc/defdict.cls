//
// $Id$
//

//*******************************************************************
// defdict.cls: #define sz¢t r oszt ly.
// 1999, Csisz r Levente

class DEFDICT OBJECT

   attrib dict  // Egyenl“re egy EDEFDICT-ekb“l  ll¢ t”mb.
   
   nmethod add(edefdict)
   // Felvesz egy edefdict objektumot a sz¢t rba.
   // Ha m r van ilyen, akkor azt adja, ha nincs, akkor nil-t.
   
   nmethod atIdx(name)
   // A name kulcs£ elem index‚t adja a dict-ben.
   // Ha nincs, akkor 0-t ad.
   // Bels“ m–velet.

   nmethod atKey(name)
   // A name kulcs£ elemet (egy EDEFDICT objektumot) ad.
   // Ha nincs, akkor nil-t ad.

   nmethod delKey(name)
   // Kit”rli a name kucsl£ elemet a dict-b“l.
   // Ha nincs, akkor nem csin l semmit.

   nmethod printStr(printBlock)
   // A sz¢t r elemeket string-k‚nt ki¡rhat¢ form ban  tadja a 
   // printBlock-nak.
   // Ha a printBlock nincs megadva, akkor string-k‚nt visszaadja.
   
   rmethod oinit()

