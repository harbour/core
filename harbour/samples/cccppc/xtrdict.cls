//
// $Id$
//

//*******************************************************************
// xtrdict.cls: #xtranslate sz¢t†r oszt†ly.
// 1999, Csisz†r Levente

class XTRDICT OBJECT

   // Egyenlìre ezek EXTRDICT-ekbìl †ll¢ tîmbîk.
   attrib cmdict  // Az xcommand-ok sz¢t†ra.
   attrib trdict  // Az xtranslate-ek sz¢t†ra.

   attrib cmdictTree  // Az xcommand-ok f†ja.
   attrib trdictTree  // Az xtranslate-ek f†ja.
   
   nmethod add(extrdict)
   // Felvesz egy extrdict objektumot a sz¢t†rba.
   
   nmethod printStr(printBlock,xcmd)
   // A sz¢t†r elemeket string-kÇnt ki°rhat¢ form†ban †tadja a 
   // printBlock-nak.
   // Ha a printBlock nincs megadva, akkor string-kÇnt visszaadja.
   // Ha az xcmd nem Åres, akkor az xcommand-okat adja.
   
   rmethod oinit()

   nmethod getExtrList(item,xcmd)
   // Ad egy list†t azokr¢l az extrdict-ekrìl, amiknek az elsì 
   // tokene illeszkedik az item-re.
   // Ha az xcmd nem Åres, akkor az xcommand-okat adja.

