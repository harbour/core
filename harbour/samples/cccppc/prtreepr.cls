//
// $Id$
//

//*******************************************************************
// prtreepr.cls: Parser tree parser. Egy input folyamot egy elemzì
//               f†val elemzì objektum
// 1999, Csisz†r Levente

// Egy elemzì f†t lehet vele felÇp°teni Çs kezelni.

class PRTREEPR OBJECT

   attrib eprtree  // A prtree objektum.
   attrib subtree  // Amit Çppen elemzÅnk.
   
   attrib result   // Ha az elemzÇs sikeres, akkor a megtal†lt 
                   // <terminal>-ban levì result lesz itt.
                   
   attrib itemArray  // A put()-al betett itemek.
                     // A wordLen ut†ni itemek az elìreolvasott
                     // itemek.
   attrib wordLen    // Ha az elemzÇs meg†ll, akkor itt a megtal†lt
                     // sz¢ hossza van (sikeressÇg esetÇn), vagy 0,
                     // sikertelensÇg esetÇn.

   // Ezekhez lÇp vissza, ha az elemzÇs elakad.
   attrib lastStopWordLen // A legutols¢ illeszkedì sz¢ hossza.
   // attrib lastStopResult  // A legutols¢ illeszkedì sz¢hoz tartoz¢
                          // result.
   
   nmethod start()

   nmethod put(item)
   
   rmethod oinit(eprtree)
   
metaclass

   rmethod onew(eprtree)

