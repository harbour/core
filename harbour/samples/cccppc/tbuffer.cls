//
// $Id$
//

//*******************************************************************
// tbuffer.cls: KÇtir†ny£ buffer
// 1999, Csisz†r Levente

// Mñveletek: get,unget,put,unput
// A get Çs a put egy fifo t†rat val¢s°t meg.
// A stringeket îsszevonja.
// Nil-t nem t†rol a bufferben.

class TBUFFER OBJECT

   attrib buffer // {{tipus,tartalom[,szamlalo]},...}
   
   nmethod get()
   // Kiveszi Çs visszaadja a buffer elsì elemÇt.
   // Ha a buffer Åres, akkor nil-t ad.

   nmethod unget(item)
   // Az itemet visszateszi a puffer elejÇre.
   // Ha az item nil, akkor nem csin†l semmit.
   
   nmethod put(item)
   // Az itemet beteszi a puffer vÇgÇre.
   // Ha az item nil, akkor nem csin†l semmit.
   
   nmethod unput()
   // Kiveszi Çs visszaadja a buffer utols¢ elemÇt.
   // Ha a buffer Åres, akkor nil-t ad.

   nmethod clear()
   // KiÅr°ti a puffert.
      

   nmethod bItemNumber()
   // H†ny elemñ a buffer. Az îsszevont stringeket egynek sz†molja.
   
   nmethod getBItem(i)
   // Az i. buffer elemet adja.
   
   nmethod appendBuffer(aTBuffer)
   // Az aTBuffer tartalm†t hozz†appendeli a this-hez.
   
   rmethod oinit()
   
// metaclass

//    rmethod oinitclass()
      
