//
// $Id$
//

//*******************************************************************
// rmarker.cls : Result marker token oszt†ly.
// 1999, Csisz†r Levente

// Result markereket t†rol¢ token.

class RMARKER TOKEN

   attrib mmNum  // A result markerhez tartoz¢ match marker sorsz†ma 
                 // egy makr¢ defin°ci¢ban.
   attrib mmTkId // A result markerhez tartoz¢ match marker id-je
                 // egy makr¢ defin°ci¢ban.

   rmethod oinit(id,str,file,line,pos)
   // Inicializ†lja az objektumot. Az str lesz a result marker neve.
   
   nmethod getName()
   
   nmethod setMMIdxByMM(matchMarkers)
   // A matchMarkers-ben keres egy ugyanolyan nevñ marker-t, mint
   // amilyen saj†t maga, Çs az mNum-ot be†ll°tja a sorsz†m†ra.
   // Egyenlìre case insensitive.
   // Ret: .t., ha sikerÅlt, .f., ha nem.

   nmethod changeByMMList(paramValues,result,iLevel,oneLevel) 
   // A result-ba beleteszi a this †ltal meghat†rozott helyettes°tÇst
   // a paramValues-bìl az iLevel szinten. Ha a paramValues-ban az
   // iLevel szinten nincs helyettes°tendì, akkor nem csin†l semmit.
   // Ret: 
   //    0: nem sikerÅlt a helyettes°tÇs, 
   //    1: csak 'Åres' helyettes°tÇs volt, (Nem volt illesztett match
   //       marker, de Åreset helyettes°tett pl. dumb stringify result 
   //       marker, logify result marker.
   //    2: volt helyettes°tÇs.
   //    
   // ProblÇma: Ha a match marker egy list_match_marker, akkor a 
   // stringify Çs a blockify markerek elemenkÇnt csin†lj†k
   // a konverzi¢t. (Ez jelenleg nincs imlement†lva.)
   
metaclass

   rmethod onew(id,str,file,line,pos)

   nmethod isResultMarker(aToken)
   // Meg†llp°tja, hogy az aToken egy result marker-e.
   // Ret: .t., ha igezn, .f., ha nem.

