//
// $Id$
//

//*******************************************************************
// lparser.cls: Sor elemzì. A PARSER-tìl îrîkîl.
// 1999, Csisz†r Levente

// Ez az elemzì vonja îssze a folytat¢ sorokat.
// Mindig egy sort t†rol a sorvÇgjellel egyÅtt. 
// A sorokon a kîvetkezì transzform†ci¢kat vÇgzi el (a sorrend
// fontos!). 
// 
// - '//', '&&' Çs a '*' kommenteket eldobja.
// - A '/*' kommentekben csak a cr-eket Çs az lf-eket hagyja meg,
//   Çs a tokent <Åres>-re v†ltoztatja.
// - Az egym†s ut†n kîvetkezì <Åres>-eket îsszevonja.
// - Megcsin†lja a kîvetkezì îsszevon†st.
//   Az <£jsor>-t a filÇ eleje is helyettes°theti.
//
//    <£jsor> <sor> ';' [<Åres>] <£jsor> <Åres2>  <ntoken> ==> 
//       <£jsor> <£jsor> <sor> ' ' <ntoken>
//
//    <£jsor> <sor> ';' [<Åres>] <£jsor> <ntoken> ==> 
//       <£jsor> <£jsor> <sor> <ntoken>
//
// A <sor> b†rmilyen token sorozat lehet, amiben nem szerepel
// ';' Çs <£jsor>.
// 
// A <ntoken> b†rmilyen token lehet, kivÇve az <Åres> tokent.
// 
// Ha egy sor ÅressÇ v†lik, akkor az "" form†ban kell ki°rni.
// Ezt a h°v¢nak kell kezelnie, itt nehÇzkes.
 

class LPARSER PARSER
              
   rmethod readInput()
   // Kiszñri az egy soros megjegyzÇseket Çs †talak°tja a 
   // '/*'-osakat
   
   nmethod readLine()
   // Bolvas egy teljes sort a parserBuffer-be. Az egy soros
   // megjegyzÇseket eldobja, a tîbb sorosakat pedig feldolgozza
   // £gy, hogy azonos mennyisÇgñ £j sort helyez el a parseBuffer
   // elejÇre. A folytat¢ sorokat îsszevonja.
   
   rmethod readItem()
   // Olvas egy elemet. 
   // Ret: Az elem, ha OK, nil, ha vÇge van a streamnek, vagy ha
   //      hiba volt.

//    rmethod oinit(inputReader,name)
   
metaclass
   
//   rmethod onew(inputReader,name)
   
   


