//
// $Id$
//

class META BEHAVIOR
// Ennek az oszt†lynak az elemei metaoszt†lyok.


   attrib ofClass  
   // Ha ennek az metaoszt†lynak csak egy eleme lehet, akkor
   // ez egy blokk, amit kiÇrtÇkelve megkapjuk azt az egy
   // oszt†lyt. Ha ennek a metaoszt†lynak tîbb eleme is lehet,
   // akkor ez nil.

   rmethod oinit{me_oinit}(parent,name,nAttribs,nMethods,amBlock,methodsBlock)
   // Inicializ†lja a 'this' metaoszt†lyt.
   // Mj.: H°vja a Behavior oinit()-jÇt, Çs inicializ†lja
   // az ofClass-t 'nil'-re.

   rmethod onew{me_onew}(parent,name,nAttribs,nMethods,amBlock,methodsBlock)
   // LÇtrehoz egy £j oszt†lyt, Çs inicializ†lja.
   // Az £j oszt†ly a 'this' eleme lesz, teh†t az £j oszt†ly
   // oszt†lya a 'this'lesz.
   // Ha az ofClass nem Åres, akkor a 'this' meta oszt†lynak m†r
   // van eleme. Egy (kîzînsÇges) metaoszt†lynak csak egy eleme
   // lehet, °gy ez hiba!
   // Ha az ofClass Åres, akkor az £j oszt†lyt oda be°rja.

   // Mj.: Ez a 'Meta' egy elemÇnek a mñvelete. A 'Meta' elemei
   // pedig meta oszt†lyok. Teh†t ez egy meta osztaly egy elemÇt
   // hozza lÇtre, a meta oszt†lyok elemei pedig oszt†lyok. Teh†t
   // ez egy £j oszt†lyt (Çs nem metaoszt†lyt!!!) hoz lÇtre.

   rmethod ocreate{me_ocreate}()                             
   // LÇtrehoz egy £j oszt†lyt, de nem inicializ†lja.

   
   // nmethod makeClassId{me_makeClassId}(aClass)
   // Hozz†ad egy oszt†lyt az idClass-okhoz. Az oszt†ly id-jÇvel
   // tÇr vissza. Ha az oszt†lynak m†r van id-je, akkor azzal tÇr
   // vissza.

   // nmethod classFromId{me_classFromId}(id)
   // Ha van olyan oszt†ly, aminek az 'id' az id-je, akkor
   // azt adja egyÇbkÇnt nil-t.

   // nmethod findClassId{me_findClassId}(aClass)
   // Ha az oszt†lynak van id-je, akkor azt adja, egyÇbkÇnt nil-t.
