//
// $Id$
//

class BEHAVIOR OBJECT

   attrib parent

   attrib name

   attrib objSize
   // Azon objektumok mÇrete, amik az oszt†ly elemei.

   attrib attribs
   // Egy 'name' t°pus£ objektum (ami ennek az
   // oszt†lynak az eleme) attrib£tumai
   // {sorsz†m,nÇv,t°pus} form†ban.

   attrib methods
   // Egy 'name' t°pus£ objektum (ami ennek az
   // oszt†lynak az eleme) mñveletei
   // {sorsz†m,nÇv,paramÇterek,implSpec} form†ban.
   // Mj.: A mñveleteket vÇgrehajt¢ blokkok az
   // objMethodsImplement †ltal adott blokkban
   // vannak.

   attrib classId
   // Ha az oszt†lynak van id-je, akkor az itt t†rolva van.
   // Ha nincs, akkor ez nil.

   attrib parentClassIds
   // Vagy nil, vagy egy olyan tîmb, amiben a i. elem nem nil,
   // haz az adott oszt†ly a this ìsei kîzîtt van Çs van id-je.

   attrib amBlock              
   // Egy blokk, aminek a vÇgrehajt†sa be°rja
   // egy tîmbbe az oszt†ly †ltal defini†lt
   // attrib£tumokat Çs egy m†sik tîmbbe pedig 
   // az oszt†ly †ltal defini†lt mñveleteket.

   attrib methodsBlock
   // Egy blokk, aminek a vÇgrehajt†sa
   // be°rja egy tîmbbe az oszt†ly †ltal defini†lt
   // mñveletek blokkjait.
   // (Ezt a tîmbît adja az objMethodsImplement.)

   attrib objMethodsImplement  
   // Egy blokkot tartalmaz, amit kiÇrtÇkelve
   // megkapjuk azt a tîmbît, aminek az elsì
   // eleme az oszt†ly, a tov†bbi elemei pedig
   // az oszt†lyba tartoz¢ objektumok mñveleti
   // blokkjai, Çs m†s implement†ci¢s elemek.

   nmethod onew{be_onew}()
   // KÇsz°t egy £j objektumot, Çs inicializ†lja £gy, hogy megh°vja
   // az oinit() mñveletÇt.
   // Az objektum a 'this' oszt†lyba fog tartozni.

   nmethod ocreate{be_ocreate}()
   // KÇsz°t egy £j objektumot, de nem inicializ†lja.
   // Az objektum a 'this' oszt†lyba fog tartozni.

   nmethod rawoinit{be_rawoinit}(parent,name,nAttribs,nMethods,amBlock,methodsBlock)
   // Inicializ†lja a 'this' oszt†lyt.

   rmethod oinit{be_oinit}(parent,name,nAttribs,nMethods,amBlock,methodsBlock)
   // Inicializ†lja a 'this' oszt†lyt a rawoinit()-el, de elìbb
   // ellenìrzi, hogy az oszt†ly metaoszt†ly†nak van-e m†r eleme.
   // Ha igen, akkor kÅld egy figyelmeztetÇst.

   nmethod oinitclass{retobj}()
   // A Meta onew()-ja h°vja az oinit() ut†n. AzÇrt van itt, hogy ne
   // kelljen az oinit() hossz£ paramÇterlist†j†val veszìdni.

   nmethod getMethodsImplement{be_getMethodsImplement}()
   // Ad egy tîmbît, amiben a mñveletek blokkjai tal†lhat¢k.

   rmethod isClass{be_isClass}()  
   // Mindig igazat ad. (Ez az objektum oszt†ly.)

   nmethod attribIdx{be_attribIdx}(name)
   // A name nevñ attrib£tum indexÇt adja, 'nil', ha nincs.

   nmethod needClassId{be_needClassId}()
   // Ha .t., akkor az oszt†ly†nak kell azonos°t¢t adni.
   // Ha .f., akkor az oszt†ly†nak nem kell azonos°t¢t adni.
   // Jellemzì implement†ci¢ja:
   // return class:getClass()==mc<oszt†lynÇv>CLASS() 
   // Mj.: A 'class=cl<oszt†lynÇv>()' nem j¢.

   nmethod isInheritFrom{be_isInheritFrom}(parentClass)
   // Ha a this îrîkîl az aClass-t¢l, vagy this==aClass, akkor
   // igaz, egyÇbkÇnt hamis.
   // Ha a parentClass-nak van id-je, akkor ez a mñvelet sokkal
   // gyorsabban hajt¢dik vÇgre, mint ha nincs.
