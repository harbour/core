//
// $Id$
//

class PACKAGE OBJECT

   attrib parentPkg             
   // A csomag szÅlìje. Ha nincs, akkor nil.

   attrib name
   // A csomag neve.

   attrib nAttribs
   // H†ny attributum van.

   attrib attribs
   // Egy tîmb az attrib£tumok sorsz†m†val,nevÇvel,t°pus†val

   attrib methods
   // Egy tîmb a mñveletek sorsz†m†val, nevÇvel, 
   // paramÇtereivel, implSpec-Çvel.

   attrib pkgId
   // A csomag azonos°t¢ja, minden csomagnak van.
   // Ha egy csomagnak van ìse, akkor ez megegyezik az ìsÇnek
   // az id-jÇvel. Ha nincs ìse, akkor pedig kÅlînbîzik b†rmelyik
   // olyan csomag pkgId-jÇtìl, aminek szintÇn nincs ìse.

   attrib amBlock
   // Egy blokk, aminek a vÇgrehajt†sa be°rja
   // egy tîmbbe a csomag †ltal defini†lt
   // attrib£tumokat Çs egy m†sik tîmbbe pedig
   // a csomag †ltal defini†lt mñveleteket.

   attrib methodsBlock
   // Egy blokk, aminek a vÇgrehajt†sa
   // be°rja egy tîmbbe a csomag †ltal defini†lt
   // mñveletek blokkjait.
   // (Ezt a tîmbît adja a pkgMethodsImplement.)

   attrib pkgMethodsImplement
   // Egy blokkot tartalmaz, amit kiÇrtÇkelve
   // megkapjuk azt a tîmbît, aminek az elsì
   // eleme a csomag, a tov†bbi elemei pedig
   // a csomagban haszn†lhat¢ mñveletek
   // blokkjai, Çs m†s implement†ci¢s elemek.

   rmethod oinit(parent,name,nAttribs,nMethods,amBlock,methodsBlock,pkgId)

   nmethod connectTo(obj)

metaclass

   attrib numId

   rmethod onew(parent,name,nAttribs,nMethods,amBlock,methodsBlock,pkgId)

   nmethod nextId()
