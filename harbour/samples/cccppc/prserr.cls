//
// $Id$
//

//*******************************************************************
// prserr.cls: Hib†kat t†rol¢ Çs kezelì objektum.
// 1999, Csisz†r Levente

class PRSERR OBJECT

   attrib group    // A hiba csoport neve. èltal†ban az osz†ly neve, 
                   // ahol a hiba defini†lva van.
   attrib id       // A hiba azonos°t¢ja.
   
   attrib params   // Az (group,id) p†r mondja meg, hogy minek kell 
                   // lennie.

   attrib file
   attrib line
   attrib pos
   
   nmethod defaultErrFormat(name)
   // Megadja a hib†t szîvegesen default m¢don form†zva.
   
   nmethod getErrStr()
   // Kii°rja a hib†t form†zva, szîvegesen.
   // Ha a {group,id} p†ros regisztr†lva van, akkor a printBlock-al
   // megform†zza, ha nem, akkor unknown error megjelîlÇssel.
   
   rmethod oinit(group_and_id,params,file,line,pos)
   // A group_and_id egy {group,id} p†r.
   // A params szerkezete a group_and_id †ltal van defini†lva.

metaclass

   attrib errDict   // Egy tîmb, amiben {group,{id, printBlock},...} 
                    // elemek vannak.
                    // A writeBlock mondja meg, hogyam kell ki°rni az
                    // egyes hib†kat.

   rmethod onew(group_and_id,params,file,line,pos)

   nmethod onewFromToken(group_and_id,params,token)
   // A token file,line,pos-†t haszn†lja fel.

   nmethod registerError(group_and_id,name,printBlock)
   // Regisztr†l egy hib†t (hiba t°pust)
   // Az ilyen hib†kat tudja azut†n a getErrStr() form†zva ki°rni.


   rmethod oinitclass()

