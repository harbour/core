/*
 * $Id$
 */

//*******************************************************************
// expr.prg: KifejezÇs hat†rok meg†llap°t†sa.
// 1999, Csisz†r Levente

//*******************************************************************
#include "objgen.ch"

//*******************************************************************
#include "prtree.och"
#include "prtreepr.och"

//*******************************************************************
#define EXPPRTREE_MAXDEEP 4

//*******************************************************************
function expPrtree()
// Megadja a kifejezÇseket vizsg†l¢ elemzì f†t.

// Ha az adott sz¢r¢l (sorozatr¢l) tudni lehet, hogy hol van benne
// a kifejezÇs hat†r, akkor a result-ban egy sz†m †ll, ami
// a sorozat utols¢, mÇg a kifejezÇshez tartoz¢ tagj†nat a 
// sorsz†ma. Ha ilyen nincs, akkor a result nil.

// A '!', '@', '++', '=' oszt†lyok innen hi†nyoznak, mert azok ut†n
// minden jîhet.

static prtree

   if (prtree==nil)
   
      prtree:=C.PRTREE:onew()
      
      #ifdef OLD
      // Kettì hossz£ak, amiket elfogadjunk.
      PRTREE.prtree:addWord(nil,{"%","&"}    )
      PRTREE.prtree:addWord(nil,{"%","++"}   )
      PRTREE.prtree:addWord(nil,{"%","nev"}  )
      PRTREE.prtree:addWord(nil,{"%","(a)"}  )
      PRTREE.prtree:addWord(nil,{"%",".T."}  )
      PRTREE.prtree:addWord(nil,{"%","1"}    )
      PRTREE.prtree:addWord(nil,{"^","&"}    )
      PRTREE.prtree:addWord(nil,{"^","++"}   )
      PRTREE.prtree:addWord(nil,{"^","nev"}  )
      PRTREE.prtree:addWord(nil,{"^","(a)"}  )
      PRTREE.prtree:addWord(nil,{"^",".T."}  )
      PRTREE.prtree:addWord(nil,{"^","1"}    )
      PRTREE.prtree:addWord(nil,{"&","!"}    )
      PRTREE.prtree:addWord(nil,{"&","@"}    )
      PRTREE.prtree:addWord(nil,{"&","%"}    )
      PRTREE.prtree:addWord(nil,{"&","^"}    )
      PRTREE.prtree:addWord(nil,{"&","&"}    )
      PRTREE.prtree:addWord(nil,{"&","*"}    )
      PRTREE.prtree:addWord(nil,{"&","-"}    )
      PRTREE.prtree:addWord(nil,{"&","+"}    )
      PRTREE.prtree:addWord(nil,{"&","="}    )
      PRTREE.prtree:addWord(nil,{"&","/"}    )
      PRTREE.prtree:addWord(nil,{"&","++"}   )
      PRTREE.prtree:addWord(nil,{"&","nev"}  )
      PRTREE.prtree:addWord(nil,{"&","(a)"}  )
      PRTREE.prtree:addWord(nil,{"&",".T."}  )
      PRTREE.prtree:addWord(nil,{"&","1"}    )
      PRTREE.prtree:addWord(nil,{"*","&"}    )
      PRTREE.prtree:addWord(nil,{"*","++"}   )
      PRTREE.prtree:addWord(nil,{"*","nev"}  )
      PRTREE.prtree:addWord(nil,{"*","(a)"}  )
      PRTREE.prtree:addWord(nil,{"*",".T."}  )
      PRTREE.prtree:addWord(nil,{"*","1"}    )
      PRTREE.prtree:addWord(nil,{"-","&"}    )
      PRTREE.prtree:addWord(nil,{"-","++"}   )
      PRTREE.prtree:addWord(nil,{"-","nev"}  )
      PRTREE.prtree:addWord(nil,{"-","(a)"}  )
      PRTREE.prtree:addWord(nil,{"-",".T."}  )
      PRTREE.prtree:addWord(nil,{"-","1"}    )
      PRTREE.prtree:addWord(nil,{"+","&"}    )
      PRTREE.prtree:addWord(nil,{"+","++"}   )
      PRTREE.prtree:addWord(nil,{"+","nev"}  )
      PRTREE.prtree:addWord(nil,{"+","(a)"}  )
      PRTREE.prtree:addWord(nil,{"+",".T."}  )
      PRTREE.prtree:addWord(nil,{"+","1"}    )
      PRTREE.prtree:addWord(nil,{"/","&"}    )
      PRTREE.prtree:addWord(nil,{"/","++"}   )
      PRTREE.prtree:addWord(nil,{"/","nev"}  )
      PRTREE.prtree:addWord(nil,{"/","(a)"}  )
      PRTREE.prtree:addWord(nil,{"/",".T."}  )
      PRTREE.prtree:addWord(nil,{"/","1"}    )
      PRTREE.prtree:addWord(nil,{"nev","!"}  )
      PRTREE.prtree:addWord(nil,{"nev","@"}  )
      PRTREE.prtree:addWord(nil,{"nev","%"}  )
      PRTREE.prtree:addWord(nil,{"nev","^"}  )
      PRTREE.prtree:addWord(nil,{"nev","&"}  )
      PRTREE.prtree:addWord(nil,{"nev","*"}  )
      PRTREE.prtree:addWord(nil,{"nev","-"}  )
      PRTREE.prtree:addWord(nil,{"nev","+"}  )
      PRTREE.prtree:addWord(nil,{"nev","="}  )
      PRTREE.prtree:addWord(nil,{"nev","/"}  )
      PRTREE.prtree:addWord(nil,{"nev","++"} )
      PRTREE.prtree:addWord(nil,{"nev","(a)"}) 
      PRTREE.prtree:addWord(nil,{"(a)","!"}  )
      PRTREE.prtree:addWord(nil,{"(a)","@"}  )
      PRTREE.prtree:addWord(nil,{"(a)","%"}  )
      PRTREE.prtree:addWord(nil,{"(a)","^"}  )
      PRTREE.prtree:addWord(nil,{"(a)","&"}  )
      PRTREE.prtree:addWord(nil,{"(a)","*"}  )
      PRTREE.prtree:addWord(nil,{"(a)","-"}  )
      PRTREE.prtree:addWord(nil,{"(a)","+"}  )
      PRTREE.prtree:addWord(nil,{"(a)","="}  )
      PRTREE.prtree:addWord(nil,{"(a)","/"}  )
      PRTREE.prtree:addWord(nil,{"(a)","++"} )
      PRTREE.prtree:addWord(nil,{"(a)","(a)"}) 
      PRTREE.prtree:addWord(nil,{".T.","!"}  )
      PRTREE.prtree:addWord(nil,{".T.","@"}  )
      PRTREE.prtree:addWord(nil,{".T.","%"}  )
      PRTREE.prtree:addWord(nil,{".T.","^"}  )
      PRTREE.prtree:addWord(nil,{".T.","&"}  )
      PRTREE.prtree:addWord(nil,{".T.","*"}  )
      PRTREE.prtree:addWord(nil,{".T.","-"}  )
      PRTREE.prtree:addWord(nil,{".T.","+"}  )
      PRTREE.prtree:addWord(nil,{".T.","="}  )
      PRTREE.prtree:addWord(nil,{".T.","/"}  )
      PRTREE.prtree:addWord(nil,{".T.","++"} )
      PRTREE.prtree:addWord(nil,{".T.","(a)"}) 
      PRTREE.prtree:addWord(nil,{"1","!"}    )
      PRTREE.prtree:addWord(nil,{"1","@"}    )
      PRTREE.prtree:addWord(nil,{"1","%"}    )
      PRTREE.prtree:addWord(nil,{"1","^"}    )
      PRTREE.prtree:addWord(nil,{"1","&"}    )
      PRTREE.prtree:addWord(nil,{"1","*"}    )
      PRTREE.prtree:addWord(nil,{"1","-"}    )
      PRTREE.prtree:addWord(nil,{"1","+"}    )
      PRTREE.prtree:addWord(nil,{"1","="}    )
      PRTREE.prtree:addWord(nil,{"1","/"}    )
      PRTREE.prtree:addWord(nil,{"1","++"}   )
      PRTREE.prtree:addWord(nil,{"1","(a)"}  )

      #endif

      // KÇt token kîzîtt kifejezÇshat†rok.
      PRTREE.prtree:addWord(1,{"%","!"}    )
      PRTREE.prtree:addWord(1,{"%","@"}    )
      PRTREE.prtree:addWord(1,{"%","%"}    )
      PRTREE.prtree:addWord(1,{"%","^"}    )
      PRTREE.prtree:addWord(1,{"%","*"}    )
      PRTREE.prtree:addWord(1,{"%","-"}    )
      PRTREE.prtree:addWord(1,{"%","+"}    )
      PRTREE.prtree:addWord(1,{"%","="}    )
      PRTREE.prtree:addWord(1,{"%","/"}    )
      PRTREE.prtree:addWord(1,{"^","!"}    )
      PRTREE.prtree:addWord(1,{"^","@"}    )
      PRTREE.prtree:addWord(1,{"^","%"}    )
      PRTREE.prtree:addWord(1,{"^","^"}    )
      PRTREE.prtree:addWord(1,{"^","*"}    )
      PRTREE.prtree:addWord(1,{"^","-"}    )
      PRTREE.prtree:addWord(1,{"^","+"}    )
      PRTREE.prtree:addWord(1,{"^","="}    )
      PRTREE.prtree:addWord(1,{"^","/"}    )
      PRTREE.prtree:addWord(1,{"*","!"}    )
      PRTREE.prtree:addWord(1,{"*","@"}    )
      PRTREE.prtree:addWord(1,{"*","%"}    )
      PRTREE.prtree:addWord(1,{"*","^"}    )
      PRTREE.prtree:addWord(1,{"*","*"}    )
      PRTREE.prtree:addWord(1,{"*","-"}    )
      PRTREE.prtree:addWord(1,{"*","+"}    )
      PRTREE.prtree:addWord(1,{"*","="}    )
      PRTREE.prtree:addWord(1,{"*","/"}    )
      PRTREE.prtree:addWord(1,{"-","!"}    )
      PRTREE.prtree:addWord(1,{"-","@"}    )
      PRTREE.prtree:addWord(1,{"-","%"}    )
      PRTREE.prtree:addWord(1,{"-","^"}    )
      PRTREE.prtree:addWord(1,{"-","*"}    )
      PRTREE.prtree:addWord(1,{"-","-"}    )
      PRTREE.prtree:addWord(1,{"-","+"}    )
      PRTREE.prtree:addWord(1,{"-","="}    )
      PRTREE.prtree:addWord(1,{"-","/"}    )
      PRTREE.prtree:addWord(1,{"+","!"}    )
      PRTREE.prtree:addWord(1,{"+","@"}    )
      PRTREE.prtree:addWord(1,{"+","%"}    )
      PRTREE.prtree:addWord(1,{"+","^"}    )
      PRTREE.prtree:addWord(1,{"+","*"}    )
      PRTREE.prtree:addWord(1,{"+","-"}    )
      PRTREE.prtree:addWord(1,{"+","+"}    )
      PRTREE.prtree:addWord(1,{"+","="}    )
      PRTREE.prtree:addWord(1,{"+","/"}    )
      PRTREE.prtree:addWord(1,{"/","!"}    )
      PRTREE.prtree:addWord(1,{"/","@"}    )
      PRTREE.prtree:addWord(1,{"/","%"}    )
      PRTREE.prtree:addWord(1,{"/","^"}    )
      PRTREE.prtree:addWord(1,{"/","*"}    )
      PRTREE.prtree:addWord(1,{"/","-"}    )
      PRTREE.prtree:addWord(1,{"/","+"}    )
      PRTREE.prtree:addWord(1,{"/","="}    )
      PRTREE.prtree:addWord(1,{"/","/"}    )
      PRTREE.prtree:addWord(1,{"nev","nev"}) 
      PRTREE.prtree:addWord(1,{"nev",".T."}) 
      PRTREE.prtree:addWord(1,{"nev","1"}  )
      PRTREE.prtree:addWord(1,{"(a)","nev"}) 
      PRTREE.prtree:addWord(1,{"(a)",".T."}) 
      PRTREE.prtree:addWord(1,{"(a)","1"}  )
      PRTREE.prtree:addWord(1,{".T.","nev"}) 
      PRTREE.prtree:addWord(1,{".T.",".T."}) 
      PRTREE.prtree:addWord(1,{".T.","1"}  )
      PRTREE.prtree:addWord(1,{"1","nev"}  )
      PRTREE.prtree:addWord(1,{"1",".T."}  )
      PRTREE.prtree:addWord(1,{"1","1"}    )

      // H†rom hossz£ak, amiket elfogadunk.
      PRTREE.prtree:addWord(nil,{"/","-","nev"}  )
      PRTREE.prtree:addWord(nil,{"/","-","1"}    )
      PRTREE.prtree:addWord(nil,{"%","-","nev"}  )
      PRTREE.prtree:addWord(nil,{"%","-","1"}    )
      PRTREE.prtree:addWord(nil,{"^","-","nev"}  )
      PRTREE.prtree:addWord(nil,{"^","-","1"}    )
      PRTREE.prtree:addWord(nil,{"*","-","nev"}  )
      PRTREE.prtree:addWord(nil,{"*","-","1"}    )
      PRTREE.prtree:addWord(nil,{"-","-","nev"}  )
      PRTREE.prtree:addWord(nil,{"-","-","1"}    )
      PRTREE.prtree:addWord(nil,{"+","-","nev"}  )
      PRTREE.prtree:addWord(nil,{"+","-","1"}    )
      PRTREE.prtree:addWord(nil,{"+","/","nev"}  )

      // Speci†lis kivÇtelek, amiket el kell fogadni.
      PRTREE.prtree:addWord(nil,{"*","/","nev"}  )
      
      // Speci†lis kivÇtelek, amikben kifejezÇshat†r van.
      PRTREE.prtree:addWord(2,{"nev","++","nev"} )
      PRTREE.prtree:addWord(2,{"nev","++",".T."} )
      PRTREE.prtree:addWord(2,{"nev","++","1"}   )
      PRTREE.prtree:addWord(2,{"(a)","++","nev"} )
      PRTREE.prtree:addWord(2,{"(a)","++",".T."} )
      PRTREE.prtree:addWord(2,{"(a)","++","1"}   )
      PRTREE.prtree:addWord(2,{".T.","++","nev"} )
      PRTREE.prtree:addWord(2,{".T.","++",".T."} )
      PRTREE.prtree:addWord(2,{".T.","++","1"}   )
      PRTREE.prtree:addWord(2,{"1","++","nev"}   )
      PRTREE.prtree:addWord(2,{"1","++",".T."}   )
      PRTREE.prtree:addWord(2,{"1","++","1"}     )  
      
   endif
return prtree


//*******************************************************************
#ifdef OLD
function exprChk2(clf1,clf2)
// A kÇt classify-r¢l meg†llap°tja, hogy kîvetkezhetnek-e egym†s ut†n.
// Egyik sem lehet Åres (sorvÇgjel, etc.)
local prtreepr,w

   if (clf1=="!" .or. clf1=="@" .or. clf1=="++" .or. clf1=="=")
      return .t.
   endif


   prtreepr:=C.PRTREEPR:onew(expPrtree2())
   if (nil==(w:=PRTREEPR.prtreepr:put(clf1)))
      w:=PRTREEPR.prtreepr:put(clf2)
   endif
   
   // w==nil: Ez nem lehet, mert a f†ban csak kÇt elemñ szavak 
   //         vannak.
return w!=.f.

//*******************************************************************
function expPrtree3()
// Megadja a h†rom elemñ kifejezÇsekre a kivÇteleket.
// Ezek azok, amiket az exprChk2() nem enged el, de j¢k.

static prtree

   if (prtree==nil)
   
      prtree:=C.PRTREE:onew()
      
      PRTREE.prtree:addWord(nil,{"/","-","nev"}  )
      PRTREE.prtree:addWord(nil,{"/","-","1"}  )
      PRTREE.prtree:addWord(nil,{"%","-","nev"}  )
      PRTREE.prtree:addWord(nil,{"%","-","1"}  )
      PRTREE.prtree:addWord(nil,{"^","-","nev"}  )
      PRTREE.prtree:addWord(nil,{"^","-","1"}  )
      PRTREE.prtree:addWord(nil,{"*","-","nev"}  )
      PRTREE.prtree:addWord(nil,{"*","-","1"}  )
      PRTREE.prtree:addWord(nil,{"-","-","nev"}  )
      PRTREE.prtree:addWord(nil,{"-","-","1"}  )
      PRTREE.prtree:addWord(nil,{"+","-","nev"}  )
      PRTREE.prtree:addWord(nil,{"+","-","1"}  )
      PRTREE.prtree:addWord(nil,{"+","/","nev"}  )
      // PRTREE.prtree:addWord(nil,{"","",""}  )
      // PRTREE.prtree:addWord(nil,{"","",""}  )
      // PRTREE.prtree:addWord(nil,{"","",""}  )
      // PRTREE.prtree:addWord(nil,{"","",""}  )
      
   endif
return prtree

//*******************************************************************
function exprChk3(clf1,clf2,clf3)
// A h†rom classify-r¢l meg†llap°tja, hogy kîvetkezhetnek-e egym†s 
// ut†n.
// Csak akkor h°vhat¢, ha (clf2,clf3)-at az exprChk2() nem fogadta el.
// Egyik sem lehet Åres (sorvÇgjel, etc.)
local prtreepr,w

   prtreepr:=C.PRTREEPR:onew(expPrtree3())
   if (nil==(w:=PRTREEPR.prtreepr:put(clf1)))
      if (nil==(w:=PRTREEPR.prtreepr:put(clf2)))
         w:=PRTREEPR.prtreepr:put(clf3)
      endif
   endif
   
   // w==nil: Ez nem lehet, mert a f†ban csak kÇt elemñ szavak 
   //         vannak.
return w!=.f.

//*******************************************************************
function exprChk(clfArray)
// Meg†llp°tja, hogy a clfArray utols¢ n. eleme kîzîtt hol van
// a kifejezÇs hat†r.
// Ret: nil, ha nincs kifejezÇs hat†r.
//      sz†m: a clfArray-ban az utols¢, mÇg a kifejezÇshez tartoz¢
//            elem indexe.
local clf0,clf1,clf2
          
   if (len(clfArray)<2)
      return nil
   endif
   
   clf0:=clfArray[len(clfArray)]
   clf1:=clfArray[len(clfArray)-1]

   if (exprChk2(clf1,clf0))
      return nil
   elseif (len(clfArray)>=3)
      clf2:=clfArray[len(clfArray)-2]
      if (exprChk3(clf2,clf1,clf0))
         return nil
      endif
   endif
return len(clfArray)-1
//*******************************************************************
#endif



//*******************************************************************
function exprChk(clfArray)
// Meg†llap°tja, hogy a clfArray utols¢ EXPPRTREE_MAXDEEP eleme 
// kîzîtt hol van a kifejezÇs hat†r.
// Felteszi, hogy a clfArray elsì n-1 elemÇrìl nem meg†llap°that¢, 
// hogy hol van a kifejezÇshat†r.
// Ret: nil, ha nincs kifejezÇs hat†r.
//      sz†m: a clfArray-ban az utols¢, mÇg a kifejezÇshez tartoz¢
//            elem indexe.
/*
   A kîvetkezìkÇppen mñkîdik:
   n=EXPPRTREE_MAXDEEP
   MegnÇzi, hogy a clfArray utols¢ n elemÇre meg†llap°that¢-e
   kifejezÇshat†r, ha igen, OK, ha nem, akkor az n-et csîkkenti 
   egy-el. Ha az n<2, akkor a kifejezÇshat†r nem †llap°that¢ meg.
   
   Egy n-re pedig £gy †llap°tja meg, hogy van-e kifejezÇshat†r, 
   hogy elemezteti az exprPrTree-vel. Ha benne van, akkor a
   result megmondja az eredmÇnyt. Ha nincs benne, akkor elfogadjuk.
   Ha az elemzì azt mondja, hogy mÇg kell olvasni, akkor 
   elfogadjuk.
*/
local n, prtreepr,i,w

   for n:=EXPPRTREE_MAXDEEP to 2 step -1
      if (len(clfArray)>=n)
         prtreepr:=C.PRTREEPR:onew(expPrtree())
         for i:=len(clfarray)-n+1 to len(clfArray)
            if (nil!=(w:=PRTREEPR.prtreepr:put(clfArray[i])))
               // Az elemzì meg†llt.
               if (w)
                  // Benne van. 
                  if (PRTREEPR.prtreepr:result==nil)
                     // Elfogadva.
                     return nil
                  endif
                  // Megvan a kifejezÇshat†r.
                  return len(clfArray)-n+PRTREEPR.prtreepr:result
               else
                  // Nincs benne, megyÅnk tov†bb.
                  exit
               endif
            endif
         end for
      endif
   end for
   
return nil   


//*******************************************************************

