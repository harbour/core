//
// $Id$
//

//*******************************************************************
// prtree.cls: Parser tree. Elemz“ f t kezel“ objektum
// 1999, Csisz r Levente

// Egy elemz“ f t lehet vele fel‚p¡teni ‚s kezelni.

class PRTREE OBJECT

   attrib nWords  // H ny sz¢ van benne.
   attrib tree    // Ez maga a fa.
   // Az elemz“ fa:
   // <root>:=<node>
   // <node>:={ [<terminal>,] <inner branch1>, <inner branch2>, ... }
   // 
   // <inner branch>:={item,<node>}
   // 
   // <terminal>:={nil,result}
   //
   // item:   A fa i. szintj‚n a keresett sz¢ i. eleme.
   // result: Amikor az elemz‚s k‚sz ‚s erem‚nyes, akkor a megtal lt
   //         <terminal>-ban lev“ result lesz a this:result-ban.
   
   
   nmethod addWord(result,itemArray)
   
   rmethod oinit()
   
