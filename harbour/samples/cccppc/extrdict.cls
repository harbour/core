//
// $Id$
//

//*******************************************************************
// extrdict.cls: #xtranslate sz¢t r elem
// 1999, Csisz r Levente

class EXTRDICT OBJECT

   attrib cmdType        // 0: xtranslatae, 1: xcommand.
   attrib leftSide
   attrib rightSide
   attrib deffilelinepos
   attrib numMatchMarkers
   attrib undefRMarkers  // Azok a result markerek, amikhez nincs 
                         // match marker
   
   nmethod change(paramValues)
   // A paramValues tartalmazza a match tokenekre illesztett
   // tokeneket.
   
   nmethod printStr()
   // Stringk‚nt adja meg az elemet #xtranslate leftSide => rightSide
   // form ban.
   
//   nmethod setNextToken()
   // Az ”sszes match markerben be ll¡tja a nextToken-t, ha van.
   
//   nmethod calcSorszam()
   // - ™sszeszedi a match markereket.
   // - Minden match markerbe be¡rja a sorsz m t.
   // - Minden result marker-be be¡rja a hozz  tartoz¢ match
   //   marker sorsz m t.
   
   rmethod oinit(cmdType,leftSide,rightSide,deffile,defline,defpos)


metaclass

   rmethod onew(cmdType,leftSide,rightSide,deffile,defline,defpos)


