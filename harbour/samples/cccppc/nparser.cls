//
// $Id$
//

//*******************************************************************
// nparser.cls: NÇv elemzì. A PARSER-tìl îrîkîl.
// 1999, Csisz†r Levente

// Ez az elemzì vÇgzi el az alapvetì elemzÇseket, felbontja az input
// stream-et a kîvetkezì tokenekre:
//   - NÇv
//   - sz†msor (nincs benne pont '.')
//   - öres
//   - MegjegyzÇs (csillag, perper, percsillag)
//   - String (macskakîrmîs, idÇzìjeles)
//   - ójsor
//   - Karakter.
//   Az egyÇb tokenek mindig pontosan egy karakteresek.

// A karaktereken k°vÅl megÇrti a kîvetkezì egysÇgeket:
// CTK_BOS,CTK_EOS,CTK_PRINTLINE
// Az EOS-ra kÅld egy £j sor karatert

// Mj.: A ';' kezelÇsÇt Çs a sor îsszevon†sokat az lparser elemzì
// csin†lja.

class NPARSER PARSER
   
   // attrib item
   // attrib buf 
   attrib soreleje
   attrib tokenPos // Egy £j token olvas†sakor a {file,line,pos}-t
                   // ide letesszÅk. A makeToken ebbìl dolgozik.
      
   rmethod readItem()
   // Olvas egy elemet. 
   // Ret: Az elem, ha OK, nil, ha vÇge van a streamnek, vagy ha
   //      hiba volt.

   nmethod strEolParserBuffer()
   // Megadja a parserBuffer tartalm†t stringkÇnt.
   // A nem stringeket az eol kivÇtelÇvel eldobja.
   
   nmethod makeToken(tkId,str)
   // KÇsz°t egy tkId t°pus£ tokent 'str' tartalommal.
                      
   nmethod makeTokenSt(tkId,stArray)
   // KÇsz°t egy tkId t°pus£ tokenstr-t az stArray-nak megfelelìen.
   
   nmethod makeTokenLSt(tkId,stArray)
   // KÇsz°t egy tkId t°pus£ tokenstr-t az stArray-nak megfelelìen.
   // A str-be be°rja a #line sort, ha a mÇlysÇg vagy a sorsz†m 
   // nagyobb, mint 1.
   
   
   nmethod makeErrToken(tkId,str,errCode)
   // KÇsz°t egy tkId t°pus£ tokent 'str' tartalommal Çs errCode 
   // hibak¢ddal.
                   
   nmethod parseTPTree()
   
   nmethod parse()
   // Ez vÇgzi a tÇnyleges elemzÇst, rekurz°van is lehet h°vni.
   // A this:item-t elemzi, szÅksÇg esetÇn mÇg olvashat.
   
   nmethod parseLineComment(tkId)
   // '//' vagy '*' megjegyzÇs
   
   nmethod parsePcComment()
   // '/*'-os megjegyzÇs
   
   nmethod parseString(hatarolo)
   // Felsìvesszìs Çs macskakîrmîs string.
   
   nmethod parseName()
   // NÇv.
   
   nmethod parseNumberArray()
   // Sz†m sor. Ebben csak sz†mok vannak, pont nem lehet.



   rmethod oinit(inputReader,name,errorStream)
   
metaclass
   
   rmethod onew(inputReader,name,errorStream)
   
   attrib whitespacechar
   // attrib specchar
   attrib startnamechar
   attrib numberchar
   attrib namechar
   
   attrib tokens
   attrib tokenptree
   
   rmethod oinitclass()

   nmethod isWhitespacechar(c)
   // nmethod isSpecchar(c)
   nmethod isStartnamechar(c)
   nmethod isNumberchar(c)
   nmethod isNamechar(c)
   
   nmethod setTokens()
   // Feltîlti a class:tokens v†ltoz¢t az oszt†lyban defini†lt 
   // tokenekkel.

   nmethod calcTokenPTree()
   // KÇsz°t egy elemzì f†t a tokenek elemzÇsÇhez a 
   // class:tokenPTree-be.

