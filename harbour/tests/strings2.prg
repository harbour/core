//
// $Id$
//

procedure main()
   local AString := "A should be 65"
   local Another := "   lost in space   "

   QOut( right(AString, 2) + substr(AString, 2, 11) + left(AString, 1))

   QOut('"' + ltrim(Another) + '"')

   QOut('"' + rtrim(Another) + '"')

   QOut('"' + alltrim(Another) + '"')

   QOut('"' + padr(AString, 20) + '"')

   QOut('"' + padr(AString, 20, '_') + '"')

   QOut('"' + padl(AString, 20) + '"')

   QOut('"' + padl(AString, 20, '_') + '"')

   QOut('"' + padc(AString, 20) + '"')

   QOut('"' + padc(AString, 20, '_') + '"')

   QOut('"' + padc(AString, 21) + '"')

   QOut('"' + padc(AString, 21, '_') + '"')
return

