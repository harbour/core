/* $Id$

   Harbour Project source code
   www - http://www.Harbour-Project.org

   Written by Vladimir Kazimirchik <v_kazimirchik@yahoo.com>
   http://i.am/kzm

   Released into public domain.
*/

#include "box.ch"
#include "inkey.ch"

Function Alert(cMessage, aOptions, nDelay)

   Local nRet := 0
   Local aSay, nPos, nWidth, nOpWidth, nInitRow, nInitCol, iEval, nChoice
   Local nKey, aPos, nCurrent
   Local nCursor := SetCursor(0)
   Local cScreen

   If aOptions = Nil
      aOptions := { 'Ok' }
   End

   If nDelay = Nil
      nDelay := 0
   End

   aSay := {}
   While (nPos := At(';', cMessage)) != 0
      AAdd(aSay, Left(cMessage, nPos - 1))
      cMessage := SubStr(cMessage, nPos + 1)
   End
   AAdd(aSay, cMessage)

   /* The longest line */
   nWidth := 0
   AEval(aSay, { |x| nWidth := Max(Len(x), nWidth) })

   /* Total width of the botton line (the one with choices) */
   nOpWidth := 0
   AEval(aOptions, { |x| nOpWidth += Len(x) + 3 })
   nOpWidth += 3

   /* what's wider ? */
   nWidth := Max(nWidth, nOpWidth) + 2   /* 2 spaces on the edges */

   /* box coordinates */
   nInitRow := Int((MaxRow() - (Len(aSay) + 4)) / 2) + 1
   nInitCol := Int((MaxCol() - (nWidth + 4)) / 2) + 1

   /* detect prompts positions */
   aPos := {}
   nCurrent := nInitCol + Int((nWidth - nOpWidth) / 2) + 4
   AEval(aOptions, { |x| AAdd(aPos, nCurrent), nCurrent += Len(x) + 3 })


   cScreen = SaveScreen( nInitRow, nInitCol, nInitRow + Len(aSay) + 3, nInitCol + nWidth + 1 )
   /* draw box */
   @ nInitRow, nInitCol, nInitRow + Len(aSay) + 3, nInitCol + nWidth + 1  ;
                  Box B_SINGLE + ' ' Color 'w+/r'
   For iEval := 1 To Len(aSay)
      @ nInitRow + iEval, nInitCol + 1 Say PadC(aSay[iEval], nWidth)  ;
            Color 'w+/r'
   Next

   nChoice = 1
   /* choice loop */
   While .T.
      For iEval := 1 To Len(aOptions)
         @ nInitRow + Len(aSay) + 2, aPos[iEval] Say aOptions[iEval]  ;
             Color If(iEval = nChoice, 'w+/b', 'w+/r')
      Next
      nKey := Inkey(nDelay)
      If nKey = K_ENTER .Or. nKey = 0
         nRet := nChoice
         Exit
      ElseIf nKey = K_ESC
         Exit
      ElseIf (nKey = K_LEFT .Or. nKey == K_SH_TAB) .And. Len(aOptions) > 1
         nChoice --
         If nChoice = 0
            nChoice := Len(aOptions)
         End

      ElseIf (nKey = K_RIGHT .Or. nKey == K_TAB) .And. Len(aOptions) > 1
         nChoice ++
         If nChoice > Len(aOptions)
            nChoice := 1
         End
      End
   End

   /* Restore screen */
   RestScreen( nInitRow, nInitCol, nInitRow + Len(aSay) + 3, nInitCol + nWidth + 1, cScreen )
   SetCursor(nCursor)

Return nRet

