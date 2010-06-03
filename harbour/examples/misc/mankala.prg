/*
 * $Id$
 */

//
// Mankala. The first Harbour board game.
//
// Written by Eddie Runia <eddie@runia.com>
// www - http://harbour-project.org
//
// Date : 30/04/1999
// Time : 14:00
//
// Placed in the public domain
//

function Main()

   local cAnswer := "X"
   local cPlayer
   local lWon    := .F.
   local aBoard  := {2,2,2,2}
   local nMove
   local cMove
   local nLoop
   local nLoop2

   QOut( "Mankala. Another Harbour Game Production")
   QOut()

   if Upper( Read( "Do you want instructions ? " ) ) == "Y"

      aEval ( { ;
         " Mankala. The first Harbour board game.                                   " , ;
         "                                                                          " , ;
         " It is just you against the computer. The board is as follows :           " , ;
         "                                                                          " , ;
         "   Computer                                                               " , ;
         "  ÚÄÄÄÂÄÄÄ¿                                                               " , ;
         "  ³X X³X X³                                                               " , ;
         "  ÃÄÄÄÅÄÄÄ´                                                               " , ;
         "  ³X X³X X³                                                               " , ;
         "  ÀÄÄÄÁÄÄÄÙ                                                               " , ;
         "     You                                                                  " , ;
         "                                                                          " , ;
         " The game begins with two stones in each square.                          " , ;
         "                                                                          " }, ;
         {| cItem | QOut( cItem )} )

      Read( "Pause : ")

      aEval ( { ;
         " You can choose to play either the left or the right (L/R) square.        " , ;
         "                                                                          " , ;
         " If you select a square, the stones are moved anti-clockwise through the  " , ;
         " squares.                                                                 " , ;
         "                                                                          " , ;
         " Example : You select Left. The board will now become :                   " , ;
         "                                                                          " , ;
         "   Computer                                                               " , ;
         "  ÚÄÄÄÂÄÄÄ¿                                                               " , ;
         "  ³X X³XXX³                                                               " , ;
         "  ÃÄÄÄÅÄÄÄ´                                                               " , ;
         "  ³   ³XXX³                                                               " , ;
         "  ÀÄÄÄÁÄÄÄÙ                                                               " , ;
         "     You                                                                  " , ;
         "                                                                          " , ;
         " The person which gets all the stones has won the game                    " , ;
         "                                                                          " , ;
         " Just try to beat the computer :-)                                        " }, ;
         {| cItem | QOut( cItem )} )

      Read( "Pause:" )

   endif

   do while cAnswer != "Y" .and. cAnswer != "N"
      cAnswer := Read( "Would you like to play first ?" )
      cAnswer := Upper( cAnswer )               // Nested functions ??
   enddo

   if cAnswer == "N"
      cPlayer := "Computer"
   else
      cPlayer := "Human"
   endif

   do while !lWon

      QOut()
      QOut("Computer ",aBoard[2],' ',aBoard[1])
      QOut("Human    ",aBoard[3],' ',aBoard[4])
      QOut()
      QOut("Player   ",cPlayer)

      if (aBoard[2]==0 .and. aBoard[1]==0) .or. ;
         (aBoard[3]==0 .and. aBoard[4]==0)
         lWon := .T.
      endif

      if !lWon
         if cPlayer == "Computer"

           do case
             case aBoard[1]==0
                nMove := 1
             case aBoard[1]==3 .and. aBoard[2]==2 .and. ;
                  aBoard[3]==2 .and. aBoard[4]==1
                nMove := 1
             case aBoard[1]==1 .and. aBoard[2]==6 .and. ;
                  aBoard[3]==1 .and. aBoard[4]==0
                nMove := 1
             case aBoard[1]==1 .and. aBoard[2]==1 .and. ;
                  aBoard[3]==6 .and. aBoard[4]==0
                nMove := 1
             case aBoard[1]==4 .and. aBoard[2]==1 .and. ;
                  aBoard[3]==3 .and. aBoard[4]==0
                nMove := 1
             case aBoard[1]==3 .and. aBoard[2]==1 .and. ;
                  aBoard[3]==4 .and. aBoard[4]==0
                nMove := 1
             otherwise
                nMove := 0
           endcase

         else

            nMove := 0
            do while nMove == 0
               cMove := Read( "Left/Right :" )
               cMove := Upper( cMove )
               if cMove == "L"
                  nMove := 2
               else
                  if cMove == "R"
                     nMove := 3
                  endif
               endif
            enddo
            lWon := aBoard[nMove+1] == 0

         endif

         nLoop2 := nMove
         nLoop := aBoard[nMove+1]
         aBoard[nMove+1] := 0

         do while nLoop != 0
            nLoop2++
            aBoard[(nLoop2 % 4)+1]++            // It works :-)
            nLoop--
         enddo

         if cPlayer == "Human"
            cPlayer := "Computer"
         else
            cPlayer := "Human"
         endif

      endif
   enddo

   if cPlayer == "Human"
      QOut( "You have beaten me :-)" )
   else
      QOut( "You'll never learn !" )
   endif

return nil


function Read( cPrompt )
return __Accept( cPrompt )
