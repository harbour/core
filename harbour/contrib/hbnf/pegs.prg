/*
 * $Id$
 */

/*
 * File......: pegs.prg
 * Author....: Greg Lief
 * CIS ID....: 72460,1760
 *
 * This function is an original work by Mr. Grump and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1991 03:09:44   GLENN
 * Allowed "No peg at that location" messagee to exceed the boundary of the
 * box at the bottom of the matrix.  Just shortened the message to "No
 * piece there, per Greg's instructions.
 *
 *    Rev 1.2   15 Aug 1991 23:04:18   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:38   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:00   GLENN
 * Nanforum Toolkit
 *
 */

#include "inkey.ch"
#include "setcurs.ch"

#translate SINGLEBOX(<top>, <left>, <bottom>, <right>) => ;
           @ <top>, <left>, <bottom>, <right> BOX "ÚÄ¿³ÙÄÀ³ "
#translate DOUBLEBOX(<top>, <left>, <bottom>, <right>) => ;
           @ <top>, <left>, <bottom>, <right> BOX 'ÉÍ»º¼ÍÈº '
memvar getlist

/*
   here's the board array -- structure of which is:
   board_[xx, 1] - subarray containing box coordinates for this peg
   board_[xx, 2] - subarray containing all adjacent locations
   board_[xx, 3] - subarray containing all target locations
   board_[xx, 4] - is the location occupied or not? .T. -> Yes, .F. -> No
*/
THREAD static board_ := { { {0, 29, 2, 34}, {2, 4}, {3, 9}, .T. } , ;
             { {0, 37, 2, 42}, {5}, {10}, .T.}      , ;
             { {0, 45, 2, 50}, {2, 6}, {1, 11}, .T. } , ;
             { {3, 29, 5, 34}, {5, 9}, {6, 16}, .T. } , ;
             { {3, 37, 5, 42}, {10}, {17}, .T. } , ;
             { {3, 45, 5, 50}, {5, 11}, {4, 18}, .T. } , ;
             { {6, 13, 8, 18}, {8, 14}, {9, 21}, .T. } , ;
             { {6, 21, 8, 26}, {9, 15}, {10, 22}, .T. } , ;
             { {6, 29, 8, 34}, {4, 8, 10, 16}, {1, 7, 11, 23}, .T. } , ;
             { {6, 37, 8, 42}, {5, 9, 11, 17}, {2, 8, 12, 24}, .T. } , ;
             { {6, 45, 8, 50}, {6, 10, 12, 18}, {3, 9, 13, 25}, .T. } , ;
             { {6, 53, 8, 58}, {11, 19}, {10, 26}, .T. } , ;
             { {6, 61, 8, 66}, {12, 20}, {11, 27}, .T. } , ;
             { {9, 13, 11, 18}, {15}, {16}, .T. } , ;
             { {9, 21, 11, 26}, {16}, {17}, .T. } , ;
             { {9, 29, 11, 34}, {9, 15, 17, 23}, {4, 14, 18, 28}, .T. } , ;
             { {9, 37, 11, 42}, {10, 16, 18, 24}, {5, 15, 19, 29}, .F. } , ;
             { {9, 45, 11, 50}, {11, 17, 19, 25}, {6, 16, 20, 30}, .T. } , ;
             { {9, 53, 11, 58}, {18}, {17}, .T. } , ;
             { {9, 61, 11, 66}, {19}, {18}, .T. } , ;
             { {12, 13, 14, 18}, {14, 22}, {7, 23}, .T. } , ;
             { {12, 21, 14, 26}, {15, 23}, {8, 24}, .T. } , ;
             { {12, 29, 14, 34}, {16, 22, 24, 28}, {9, 21, 25, 31}, .T. } , ;
             { {12, 37, 14, 42}, {17, 23, 25, 29}, {10, 22, 26, 32}, .T. } , ;
             { {12, 45, 14, 50}, {18, 24, 26, 30}, {11, 23, 27, 33}, .T. } , ;
             { {12, 53, 14, 58}, {19, 25}, {12, 24}, .T. } , ;
             { {12, 61, 14, 66}, {20, 26}, {13, 25}, .T. } , ;
             { {15, 29, 17, 34}, {23, 29}, {16, 30}, .T. } , ;
             { {15, 37, 17, 42}, {24}, {17}, .T. } , ;
             { {15, 45, 17, 50}, {25, 29}, {18, 28}, .T. } , ;
             { {18, 29, 20, 34}, {28, 32}, {23, 33}, .T. } , ;
             { {18, 37, 20, 42}, {29}, {24}, .T. } , ;
             { {18, 45, 20, 50}, {30, 32}, {25, 31}, .T. } }

function FT_PEGS
LOCAL XX, MOVE, MPOS, POSSIBLE_, BUFFER, TOPROW, OLDSCORE, MOVE2, ;
      SCANBLOCK, OLDCOLOR := SETCOLOR('w/n'), ;
      oldscrn := savescreen(0, 0, maxrow(), maxcol())
/*
   the following code block is used in conjunction with ASCAN()
   to validate entry when there is more than one possible move
*/
scanblock := { | a | a[2] == move2 }
cls
setcolor('w/r')
SINGLEBOX(22, 31, 24, 48)
@ 23, 33 say "Your move:"
aeval(board_, { | a, x | HB_SYMBOL_UNUSED( a ), drawbox( x ) } )
do while lastkey() != K_ESC .and. moremoves()
   move := 1
   setcolor('w/n')
   @ 23, 44 get move picture '##' range 1, 33
   read
   if move > 0
      do case
         case ! board_[move][4]
            err_msg("No piece there!")
         otherwise
            possible_ := {}
            for xx := 1 to len(board_[move][2])
               if board_[board_[move][2,xx] ][4] .and. ;
                  ! board_[board_[move][3,xx] ][4]
                  aadd(possible_, { board_[move][2,xx], board_[move][3,xx] })
               endif
            next
            // only one available move -- do it
            do case
               case len(possible_) == 1
                  // clear out original position and the position you jumped over
                  board_[move][4] := board_[possible_[1, 1] ][4] := .F.
                  board_[possible_[1, 2] ][4] := .T.
                  drawbox(move, board_[move])
                  drawbox(possible_[1,1])
                  drawbox(possible_[1,2])
               case len(possible_) == 0
                  err_msg('Illegal move!')
               otherwise
                  move2 := possible_[1, 2]
                  toprow := 21 - len(possible_)
                  setcolor('+w/b')
                  buffer := savescreen(toprow, 55, 22, 74)
                  DOUBLEBOX(toprow, 55, 22, 74)
                  @ toprow, 58 say 'Possible Moves'
                  devpos(toprow, 65)
                  aeval(possible_, { | a | devpos(row()+1, 65), ;
                                           devoutpict(a[2], '##') } )
                  oldscore := set(_SET_SCOREBOARD, .f.)
                  @23, 44 get move2 picture '##' ;
                          valid ascan(possible_, scanblock) > 0
                  read
                  restscreen(toprow, 55, 22, 74, buffer)
                  set(_SET_SCOREBOARD, oldscore)
                  mpos := ascan(possible_, { | a | move2 == a[2] })
                  // clear out original position and the position you jumped over
                  board_[move][4] := board_[possible_[mpos, 1] ][4] := .F.
                  board_[move2][4] := .T.
                  drawbox(move)
                  drawbox(possible_[mpos,1])
                  drawbox(move2)

            endcase
      endcase
      move := 1
   endif
enddo
setcolor(oldcolor)
restscreen(0, 0, maxrow(), maxcol(), oldscrn)
return NIL

* end function FT_PEGS()
*--------------------------------------------------------------------*

static function DrawBox(nelement)
setcolor(iif(board_[nelement][4], '+w/rb', 'w/n'))
@ board_[nelement][1,1], board_[nelement][1,2], board_[nelement][1,3], ;
  board_[nelement][1,4] box "ÚÄ¿³ÙÄÀ³ "
DevPos(board_[nelement][1,1] + 1, board_[nelement][1,2] + 2)
DevOut(ltrim(str(nelement)))
return NIL

* end static function DrawBox()
*--------------------------------------------------------------------*

static function err_msg(msg)
local buffer := savescreen(23, 33, 23, 47)
setcursor(SC_NONE)
setcolor('+w/r')
@ 23, 33 say msg
inkey(2)
setcursor(SC_NORMAL)
restscreen(23, 33, 23, 47, buffer)
return nil

* end static function Err_Msg()
*--------------------------------------------------------------------*

static function moremoves()
local xx, yy, canmove := .f., piecesleft := 0, buffer
for xx := 1 to 33
   for yy := 1 to len(board_[xx][2])
      if board_[xx][4] .and.  ;            // if current location is filled
            board_[board_[xx][2,yy] ][4] .and. ;  // adjacent must be filled
            ! board_[board_[xx][3,yy] ][4]           // target must be empty
         canmove := .t.
         exit
      endif
   next
   // increment number of pieces left
   if board_[xx][4]
      piecesleft++
   endif
next
if ! canmove
   setcolor('+w/b')
   buffer := savescreen(18, 55, 21, 74)
   DOUBLEBOX(18, 55, 21, 74)
   @ 19, 58 say "No more moves!"
   @ 20, 58 say ltrim(str(piecesleft)) + " pieces left"
   inkey(0)
   restscreen(18, 55, 21, 74, buffer)
endif
return canmove

* end static function MoreMoves()
*--------------------------------------------------------------------*

* eof pegs.prg
