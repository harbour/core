
#require "hbnf"

#include "ftmenuto.ch"

PROCEDURE Main()

   LOCAL nChoice := 1

   CLS

   // Simple prompt
   @  1, 1 PROMPT "Menu choice #1"

   // Prompt with color
   @  3, 1 PROMPT "Menu choice #2" COLOR "W+/R,W+/B"

   // Prompt with a message
   @  5, 1 PROMPT "Menu choice #3" MESSAGE "Go to lunch"

   // Prompt with pinpoint message control
   @  7, 1 PROMPT "Menu choice #4" MESSAGE "Drop Dead" ;
      MSGROW 22 MSGCOL 4 MSGCOLOR "GR+/N"

   // Prompt with a trigger character ("#" character)
   @ 11, 1 PROMPT "Menu choice #6" TRIGGER 13

   // Prompt with trigger character color control
   @ 13, 1 PROMPT "Menu Choice #7" TRIGGER 13 TRIGGERCOLOR "R+/BG,G+/N"

   // Prompt with right and left arrow keys disabled
   @ 15, 1 PROMPT "Menu Choice #8" RIGHT 8 LEFT 8

   MENU TO nChoice

   ? nChoice

   RETURN
