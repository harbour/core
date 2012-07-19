/*
 * $Id$
 */
#command TOTAL [TO <(file)>] [ON <key>]                                 ;
      [FIELDS <fields,...>]                                          ;
      [FOR <for>]                                                    ;
      [WHILE <while>]                                                ;
      [NEXT <next>]                                                  ;
      [RECORD <rec>]                                                 ;
      [<rest:REST>]                                                  ;
      [ALL]                                                          ;
      ;
      => __dbTotal(                                                     ;
      < ( file ) > , < { key } > , { < ( fields ) > },                  ;
      < { for } > , < { while } > , < next > , < rec > , < .rest. >         ;
      )

   Notes:

   Frankly, I never USE this command. And I DO not know how this command can be
   useful. IF somebody can give me a sample program, please ..
   I READ the NG but it is not too CLEAR TO me :(
