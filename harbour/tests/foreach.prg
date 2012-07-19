/*
 * $Id$
 */

PROCEDURE Main()

   LOCAL A := { "one  ", "two  ", "three" }
   LOCAL AA := { "AA-one  ", "AA-two  ", "AA-three", "AA-four " }
   LOCAL c := "abcdefghij"
   LOCAL enum := "b"
   LOCAL bb, cc
   LOCAL i

/*
   test(@a,b)
   test(a,@b)
   test(@a,@b)
*/

   ? "========================================================"
   ? "before loop: ENUM=", ENUM
   ? "before loop: a[1]=", a[ 1 ], "a[2]=", a[ 2 ], "a[3]=", a[ 3 ]
   FOR EACH enum IN A
      ? "start: ENUM=", ENUM
      IF ENUM = "two"
         ENUM := Upper( ENUM )
      ENDIF
      ? "end:   ENUM=", ENUM, "| index:", ENUM:__enumIndex, "| value:", ENUM:__enumValue, "| base: ", ValType( ENUM:__enumBase )
   NEXT
   ? "after loop ENUM=", ENUM
   ? "after loop: a[1]=", a[ 1 ], "a[2]=", a[ 2 ], "a[3]=", a[ 3 ]
   ? "-----------------"
   ?
   Inkey( 0 )


   ? "========================================================"
   ? "Testing passing by reference"
   ? "before loop: ENUM=", ENUM
   ? "after loop: a[1]=", a[ 1 ], "a[2]=", a[ 2 ], "a[3]=", a[ 3 ]
   FOR EACH ENUM IN A
      IF Upper( ENUM ) = "TWO"
         ENUM := Upper( ENUM )
         ? "before passing by @ | ENUM=", ENUM, "| index:", ENUM:__enumIndex, "| value:", ENUM:__enumValue, "| base: ", ValType( ENUM:__enumBase )
         testBYREF( @ENUM )
         ? " after passing by @ | ENUM=", ENUM, "| index:", ENUM:__enumIndex, "| value:", ENUM:__enumValue, "| base: ", ValType( ENUM:__enumBase )
      ENDIF
   NEXT
   ? "after loop ENUM=", ENUM
   ? "after loop: a[1]=", a[ 1 ], "a[2]=", a[ 2 ], "a[3]=", a[ 3 ]
   Inkey( 0 )

   ? "========================================================"
   ? "Testing BREAK"
   ? "before loop: ENUM=", ENUM
   ? "after loop: a[1]=", a[ 1 ], "a[2]=", a[ 2 ], "a[3]=", a[ 3 ]
   BEGIN SEQUENCE
      FOR EACH enum IN A DESCEND
         ? "loop:   ENUM=", ENUM, "| index:", ENUM:__enumIndex, "| value:", ENUM:__enumValue, "| base: ", ValType( ENUM:__enumBase )
         TESTbreak( ENUM )
      NEXT

   RECOVER USING i
      ? "after loop ENUM=", ENUM
      ? "after loop: a[1]=", a[ 1 ], "a[2]=", a[ 2 ], "a[3]=", a[ 3 ]
      ? "recover variable i=", i
   END SEQUENCE
   Inkey( 0 )

   ? "========================================================"
   ? "before loop: ENUM=", ENUM
   ? "before loop: c=", c
   BEGIN SEQUENCE
      FOR EACH enum IN c
         ? "start: ENUM=", ENUM
         IF enum = "d"
            enum := Upper( enum )
         ENDIF
         Testbreak( enum )
         ? "end:   ENUM=", ENUM, "| index:", ENUM:__enumIndex, "| value:", ENUM:__enumValue, "| base: ", ValType( ENUM:__enumBase )
      NEXT
   RECOVER USING i
      ? "after loop ENUM=", ENUM
      ? "after loop: c=", c
      ? "recover variable i=", i
   END SEQUENCE


   ? "========================================================"
   FOR EACH enum, bb, cc IN A, AA, c
      ? enum, enum:__enumIndex, enum:__enumValue
      ? bb, bb:__enumIndex, bb:__enumValue
      ? cc, cc:__enumIndex, cc:__enumValue
   NEXT
   Inkey( 0 )

   ? "========================================================"
   FOR EACH enum, bb, cc IN A, AA, c DESCEND
      ? enum, enum:__enumIndex, enum:__enumValue
      ? bb, bb:__enumIndex, bb:__enumValue
      ? cc, cc:__enumIndex, cc:__enumValue
   NEXT

   FOR EACH enum IN a
      BEGIN SEQUENCE
         IF enum = "2"
            BREAK
         ENDIF
      END SEQUENCE
   NEXT

   FOR EACH enum IN a
      BEGIN SEQUENCE
         IF enum = "2"
            ? "Breaking... enum=", enum
            BREAK enum
         ENDIF
      RECOVER USING enum
         ? "after recovery: enum=", enum
      END SEQUENCE
   NEXT

   RETURN

PROCEDURE TESTbreak( v )

   IF v = "2" .OR. v = "d"
      ? "issuing break"
      Break( v )
   ENDIF

   RETURN

PROCEDURE TESTBYREF( enum )

   ? "start of testBYREF ENUM=", ENUM
   FOR EACH ENUM IN { 1, 2, 3 }
      ? "  -testBYREF=", ENUM
   NEXT
   ? "end of loop: ENUM=", ENUM
   ENUM := "22222"
   ? "end of testBYREF ENUM=", ENUM

   RETURN
