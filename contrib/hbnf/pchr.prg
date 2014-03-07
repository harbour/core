/*
 * Author....: Jim Gale
 * CIS ID....: 73670,2561
 *
 * This is an original work by Jim Gale and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   17 Aug 1991 15:40:16   GLENN
 * Don Caton fixed some spelling errors in the doc
 *
 *    Rev 1.1   15 Aug 1991 23:06:00   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   12 Jun 1991 01:45:04   GLENN
 * Initial revision.
 *
 */

/* NOTE: In original NF, control codes were also accepted when
         having extra characters (f.e. "/GRAPHICMODE" instead of "/GRAPHIC"),
         but only if _SET_EXACT was set to .F., Harbour accepts them
         that way regardless of _SET_EXACT setting. [vszakats] */

FUNCTION ft_PChr( c_nums )

   LOCAL c_ret := "", c_st := 0, c_part, c_st2, c_hex := "0123456789ABCDEF"
   LOCAL c_upper, c_t1, c_t2

   IF hb_LeftEq( c_nums, "," ) .OR. RTrim( c_nums ) == ""
      RETURN ""
   ENDIF

   c_nums := RTrim( c_nums ) + ",~,"
   c_part := SubStr( c_nums, c_st + 1, At( ",", SubStr( c_nums, c_st + 2 ) ) )

   DO WHILE !( c_part == "~" .OR. c_part == "" )

      IF hb_LeftEq( c_part, '"' )

         c_st2 := At( '"', SubStr( c_part, 2 ) ) + 1
         c_ret += SubStr( c_part, 2, c_st2 - 2 )

      ELSEIF hb_LeftEq( c_part, "&" )

         c_upper := Upper( c_part )
         IF ( c_t1 := At( SubStr( c_upper, 2, 1 ), c_hex ) - 1 ) > -1
            IF ( c_t2 := At( SubStr( c_upper, 3, 1 ), c_hex ) - 1 ) > -1
               c_t1 := c_t1 * 16 + c_t2
            ENDIF
            c_ret += Chr( c_t1 )
         ENDIF

      ELSEIF ( Val( c_part ) > 0 .AND. Val( c_part ) < 256 ) .OR. c_part == "0"

         c_ret += Chr( Val( c_part ) )

      ELSE

         IF hb_LeftEq( c_part, "/" )

            DO CASE
            CASE hb_LeftEqI( c_part, "/GRAPHIC" )
               c_ret += hb_BChar( 27 ) + hb_BChar( 116 ) + hb_BChar( 1 )
            CASE hb_LeftEqI( c_part, "/ITALIC" )
               c_ret += hb_BChar( 27 ) + hb_BChar( 116 ) + hb_BChar( 0 )
            CASE hb_LeftEqI( c_part, "/PICTURE" )
               c_ret += ;
                  hb_BChar( 27 ) + hb_BChar( 116 ) + hb_BChar( 1 ) + ;
                  hb_BChar( 27 ) + hb_BChar( 120 ) + hb_BChar( 1 ) + ;
                  hb_BChar( 27 ) + hb_BChar( 107 ) + hb_BChar( 1 ) + ;
                  hb_BChar( 27 ) + hb_BChar( 77 ) + hb_BChar( 27 ) + "U"
            CASE hb_LeftEqI( c_part, "/COND" ) .OR. hb_LeftEqI( c_part, "/SI" )
               c_ret += hb_BChar( 15 )
            CASE hb_LeftEqI( c_part, "/ROMAN" )
               c_ret += hb_BChar( 27 ) + hb_BChar( 107 ) + hb_BChar( 0 )
            CASE hb_LeftEqI( c_part, "/SANS" )
               c_ret += hb_BChar( 27 ) + hb_BChar( 107 ) + hb_BChar( 1 )
            CASE hb_LeftEqI( c_part, "/NLQ" )
               c_ret += hb_BChar( 27 ) + hb_BChar( 120 ) + hb_BChar( 1 )
            CASE hb_LeftEqI( c_part, "/DRAFT" )
               c_ret += hb_BChar( 27 ) + hb_BChar( 120 ) + hb_BChar( 0 )
            CASE hb_LeftEqI( c_part, "/ELITE" )
               c_ret += hb_BChar( 27 ) + hb_BChar( 77 )
            CASE hb_LeftEqI( c_part, "/PICA" )
               c_ret += hb_BChar( 27 ) + hb_BChar( 80 )
            CASE hb_LeftEqI( c_part, "/EMPHOFF" )
               c_ret += hb_BChar( 27 ) + hb_BChar( 70 )
            CASE hb_LeftEqI( c_part, "/EMPH" )
               c_ret += hb_BChar( 27 ) + hb_BChar( 69 )
            CASE hb_LeftEqI( c_part, "/1/6" )
               c_ret += hb_BChar( 27 ) + hb_BChar( 50 )
            CASE hb_LeftEqI( c_part, "/1/8" )
               c_ret += hb_BChar( 27 ) + hb_BChar( 48 )
            CASE hb_LeftEqI( c_part, "/SKIPOFF" )
               c_ret += hb_BChar( 27 ) + hb_BChar( 79 )
            CASE hb_LeftEqI( c_part, "/SKIP" )
               c_ret += hb_BChar( 27 ) + hb_BChar( 78 )
            CASE hb_LeftEqI( c_part, "/FF" ) .OR. hb_LeftEqI( c_part, "/EJECT" )
               c_ret += hb_BChar( 12 )
            CASE hb_LeftEqI( c_part, "/INIT" ) .OR. hb_LeftEqI( c_part, "/RESET" )
               c_ret += hb_BChar( 27 ) + hb_BChar( 64 )
            CASE hb_LeftEqI( c_part, "/SPANISH" )
               c_ret += hb_BChar( 27 ) + hb_BChar( 82 ) + hb_BChar( 12 )
            CASE hb_LeftEqI( c_part, "/USA" )
               c_ret += hb_BChar( 27 ) + hb_BChar( 82 ) + hb_BChar( 0 )
            CASE hb_LeftEqI( c_part, "/ONE" )
               c_ret += hb_BChar( 27 ) + "U" + hb_BChar( 1 )
            CASE hb_LeftEqI( c_part, "/TWO" )
               c_ret += hb_BChar( 27 ) + "U" + hb_BChar( 0 )
            CASE hb_LeftEqI( c_part, "/FAST" )
               c_ret += hb_BChar( 27 ) + "s" + hb_BChar( 0 )
            CASE hb_LeftEqI( c_part, "/SLOW" )
               c_ret += hb_BChar( 27 ) + "s" + hb_BChar( 1 )
            CASE hb_LeftEqI( c_part, "/OFF" )
               c_ret += hb_BChar( 19 )
            CASE hb_LeftEqI( c_part, "/ON" )
               c_ret += hb_BChar( 17 )
            CASE hb_LeftEqI( c_part, "/BEEP" ) .OR. hb_LeftEqI( c_part, "/BELL" )
               c_ret += hb_BChar( 7 )
            CASE hb_LeftEqI( c_part, "/CAN" )
               c_ret += hb_BChar( 24 )
            ENDCASE
         ENDIF
      ENDIF

      c_st := At( ",", SubStr( c_nums, c_st + 1 ) ) + c_st
      c_part := SubStr( c_nums, c_st + 1, At( ",", SubStr( c_nums, c_st + 2 ) ) )

   ENDDO

   RETURN c_ret
