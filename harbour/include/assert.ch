/*
 * $Id$
 */

#ifndef _ASSERT_CH
#define _ASSERT_CH

#ifdef NDEBUG

/* When NDEBUG is definded, ignore all ASSERT() calls */
#command ASSERT( <exp> [, <msg>] )      =>

#else

#command ASSERT( <exp> [, <msg>] )      =>                              ;
         IF ( !(<exp>) )                                                ;
       ;   OUTSTD(                                                      ;
                   CHR(13) + CHR(10) + PROCNAME(0) +                    ;
                   "(" + LTRIM(STR(PROCLINE())) + ")" +                 ;
                   "  Assertion failed: " +                             ;
                   IF( <.msg.>, <msg>, <"exp"> )                        ;
                 )                                                      ;
       ;   QUIT                                                         ;
       ; ENDIF

#endif /* NDEBUG */

#endif /* _ASSERT_CH */

