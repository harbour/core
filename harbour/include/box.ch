/*
 * $Id$
 */

#ifndef _BOX_CH
#define _BOX_CH

/* #defines for DISPBOX() */

/* Single-line */
#define B_SINGLE        ( Chr(218) + Chr(196) + Chr(191) + Chr(179) + ;
                          Chr(217) + Chr(196) + Chr(192) + Chr(179) )

/* Double-line */
#define B_DOUBLE        ( Chr(201) + Chr(205) + Chr(187) + Chr(186) + ;
                          Chr(188) + Chr(205) + Chr(200) + Chr(186) )

/* Single-line top, double-line sides */
#define B_SINGLE_DOUBLE ( Chr(214) + Chr(196) + Chr(183) + Chr(186) + ;
                          Chr(189) + Chr(196) + Chr(211) + Chr(186) )

/* Double-line top, single-line sides */
#define B_DOUBLE_SINGLE ( Chr(213) + Chr(205) + Chr(184) + Chr(179) + ;
                          Chr(190) + Chr(205) + Chr(212) + Chr(179) )

#define B_THIN          ( Chr(219) + Chr(223) + Chr(219) + Chr(219) + ;
                          Chr(219) + Chr(220) + Chr(219) + Chr(219) )

#define B_FAT           ( Chr(219) + Chr(219) + Chr(219) + Chr(219) + ;
                          Chr(219) + Chr(219) + Chr(219) + Chr(219) )

#endif /* _BOX_CH */
