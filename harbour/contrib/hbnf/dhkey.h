/*
 * $Id$
 */

/*
 * Author....: Mike Taylor
 * CIS ID....: ?
 *
 * This is an original work by Mike Taylor and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   14 Jun 1991 19:55:20   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:03:04   GLENN
 * Nanforum Toolkit
 *
 *
 */


/* key constants for d_keyin() routine.                          */
/*  some of the values allow for Wordstar-like codes to work for */
/*  cursor movement.  for example ctrl-x is 0x18 and it's mapped */
/*  to the down arrow.                                           */

#define UP    5       /* Ctrl-e */
#define DN    24      /* Ctrl-x */
#define LFT   19      /* Ctrl-s */
#define RGT   4       /* Ctrl-d */
#define CLFT  1       /* Ctrl-a */
#define CRGT  6       /* Ctrl-f */
#define HOME  210
#define ENND  211
#define PGUP  18      /* Ctrl-r */
#define PGDN  3       /* Ctrl-c */
#define INS   22      /* Ctrl-v */
#define DEL   7       /* Ctrl-g */
#define ESC   27      /* escape */
#define RET   13      /* return */
#define TAB   9       /* horiz. tab */
#define BKSP  8       /* backspace  */
#define CTRLA 1       /* Ctrl-a */
#define CTRLB 2       /* Ctrl-b */
#define CTRLC 3       /* Ctrl-c */
#define CTRLD 4       /* Ctrl-d */
#define CTRLE 5       /* Ctrl-e */
#define CTRLF 6       /* Ctrl-f */
#define CTRLG 7       /* Ctrl-g */
#define CTRLH 8       /* Ctrl-h */
#define CTRLI 9       /* Ctrl-i */
#define CTRLJ 10      /* Ctrl-j */
#define CTRLK 11      /* Ctrl-k */
#define CTRLL 12      /* Ctrl-l */
#define CTRLM 13      /* Ctrl-m */
#define CTRLN 14      /* Ctrl-n */
#define CTRLO 15      /* Ctrl-o */
#define CTRLP 16      /* Ctrl-p */
#define CTRLQ 17      /* Ctrl-q */
#define CTRLR 18      /* Ctrl-r */
#define CTRLS 19      /* Ctrl-s */
#define CTRLT 20      /* Ctrl-t */
#define CTRLU 21      /* Ctrl-u */
#define CTRLV 22      /* Ctrl-v */
#define CTRLW 23      /* Ctrl-w */
#define CTRLX 24      /* Ctrl-x */
#define CTRLY 25      /* Ctrl-y */
#define CTRLZ 26      /* Ctrl-z */

/* the following values are mapped into > 128 so that the cannot */
/*  be mistaken for an actual keypress, keyin() performs all of  */
/*  the mapping.                                                 */

#define F0     140
#define F1     141
#define F2     142
#define F3     143
#define F4     144
#define F5     145
#define F6     146
#define F7     147
#define F8     148
#define F9     149
#define AF0    150    /* alt-f0   */
#define AF1    151    /* alt-f1   */
#define AF2    152    /* alt-f2   */
#define AF3    153    /* alt-f3   */
#define AF4    154    /* alt-f4   */
#define AF5    155    /* alt-f5   */
#define AF6    156    /* alt-f6   */
#define AF7    157    /* alt-f7   */
#define AF8    158    /* alt-f8   */
#define AF9    159    /* alt-f9   */
#define CF1    160    /* ctrl-f1  */
#define CF2    161    /* ctrl-f2  */
#define CF3    162    /* ctrl-f3  */
#define CF4    163    /* ctrl-f4  */
#define CF5    164    /* ctrl-f5  */
#define CF6    165    /* ctrl-f6  */
#define CF7    166    /* ctrl-f7  */
#define CF8    167    /* ctrl-f8  */
#define CF9    168    /* ctrl-f9  */
#define CF0    169    /* ctrl-f0  */
#define CENND  170    /* ctrl-end  */
#define CHOME  171    /* ctrl-home */
#define CPGDN  172    /* ctrl-page up     */
#define CPGUP  132    /* ctrl-page down   */
#define CINS   175    /* ctrl-insert key  */
#define ALT1   176    /* alt-1 not keypad */
#define ALT2   177    /* alt-2  "  */
#define ALT3   178    /* alt-3  "  */
#define ALT4   179    /* alt-4  "  */
#define ALT5   180    /* alt-5  "  */
#define ALT6   181    /* alt-6  "  */
#define ALT7   182    /* alt-7  "  */
#define ALT8   183    /* alt-8  "  */
#define ALT9   128    /* alt-9  "  */
#define ALT0   129    /* alt-0  "  */
#define ADASH  130    /* alt-dash  */
#define AEQL   131    /* alt-equal */
#define ALTA   184    /* alt-a  */
#define ALTB   185    /* alt-b  */
#define ALTC   186    /* alt-c  */
#define ALTD   187    /* alt-d  */
#define ALTE   188    /* alt-e  */
#define ALTF   189    /* alt-f  */
#define ALTG   190    /* alt-g  */
#define ALTH   191    /* alt-h  */
#define ALTI   192    /* alt-i  */
#define ALTJ   193    /* alt-j  */
#define ALTK   194    /* alt-k  */
#define ALTL   195    /* alt-l  */
#define ALTM   196    /* alt-m  */
#define ALTN   197    /* alt-n  */
#define ALTO   198    /* alt-o  */
#define ALTP   199    /* alt-p  */
#define ALTQ   200    /* alt-q  */
#define ALTR   201    /* alt-r  */
#define ALTS   202    /* alt-s  */
#define ALTT   203    /* alt-t  */
#define ALTU   204    /* alt-u  */
#define ALTV   205    /* alt-v  */
#define ALTW   206    /* alt-w  */
#define ALTX   207    /* alt-x  */
#define ALTY   208    /* alt-y  */
#define ALTZ   209    /* alt-z  */
