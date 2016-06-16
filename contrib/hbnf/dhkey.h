/* This is an original work by Mike Taylor and is placed in the public domain.

      Rev 1.1   14 Jun 1991 19:55:20   GLENN
   Minor edit to file header

      Rev 1.0   01 Apr 1991 01:03:04   GLENN
   Nanforum Toolkit
 */

/* Key constants for d_keyin() routine.
   Some of the values allow for Wordstar-like codes to work for
   cursor movement.  for example ctrl-x is 0x18 and it's mapped
   to the down arrow. */

#define UP     5    /* Ctrl-e */
#define DN     24   /* Ctrl-x */
#define LFT    19   /* Ctrl-s */
#define RGT    4    /* Ctrl-d */
#define CLFT   1    /* Ctrl-a */
#define CRGT   6    /* Ctrl-f */
#define HOME   210
#define ENND   211
#define PGUP   18   /* Ctrl-r */
#define PGDN   3    /* Ctrl-c */
#define INS    22   /* Ctrl-v */
#define DEL    7    /* Ctrl-g */
#define ESC    27   /* escape */
#define RET    13   /* return */
#define TAB    9    /* horiz. tab */
#define BKSP   8    /* backspace */
#define CTRLA  1    /* Ctrl-a */
#define CTRLB  2    /* Ctrl-b */
#define CTRLC  3    /* Ctrl-c */
#define CTRLD  4    /* Ctrl-d */
#define CTRLE  5    /* Ctrl-e */
#define CTRLF  6    /* Ctrl-f */
#define CTRLG  7    /* Ctrl-g */
#define CTRLH  8    /* Ctrl-h */
#define CTRLI  9    /* Ctrl-i */
#define CTRLJ  10   /* Ctrl-j */
#define CTRLK  11   /* Ctrl-k */
#define CTRLL  12   /* Ctrl-l */
#define CTRLM  13   /* Ctrl-m */
#define CTRLN  14   /* Ctrl-n */
#define CTRLO  15   /* Ctrl-o */
#define CTRLP  16   /* Ctrl-p */
#define CTRLQ  17   /* Ctrl-q */
#define CTRLR  18   /* Ctrl-r */
#define CTRLS  19   /* Ctrl-s */
#define CTRLT  20   /* Ctrl-t */
#define CTRLU  21   /* Ctrl-u */
#define CTRLV  22   /* Ctrl-v */
#define CTRLW  23   /* Ctrl-w */
#define CTRLX  24   /* Ctrl-x */
#define CTRLY  25   /* Ctrl-y */
#define CTRLZ  26   /* Ctrl-z */

/* The following values are mapped into > 128 so that the cannot
   be mistaken for an actual keypress, keyin() performs all of
   the mapping. */

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
#define AF0    150  /* Alt-F0 */
#define AF1    151  /* Alt-F1 */
#define AF2    152  /* Alt-F2 */
#define AF3    153  /* Alt-F3 */
#define AF4    154  /* Alt-F4 */
#define AF5    155  /* Alt-F5 */
#define AF6    156  /* Alt-F6 */
#define AF7    157  /* Alt-F7 */
#define AF8    158  /* Alt-F8 */
#define AF9    159  /* Alt-F9 */
#define CF1    160  /* Ctrl-F1 */
#define CF2    161  /* Ctrl-F2 */
#define CF3    162  /* Ctrl-F3 */
#define CF4    163  /* Ctrl-F4 */
#define CF5    164  /* Ctrl-F5 */
#define CF6    165  /* Ctrl-F6 */
#define CF7    166  /* Ctrl-F7 */
#define CF8    167  /* Ctrl-F8 */
#define CF9    168  /* Ctrl-F9 */
#define CF0    169  /* Ctrl-F0 */
#define CENND  170  /* Ctrl-End */
#define CHOME  171  /* Ctrl-Home */
#define CPGDN  172  /* Ctrl-PgUp */
#define CPGUP  132  /* Ctrl-PgDn */
#define CINS   175  /* Ctrl-Ins */
#define ALT1   176  /* Alt-1 not keypad */
#define ALT2   177  /* Alt-2  " */
#define ALT3   178  /* Alt-3  " */
#define ALT4   179  /* Alt-4  " */
#define ALT5   180  /* Alt-5  " */
#define ALT6   181  /* Alt-6  " */
#define ALT7   182  /* Alt-7  " */
#define ALT8   183  /* Alt-8  " */
#define ALT9   128  /* Alt-9  " */
#define ALT0   129  /* Alt-0  " */
#define ADASH  130  /* Alt-Dash */
#define AEQL   131  /* Alt-Equal */
#define ALTA   184  /* Alt-a */
#define ALTB   185  /* Alt-b */
#define ALTC   186  /* Alt-c */
#define ALTD   187  /* Alt-d */
#define ALTE   188  /* Alt-e */
#define ALTF   189  /* Alt-f */
#define ALTG   190  /* Alt-g */
#define ALTH   191  /* Alt-h */
#define ALTI   192  /* Alt-i */
#define ALTJ   193  /* Alt-j */
#define ALTK   194  /* Alt-k */
#define ALTL   195  /* Alt-l */
#define ALTM   196  /* Alt-m */
#define ALTN   197  /* Alt-n */
#define ALTO   198  /* Alt-o */
#define ALTP   199  /* Alt-p */
#define ALTQ   200  /* Alt-q */
#define ALTR   201  /* Alt-r */
#define ALTS   202  /* Alt-s */
#define ALTT   203  /* Alt-t */
#define ALTU   204  /* Alt-u */
#define ALTV   205  /* Alt-v */
#define ALTW   206  /* Alt-w */
#define ALTX   207  /* Alt-x */
#define ALTY   208  /* Alt-y */
#define ALTZ   209  /* Alt-z */
