/*
 * $Id$
 */

// Input event masks

#define INKEY_MOVE      1
#define INKEY_LDOWN     2
#define INKEY_LUP       4
#define INKEY_RDOWN     8
#define INKEY_RUP       16
#define INKEY_KEYBOARD  128
#define INKEY_ALL       159
#define INKEY_EXTENDED  256

// Mouse events

#define K_MOUSEMOVE     1001
#define K_LBUTTONDOWN   1002
#define K_LBUTTONUP     1003
#define K_RBUTTONDOWN   1004
#define K_RBUTTONUP     1005
#define K_LDBLCLK       1006
#define K_RDBLCLK       1007

// Special keyboard keys

#define K_ESC           27

#define K_ALT_A         286   //   Alt-A
#define K_ALT_B         304   //   Alt-B
#define K_ALT_C         302   //   Alt-C
#define K_ALT_D         288   //   Alt-D
#define K_ALT_E         274   //   Alt-E
#define K_ALT_F         289   //   Alt-F
#define K_ALT_G         290   //   Alt-G
#define K_ALT_H         291   //   Alt-H
#define K_ALT_I         279   //   Alt-I
#define K_ALT_J         292   //   Alt-J
#define K_ALT_K         293   //   Alt-K
#define K_ALT_L         294   //   Alt-L
#define K_ALT_M         306   //   Alt-M
#define K_ALT_N         305   //   Alt-N
#define K_ALT_O         280   //   Alt-O
#define K_ALT_P         281   //   Alt-P
#define K_ALT_Q         272   //   Alt-Q
#define K_ALT_R         275   //   Alt-R
#define K_ALT_S         287   //   Alt-S
#define K_ALT_T         276   //   Alt-T
#define K_ALT_U         278   //   Alt-U
#define K_ALT_V         303   //   Alt-V
#define K_ALT_W         273   //   Alt-W
#define K_ALT_X         301   //   Alt-X
#define K_ALT_Y         277   //   Alt-Y
#define K_ALT_Z         300   //   Alt-Z
#define K_ALT_1         376   //   Alt-1
#define K_ALT_2         377   //   Alt-2
#define K_ALT_3         378   //   Alt-3
#define K_ALT_4         379   //   Alt-4
#define K_ALT_5         380   //   Alt-5
#define K_ALT_6         381   //   Alt-6
#define K_ALT_7         382   //   Alt-7
#define K_ALT_8         383   //   Alt-8
#define K_ALT_9         384   //   Alt-9
#define K_ALT_0         385   //   Alt-0

// Cursor movement keys

#define K_UP                5   //   Up arrow, Ctrl-E
#define K_DOWN             24   //   Down arrow, Ctrl-X
#define K_LEFT             19   //   Left arrow, Ctrl-S
#define K_RIGHT             4   //   Right arrow, Ctrl-D
#define K_HOME              1   //   Home, Ctrl-A
#define K_END               6   //   End, Ctrl-F
#define K_PGUP             18   //   PgUp, Ctrl-R
#define K_PGDN              3   //   PgDn, Ctrl-C

#define K_CTRL_UP         397   // * Ctrl-Up arrow
#define K_CTRL_DOWN       401   // * Ctrl-Down arrow
#define K_CTRL_LEFT        26   //   Ctrl-Left arrow, Ctrl-Z
#define K_CTRL_RIGHT        2   //   Ctrl-Right arrow, Ctrl-B
#define K_CTRL_HOME        29   //   Ctrl-Home, Ctrl-]
#define K_CTRL_END         23   //   Ctrl-End, Ctrl-W
#define K_CTRL_PGUP        31   //   Ctrl-PgUp, Ctrl-Hyphen
#define K_CTRL_PGDN        30   //   Ctrl-PgDn, Ctrl-^

#define K_ALT_UP          408   // * Alt-Up arrow
#define K_ALT_DOWN        416   // * Alt-Down arrow
#define K_ALT_LEFT        411   // * Alt-Left arrow
#define K_ALT_RIGHT       413   // * Alt-Right arrow
#define K_ALT_HOME        407   // * Alt-Home
#define K_ALT_END         415   // * Alt-End
#define K_ALT_PGUP        409   // * Alt-PgUp
#define K_ALT_PGDN        417   // * Alt-PgDn



// Misc. keys

#define K_ENTER            13   //   Enter, Ctrl-M
#define K_INTRO            13   //
#define K_RETURN           13   //   Return, Ctrl-M
#define K_SPACE            32   //   Space bar
#define K_ESC              27   //   Esc, Ctrl-[

#define K_CTRL_ENTER       10   //   Ctrl-Enter
#define K_CTRL_RETURN      10   //   Ctrl-Return
#define K_CTRL_RET         10   //   Ctrl-Return (Compat.)
#define K_CTRL_PRTSCR     379   // * Ctrl-Print Screen
#define K_CTRL_QUESTION   309   //   Ctrl-?

#define K_ALT_ENTER       284   // * Alt-Enter
#define K_ALT_RETURN      284   // * Alt-Return
#define K_ALT_EQUALS      387   // * Alt-Equals
#define K_ALT_ESC         257   // * Alt-Esc


// Keypad keys

#define KP_ALT_ENTER      422   // * Keypad Alt-Enter

#define KP_CTRL_5         399   // * Keypad Ctrl-5
#define KP_CTRL_SLASH     405   // * Keypad Ctrl-/
#define KP_CTRL_ASTERISK  406   // * Keypad Ctrl-*
#define KP_CTRL_MINUS     398   // * Keypad Ctrl--
#define KP_CTRL_PLUS      400   // * Keypad Ctrl-+

#define KP_ALT_5            5   // * Keypad Alt-5
#define KP_ALT_SLASH      420   // * Keypad Alt-/
#define KP_ALT_ASTERISK   311   // * Keypad Alt-*
#define KP_ALT_MINUS      330   // * Keypad Alt--
#define KP_ALT_PLUS       334   // * Keypad Alt-+


// Editing keys

#define K_INS              22   //   Ins, Ctrl-V
#define K_DEL               7   //   Del, Ctrl-G
#define K_BS                8   //   Backspace, Ctrl-H
#define K_TAB               9   //   Tab, Ctrl-I
#define K_SH_TAB          271   //   Shift-Tab

#define K_CTRL_INS        402   // * Ctrl-Ins
#define K_CTRL_DEL        403   // * Ctrl-Del
#define K_CTRL_BS         127   //   Ctrl-Backspace
#define K_CTRL_TAB        404   // * Ctrl-Tab

#define K_ALT_INS         418   // * Alt-Ins
#define K_ALT_DEL         419   // * Alt-Del
#define K_ALT_BS          270   // * Alt-Backspace
#define K_ALT_TAB         421   // * Alt-Tab


// Control keys

#define K_CTRL_A        1    //   Ctrl-A, Home
#define K_CTRL_B        2    //   Ctrl-B, Ctrl-Right arrow
#define K_CTRL_C        3    //   Ctrl-C, PgDn, Ctrl-ScrollLock
#define K_CTRL_D        4    //   Ctrl-D, Right arrow
#define K_CTRL_E        5    //   Ctrl-E, Up arrow
#define K_CTRL_F        6    //   Ctrl-F, End
#define K_CTRL_G        7    //   Ctrl-G, Del
#define K_CTRL_H        8    //   Ctrl-H, Backspace
#define K_CTRL_I        9    //   Ctrl-I, Tab
#define K_CTRL_J       10   //   Ctrl-J
#define K_CTRL_K       11   //   Ctrl-K
#define K_CTRL_L       12   //   Ctrl-L
#define K_CTRL_M       13   //   Ctrl-M, Return
#define K_CTRL_N       14   //   Ctrl-N
#define K_CTRL_O       15   //   Ctrl-O
#define K_CTRL_P       16   //   Ctrl-P
#define K_CTRL_Q       17   //   Ctrl-Q
#define K_CTRL_R       18   //   Ctrl-R, PgUp
#define K_CTRL_S       19   //   Ctrl-S, Left arrow
#define K_CTRL_T       20   //   Ctrl-T
#define K_CTRL_U       21   //   Ctrl-U
#define K_CTRL_V       22   //   Ctrl-V, Ins
#define K_CTRL_W       23   //   Ctrl-W, Ctrl-End
#define K_CTRL_X       24   //   Ctrl-X, Down arrow
#define K_CTRL_Y       25   //   Ctrl-Y
#define K_CTRL_Z       26   //   Ctrl-Z, Ctrl-Left arrow


