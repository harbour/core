/*
 * Harbour Project source code:
 *    demonstration/test code for GT keyboard input
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#include "inkey.ch"

#ifdef __HARBOUR__
#include "hbgtinfo.ch"
REQUEST HB_CODEPAGE_PLMAZ
REQUEST HB_CODEPAGE_PLISO
REQUEST HB_CODEPAGE_PL852
REQUEST HB_CODEPAGE_PLWIN
REQUEST HB_CODEPAGE_UTF8EX
#else
#include "clipper.ch"
#endif

#ifndef HB_K_RESIZE
#define HB_K_RESIZE 1101
#endif

PROCEDURE Main( cTermCP, cHostCP, lBoxChar )

   LOCAL k, kX, i, s
   LOCAL aKeys := { ;
      { "K_UP",             K_UP,             "Up arrow, Ctrl-E"                }, ;
      { "K_DOWN",           K_DOWN,           "Down arrow, Ctrl-X"              }, ;
      { "K_LEFT",           K_LEFT,           "Left arrow, Ctrl-S"              }, ;
      { "K_RIGHT",          K_RIGHT,          "Right arrow, Ctrl-D"             }, ;
      { "K_HOME",           K_HOME,           "Home, Ctrl-A"                    }, ;
      { "K_END",            K_END,            "End, Ctrl-F"                     }, ;
      { "K_PGUP",           K_PGUP,           "PgUp, Ctrl-R"                    }, ;
      { "K_PGDN",           K_PGDN,           "PgDn, Ctrl-C"                    }, ;
      { "K_CTRL_UP",        K_CTRL_UP,        "Ctrl-Up arrow"                   }, ;
      { "K_CTRL_DOWN",      K_CTRL_DOWN,      "Ctrl-Down arrow"                 }, ;
      { "K_CTRL_LEFT",      K_CTRL_LEFT,      "Ctrl-Left arrow, Ctrl-Z"         }, ;
      { "K_CTRL_RIGHT",     K_CTRL_RIGHT,     "Ctrl-Right arrow, Ctrl-B"        }, ;
      { "K_CTRL_HOME",      K_CTRL_HOME,      "Ctrl-Home, Ctrl-]"               }, ;
      { "K_CTRL_END",       K_CTRL_END,       "Ctrl-End, Ctrl-W"                }, ;
      { "K_CTRL_PGUP",      K_CTRL_PGUP,      "Ctrl-PgUp, Ctrl-Hyphen"          }, ;
      { "K_CTRL_PGDN",      K_CTRL_PGDN,      "Ctrl-PgDn, Ctrl-^"               }, ;
      { "K_ALT_UP",         K_ALT_UP,         "Alt-Up arrow"                    }, ;
      { "K_ALT_DOWN",       K_ALT_DOWN,       "Alt-Down arrow"                  }, ;
      { "K_ALT_LEFT",       K_ALT_LEFT,       "Alt-Left arrow"                  }, ;
      { "K_ALT_RIGHT",      K_ALT_RIGHT,      "Alt-Right arrow"                 }, ;
      { "K_ALT_HOME",       K_ALT_HOME,       "Alt-Home"                        }, ;
      { "K_ALT_END",        K_ALT_END,        "Alt-End"                         }, ;
      { "K_ALT_PGUP",       K_ALT_PGUP,       "Alt-PgUp"                        }, ;
      { "K_ALT_PGDN",       K_ALT_PGDN,       "Alt-PgDn"                        }, ;
      { "K_ENTER",          K_ENTER,          "Enter, Ctrl-M"                   }, ;
      { "K_RETURN",         K_RETURN,         "Return, Ctrl-M"                  }, ;
      { "K_SPACE",          K_SPACE,          "Space bar"                       }, ;
      { "K_ESC",            K_ESC,            "Esc, Ctrl-["                     }, ;
      { "K_CTRL_ENTER",     K_CTRL_ENTER,     "Ctrl-Enter"                      }, ;
      { "K_CTRL_RETURN",    K_CTRL_RETURN,    "Ctrl-Return"                     }, ;
      { "K_CTRL_RET",       K_CTRL_RET,       "Ctrl-Return (Compat.)"           }, ;
      { "K_CTRL_PRTSCR",    K_CTRL_PRTSCR,    "Ctrl-Print Screen"               }, ;
      { "K_ALT_COMMA",      K_ALT_COMMA,      "Alt-,"                           }, ;
      { "K_ALT_PERIOD",     K_ALT_PERIOD,     "Alt-."                           }, ;
      { "K_CTRL_QUESTION",  K_CTRL_QUESTION,  "Ctrl-?, Alt-Slash"               }, ;
      { "K_ALT_SLASH",      K_ALT_SLASH,      "Alt-Slash"                       }, ;
      { "K_ALT_BACKSLASH",  K_ALT_BACKSLASH,  "Alt-Backslash"                   }, ;
      { "K_ALT_ENTER",      K_ALT_ENTER,      "Alt-Enter"                       }, ;
      { "K_ALT_RETURN",     K_ALT_RETURN,     "Alt-Return"                      }, ;
      { "K_ALT_MINUS",      K_ALT_MINUS,      "Alt-Minus"                       }, ;
      { "K_ALT_EQUALS",     K_ALT_EQUALS,     "Alt-Equals"                      }, ;
      { "K_ALT_ESC",        K_ALT_ESC,        "Alt-Esc"                         }, ;
      { "K_ALT_BACKQUOTE",  K_ALT_BACKQUOTE,  "Alt-backquote"                   }, ;
      { "K_ALT_OSB",        K_ALT_OSB,        "Alt-["                           }, ;
      { "K_ALT_CSB",        K_ALT_CSB,        "Alt-]"                           }, ;
      { "K_ALT_SC",         K_ALT_SC,         "Alt-;"                           }, ;
      { "K_ALT_QUOTE",      K_ALT_QUOTE,      "Alt-'"                           }, ;
      { "KP_CENTER",        KP_CENTER,        "Keypad CENTER (5)"               }, ;
      { "KP_ALT_ENTER",     KP_ALT_ENTER,     "Keypad Alt-Enter"                }, ;
      { "KP_CTRL_5",        KP_CTRL_5,        "Keypad Ctrl-5"                   }, ;
      { "KP_CTRL_SLASH",    KP_CTRL_SLASH,    "Keypad Ctrl-/"                   }, ;
      { "KP_CTRL_ASTERISK", KP_CTRL_ASTERISK, "Keypad Ctrl-*"                   }, ;
      { "KP_CTRL_MINUS",    KP_CTRL_MINUS,    "Keypad Ctrl--"                   }, ;
      { "KP_CTRL_PLUS",     KP_CTRL_PLUS,     "Keypad Ctrl-+"                   }, ;
      { "KP_ALT_5",         KP_ALT_5,         "Keypad Alt-5"                    }, ;
      { "KP_ALT_SLASH",     KP_ALT_SLASH,     "Keypad Alt-/"                    }, ;
      { "KP_ALT_ASTERISK",  KP_ALT_ASTERISK,  "Keypad Alt-*"                    }, ;
      { "KP_ALT_MINUS",     KP_ALT_MINUS,     "Keypad Alt--"                    }, ;
      { "KP_ALT_PLUS",      KP_ALT_PLUS,      "Keypad Alt-+"                    }, ;
      { "K_INS",            K_INS,            "Ins, Ctrl-V"                     }, ;
      { "K_DEL",            K_DEL,            "Del, Ctrl-G"                     }, ;
      { "K_BS",             K_BS,             "Backspace, Ctrl-H"               }, ;
      { "K_SH_BS",          K_SH_BS,          "Shift-Backspace, Ctrl-Shift-Tab" }, ;
      { "K_CTRL_SH_TAB",    K_CTRL_SH_TAB,    "Ctrl-Shift-Tab"                  }, ;
      { "K_TAB",            K_TAB,            "Tab, Ctrl-I"                     }, ;
      { "K_SH_TAB",         K_SH_TAB,         "Shift-Tab"                       }, ;
      { "K_CTRL_INS",       K_CTRL_INS,       "Ctrl-Ins"                        }, ;
      { "K_CTRL_DEL",       K_CTRL_DEL,       "Ctrl-Del"                        }, ;
      { "K_CTRL_BS",        K_CTRL_BS,        "Ctrl-Backspace"                  }, ;
      { "K_CTRL_TAB",       K_CTRL_TAB,       "Ctrl-Tab"                        }, ;
      { "K_ALT_INS",        K_ALT_INS,        "Alt-Ins"                         }, ;
      { "K_ALT_DEL",        K_ALT_DEL,        "Alt-Del"                         }, ;
      { "K_ALT_BS",         K_ALT_BS,         "Alt-Backspace"                   }, ;
      { "K_ALT_TAB",        K_ALT_TAB,        "Alt-Tab"                         } }

   AAdd( aKeys, { "K_CTRL_A",        K_CTRL_A,        "Ctrl-A, Home"                     } )
   AAdd( aKeys, { "K_CTRL_B",        K_CTRL_B,        "Ctrl-B, Ctrl-Right arrow"         } )
   AAdd( aKeys, { "K_CTRL_C",        K_CTRL_C,        "Ctrl-C, PgDn, Ctrl-ScrollLock"    } )
   AAdd( aKeys, { "K_CTRL_D",        K_CTRL_D,        "Ctrl-D, Right arrow"              } )
   AAdd( aKeys, { "K_CTRL_E",        K_CTRL_E,        "Ctrl-E, Up arrow"                 } )
   AAdd( aKeys, { "K_CTRL_F",        K_CTRL_F,        "Ctrl-F, End"                      } )
   AAdd( aKeys, { "K_CTRL_G",        K_CTRL_G,        "Ctrl-G, Del"                      } )
   AAdd( aKeys, { "K_CTRL_H",        K_CTRL_H,        "Ctrl-H, Backspace"                } )
   AAdd( aKeys, { "K_CTRL_I",        K_CTRL_I,        "Ctrl-I, Tab"                      } )
   AAdd( aKeys, { "K_CTRL_J",        K_CTRL_J,        "Ctrl-J"                           } )
   AAdd( aKeys, { "K_CTRL_K",        K_CTRL_K,        "Ctrl-K"                           } )
   AAdd( aKeys, { "K_CTRL_L",        K_CTRL_L,        "Ctrl-L"                           } )
   AAdd( aKeys, { "K_CTRL_M",        K_CTRL_M,        "Ctrl-M, Return"                   } )
   AAdd( aKeys, { "K_CTRL_N",        K_CTRL_N,        "Ctrl-N"                           } )
   AAdd( aKeys, { "K_CTRL_O",        K_CTRL_O,        "Ctrl-O"                           } )
   AAdd( aKeys, { "K_CTRL_P",        K_CTRL_P,        "Ctrl-P"                           } )
   AAdd( aKeys, { "K_CTRL_Q",        K_CTRL_Q,        "Ctrl-Q"                           } )
   AAdd( aKeys, { "K_CTRL_R",        K_CTRL_R,        "Ctrl-R, PgUp"                     } )
   AAdd( aKeys, { "K_CTRL_S",        K_CTRL_S,        "Ctrl-S, Left arrow"               } )
   AAdd( aKeys, { "K_CTRL_T",        K_CTRL_T,        "Ctrl-T"                           } )
   AAdd( aKeys, { "K_CTRL_U",        K_CTRL_U,        "Ctrl-U"                           } )
   AAdd( aKeys, { "K_CTRL_V",        K_CTRL_V,        "Ctrl-V, Ins"                      } )
   AAdd( aKeys, { "K_CTRL_W",        K_CTRL_W,        "Ctrl-W, Ctrl-End"                 } )
   AAdd( aKeys, { "K_CTRL_X",        K_CTRL_X,        "Ctrl-X, Down arrow"               } )
   AAdd( aKeys, { "K_CTRL_Y",        K_CTRL_Y,        "Ctrl-Y"                           } )
   AAdd( aKeys, { "K_CTRL_Z",        K_CTRL_Z,        "Ctrl-Z, Ctrl-Left arrow"          } )

   AAdd( aKeys, { "K_ALT_A",         K_ALT_A,         "Alt-A"                            } )
   AAdd( aKeys, { "K_ALT_B",         K_ALT_B,         "Alt-B"                            } )
   AAdd( aKeys, { "K_ALT_C",         K_ALT_C,         "Alt-C"                            } )
   AAdd( aKeys, { "K_ALT_D",         K_ALT_D,         "Alt-D"                            } )
   AAdd( aKeys, { "K_ALT_E",         K_ALT_E,         "Alt-E"                            } )
   AAdd( aKeys, { "K_ALT_F",         K_ALT_F,         "Alt-F"                            } )
   AAdd( aKeys, { "K_ALT_G",         K_ALT_G,         "Alt-G"                            } )
   AAdd( aKeys, { "K_ALT_H",         K_ALT_H,         "Alt-H"                            } )
   AAdd( aKeys, { "K_ALT_I",         K_ALT_I,         "Alt-I"                            } )
   AAdd( aKeys, { "K_ALT_J",         K_ALT_J,         "Alt-J"                            } )
   AAdd( aKeys, { "K_ALT_K",         K_ALT_K,         "Alt-K"                            } )
   AAdd( aKeys, { "K_ALT_L",         K_ALT_L,         "Alt-L"                            } )
   AAdd( aKeys, { "K_ALT_M",         K_ALT_M,         "Alt-M"                            } )
   AAdd( aKeys, { "K_ALT_N",         K_ALT_N,         "Alt-N"                            } )
   AAdd( aKeys, { "K_ALT_O",         K_ALT_O,         "Alt-O"                            } )
   AAdd( aKeys, { "K_ALT_P",         K_ALT_P,         "Alt-P"                            } )
   AAdd( aKeys, { "K_ALT_Q",         K_ALT_Q,         "Alt-Q"                            } )
   AAdd( aKeys, { "K_ALT_R",         K_ALT_R,         "Alt-R"                            } )
   AAdd( aKeys, { "K_ALT_S",         K_ALT_S,         "Alt-S"                            } )
   AAdd( aKeys, { "K_ALT_T",         K_ALT_T,         "Alt-T"                            } )
   AAdd( aKeys, { "K_ALT_U",         K_ALT_U,         "Alt-U"                            } )
   AAdd( aKeys, { "K_ALT_V",         K_ALT_V,         "Alt-V"                            } )
   AAdd( aKeys, { "K_ALT_W",         K_ALT_W,         "Alt-W"                            } )
   AAdd( aKeys, { "K_ALT_X",         K_ALT_X,         "Alt-X"                            } )
   AAdd( aKeys, { "K_ALT_Y",         K_ALT_Y,         "Alt-Y"                            } )
   AAdd( aKeys, { "K_ALT_Z",         K_ALT_Z,         "Alt-Z"                            } )
   AAdd( aKeys, { "K_ALT_1",         K_ALT_1,         "Alt-1"                            } )
   AAdd( aKeys, { "K_ALT_2",         K_ALT_2,         "Alt-2"                            } )
   AAdd( aKeys, { "K_ALT_3",         K_ALT_3,         "Alt-3"                            } )
   AAdd( aKeys, { "K_ALT_4",         K_ALT_4,         "Alt-4"                            } )
   AAdd( aKeys, { "K_ALT_5",         K_ALT_5,         "Alt-5"                            } )
   AAdd( aKeys, { "K_ALT_6",         K_ALT_6,         "Alt-6"                            } )
   AAdd( aKeys, { "K_ALT_7",         K_ALT_7,         "Alt-7"                            } )
   AAdd( aKeys, { "K_ALT_8",         K_ALT_8,         "Alt-8"                            } )
   AAdd( aKeys, { "K_ALT_9",         K_ALT_9,         "Alt-9"                            } )
   AAdd( aKeys, { "K_ALT_0",         K_ALT_0,         "Alt-0"                            } )

   AAdd( aKeys, { "K_F1",            K_F1,            "F1, Ctrl-Backslash"               } )
   AAdd( aKeys, { "K_F2",            K_F2,            "F2"                               } )
   AAdd( aKeys, { "K_F3",            K_F3,            "F3"                               } )
   AAdd( aKeys, { "K_F4",            K_F4,            "F4"                               } )
   AAdd( aKeys, { "K_F5",            K_F5,            "F5"                               } )
   AAdd( aKeys, { "K_F6",            K_F6,            "F6"                               } )
   AAdd( aKeys, { "K_F7",            K_F7,            "F7"                               } )
   AAdd( aKeys, { "K_F8",            K_F8,            "F8"                               } )
   AAdd( aKeys, { "K_F9",            K_F9,            "F9"                               } )
   AAdd( aKeys, { "K_F10",           K_F10,           "F10"                              } )
   AAdd( aKeys, { "K_F11",           K_F11,           "F11"                              } )
   AAdd( aKeys, { "K_F12",           K_F12,           "F12"                              } )
   AAdd( aKeys, { "K_CTRL_F1",       K_CTRL_F1,       "Ctrl-F1"                          } )
   AAdd( aKeys, { "K_CTRL_F2",       K_CTRL_F2,       "Ctrl-F2"                          } )
   AAdd( aKeys, { "K_CTRL_F3",       K_CTRL_F3,       "Ctrl-F4"                          } )
   AAdd( aKeys, { "K_CTRL_F4",       K_CTRL_F4,       "Ctrl-F3"                          } )
   AAdd( aKeys, { "K_CTRL_F5",       K_CTRL_F5,       "Ctrl-F5"                          } )
   AAdd( aKeys, { "K_CTRL_F6",       K_CTRL_F6,       "Ctrl-F6"                          } )
   AAdd( aKeys, { "K_CTRL_F7",       K_CTRL_F7,       "Ctrl-F7"                          } )
   AAdd( aKeys, { "K_CTRL_F8",       K_CTRL_F8,       "Ctrl-F8"                          } )
   AAdd( aKeys, { "K_CTRL_F9",       K_CTRL_F9,       "Ctrl-F9"                          } )
   AAdd( aKeys, { "K_CTRL_F10",      K_CTRL_F10,      "Ctrl-F10"                         } )
   AAdd( aKeys, { "K_CTRL_F11",      K_CTRL_F11,      "Ctrl-F11"                         } )
   AAdd( aKeys, { "K_CTRL_F12",      K_CTRL_F12,      "Ctrl-F12"                         } )
   AAdd( aKeys, { "K_ALT_F1",        K_ALT_F1,        "Alt-F1"                           } )
   AAdd( aKeys, { "K_ALT_F2",        K_ALT_F2,        "Alt-F2"                           } )
   AAdd( aKeys, { "K_ALT_F3",        K_ALT_F3,        "Alt-F3"                           } )
   AAdd( aKeys, { "K_ALT_F4",        K_ALT_F4,        "Alt-F4"                           } )
   AAdd( aKeys, { "K_ALT_F5",        K_ALT_F5,        "Alt-F5"                           } )
   AAdd( aKeys, { "K_ALT_F6",        K_ALT_F6,        "Alt-F6"                           } )
   AAdd( aKeys, { "K_ALT_F7",        K_ALT_F7,        "Alt-F7"                           } )
   AAdd( aKeys, { "K_ALT_F8",        K_ALT_F8,        "Alt-F8"                           } )
   AAdd( aKeys, { "K_ALT_F9",        K_ALT_F9,        "Alt-F9"                           } )
   AAdd( aKeys, { "K_ALT_F10",       K_ALT_F10,       "Alt-F10"                          } )
   AAdd( aKeys, { "K_ALT_F11",       K_ALT_F11,       "Alt-F11"                          } )
   AAdd( aKeys, { "K_ALT_F12",       K_ALT_F12,       "Alt-F12"                          } )
   AAdd( aKeys, { "K_SH_F1",         K_SH_F1,         "Shift-F1"                         } )
   AAdd( aKeys, { "K_SH_F2",         K_SH_F2,         "Shift-F2"                         } )
   AAdd( aKeys, { "K_SH_F3",         K_SH_F3,         "Shift-F3"                         } )
   AAdd( aKeys, { "K_SH_F4",         K_SH_F4,         "Shift-F4"                         } )
   AAdd( aKeys, { "K_SH_F5",         K_SH_F5,         "Shift-F5"                         } )
   AAdd( aKeys, { "K_SH_F6",         K_SH_F6,         "Shift-F6"                         } )
   AAdd( aKeys, { "K_SH_F7",         K_SH_F7,         "Shift-F7"                         } )
   AAdd( aKeys, { "K_SH_F8",         K_SH_F8,         "Shift-F8"                         } )
   AAdd( aKeys, { "K_SH_F9",         K_SH_F9,         "Shift-F9"                         } )
   AAdd( aKeys, { "K_SH_F10",        K_SH_F10,        "Shift-F10"                        } )
   AAdd( aKeys, { "K_SH_F11",        K_SH_F11,        "Shift-F11"                        } )
   AAdd( aKeys, { "K_SH_F12",        K_SH_F12,        "Shift-F12"                        } )

   AAdd( aKeys, { "K_MOUSEMOVE",     K_MOUSEMOVE,     "mouse move"                       } )
   AAdd( aKeys, { "K_LBUTTONDOWN",   K_LBUTTONDOWN,   "mouse left button down"           } )
   AAdd( aKeys, { "K_LBUTTONUP",     K_LBUTTONUP,     "mouse left button up"             } )
   AAdd( aKeys, { "K_RBUTTONDOWN",   K_RBUTTONDOWN,   "mouse right button down"          } )
   AAdd( aKeys, { "K_RBUTTONUP",     K_RBUTTONUP,     "mouse right button up"            } )
   AAdd( aKeys, { "K_LDBLCLK",       K_LDBLCLK,       "mouse left button double click"   } )
   AAdd( aKeys, { "K_RDBLCLK",       K_RDBLCLK,       "mouse right button double click"  } )
   AAdd( aKeys, { "K_MBUTTONDOWN",   K_MBUTTONDOWN,   "mouse middle button down"         } )
   AAdd( aKeys, { "K_MBUTTONUP",     K_MBUTTONUP,     "mouse middle button up"           } )
   AAdd( aKeys, { "K_MDBLCLK",       K_MDBLCLK,       "mouse middle button double click" } )
   AAdd( aKeys, { "K_MMLEFTDOWN",    K_MMLEFTDOWN,    "Mouse Move Left Down"             } )
   AAdd( aKeys, { "K_MMRIGHTDOWN",   K_MMRIGHTDOWN,   "Mouse Move Right Down"            } )
   AAdd( aKeys, { "K_MMMIDDLEDOWN",  K_MMMIDDLEDOWN,  "Mouse Move Middle Down"           } )
   AAdd( aKeys, { "K_MWFORWARD",     K_MWFORWARD,     "Mouse Wheel Forward"              } )
   AAdd( aKeys, { "K_MWBACKWARD",    K_MWBACKWARD,    "Mouse Wheel Backward"             } )
   AAdd( aKeys, { "K_NCMOUSEMOVE",   K_NCMOUSEMOVE,   "Non-Client Area Mouse Movement"   } )

#ifdef __HARBOUR__
   AAdd( aKeys, { "HB_K_RESIZE",     HB_K_RESIZE,     "screen dimension changed"         } )
   AAdd( aKeys, { "HB_K_CLOSE",      HB_K_CLOSE,      "close button hit"                 } )
   AAdd( aKeys, { "HB_K_GOTFOCUS",   HB_K_GOTFOCUS,   "focus restored"                   } )
   AAdd( aKeys, { "HB_K_LOSTFOCUS",  HB_K_LOSTFOCUS,  "focus lost"                       } )
   AAdd( aKeys, { "HB_K_CONNECT",    HB_K_CONNECT,    "remote terminal connected"        } )
   AAdd( aKeys, { "HB_K_DISCONNECT", HB_K_DISCONNECT, "remote terminal disconnected"     } )
#endif

#ifdef __HARBOUR__
   Set( _SET_EVENTMASK, hb_bitOr( HB_INKEY_ALL, HB_INKEY_EXT ) )
   hb_gtInfo( HB_GTI_CURSORBLINKRATE, 1000 )
   hb_gtInfo( HB_GTI_ESCDELAY, 50 )
#ifdef _COMMENT_
   hb_gtinfo( HB_GTI_FONTATTRIBUTE, 0 )
   hb_gtinfo( HB_GTI_FONTATTRIBUTE, hb_bitOr( HB_GTI_FONTA_DRAWBOX, hb_gtinfo( HB_GTI_FONTATTRIBUTE ) ) )
   hb_gtinfo( HB_GTI_FONTATTRIBUTE, hb_bitOr( HB_GTI_FONTA_CTRLCHARS, hb_gtinfo( HB_GTI_FONTATTRIBUTE ) ) )
   hb_gtinfo( HB_GTI_FONTATTRIBUTE, hb_bitOr( HB_GTI_FONTA_FIXMETRIC, hb_gtinfo( HB_GTI_FONTATTRIBUTE ) ) )
   hb_gtinfo( HB_GTI_FONTATTRIBUTE, hb_bitOr( HB_GTI_FONTA_CLRBKG, hb_gtinfo( HB_GTI_FONTATTRIBUTE ) ) )
   hb_gtInfo( HB_GTI_RESIZABLE, .F. )
   hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )
   hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_FONT )
   hb_gtInfo( HB_GTI_MAXIMIZED, .T. )
   hb_gtInfo( HB_GTI_ISFULLSCREEN, .T. )
   hb_gtInfo( HB_GTI_ALTENTER, .T. )
#endif
   hb_gtInfo( HB_GTI_CLOSABLE, .F. )
   hb_gtInfo( HB_GTI_SELECTCOPY, .T. )
   IF Empty( cTermCP )
      cTermCP := "UTF8"
   ELSE
      cTermCP := Upper( cTermCP )
   ENDIF
   IF Empty( cHostCP )
      cHostCP := "UTF8"
   ELSE
      cHostCP := Upper( cHostCP )
   ENDIF
   lBoxChar := ! Empty( lBoxChar )
   hb_cdpSelect( cHostCP )
   hb_SetTermCP( cTermCP, cHostCP, lBoxChar )
#else
#ifdef _SET_EVENTMASK
   Set( _SET_EVENTMASK, INKEY_ALL )
#endif
#endif

   MDblClk( 250 )
   SetCancel( .F. )
#ifdef _COMMENT_
   AltD( 0 )
#endif

   ? OS(), Version(), Date(), Time()
#ifdef __HARBOUR__
   ? hb_gtVersion( 1 ), "GT" + hb_gtVersion()
   ? "Host codpage:", hb_cdpSelect() + ", terminal codepage:", cTermCP
#endif
   ? "@ - interrupt, keycodes checking:"
   ?

   DO WHILE .T.
      kX := Inkey( 0 )
      k := hb_keyStd( kX )
      IF ( i := AScan( aKeys, {| x | x[ 2 ] == k } ) ) != 0
         ? " key:", Str( aKeys[ i ][ 2 ], 7 ), PadR( aKeys[ i ][ 1 ], 18 ) + aKeys[ i ][ 3 ]
      ELSEIF ( k >= 32 .AND. k <= 126 ) .OR. ( k >= 160 .AND. k <= 255 ) .OR. ;
             Len( hb_keyChar( k ) ) > 0
#ifdef __HARBOUR__
         ? "char:", iif( k > 256, " U+" + hb_NumToHex( hb_keyVal( k ), 4 ), Str( k, 7 ) ), ;
           " " + hb_keyChar( k )
#else
         ? "char:", Str( k, 7 ), " " + hb_keyChar( k )
#endif
      ELSE
#ifdef __HARBOUR__
         ? " key:", Str( k, 7 ), "ext: 0x" + hb_NumToHex( kX, 8 ), "->", ;
           hb_NumToHex( hb_keyMod( kX ), 2 ) + ":" + hb_NumToHex( hb_keyVal( kX ), 8 )
#else
         ? " key:", Str( k, 7 )
#endif
      ENDIF
#ifdef _COMMENT_
      ?? "  (" + hb_ntos( MaxRow() ) + ":" + hb_ntos( MaxCol() ) + ")"
#endif

      DO CASE
      CASE k == hb_keyCode( "@" ) .AND. NextKey() == 0
         EXIT
      CASE k == K_INS
         Set( _SET_CURSOR, ( Set( _SET_CURSOR ) + 1 ) % 5 )
         ?? "  cursor:" + hb_ntos( Set( _SET_CURSOR ) )
      CASE k == HB_K_RESIZE
         ?? "  (" + hb_ntos( MaxRow() + 1 ) + "," + hb_ntos( MaxCol() + 1 ) + ")"
      CASE k >= 1000 .AND. k < 1100
         ?? "  mpos(" + hb_ntos( MRow() ) + "," + hb_ntos( MCol() ) + ")"
#ifdef __HARBOUR__
      CASE k == K_CTRL_INS
         IF Alert( "Would you like to show clipboard text?", { "YES", "NO" } ) == 1
            s := hb_gtInfo( HB_GTI_CLIPBOARDDATA )
            ? "Clipboard text: [" + s + "]"
         ENDIF
      CASE k == K_CTRL_END
         IF Alert( "Would you like to set clipboard text?", { "YES", "NO" } ) == 1
            s := hb_TSToStr( hb_DateTime() ) + hb_eol() + ;
               "Harbour GT" + hb_gtVersion() + " clipboard test" + hb_eol()
            ? "New clipboard text: [" + s + "]"
            hb_gtInfo( HB_GTI_CLIPBOARDDATA, s )
         ENDIF
#endif
      ENDCASE
   ENDDO
   ?

   RETURN
