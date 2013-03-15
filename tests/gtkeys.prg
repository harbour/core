/*
 * $Id$
 */

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
#else
#define hb_keyCode( n ) Asc( n )
#define hb_keyChar( c ) Chr( c )
#define hb_ntos( n ) LTrim( Str( n ) )
#endif

PROCEDURE Main( cTermCP, cHostCP, lBoxChar )

   LOCAL k, i, s
   LOCAL aKeys := { ;
      { "K_UP",               5, "Up arrow, Ctrl-E"                }, ;
      { "K_DOWN",            24, "Down arrow, Ctrl-X"              }, ;
      { "K_LEFT",            19, "Left arrow, Ctrl-S"              }, ;
      { "K_RIGHT",            4, "Right arrow, Ctrl-D"             }, ;
      { "K_HOME",             1, "Home, Ctrl-A"                    }, ;
      { "K_END",              6, "End, Ctrl-F"                     }, ;
      { "K_PGUP",            18, "PgUp, Ctrl-R"                    }, ;
      { "K_PGDN",             3, "PgDn, Ctrl-C"                    }, ;
      { "K_CTRL_UP",        397, "Ctrl-Up arrow"                   }, ;
      { "K_CTRL_DOWN",      401, "Ctrl-Down arrow"                 }, ;
      { "K_CTRL_LEFT",       26, "Ctrl-Left arrow, Ctrl-Z"         }, ;
      { "K_CTRL_RIGHT",       2, "Ctrl-Right arrow, Ctrl-B"        }, ;
      { "K_CTRL_HOME",       29, "Ctrl-Home, Ctrl-]"               }, ;
      { "K_CTRL_END",        23, "Ctrl-End, Ctrl-W"                }, ;
      { "K_CTRL_PGUP",       31, "Ctrl-PgUp, Ctrl-Hyphen"          }, ;
      { "K_CTRL_PGDN",       30, "Ctrl-PgDn, Ctrl-^"               }, ;
      { "K_ALT_UP",         408, "Alt-Up arrow"                    }, ;
      { "K_ALT_DOWN",       416, "Alt-Down arrow"                  }, ;
      { "K_ALT_LEFT",       411, "Alt-Left arrow"                  }, ;
      { "K_ALT_RIGHT",      413, "Alt-Right arrow"                 }, ;
      { "K_ALT_HOME",       407, "Alt-Home"                        }, ;
      { "K_ALT_END",        415, "Alt-End"                         }, ;
      { "K_ALT_PGUP",       409, "Alt-PgUp"                        }, ;
      { "K_ALT_PGDN",       417, "Alt-PgDn"                        }, ;
      { "K_ENTER",           13, "Enter, Ctrl-M"                   }, ;
      { "K_RETURN",          13, "Return, Ctrl-M"                  }, ;
      { "K_SPACE",           32, "Space bar"                       }, ;
      { "K_ESC",             27, "Esc, Ctrl-["                     }, ;
      { "K_CTRL_ENTER",      10, "Ctrl-Enter"                      }, ;
      { "K_CTRL_RETURN",     10, "Ctrl-Return"                     }, ;
      { "K_CTRL_RET",        10, "Ctrl-Return (Compat.)"           }, ;
      { "K_CTRL_PRTSCR",    379, "Ctrl-Print Screen"               }, ;
      { "K_CTRL_QUESTION",  309, "Ctrl-?"                          }, ;
      { "K_ALT_ENTER",      284, "Alt-Enter"                       }, ;
      { "K_ALT_RETURN",     284, "Alt-Return"                      }, ;
      { "K_ALT_EQUALS",     387, "Alt-Equals"                      }, ;
      { "K_ALT_ESC",        257, "Alt-Esc"                         }, ;
      { "KP_CENTER",        332, "Keypad CENTER (5)"               }, ;
      { "KP_ALT_ENTER",     422, "Keypad Alt-Enter"                }, ;
      { "KP_CTRL_5",        399, "Keypad Ctrl-5"                   }, ;
      { "KP_CTRL_SLASH",    405, "Keypad Ctrl-/"                   }, ;
      { "KP_CTRL_ASTERISK", 406, "Keypad Ctrl-*"                   }, ;
      { "KP_CTRL_MINUS",    398, "Keypad Ctrl--"                   }, ;
      { "KP_CTRL_PLUS",     400, "Keypad Ctrl-+"                   }, ;
      { "KP_ALT_5",           5, "Keypad Alt-5"                    }, ;
      { "KP_ALT_SLASH",     420, "Keypad Alt-/"                    }, ;
      { "KP_ALT_ASTERISK",  311, "Keypad Alt-*"                    }, ;
      { "KP_ALT_MINUS",     330, "Keypad Alt--"                    }, ;
      { "KP_ALT_PLUS",      334, "Keypad Alt-+"                    }, ;
      { "K_INS",             22, "Ins, Ctrl-V"                     }, ;
      { "K_DEL",              7, "Del, Ctrl-G"                     }, ;
      { "K_BS",               8, "Backspace, Ctrl-H"               }, ;
      { "K_TAB",              9, "Tab, Ctrl-I"                     }, ;
      { "K_SH_TAB",         271, "Shift-Tab"                       }, ;
      { "K_CTRL_INS",       402, "Ctrl-Ins"                        }, ;
      { "K_CTRL_DEL",       403, "Ctrl-Del"                        }, ;
      { "K_CTRL_BS",        127, "Ctrl-Backspace"                  }, ;
      { "K_CTRL_TAB",       404, "Ctrl-Tab"                        }, ;
      { "K_ALT_INS",        418, "Alt-Ins"                         }, ;
      { "K_ALT_DEL",        419, "Alt-Del"                         }, ;
      { "K_ALT_BS",         270, "Alt-Backspace"                   }, ;
      { "K_ALT_TAB",        421, "Alt-Tab"                         }, ;
      { "K_CTRL_A",           1, "Ctrl-A, Home"                    }, ;
      { "K_CTRL_B",           2, "Ctrl-B, Ctrl-Right arrow"        }, ;
      { "K_CTRL_C",           3, "Ctrl-C, PgDn, Ctrl-ScrollLock"   }, ;
      { "K_CTRL_D",           4, "Ctrl-D, Right arrow"             }, ;
      { "K_CTRL_E",           5, "Ctrl-E, Up arrow"                }, ;
      { "K_CTRL_F",           6, "Ctrl-F, End"                     }, ;
      { "K_CTRL_G",           7, "Ctrl-G, Del"                     }, ;
      { "K_CTRL_H",           8, "Ctrl-H, Backspace"               }, ;
      { "K_CTRL_I",           9, "Ctrl-I, Tab"                     }, ;
      { "K_CTRL_J",          10, "Ctrl-J"                          }, ;
      { "K_CTRL_K",          11, "Ctrl-K"                          }, ;
      { "K_CTRL_L",          12, "Ctrl-L"                          }, ;
      { "K_CTRL_M",          13, "Ctrl-M, Return"                  }, ;
      { "K_CTRL_N",          14, "Ctrl-N"                          }, ;
      { "K_CTRL_O",          15, "Ctrl-O"                          }, ;
      { "K_CTRL_P",          16, "Ctrl-P"                          }, ;
      { "K_CTRL_Q",          17, "Ctrl-Q"                          }, ;
      { "K_CTRL_R",          18, "Ctrl-R, PgUp"                    }, ;
      { "K_CTRL_S",          19, "Ctrl-S, Left arrow"              }, ;
      { "K_CTRL_T",          20, "Ctrl-T"                          }, ;
      { "K_CTRL_U",          21, "Ctrl-U"                          }, ;
      { "K_CTRL_V",          22, "Ctrl-V, Ins"                     }, ;
      { "K_CTRL_W",          23, "Ctrl-W, Ctrl-End"                }, ;
      { "K_CTRL_X",          24, "Ctrl-X, Down arrow"              }, ;
      { "K_CTRL_Y",          25, "Ctrl-Y"                          }, ;
      { "K_CTRL_Z",          26, "Ctrl-Z, Ctrl-Left arrow"         } }

   AAdd( aKeys, { "K_ALT_A",          286, "Alt-A"                          }  )
   AAdd( aKeys, { "K_ALT_B",          304, "Alt-B"                          }  )
   AAdd( aKeys, { "K_ALT_C",          302, "Alt-C"                          }  )
   AAdd( aKeys, { "K_ALT_D",          288, "Alt-D"                          }  )
   AAdd( aKeys, { "K_ALT_E",          274, "Alt-E"                          }  )
   AAdd( aKeys, { "K_ALT_F",          289, "Alt-F"                          }  )
   AAdd( aKeys, { "K_ALT_G",          290, "Alt-G"                          }  )
   AAdd( aKeys, { "K_ALT_H",          291, "Alt-H"                          }  )
   AAdd( aKeys, { "K_ALT_I",          279, "Alt-I"                          }  )
   AAdd( aKeys, { "K_ALT_J",          292, "Alt-J"                          }  )
   AAdd( aKeys, { "K_ALT_K",          293, "Alt-K"                          }  )
   AAdd( aKeys, { "K_ALT_L",          294, "Alt-L"                          }  )
   AAdd( aKeys, { "K_ALT_M",          306, "Alt-M"                          }  )
   AAdd( aKeys, { "K_ALT_N",          305, "Alt-N"                          }  )
   AAdd( aKeys, { "K_ALT_O",          280, "Alt-O"                          }  )
   AAdd( aKeys, { "K_ALT_P",          281, "Alt-P"                          }  )
   AAdd( aKeys, { "K_ALT_Q",          272, "Alt-Q"                          }  )
   AAdd( aKeys, { "K_ALT_R",          275, "Alt-R"                          }  )
   AAdd( aKeys, { "K_ALT_S",          287, "Alt-S"                          }  )
   AAdd( aKeys, { "K_ALT_T",          276, "Alt-T"                          }  )
   AAdd( aKeys, { "K_ALT_U",          278, "Alt-U"                          }  )
   AAdd( aKeys, { "K_ALT_V",          303, "Alt-V"                          }  )
   AAdd( aKeys, { "K_ALT_W",          273, "Alt-W"                          }  )
   AAdd( aKeys, { "K_ALT_X",          301, "Alt-X"                          }  )
   AAdd( aKeys, { "K_ALT_Y",          277, "Alt-Y"                          }  )
   AAdd( aKeys, { "K_ALT_Z",          300, "Alt-Z"                          }  )
   AAdd( aKeys, { "K_ALT_1",          376, "Alt-1"                          }  )
   AAdd( aKeys, { "K_ALT_2",          377, "Alt-2"                          }  )
   AAdd( aKeys, { "K_ALT_3",          378, "Alt-3"                          }  )
   AAdd( aKeys, { "K_ALT_4",          379, "Alt-4"                          }  )
   AAdd( aKeys, { "K_ALT_5",          380, "Alt-5"                          }  )
   AAdd( aKeys, { "K_ALT_6",          381, "Alt-6"                          }  )
   AAdd( aKeys, { "K_ALT_7",          382, "Alt-7"                          }  )
   AAdd( aKeys, { "K_ALT_8",          383, "Alt-8"                          }  )
   AAdd( aKeys, { "K_ALT_9",          384, "Alt-9"                          }  )
   AAdd( aKeys, { "K_ALT_0",          385, "Alt-0"                          }  )
   AAdd( aKeys, { "K_F1",              28, "F1, Ctrl-Backslash"             }  )
   AAdd( aKeys, { "K_F2",              - 1, "F2"                             }  )
   AAdd( aKeys, { "K_F3",              - 2, "F3"                             }  )
   AAdd( aKeys, { "K_F4",              - 3, "F4"                             }  )
   AAdd( aKeys, { "K_F5",              - 4, "F5"                             }  )
   AAdd( aKeys, { "K_F6",              - 5, "F6"                             }  )
   AAdd( aKeys, { "K_F7",              - 6, "F7"                             }  )
   AAdd( aKeys, { "K_F8",              - 7, "F8"                             }  )
   AAdd( aKeys, { "K_F9",              - 8, "F9"                             }  )
   AAdd( aKeys, { "K_F10",             - 9, "F10"                            }  )
   AAdd( aKeys, { "K_F11",            - 40, "F11"                            }  )
   AAdd( aKeys, { "K_F12",            - 41, "F12"                            }  )
   AAdd( aKeys, { "K_CTRL_F1",        - 20, "Ctrl-F1"                        }  )
   AAdd( aKeys, { "K_CTRL_F2",        - 21, "Ctrl-F2"                        }  )
   AAdd( aKeys, { "K_CTRL_F3",        - 22, "Ctrl-F4"                        }  )
   AAdd( aKeys, { "K_CTRL_F4",        - 23, "Ctrl-F3"                        }  )
   AAdd( aKeys, { "K_CTRL_F5",        - 24, "Ctrl-F5"                        }  )
   AAdd( aKeys, { "K_CTRL_F6",        - 25, "Ctrl-F6"                        }  )
   AAdd( aKeys, { "K_CTRL_F7",        - 26, "Ctrl-F7"                        }  )
   AAdd( aKeys, { "K_CTRL_F8",        - 27, "Ctrl-F8"                        }  )
   AAdd( aKeys, { "K_CTRL_F9",        - 28, "Ctrl-F9"                        }  )
   AAdd( aKeys, { "K_CTRL_F10",       - 29, "Ctrl-F10"                       }  )
   AAdd( aKeys, { "K_CTRL_F11",       - 44, "Ctrl-F11"                       }  )
   AAdd( aKeys, { "K_CTRL_F12",       - 45, "Ctrl-F12"                       }  )
   AAdd( aKeys, { "K_ALT_F1",         - 30, "Alt-F1"                         }  )
   AAdd( aKeys, { "K_ALT_F2",         - 31, "Alt-F2"                         }  )
   AAdd( aKeys, { "K_ALT_F3",         - 32, "Alt-F3"                         }  )
   AAdd( aKeys, { "K_ALT_F4",         - 33, "Alt-F4"                         }  )
   AAdd( aKeys, { "K_ALT_F5",         - 34, "Alt-F5"                         }  )
   AAdd( aKeys, { "K_ALT_F6",         - 35, "Alt-F6"                         }  )
   AAdd( aKeys, { "K_ALT_F7",         - 36, "Alt-F7"                         }  )
   AAdd( aKeys, { "K_ALT_F8",         - 37, "Alt-F8"                         }  )
   AAdd( aKeys, { "K_ALT_F9",         - 38, "Alt-F9"                         }  )
   AAdd( aKeys, { "K_ALT_F10",        - 39, "Alt-F10"                        }  )
   AAdd( aKeys, { "K_ALT_F11",        - 46, "Alt-F11"                        }  )
   AAdd( aKeys, { "K_ALT_F12",        - 47, "Alt-F12"                        }  )
   AAdd( aKeys, { "K_SH_F1",          - 10, "Shift-F1"                       }  )
   AAdd( aKeys, { "K_SH_F2",          - 11, "Shift-F2"                       }  )
   AAdd( aKeys, { "K_SH_F3",          - 12, "Shift-F3"                       }  )
   AAdd( aKeys, { "K_SH_F4",          - 13, "Shift-F4"                       }  )
   AAdd( aKeys, { "K_SH_F5",          - 14, "Shift-F5"                       }  )
   AAdd( aKeys, { "K_SH_F6",          - 15, "Shift-F6"                       }  )
   AAdd( aKeys, { "K_SH_F7",          - 16, "Shift-F7"                       }  )
   AAdd( aKeys, { "K_SH_F8",          - 17, "Shift-F8"                       }  )
   AAdd( aKeys, { "K_SH_F9",          - 18, "Shift-F9"                       }  )
   AAdd( aKeys, { "K_SH_F10",         - 19, "Shift-F10"                      }  )
   AAdd( aKeys, { "K_SH_F11",         - 42, "Shift-F11"                      }  )
   AAdd( aKeys, { "K_SH_F12",         - 43, "Shift-F12"                      }  )
   AAdd( aKeys, { "K_MOUSEMOVE",     1001, "mouse move"                     }  )
   AAdd( aKeys, { "K_LBUTTONDOWN",   1002, "mouse left button down"         }  )
   AAdd( aKeys, { "K_LBUTTONUP",     1003, "mouse left button up"           }  )
   AAdd( aKeys, { "K_RBUTTONDOWN",   1004, "mouse right button down"        }  )
   AAdd( aKeys, { "K_RBUTTONUP",     1005, "mouse right button up"          }  )
   AAdd( aKeys, { "K_LDBLCLK",       1006, "mouse left button double click" }  )
   AAdd( aKeys, { "K_RDBLCLK",       1007, "mouse right button double click" }  )
   AAdd( aKeys, { "K_MBUTTONDOWN",   1008, "mouse middle button down"       }  )
   AAdd( aKeys, { "K_MBUTTONUP",     1009, "mouse middle button up"         }  )
   AAdd( aKeys, { "K_MDBLCLK",       1010, "mouse middle button double click" }  )
   AAdd( aKeys, { "K_MMLEFTDOWN",    1011, "Mouse Move Left Down"           }  )
   AAdd( aKeys, { "K_MMRIGHTDOWN",   1012, "Mouse Move Right Down"          }  )
   AAdd( aKeys, { "K_MMMIDDLEDOWN",  1013, "Mouse Move Middle Down"         }  )
   AAdd( aKeys, { "K_MWFORWARD",     1014, "Mouse Wheel Forward"            }  )
   AAdd( aKeys, { "K_MWBACKWARD",    1015, "Mouse Wheel Backward"           }  )
   AAdd( aKeys, { "K_NCMOUSEMOVE",   1016, "Non-Client Area Mouse Movement" }  )
   AAdd( aKeys, { "HB_K_RESIZE",     1101, "screen dimension changed"       }  )
   AAdd( aKeys, { "HB_K_CLOSE",      1102, "close button hit"               }  )
   AAdd( aKeys, { "HB_K_GETFOCUS",   1103, "focus restored"                 }  )
   AAdd( aKeys, { "HB_K_LOSTFOCUS",  1104, "focus lost"                     }  )
   AAdd( aKeys, { "HB_K_CONNECT",    1105, "remote terminal connected"      }  )
   AAdd( aKeys, { "HB_K_DISCONNECT", 1106, "remote terminal disconnected"   }  )


#ifdef __HARBOUR__
   Set( _SET_EVENTMASK, HB_INKEY_ALL )
   // hb_gtInfo( HB_GTI_RESIZABLE, .F. )
   // hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )
   // hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_FONT )
   hb_gtInfo( HB_GTI_ISFULLSCREEN, .T. )
   hb_gtInfo( HB_GTI_ALTENTER, .T. )
   hb_gtInfo( HB_GTI_CLOSABLE, .F. )
   hb_gtInfo( HB_GTI_ESCDELAY, 50 )
   IF Empty( cTermCP )
      cTermCP := "PLISO"
   ELSE
      cTermCP := Upper( cTermCP )
   ENDIF
   IF Empty( cHostCP )
      cHostCP := "PLMAZ"
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
   // AltD(0)

   ? OS(), Version(), Date(), Time()
#ifdef __HARBOUR__
   ? hb_gtVersion( 1 ), "GT" + hb_gtVersion()
   ? "Host codpage: " + hb_cdpSelect() + ", terminal codepage: " + cTermCP
#endif
   ? "@ - interrupt, keycodes checking: "
   ?

   WHILE .T.
      k := Inkey( 0 )
      IF ( i := AScan( aKeys, {| x | x[ 2 ] == k } ) ) != 0
         ? " key:" + Str( aKeys[ i, 2 ], 7 ) + "  " + PadR( aKeys[ i, 1 ], 18 ) + aKeys[ i, 3 ]
      ELSEIF k >= 32 .AND. k <= 126 .OR. ( k >= 160 .AND. k <= 255 ) .OR. ;
            ( k >= 0 .AND. k <= 255 .AND. IsAlpha( hb_keyChar( k ) ) )
         ? "char:" + Str( k, 7 ) + "  " + hb_keyChar( k )
      ELSE
         ? " key:" + Str( k, 7 )
      ENDIF
//    ?? "  (" + hb_ntos( MaxRow() ) + ":" + hb_ntos( MaxCol() ) + ")"

      IF k == hb_keyCode( "@" ) .AND. NextKey() == 0
         EXIT
#ifdef __HARBOUR__
      ELSEIF k == K_CTRL_INS
         IF Alert( "Would you like to show clipboard text?", { "YES", "NO" } ) == 1
            s := hb_gtInfo( HB_GTI_CLIPBOARDDATA )
            ? "Clipboard text: [" + s + "]"
         ENDIF
      ELSEIF k == K_CTRL_END
         IF Alert( "Would you like to set clipboard text?", { "YES", "NO" } ) == 1
            s := hb_TSToStr( hb_DateTime() ) + hb_eol() + ;
               "Harbour GT" + hb_gtVersion() + " clipboard test" + hb_eol()
            ? "New clipboard text: [" + s + "]"
            hb_gtInfo( HB_GTI_CLIPBOARDDATA, s )
         ENDIF
#endif
      ENDIF
   ENDDO
   ?

   RETURN
