/*
 * $Id$
 */

#ifndef _XBPDEV_CH

/*----------------------------------------------------------------------*/

#define XBPPRN_STATUS_PAUSED                      0
#define XBPPRN_STATUS_READY                       1
#define XBPPRN_STATUS_BUSY                        ( -1 )
#define XBPPRN_STATUS_SERVICE                     ( -2 )
#define XBPPRN_STATUS_NOPAPER                     ( -3 )
#define XBPPRN_STATUS_NOT_READY                   ( -4 )
#define XBPPRN_STATUS_NA                          ( -5 )
#define XBPPRN_STATUS_ERROR                       ( -6 )
#define XBPPRN_STATUS_OFFLINE                     ( -7 )

#define XBPPRN_RESOLUTION_DRAFT                   ( -1 )
#define XBPPRN_RESOLUTION_LOW                     ( -2 )
#define XBPPRN_RESOLUTION_MEDIUM                  ( -3 )
#define XBPPRN_RESOLUTION_HIGH                    ( -4 )

#define XBPPRN_ORIENT_PORTRAIT                    1
#define XBPPRN_ORIENT_LANDSCAPE                   2

#define XBPPRN_FORM_LETTER                        1
#define XBPPRN_FORM_LETTERSMALL                   2
#define XBPPRN_FORM_TABLOID                       3
#define XBPPRN_FORM_LEDGER                        4
#define XBPPRN_FORM_LEGAL                         5
#define XBPPRN_FORM_STATEMENT                     6
#define XBPPRN_FORM_EXECUTIVE                     7
#define XBPPRN_FORM_A3                            8
#define XBPPRN_FORM_A4                            9
#define XBPPRN_FORM_A4SMALL                       10
#define XBPPRN_FORM_A5                            11
#define XBPPRN_FORM_B4                            12
#define XBPPRN_FORM_B5                            13
#define XBPPRN_FORM_FOLIO                         14
#define XBPPRN_FORM_QUARTO                        15
#define XBPPRN_FORM_10X14                         16
#define XBPPRN_FORM_11X17                         17
#define XBPPRN_FORM_NOTE                          18
#define XBPPRN_FORM_ENVELOPE_9                    19
#define XBPPRN_FORM_ENVELOPE_10                   20
#define XBPPRN_FORM_ENVELOPE_11                   21
#define XBPPRN_FORM_ENVELOPE_12                   22
#define XBPPRN_FORM_ENVELOPE_14                   23
#define XBPPRN_FORM_CSHEET                        24
#define XBPPRN_FORM_DSHEET                        25
#define XBPPRN_FORM_ESHEET                        26
#define XBPPRN_FORM_ENVELOPE_DL                   27
#define XBPPRN_FORM_ENVELOPE_C5                   28
#define XBPPRN_FORM_ENVELOPE_C3                   29
#define XBPPRN_FORM_ENVELOPE_C4                   30
#define XBPPRN_FORM_ENVELOPE_C6                   31
#define XBPPRN_FORM_ENVELOPE_C65                  32
#define XBPPRN_FORM_ENVELOPE_B4                   33
#define XBPPRN_FORM_ENVELOPE_B5                   34
#define XBPPRN_FORM_ENVELOPE_B6                   35
#define XBPPRN_FORM_ENVELOPE_ITALY                36
#define XBPPRN_FORM_ENVELOPE_MONARCH              37
#define XBPPRN_FORM_ENVELOPE_PERS                 38
#define XBPPRN_FORM_FANFOLD_US                    39
#define XBPPRN_FORM_FANFOLD_GER                   40
#define XBPPRN_FORM_FANFOLD_LGL_GER               41
#define XBPPRN_FORM_ISO_B4                        42
#define XBPPRN_FORM_JAPANESE_POSTCARD             43
#define XBPPRN_FORM_9X11                          44
#define XBPPRN_FORM_10X11                         45
#define XBPPRN_FORM_15X11                         46
#define XBPPRN_FORM_ENVELOPE_INV                  47
#define XBPPRN_FORM_LETTER_EXTRA                  50
#define XBPPRN_FORM_LEGAL_EXTRA                   51
#define XBPPRN_FORM_TABLOID_EXTRA                 52
#define XBPPRN_FORM_A4_EXTRA                      53
#define XBPPRN_FORM_LETTER_TRANSVERSE             54
#define XBPPRN_FORM_A4_TRANSVERSE                 55
#define XBPPRN_FORM_LETTER_XTRA_TRANS             56
#define XBPPRN_FORM_A_PLUS                        57
#define XBPPRN_FORM_B_PLUS                        58
#define XBPPRN_FORM_LETTER_PLUS                   59
#define XBPPRN_FORM_A4_PLUS                       60
#define XBPPRN_FORM_A5_TRANSVERSE                 61
#define XBPPRN_FORM_B5_TRANSVERSE                 62
#define XBPPRN_FORM_A3_EXTRA                      63
#define XBPPRN_FORM_A5_EXTRA                      64
#define XBPPRN_FORM_B5_EXTRA                      65
#define XBPPRN_FORM_A2                            66
#define XBPPRN_FORM_A3_TRANSVERSE                 67
#define XBPPRN_FORM_A3_EXTRA_TRANS                68

#define XBPPRN_FORM_USER                          256

#define XBPPRN_COLORMODE_OFF                      1
#define XBPPRN_COLORMODE_ON                       2

#define XBPPRN_DUPLEXMODE_OFF                     1
#define XBPPRN_DUPLEXMODE_MEMO                    2
#define XBPPRN_DUPLEXMODE_BOOK                    3

#define XBPPRN_FONTMODE_GRAPHIC                   1
#define XBPPRN_FONTMODE_DOWNLOAD                  2
#define XBPPRN_FONTMODE_SUBSTITUTE                3
#define XBPPRN_FONTMODE_DOWNLOADOUTL              4

#define XBPPRN_COLLATIONMODE_OFF                  0
#define XBPPRN_COLLATIONMODE_ON                   1

#define XBPPRN_PAPERBIN_SINGLE                    1
#define XBPPRN_PAPERBIN_LOWER                     2
#define XBPPRN_PAPERBIN_MIDDLE                    3
#define XBPPRN_PAPERBIN_MANUAL                    4
#define XBPPRN_PAPERBIN_ENVELOPE                  5
#define XBPPRN_PAPERBIN_ENVMANUAL                 6
#define XBPPRN_PAPERBIN_AUTO                      7
#define XBPPRN_PAPERBIN_TRACTOR                   8
#define XBPPRN_PAPERBIN_SMALLFORMAT               9
#define XBPPRN_PAPERBIN_LARGEFORMAT               10
#define XBPPRN_PAPERBIN_LARGECAPACITY             11
#define XBPPRN_PAPERBIN_CASETTE                   14
#define XBPPRN_PAPERBIN_FORMSOURCE                15

/*----------------------------------------------------------------------*/

#define _XBPDEV_CH
#endif
