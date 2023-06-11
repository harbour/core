/*
    gtnap bindings to use in Harbour programs
    TODO: More info
*/

/* Return codes for NAP_WINDOW_MODAL() */

#define NAP_MODAL_ESC               1

#define NAP_MODAL_ENTER             2

#define NAP_MODAL_X_BUTTON          3

#define NAP_MODAL_TOOLBAR           4

#define NAP_MODAL_TIMESTAMP			5

#define NAP_MODAL_BUTTON_AUTOCLOSE  1000    /* + index of button >= 1 */

#define NAP_MODAL_HOTKEY_AUTOCLOSE  2000    /* + vkey_t use NAP_INKEY(ret - NAP_MODAL_HOTKEY_AUTOCLOSE) */

#define NAP_MODAL_IMAGE_AUTOCLOSE   3000    /* + index of image >= 1 */

#define NAP_MODAL_MENU_AUTOCLOSE    4000    /* + index of option >= 1 */

#define NAP_MODAL_TEXT_CONFIRM      5000

#define NAP_MODAL_LAST_INPUT        6000

#define NAP_MODAL_ROW_CLICK         10000   /* + RECNO clicked or ROW pos clicked */

#define NAP_MAX_IMAGES              50

#define NAP_MAX_BUTTONS             50

#define NAP_MAX_VKEY                112

#define NAP_WINDOW_FIST_ID          10000
