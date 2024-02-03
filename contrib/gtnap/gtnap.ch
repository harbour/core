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


/*
 * LibreOffice SDK
 */

/* Error codes for LibreOffice SDK */
#define SDKRES_OK                       1
#define SDKRES_NO_ENVAR                 2
#define SDKRES_PROC_KILL_FAIL           3
#define SDKRES_PROC_INIT_FAIL           4
#define SDKRES_CONECT_FAIL              5
#define SDKRES_COMPONENT_LOADER         6
#define SDKRES_CREATE_FILE_ERROR        7
#define SDKRES_OPEN_FILE_ERROR          8
#define SDKRES_SAVE_FILE_ERROR          9
#define SDKRES_CLOSE_DOC_ERROR          10
#define SDKRES_ACCESS_DOC_ERROR         11
#define SDKRES_ACCESS_CELL_ERROR        12
#define SDKRES_EDIT_CELL_ERROR          13
#define SDKRES_FORMAT_CELL_ERROR        14
#define SDKRES_ACCESS_COLUMN_ERROR      15
#define SDKRES_FORMAT_COLUMN_ERROR      16
#define SDKRES_ACCESS_ROW_ERROR         17
#define SDKRES_FORMAT_ROW_ERROR         18
#define SDKRES_TEXT_PROPERTY_ERROR      19
#define SDKRES_TEXT_ADD_ERROR           20
#define SDKRES_PAGE_PROPERTY_ERROR      21
#define SDKRES_PRINTER_CONFIG_ERROR     22
#define SDKRES_PRINT_ERROR              23

/* Horizontal alignment for LibreOffice SDK */
#define SDK_HALIGN_LEFT             1
#define SDK_HALIGN_CENTER           2
#define SDK_HALIGN_RIGHT            3
#define SDK_HALIGN_JUSTIFY          4

/* Vertical alignment for LibreOffice SDK */
#define SDK_VALIGN_TOP              1
#define SDK_VALIGN_CENTER           2
#define SDK_VALIGN_BOTTOM           3

/* Number format for LibreOffice SDK */
#define SDK_NUMFORMAT_INT           1
#define SDK_NUMFORMAT_INT_1000      2
#define SDK_NUMFORMAT_DEC2          3
#define SDK_NUMFORMAT_DEC2_1000     4
#define SDK_NUMFORMAT_PERC_INT      5
#define SDK_NUMFORMAT_PERC_DEC2     6

#define SDK_NUMFORMAT_DATE_SYS_DDMMM            7
#define SDK_NUMFORMAT_DATE_SYS_DDMMYY           8
#define SDK_NUMFORMAT_DATE_SYS_DDMMYYYY         9
#define SDK_NUMFORMAT_DATE_SYS_DMMMMYYYY        10
#define SDK_NUMFORMAT_DATE_SYS_DMMMYY           11
#define SDK_NUMFORMAT_DATE_SYS_DMMMYYYY         12
#define SDK_NUMFORMAT_DATE_SYS_MMYY             13
#define SDK_NUMFORMAT_DATE_SYS_NNDMMMMYYYY      14
#define SDK_NUMFORMAT_DATE_SYS_NNDMMMYY         15
#define SDK_NUMFORMAT_DATE_SYS_NNNNDMMMMYYYY    16

/* Line style for LibreOffice SDK */
#define SDK_LINE_STYLE_NONE                     1
#define SDK_LINE_STYLE_SOLID                    2
#define SDK_LINE_STYLE_DOTTED                   3
#define SDK_LINE_STYLE_DASHED                   4
#define SDK_LINE_STYLE_DOUBLE                   5
#define SDK_LINE_STYLE_THINTHICK_SMALLGAP       6
#define SDK_LINE_STYLE_THINTHICK_MEDIUMGAP      7
#define SDK_LINE_STYLE_THINTHICK_LARGEGAP       8
#define SDK_LINE_STYLE_THICKTHIN_SMALLGAP       9
#define SDK_LINE_STYLE_THICKTHIN_MEDIUMGAP      10
#define SDK_LINE_STYLE_THICKTHIN_LARGEGAP       11
#define SDK_LINE_STYLE_EMBOSSED                 12
#define SDK_LINE_STYLE_ENGRAVED                 13
#define SDK_LINE_STYLE_OUTSET                   14
#define SDK_LINE_STYLE_INSET                    15
#define SDK_LINE_STYLE_FINE_DASHED              16
#define SDK_LINE_STYLE_DOUBLE_THIN              17
#define SDK_LINE_STYLE_DASH_DOT                 18
#define SDK_LINE_STYLE_DASH_DOT_DOT             19

/* Text space for text documents */
#define SDK_TEXT_SPACE_HEADER                   1
#define SDK_TEXT_SPACE_FOOTER                   2
#define SDK_TEXT_SPACE_PAGE                     3

/* Printer paper orientation */
#define SDK_PAPER_ORIENT_PORTRAIT               1
#define SDK_PAPER_ORIENT_LANSCAPE               2

/* Printer paper format */
#define SDK_PAPER_FORMAT_A3                     1
#define SDK_PAPER_FORMAT_A4                     2
#define SDK_PAPER_FORMAT_A5                     3
#define SDK_PAPER_FORMAT_B4                     4
#define SDK_PAPER_FORMAT_B5                     5
#define SDK_PAPER_FORMAT_LETTER                 6
#define SDK_PAPER_FORMAT_LEGAL                  7
#define SDK_PAPER_FORMAT_TABLOID                8
#define SDK_PAPER_FORMAT_USER                   9
