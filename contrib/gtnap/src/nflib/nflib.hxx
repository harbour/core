/* NAppGUI forms common base */

#include <gui/gui.hxx>
#include "nflib.def"

#ifndef __NFLIB_HXX__
#define __NFLIB_HXX__

typedef struct _flabel_t FLabel;
typedef struct _fbutton_t FButton;
typedef struct _fcheck_t FCheck;
typedef struct _fedit_t FEdit;
typedef struct _ftext_t FText;
typedef struct _fcolumn_t FColumn;
typedef struct _frow_t FRow;
/* Must be a union (when dbind supports) */
typedef struct _fwidget_t FWidget;
typedef struct _fcell_t FCell;
typedef struct _flayout_t FLayout;

/* Don't change the order. Add new values to end */
typedef enum _celltype_t
{
    ekCELL_TYPE_EMPTY,
    ekCELL_TYPE_LABEL,
    ekCELL_TYPE_BUTTON,
    ekCELL_TYPE_CHECK,
    ekCELL_TYPE_EDIT,
    ekCELL_TYPE_LAYOUT,
    ekCELL_TYPE_TEXT
} celltype_t;

/* Don't change the order. Add new values to end */
typedef enum _halign_t
{
    ekHALIGN_LEFT,
    ekHALIGN_CENTER,
    ekHALIGN_RIGHT,
    ekHALIGN_JUSTIFY
} halign_t;

/* Don't change the order. Add new values to end */
typedef enum _valign_t
{
    ekVALIGN_TOP,
    ekVALIGN_CENTER,
    ekVALIGN_BOTTOM,
    ekVALIGN_JUSTIFY
} valign_t;

struct _flabel_t
{
    String *text;
};

struct _fbutton_t
{
    String *text;
    real32_t min_width;
};

struct _fcheck_t
{
    String *text;
};

struct _fedit_t
{
    bool_t passmode;
    bool_t autosel;
    halign_t text_align;
    real32_t min_width;
};

struct _ftext_t
{
    bool_t read_only;
    real32_t min_width;
    real32_t min_height;
};

struct _fcolumn_t
{
    real32_t margin_right;
    real32_t forced_width;
};

struct _frow_t
{
    real32_t margin_bottom;
    real32_t forced_height;
};

struct _fwidget_t
{
    FLabel *label;
    FButton *button;
    FCheck *check;
    FEdit *edit;
    FText *text;
    FLayout *layout;
};

struct _fcell_t
{
    String *name;
    celltype_t type;
    halign_t halign;
    valign_t valign;
    FWidget widget;
};

struct _flayout_t
{
    String *name;
    real32_t margin_left;
    real32_t margin_top;
    real32_t margin_right;
    real32_t margin_bottom;
    ArrSt(FColumn) *cols;
    ArrSt(FRow) *rows;
    ArrSt(FCell) *cells;
};

DeclSt(FColumn);
DeclSt(FRow);
DeclSt(FCell);

#endif
