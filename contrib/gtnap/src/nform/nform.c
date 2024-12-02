/* NAppGUI forms common base */

#include "nform.h"
#include <gui/gui.h>
#include <core/dbind.h>
#include <osbs/log.h>
#include <sewer/blib.h>
#include <sewer/cassert.h>

static uint32_t i_NUM_USERS = 0;

/*---------------------------------------------------------------------------*/

static void i_nform_atexit(void)
{
    if (i_NUM_USERS != 0)
        log_printf("Error! nform is not properly closed (%d)\n", i_NUM_USERS);
}

/*---------------------------------------------------------------------------*/

static void i_dbind(void)
{
    /* Registration of editable structures */
    dbind_enum(celltype_t, ekCELL_TYPE_EMPTY, "");
    dbind_enum(celltype_t, ekCELL_TYPE_LABEL, "");
    dbind_enum(celltype_t, ekCELL_TYPE_BUTTON, "");
    dbind_enum(celltype_t, ekCELL_TYPE_CHECK, "");
    dbind_enum(celltype_t, ekCELL_TYPE_EDIT, "");
    dbind_enum(celltype_t, ekCELL_TYPE_LAYOUT, "");
    dbind_enum(halign_t, ekHALIGN_LEFT, "Left");
    dbind_enum(halign_t, ekHALIGN_CENTER, "Center");
    dbind_enum(halign_t, ekHALIGN_RIGHT, "Right");
    dbind_enum(halign_t, ekHALIGN_JUSTIFY, "Justify");
    dbind_enum(valign_t, ekVALIGN_TOP, "Top");
    dbind_enum(valign_t, ekVALIGN_CENTER, "Center");
    dbind_enum(valign_t, ekVALIGN_BOTTOM, "Bottom");
    dbind_enum(valign_t, ekVALIGN_JUSTIFY, "Justify");
    dbind(FLabel, String *, text);
    dbind(FButton, String *, text);
    dbind(FCheck, String *, text);
    dbind(FEdit, bool_t, passmode);
    dbind(FEdit, bool_t, autosel);
    dbind(FEdit, halign_t, text_align);
    dbind(FColumn, real32_t, margin_right);
    dbind(FColumn, real32_t, forced_width);
    dbind(FRow, real32_t, margin_bottom);
    dbind(FRow, real32_t, forced_height);
    dbind(FCell, String *, name);
    dbind(FCell, celltype_t, type);
    dbind(FCell, halign_t, halign);
    dbind(FCell, valign_t, valign);
    dbind(FLayout, String *, name);
    dbind(FLayout, real32_t, margin_left);
    dbind(FLayout, real32_t, margin_top);
    dbind(FLayout, real32_t, margin_right);
    dbind(FLayout, real32_t, margin_bottom);
    dbind(FLayout, ArrSt(FColumn) *, cols);
    dbind(FLayout, ArrSt(FRow) *, rows);
    dbind(FLayout, ArrSt(FCell) *, cells);

    dbind_default(FColumn, real32_t, margin_right, 0);
    dbind_default(FColumn, real32_t, forced_width, 0);
    dbind_increment(FColumn, real32_t, margin_right, 1);
    dbind_increment(FColumn, real32_t, forced_width, 1);
    dbind_range(FColumn, real32_t, margin_right, 0, 100);
    dbind_range(FColumn, real32_t, forced_width, 0, 1000);
    dbind_precision(FColumn, real32_t, margin_right, 1);
    dbind_precision(FColumn, real32_t, forced_width, 1);

    dbind_default(FRow, real32_t, margin_bottom, 0);
    dbind_default(FRow, real32_t, forced_height, 0);
    dbind_increment(FRow, real32_t, margin_bottom, 1);
    dbind_increment(FRow, real32_t, forced_height, 1);
    dbind_range(FRow, real32_t, margin_bottom, 0, 100);
    dbind_range(FRow, real32_t, forced_height, 0, 1000);
    dbind_precision(FRow, real32_t, margin_bottom, 1);
    dbind_precision(FRow, real32_t, forced_height, 1);

    dbind_default(FCell, celltype_t, type, ekCELL_TYPE_EMPTY);
    dbind_default(FCell, halign_t, halign, ekHALIGN_LEFT);
    dbind_default(FCell, valign_t, valign, ekVALIGN_TOP);

    dbind_default(FLayout, real32_t, margin_left, 0);
    dbind_default(FLayout, real32_t, margin_top, 0);
    dbind_default(FLayout, real32_t, margin_right, 0);
    dbind_default(FLayout, real32_t, margin_bottom, 0);
    dbind_increment(FLayout, real32_t, margin_left, 1);
    dbind_increment(FLayout, real32_t, margin_top, 1);
    dbind_increment(FLayout, real32_t, margin_right, 1);
    dbind_increment(FLayout, real32_t, margin_bottom, 1);
    dbind_range(FLayout, real32_t, margin_left, 0, 100);
    dbind_range(FLayout, real32_t, margin_top, 0, 100);
    dbind_range(FLayout, real32_t, margin_right, 0, 100);
    dbind_range(FLayout, real32_t, margin_bottom, 0, 100);
    dbind_precision(FLayout, real32_t, margin_left, 1);
    dbind_precision(FLayout, real32_t, margin_top, 1);
    dbind_precision(FLayout, real32_t, margin_right, 1);
    dbind_precision(FLayout, real32_t, margin_bottom, 1);

    /* Don't move, we must first declare the inner struct */
    dbind(FWidget, FLabel *, label);
    dbind(FWidget, FButton *, button);
    dbind(FWidget, FCheck *, check);
    dbind(FWidget, FEdit *, edit);
    dbind(FWidget, FLayout *, layout);
    dbind(FCell, FWidget, widget);
}

/*---------------------------------------------------------------------------*/

void nform_start(void)
{
    if (i_NUM_USERS == 0)
    {
        gui_start();
        i_dbind();
        blib_atexit(i_nform_atexit);
    }

    i_NUM_USERS += 1;
}

/*---------------------------------------------------------------------------*/

void nform_finish(void)
{
    cassert(i_NUM_USERS > 0);
    if (i_NUM_USERS == 1)
    {
        /* Unregister types (when dbind support it) */
        gui_finish();
    }

    i_NUM_USERS -= 1;
}
