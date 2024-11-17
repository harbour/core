/* Form layout */

#include "flayout.h"
#include <gui/button.h>
#include <gui/buttonh.h>
#include <gui/label.h>
#include <gui/labelh.h>
#include <gui/layout.h>
#include <gui/layouth.h>
#include <gui/edit.h>
#include <core/arrst.h>
#include <core/dbind.h>
#include <core/strings.h>
#include <sewer/cassert.h>

/*---------------------------------------------------------------------------*/

static void i_remove_column(FColumn *column)
{
    dbind_remove(column, FColumn);
}

/*---------------------------------------------------------------------------*/

static void i_remove_row(FRow *row)
{
    dbind_remove(row, FRow);
}

/*---------------------------------------------------------------------------*/

static void i_remove_cell(FCell *cell)
{
    dbind_remove(cell, FCell);
}

/*---------------------------------------------------------------------------*/

FLayout *flayout_create(const uint32_t ncols, const uint32_t nrows)
{
    FLayout *layout = dbind_create(FLayout);
    cassert(ncols > 0);
    cassert(nrows > 0);

    /* Add columns to layout */
    {
        uint32_t i = 0;
        for (i = 0; i < ncols; ++i)
        {
            FColumn *col = arrst_new(layout->cols, FColumn);
            dbind_init(col, FColumn);
        }
    }

    /* Add rows to layout */
    {
        uint32_t i = 0;
        for (i = 0; i < nrows; ++i)
        {
            FRow *row = arrst_new(layout->rows, FRow);
            dbind_init(row, FRow);
        }
    }

    /* Add cells to layout */
    {
        uint32_t n = ncols * nrows, i = 0;
        for (i = 0; i < n; ++i)
        {
            FCell *cell = arrst_new(layout->cells, FCell);
            dbind_init(cell, FCell);
        }
    }

    return layout;
}

/*---------------------------------------------------------------------------*/

void flayout_margin_left(FLayout *layout, const real32_t margin)
{
    cassert_no_null(layout);
    layout->margin_left = margin;
}

/*---------------------------------------------------------------------------*/

void flayout_margin_top(FLayout *layout, const real32_t margin)
{
    cassert_no_null(layout);
    layout->margin_top = margin;
}

/*---------------------------------------------------------------------------*/

void flayout_margin_right(FLayout *layout, const real32_t margin)
{
    cassert_no_null(layout);
    layout->margin_right = margin;
}

/*---------------------------------------------------------------------------*/

void flayout_margin_bottom(FLayout *layout, const real32_t margin)
{
    cassert_no_null(layout);
    layout->margin_bottom = margin;
}

/*---------------------------------------------------------------------------*/

void flayout_margin_col(FLayout *layout, const uint32_t col, const real32_t margin)
{
    FColumn *fcol = NULL;
    cassert_no_null(layout);
    cassert(col < arrst_size(layout->cols, FColumn) - 1);
    fcol = arrst_get(layout->cols, col, FColumn);
    fcol->margin_right = margin;
}

/*---------------------------------------------------------------------------*/

void flayout_margin_row(FLayout *layout, const uint32_t row, const real32_t margin)
{
    FRow *frow = NULL;
    cassert_no_null(layout);
    cassert(row < arrst_size(layout->rows, FRow) - 1);
    frow = arrst_get(layout->rows, row, FRow);
    frow->margin_bottom = margin;
}

/*---------------------------------------------------------------------------*/

void flayout_insert_col(FLayout *layout, const uint32_t col)
{
    uint32_t ncols = 0, nrows = 0, i = 0;
    cassert_no_null(layout);
    ncols = arrst_size(layout->cols, FColumn);
    nrows = arrst_size(layout->rows, FRow);

    /* Insert and init (empty) the new cells */
    for (i = 0; i < nrows; ++i)
    {
        uint32_t inspos = ((ncols + 1) * i) + col;
        FCell *cell = arrst_insert_n(layout->cells, inspos, 1, FCell);
        dbind_init(cell, FCell);
    }

    /* Add a new column */
    {
        FColumn *ncol = arrst_insert_n(layout->cols, col, 1, FColumn);
        dbind_init(ncol, FColumn);
    }
}

/*---------------------------------------------------------------------------*/

void flayout_remove_col(FLayout *layout, const uint32_t col)
{
    uint32_t ncols = 0, nrows = 0, i = 0;
    cassert_no_null(layout);
    cassert(col < arrst_size(layout->cols, FColumn));
    ncols = arrst_size(layout->cols, FColumn);
    nrows = arrst_size(layout->rows, FRow);

    /* Destroy the column cells */
    for (i = 0; i < nrows; ++i)
    {
        uint32_t delrow = nrows - i - 1;
        uint32_t delpos = (ncols * delrow) + col;
        arrst_delete(layout->cells, delpos, i_remove_cell, FCell);
    }

    /* Destroy the column */
    arrst_delete(layout->cols, col, i_remove_column, FColumn);
}

/*---------------------------------------------------------------------------*/

void flayout_insert_row(FLayout *layout, const uint32_t row)
{
    uint32_t ncols = 0, nrows = 0, i = 0;
    uint32_t inspos = 0;
    FCell *cells = NULL;
    cassert_no_null(layout);
    ncols = arrst_size(layout->cols, FColumn);
    nrows = arrst_size(layout->rows, FRow);
    /* Cells insert position */
    inspos = row * ncols;
    /* Cells array is in row-major order. All row cells are together in memory */
    cells = arrst_insert_n(layout->cells, inspos, ncols, FCell);

    /* Initialize the new cells (empty) */
    for (i = 0; i < ncols; ++i)
        dbind_init(cells + i, FCell);

    /* Add a new row */
    {
        FRow *nrow = arrst_insert_n(layout->rows, row, 1, FRow);
        dbind_init(nrow, FRow);
    }
}

/*---------------------------------------------------------------------------*/

void flayout_remove_row(FLayout *layout, const uint32_t row)
{
    uint32_t i, ncols = 0, nrows = 0;
    cassert_no_null(layout);
    cassert(row < arrst_size(layout->rows, FRow));
    ncols = arrst_size(layout->cols, FColumn);
    nrows = arrst_size(layout->rows, FRow);

    /* Destroy the row cells */
    for (i = 0; i < ncols; ++i)
    {
        uint32_t delcol = ncols - i - 1;
        uint32_t delpos = (ncols * row) + delcol;
        arrst_delete(layout->cells, delpos, i_remove_cell, FCell);
    }

    /* Destroy the row */
    arrst_delete(layout->rows, row, i_remove_row, FRow);
}

/*---------------------------------------------------------------------------*/

static ___INLINE FCell *i_cell(FLayout *layout, const uint32_t col, const uint32_t row)
{
    uint32_t ncols = UINT32_MAX;
    uint32_t pos = UINT32_MAX;
    cassert_no_null(layout);
    ncols = arrst_size(layout->cols, FColumn);
    pos = row * ncols + col;
    return arrst_get(layout->cells, pos, FCell);
}

/*---------------------------------------------------------------------------*/

void flayout_remove_cell(FLayout *layout, const uint32_t col, const uint32_t row)
{
    String *name = NULL;
    FCell *cell = i_cell(layout, col, row);
    cassert_no_null(cell);
    name = str_c(tc(cell->name));
    dbind_remove(cell, FCell);
    dbind_init(cell, FCell);
    str_upd(&cell->name, tc(name));
    str_destroy(&name);
}

/*---------------------------------------------------------------------------*/

void flayout_add_layout(FLayout *layout, FLayout *sublayout, const uint32_t col, const uint32_t row)
{
    FCell *cell = i_cell(layout, col, row);
    cassert_no_null(cell);
    cassert_no_null(sublayout);
    cassert(cell->type == ekCELL_TYPE_EMPTY);
    cell->type = ekCELL_TYPE_LAYOUT;
    cell->halign = ekHALIGN_JUSTIFY;
    cell->valign = ekVALIGN_JUSTIFY;
    cell->widget.layout = sublayout;
}

/*---------------------------------------------------------------------------*/

void flayout_add_label(FLayout *layout, FLabel *label, const uint32_t col, const uint32_t row)
{
    FCell *cell = i_cell(layout, col, row);
    cassert_no_null(cell);
    cassert_no_null(label);
    cassert(cell->type == ekCELL_TYPE_EMPTY);
    cell->type = ekCELL_TYPE_LABEL;
    cell->halign = ekHALIGN_LEFT;
    cell->valign = ekVALIGN_CENTER;
    cell->widget.label = label;
}

/*---------------------------------------------------------------------------*/

void flayout_add_button(FLayout *layout, FButton *button, const uint32_t col, const uint32_t row)
{
    FCell *cell = i_cell(layout, col, row);
    cassert_no_null(cell);
    cassert_no_null(button);
    cassert(cell->type == ekCELL_TYPE_EMPTY);
    cell->type = ekCELL_TYPE_BUTTON;
    cell->halign = ekHALIGN_JUSTIFY;
    cell->valign = ekVALIGN_CENTER;
    cell->widget.button = button;
}

/*---------------------------------------------------------------------------*/

void flayout_add_check(FLayout *layout, FCheck *check, const uint32_t col, const uint32_t row)
{
    FCell *cell = i_cell(layout, col, row);
    cassert_no_null(cell);
    cassert_no_null(check);
    cassert(cell->type == ekCELL_TYPE_EMPTY);
    cell->type = ekCELL_TYPE_CHECK;
    cell->halign = ekHALIGN_LEFT;
    cell->valign = ekVALIGN_CENTER;
    cell->widget.check = check;
}

/*---------------------------------------------------------------------------*/

void flayout_add_edit(FLayout *layout, FEdit *edit, const uint32_t col, const uint32_t row)
{
    FCell *cell = i_cell(layout, col, row);
    cassert_no_null(cell);
    cassert_no_null(edit);
    cassert(cell->type == ekCELL_TYPE_EMPTY);
    cell->type = ekCELL_TYPE_EDIT;
    cell->halign = ekHALIGN_JUSTIFY;
    cell->valign = ekVALIGN_CENTER;
    cell->widget.edit = edit;
}

/*---------------------------------------------------------------------------*/

uint32_t flayout_ncols(const FLayout *layout)
{
    cassert_no_null(layout);
    return arrst_size(layout->cols, FColumn);
}

/*---------------------------------------------------------------------------*/

uint32_t flayout_nrows(const FLayout *layout)
{
    cassert_no_null(layout);
    return arrst_size(layout->rows, FRow);
}

/*---------------------------------------------------------------------------*/

FColumn *flayout_column(FLayout *layout, const uint32_t col)
{
    cassert_no_null(layout);
    return arrst_get(layout->cols, col, FColumn);
}

/*---------------------------------------------------------------------------*/

FRow *flayout_row(FLayout *layout, const uint32_t row)
{
    cassert_no_null(layout);
    return arrst_get(layout->rows, row, FRow);
}

/*---------------------------------------------------------------------------*/

FCell *flayout_cell(FLayout *layout, const uint32_t col, const uint32_t row)
{
    return i_cell(layout, col, row);
}

/*---------------------------------------------------------------------------*/

const FCell *flayout_ccell(const FLayout *layout, const uint32_t col, const uint32_t row)
{
    return i_cell(cast(layout, FLayout), col, row);
}

/*---------------------------------------------------------------------------*/

static align_t i_halign(const halign_t halign)
{
    switch (halign)
    {
    case ekHALIGN_LEFT:
        return ekLEFT;
    case ekHALIGN_CENTER:
        return ekCENTER;
    case ekHALIGN_RIGHT:
        return ekRIGHT;
    case ekHALIGN_JUSTIFY:
        return ekJUSTIFY;
        cassert_default();
    }
    return ekLEFT;
}

/*---------------------------------------------------------------------------*/

static align_t i_valign(const valign_t valign)
{
    switch (valign)
    {
    case ekVALIGN_TOP:
        return ekTOP;
    case ekVALIGN_CENTER:
        return ekCENTER;
    case ekVALIGN_BOTTOM:
        return ekBOTTOM;
    case ekVALIGN_JUSTIFY:
        return ekJUSTIFY;
        cassert_default();
    }
    return ekTOP;
}

/*---------------------------------------------------------------------------*/

Layout *flayout_to_gui(const FLayout *layout, const real32_t empty_width, const real32_t empty_height)
{
    uint32_t ncols = 0, nrows = 0;
    Layout *glayout = NULL;
    cassert_no_null(layout);
    ncols = arrst_size(layout->cols, FColumn);
    nrows = arrst_size(layout->rows, FRow);
    glayout = layout_create(ncols, nrows);

    /* Layout border margins */
    layout_margin4(glayout, layout->margin_top, layout->margin_right, layout->margin_bottom, layout->margin_left);

    /* Column properties */
    arrst_foreach_const(col, layout->cols, FColumn)
        layout_hsize(glayout, col_i, col->forced_width);
        if (col_i < col_total - 1)
        {
            layout_hmargin(glayout, col_i, col->margin_right);
        }
        else
        {
            cassert(col->margin_right == 0);
        }
    arrst_end()

    /* Row properties */
    arrst_foreach_const(row, layout->rows, FRow)
        layout_vsize(glayout, row_i, row->forced_height);
        if (row_i < row_total - 1)
        {        
            layout_vmargin(glayout, row_i, row->margin_bottom);
        }
        else
        {
            cassert(row->margin_bottom == 0);
        }
    arrst_end()

    /* Cells */
    {
        uint32_t i, j;
        const FCell *cells = arrst_all_const(layout->cells, FCell);
        for (j = 0; j < nrows; ++j)
        {
            for (i = 0; i < ncols; ++i)
            {
                Cell *gcell = layout_cell(glayout, i, j);
                align_t halign = i_halign(cells->halign);
                align_t valign = i_valign(cells->valign);
                layout_halign(glayout, i, j, halign);
                layout_valign(glayout, i, j, valign);
                switch (cells->type)
                {
                case ekCELL_TYPE_EMPTY:
                    cell_force_size(gcell, empty_width, empty_height);
                    break;

                case ekCELL_TYPE_LABEL:
                {
                    FLabel *flabel = cells->widget.label;
                    Label *glabel = label_create();
                    label_text(glabel, tc(flabel->text));
                    layout_label(glayout, glabel, i, j);
                    break;
                }

                case ekCELL_TYPE_BUTTON:
                {
                    FButton *fbutton = cells->widget.button;
                    Button *gbutton = button_push();
                    button_text(gbutton, tc(fbutton->text));
                    layout_button(glayout, gbutton, i, j);
                    break;
                }

                case ekCELL_TYPE_CHECK:
                {
                    FCheck *fcheck = cells->widget.check;
                    Button *gcheck = button_check();
                    button_text(gcheck, tc(fcheck->text));
                    layout_button(glayout, gcheck, i, j);
                    break;
                }

                case ekCELL_TYPE_EDIT:
                {
                    FEdit *fedit = cells->widget.edit;
                    Edit *gedit = edit_create();
                    align_t align = i_halign(fedit->text_align);
                    edit_passmode(gedit, fedit->passmode);
                    edit_autoselect(gedit, fedit->autosel);
                    edit_align(gedit, align);
                    layout_edit(glayout, gedit, i, j);
                    break;
                }

                case ekCELL_TYPE_LAYOUT:
                {
                    Layout *gsublayout = flayout_to_gui(cells->widget.layout, empty_width, empty_height);
                    layout_layout(glayout, gsublayout, i, j);
                    break;
                }
                }

                cells += 1;
            }
        }
    }

    return glayout;
}

///*---------------------------------------------------------------------------*/
//
//Layout *flayout_gui_search(const FLayout *layout, Layout *glayout, const FLayout *wanted)
//{
//    cassert_no_null(layout);
//    if (layout != wanted)
//    {
//        const FCell *cell = arrst_all_const(layout->cells, FCell);
//        uint32_t ncols = arrst_size(layout->cols, FColumn);
//        uint32_t nrows = arrst_size(layout->rows, FRow);
//        uint32_t i, j;
//
//        for (j = 0; j < nrows; ++j)
//        {
//            for (i = 0; i < ncols; ++i)
//            {
//                if (cell->type == ekCELL_TYPE_LAYOUT)
//                {
//                    FLayout *sublayout = cell->widget.layout;
//                    Layout *subglayout = layout_get_layout(cast(glayout, Layout), i, j);
//                    Layout *fglayout = flayout_gui_search(sublayout, subglayout, wanted);
//                    if (fglayout != NULL)
//                        return fglayout;
//                }
//
//                cell += 1;
//            }
//        }
//
//        return NULL;
//    }
//    else
//    {
//        cassert(flayout_ncols(layout) == layout_ncols(glayout));
//        cassert(flayout_nrows(layout) == layout_nrows(glayout));
//        return glayout;
//    }
//}
//
///*---------------------------------------------------------------------------*/
//
//void flayout_synchro_margin(const FLayout *layout, Layout *glayout, const FLayout *sublayout)
//{
//    Layout *gsublayout = flayout_gui_search(layout, glayout, sublayout);
//    layout_margin4(gsublayout, sublayout->margin_top, sublayout->margin_right, sublayout->margin_bottom, sublayout->margin_left);
//}
