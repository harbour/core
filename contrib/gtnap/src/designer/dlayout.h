/* Design layout (editable parameters) */

#include "designer.hxx"

DLayout *dlayout_create(const uint32_t ncols, const uint32_t nrows);

void dlayout_destroy(DLayout **layout);

void dlayout_name(DLayout *layout, const char_t *name);

void dlayout_margin_left(DLayout *layout, const real32_t margin);

void dlayout_margin_top(DLayout *layout, const real32_t margin);

void dlayout_margin_right(DLayout *layout, const real32_t margin);

void dlayout_margin_bottom(DLayout *layout, const real32_t margin);

void dlayout_margin_col(DLayout *layout, const uint32_t col, const real32_t margin);

void dlayout_margin_row(DLayout *layout, const uint32_t row, const real32_t margin);

void dlayout_insert_col(DLayout *layout, const uint32_t col);

void dlayout_remove_col(DLayout *layout, const uint32_t col);

void dlayout_insert_row(DLayout *layout, const uint32_t row);

void dlayout_remove_row(DLayout *layout, const uint32_t row);

void dlayout_add_layout(DLayout *layout, DLayout *sublayout, const uint32_t col, const uint32_t row);

void dlayout_add_label(DLayout *layout, DLabel *label, const uint32_t col, const uint32_t row);

bool_t dlayout_empty_cell(const DSelect *sel);

DCell *dlayout_cell(DLayout *layout, const uint32_t col, const uint32_t row);

DCell *dlayout_cell_sel(const DSelect *sel);

/* Create a real GUI layout from editable layout properties */
Layout *dlayout_gui_layout(const DLayout *layout);

/* Compute the visual rectangles of each layout element */
void dlayout_synchro_visual(DLayout *layout, const Layout *glayout, const V2Df origin);

Layout *dlayout_search_layout(const DLayout *layout, Layout *glayout, const DLayout *required);

uint32_t dlayout_ncols(const DLayout *layout);

uint32_t dlayout_nrows(const DLayout *layout);

//real32_t dlayout_get_margin_top(const DLayout *layout);
//
//real32_t dlayout_get_margin_bottom(const DLayout *layout);
//
//real32_t dlayout_get_margin_left(const DLayout *layout);
//
//real32_t dlayout_get_margin_right(const DLayout *layout);
//
//real32_t dlayout_get_margin_col(const DLayout *layout, const uint32_t col);
//
//real32_t dlayout_get_margin_row(const DLayout *layout, const uint32_t row);

void dlayout_elem_at_pos(const DLayout *layout, const real32_t x, const real32_t y, ArrSt(DSelect) *selpath);

void dlayout_draw(const DLayout *layout, const Layout *glayout, const DSelect *hover, const DSelect *sel, const widget_t swidget, const Image *add_icon, DCtx *ctx);
