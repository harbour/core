/* Designer layout */

#include "designer.hxx"

DLayout *dlayout_create(const uint32_t ncols, const uint32_t nrows);

void dlayout_destroy(DLayout **layout);

void dlayout_insert_col(DLayout *layout, const uint32_t col);

void dlayout_remove_col(DLayout *layout, const uint32_t col);

void dlayout_insert_row(DLayout *layout, const uint32_t row);

void dlayout_remove_row(DLayout *layout, const uint32_t row);

void dlayout_remove_cell(DLayout *layout, const uint32_t col, const uint32_t row);

void dlayout_add_layout(DLayout *layout, DLayout *sublayout, const uint32_t col, const uint32_t row);

bool_t dlayout_empty_cell(const DSelect *sel);

FCell *dlayout_sel_fcell(const DSelect *sel);

void dlayout_synchro_visual(DLayout *layout, const Layout *glayout, const V2Df origin);

void dlayout_elem_at_pos(const DLayout *layout, const real32_t x, const real32_t y, ArrSt(DSelect) *selpath);

void dlayout_draw(const DLayout *layout, const Layout *glayout, const DSelect *hover, const DSelect *sel, const widget_t swidget, const Image *add_icon, DCtx *ctx);
