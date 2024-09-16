/* Design layout */

#include "designer.hxx"

DLayout *dlayout_create(const uint32_t ncols, const uint32_t nrows);

void dlayout_destroy(DLayout **layout);

void dlayout_insert_col(DLayout *layout, const uint32_t col);

void dlayout_remove_col(DLayout *layout, const uint32_t col);

void dlayout_insert_row(DLayout *layout, const uint32_t row);

void dlayout_remove_row(DLayout *layout, const uint32_t row);

void dlayout_add_label(DLayout *layout, DLabel *label, const uint32_t col, const uint32_t row);
