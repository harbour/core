/* Form layout */

#include "nform.hxx"

__EXTERN_C

_nform_api FLayout *flayout_create(const uint32_t ncols, const uint32_t nrows);

//void flayout_destroy(FLayout **layout);
//
//void flayout_name(FLayout *layout, const char_t *name);
//
//void flayout_margin_left(FLayout *layout, const real32_t margin, Layout *glayout);
//
//void flayout_margin_top(FLayout *layout, const real32_t margin);
//
//void flayout_margin_right(FLayout *layout, const real32_t margin);
//
//void flayout_margin_bottom(FLayout *layout, const real32_t margin);
//
//void flayout_margin_col(FLayout *layout, const uint32_t col, const real32_t margin);
//
//void flayout_margin_row(FLayout *layout, const uint32_t row, const real32_t margin);
//
_nform_api void flayout_insert_col(FLayout *layout, const uint32_t col);

_nform_api void flayout_remove_col(FLayout *layout, const uint32_t col);

_nform_api void flayout_insert_row(FLayout *layout, const uint32_t row);

_nform_api void flayout_remove_row(FLayout *layout, const uint32_t row);

_nform_api void flayout_remove_cell(FLayout *layout, const uint32_t col, const uint32_t row);

_nform_api void flayout_add_layout(FLayout *layout, FLayout *sublayout, const uint32_t col, const uint32_t row);

_nform_api void flayout_add_label(FLayout *layout, FLabel *label, const uint32_t col, const uint32_t row);

_nform_api void flayout_add_button(FLayout *layout, FButton *button, const uint32_t col, const uint32_t row);

_nform_api void flayout_add_check(FLayout *layout, FCheck *check, const uint32_t col, const uint32_t row);

_nform_api void flayout_add_edit(FLayout *layout, FEdit *edit, const uint32_t col, const uint32_t row);

_nform_api uint32_t flayout_ncols(const FLayout *layout);

_nform_api uint32_t flayout_nrows(const FLayout *layout);

_nform_api FColumn *flayout_column(FLayout *layout, const uint32_t col);

_nform_api FRow *flayout_row(FLayout *layout, const uint32_t row);

_nform_api FCell *flayout_cell(FLayout *layout, const uint32_t col, const uint32_t row);

_nform_api const FCell *flayout_ccell(const FLayout *layout, const uint32_t col, const uint32_t row);

_nform_api Layout *flayout_to_gui(const FLayout *layout);

_nform_api Layout *flayout_gui_search(const FLayout *layout, Layout *glayout, const FLayout *wanted);

__END_C
