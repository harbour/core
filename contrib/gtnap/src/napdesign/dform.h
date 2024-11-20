/* Design form */

#include "designer.hxx"

DForm *dform_first_example(void);

DForm *dform_empty(void);

void dform_destroy(DForm **form);

void dform_compose(DForm *form);

bool_t dform_OnMove(DForm *form, const real32_t mouse_x, const real32_t mouse_y);

bool_t dform_OnClick(DForm *form, Window *window, Panel *inspect, Panel *propedit, const widget_t widget, const real32_t mouse_x, const real32_t mouse_y, const gui_mouse_t button);

bool_t dform_OnExit(DForm *form);

bool_t dform_OnSupr(DForm *form, Panel *inspect, Panel *propedit);

void dform_synchro_cell_text(const DSelect *sel);

void dform_synchro_edit(const DSelect *sel);

void dform_synchro_layout_margin(const DSelect *sel);

void dform_synchro_column_margin(const DSelect *sel, const FColumn *fcol, const uint32_t col);

void dform_synchro_column_width(const DSelect *sel, const FColumn *fcol, const uint32_t col);

void dform_synchro_row_margin(const DSelect *sel, const FRow *frow, const uint32_t row);

void dform_synchro_row_height(const DSelect *sel, const FRow *frow, const uint32_t row);

void dform_synchro_cell_halign(const DSelect *sel, const FCell *fcell, const uint32_t col, const uint32_t row);

void dform_synchro_cell_valign(const DSelect *sel, const FCell *fcell, const uint32_t col, const uint32_t row);

void dform_draw(const DForm *form, const widget_t swidget, const Image *add_icon, DCtx *ctx);

uint32_t dform_selpath_size(const DForm *form);

const char_t *dform_selpath_caption(const DForm *form, const uint32_t col, const uint32_t row);

void dform_inspect_select(DForm *form, Panel *propedit, const uint32_t row);

void dform_simulate(DForm *form, Window *window);
