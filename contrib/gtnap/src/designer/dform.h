/* Design form */

#include "designer.hxx"

DForm *dform_first_example(void);

void dform_destroy(DForm **form);

void dform_compose(DForm *form);

bool_t dform_OnMove(DForm *form, const real32_t mouse_x, const real32_t mouse_y);

bool_t dform_OnClick(DForm *form, Window *window, Panel *inspect, Panel *propedit, const widget_t widget, const real32_t mouse_x, const real32_t mouse_y, const gui_mouse_t button);

bool_t dform_OnExit(DForm *form);

void dform_update_cell_text(DForm *form, const DSelect *sel, const char_t *text);

void dform_draw(const DForm *form, const widget_t swidget, const Image *add_icon, DCtx *ctx);

uint32_t dform_selpath_size(const DForm *form);

const char_t *dform_selpath_caption(const DForm *form, const uint32_t col, const uint32_t row);
