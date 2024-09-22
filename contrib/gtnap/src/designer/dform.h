/* Design form */

#include "designer.hxx"

DForm *dform_first_example(void);

void dform_destroy(DForm **form);

Panel *dform_panel(DForm *form);

void dform_synchro_visual(DForm *form);

void dform_draw(const DForm *form, DCtx *ctx);

