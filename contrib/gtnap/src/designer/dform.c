/* Design form */

#include "dform.h"
#include "dlayout.h"
#include <core/dbind.h>
#include <core/heap.h>
#include <sewer/cassert.h>

struct _dform_t
{
    DLayout *dlayout;
};

/*---------------------------------------------------------------------------*/

DForm *dform_first_example(void)
{
    DForm *form = heap_new0(DForm);
    DLayout *layout1 = dlayout_create(2, 6);
    DLayout *layout2 = dlayout_create(1, 4);
    DLayout *layout3 = dlayout_create(2, 1);
    dlayout_add_layout(layout3, layout1, 0, 0);
    dlayout_add_layout(layout3, layout2, 1, 0);
    form->dlayout = layout1;
    return form;
}

/*---------------------------------------------------------------------------*/

void dform_destroy(DForm **form)
{
    cassert_no_null(form);
    cassert_no_null(*form);
    dlayout_destroy(&(*form)->dlayout);
    heap_delete(form, DForm);
}
