/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "nappgui.h"

#include "hbapi.h"

//
//#include "hbapiitm.h"
//#include "hbapigt.h"
//#include "hbapirdd.h"

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_CREATE )
{
    TableView *view = tableview_create();
    hb_retptr(view);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_SIZE )
{
    TableView *view = (TableView*)hb_parptr(1);
    real32_t width = (real32_t)hb_parnd(2);
    real32_t height = (real32_t)hb_parnd(3);
    tableview_size(view, s2df(width, height));
}

/*---------------------------------------------------------------------------*/

// TODO: Configure columns
// static void i_create_columns(TableView *view, GtNapArea *gtarea)
// {
//     AREA *area = (AREA*)hb_gtnap_area(gtarea);
//     cassert_no_null(view);

//     if (area != NULL)
//     {

//         HB_USHORT i, uiFields = 0;

//         SELF_FIELDCOUNT(area, &uiFields);
//         //uiFields = 5;
//         for (i = 0; i < uiFields; ++i)
//         {
//             char_t name[128];

//             char * szName = ( char * ) hb_xgrab( area->uiMaxFieldNameLength + 1 );
//             name[0] = '\0';
//             szName[ 0 ] = '\0';
//             log_printf("PREV NAME: %s", name);
//             SELF_FIELDNAME( area, i, szName );
//             //  hb_retc_buffer( szName );
//             //  return;
//             log_printf("NAME: %s", "HELLO");

//             //log_printf("PREV NAME: %s", name);
//             //SELF_FIELDNAME(area, i, name);
//             //strcpy(name,)
//             //log_printf("NAME: %s", name);
//             tableview_new_column_text(view);
//             tableview_column_width(view, i, 100);
//             tableview_header_title(view, i, "TITLE");


//             //  char * szName = ( char * ) hb_xgrab( pArea->uiMaxFieldNameLength + 1 );
//             //  szName[ 0 ] = '\0';
//             //  SELF_FIELDNAME( pArea, uiIndex, szName );
//             //  hb_retc_buffer( szName );
//             //  return;


//         }

//     //    if( pArea )
//     //    hb_retni( uiFields );
//     }
// }

/*---------------------------------------------------------------------------*/

static void i_OnTableNotify(GtNapArea *gtarea, Event *e)
{
    uint32_t etype = event_type(e);

    switch(etype) {
    case ekEVTBLNROWS:
    {
        uint32_t *n = event_result(e, uint32_t);
        *n = hb_gtnap_area_row_count(gtarea);
        break;
    }

    case ekEVTBLCELL:
    {
        EvTbCell *cell = event_result(e, EvTbCell);
        const EvTbPos *pos = event_params(e, EvTbPos);
        cell->text = hb_gtnap_area_eval_field(gtarea, pos->col + 1, pos->row + 1);
        break;
    }
    }
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_BIND_DB )
{
    TableView *view = (TableView*)hb_parptr(1);
    GtNapArea *gtarea = hb_gtnap_new_area(view);
    //i_create_columns(view, gtarea);
    tableview_OnNotify(view, listener(gtarea, i_OnTableNotify, GtNapArea));
    //tableview_update(view);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_COLUMN_DB )
{
    TableView *view = (TableView*)hb_parptr(1);
    const char_t *title = hb_gtnap_parText(2);
    real32_t width = (real32_t)hb_parnd(3);
    PHB_ITEM codeBlock = hb_param(4, HB_IT_BLOCK);
    GtNapArea *gtarea = hb_gtnap_get_area(view);
    hb_gtnap_area_add_column(gtarea, title, width, codeBlock);
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_UPDATE_DB )
{
    TableView *view = (TableView*)hb_parptr(1);
    tableview_update(view);
}
