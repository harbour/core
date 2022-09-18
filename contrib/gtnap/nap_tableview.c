/*
    This is part of gtnap
*/

#include "hbgtnap.h"
#include "nappgui.h"

#include "hbapi.h"
#include "hbdate.h"

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
static void i_create_columns(TableView *view, GtNapArea *area)
{
    HB_USHORT i, uiFields = 0;
    cassert_no_null(view);
    cassert_no_null(area);
    cassert_no_null(area->area);
    SELF_FIELDCOUNT(area->area, &uiFields);
    //uiFields = 5;
    for (i = 0; i < uiFields; ++i)
    {
        char_t name[128];
        name[0] = '\0';
        //SELF_FIELDNAME(pAREA, 0, name);
        //strcpy(name,)
        tableview_new_column_text(view);
        tableview_column_width(view, i, 100);
        tableview_header_title(view, i, "TITILE"/*name*/);


        //  char * szName = ( char * ) hb_xgrab( pArea->uiMaxFieldNameLength + 1 );
        //  szName[ 0 ] = '\0';
        //  SELF_FIELDNAME( pArea, uiIndex, szName );
        //  hb_retc_buffer( szName );
        //  return;


    }

//    if( pArea )
//    hb_retni( uiFields );
}

/*---------------------------------------------------------------------------*/

static void i_OnTableNotify(GtNapArea *area, Event *e)
{
    uint32_t etype = event_type(e);
    cassert_no_null(area);

    switch(etype) {
    case ekEVTBLNROWS:
    {
        uint32_t *n = event_result(e, uint32_t);

        if (area->area != NULL)
        {
            HB_ULONG ulRecCount = 0;
            SELF_RECCOUNT(area->area, &ulRecCount);
            *n = (uint32_t)ulRecCount;
        }
        else
        {
            *n = 0;
        }
        break;
    }

    case ekEVTBLCELL:
    {
        EvTbCell *cell = event_result(e, EvTbCell);

        if (area->area != NULL)
        {
            const EvTbPos *pos = event_params(e, EvTbPos);
            PHB_ITEM pItem = hb_itemNew( NULL );
            HB_TYPE type = 0;

            if (area->currow != pos->row + 1)
            {
                SELF_GOTO(area->area, (HB_ULONG)(pos->row + 1));
                area->currow = pos->row + 1;
            }

            SELF_GETVALUE(area->area, (HB_USHORT)(pos->col + 1), pItem);
            type = HB_ITEM_TYPE(pItem);
            area->temp[0] = '\0';

            if (type == HB_IT_STRING)
            {
                hb_itemCopyStrUTF8(pItem, area->temp, sizeof(area->temp));
            }
            else if (type == HB_IT_DATE)
            {
                char date[16];
                hb_itemGetDS(pItem, date);
                hb_dateFormat(date, area->temp, "DD/MM/YYYY");
            }
            else if (type == HB_IT_DOUBLE)
            {
                double value = hb_itemGetND(pItem);
                bstd_sprintf(area->temp, sizeof(area->temp), "%12.4f", value);
            }

            cell->text = area->temp;
            hb_itemRelease(pItem);
        }
        else
        {
            cell->text = "";
        }

        break;
    }

    cassert_default();
    }
}

/*---------------------------------------------------------------------------*/

HB_FUNC( NAP_TABLEVIEW_BIND_DB )
{
    TableView *view = (TableView*)hb_parptr(1);
    GtNapArea *area = hb_gtnap_new_area();

    area->area = hb_rddGetCurrentWorkAreaPointer();
    area->view = view;

    if (area->area != NULL)
    {
        SELF_GOTO(area->area, 1);
        area->currow = 1;
    }

    i_create_columns(view, area);
    tableview_OnNotify(view, listener(area, i_OnTableNotify, GtNapArea));
    tableview_update(view);
}
