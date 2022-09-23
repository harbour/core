/*
    This is part of gtnap
*/

#include "gtnap.h"
#include "nappgui.h"

#include "hbapi.h"
#include "hbdate.h"
#include "hbapistr.h"
#include "hbapiitm.h"
#include "hbapigt.h"
#include "hbapirdd.h"

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
static void i_create_columns(TableView *view, GtNapArea *gtarea)
{
    AREA *area = (AREA*)hb_gtnap_area(gtarea);
    HB_USHORT i, uiFields = 0;
    cassert_no_null(view);
    cassert_no_null(area);
    SELF_FIELDCOUNT(area, &uiFields);
    //uiFields = 5;
    for (i = 0; i < uiFields; ++i)
    {
        // char_t name[128];
        // name[0] = '\0';
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

static void i_OnTableNotify(GtNapArea *gtarea, Event *e)
{
    uint32_t etype = event_type(e);
    AREA *area = (AREA*)hb_gtnap_area(gtarea);

    switch(etype) {
    case ekEVTBLNROWS:
    {
        uint32_t *n = event_result(e, uint32_t);

        if (area != NULL)
        {
            HB_ULONG ulRecCount = 0;
            SELF_RECCOUNT(area, &ulRecCount);
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

        if (area != NULL)
        {
            const EvTbPos *pos = event_params(e, EvTbPos);
            uint32_t temp_size = 0;
            char_t *temp = hb_gtnap_area_temp(gtarea, &temp_size);
            PHB_ITEM pItem = hb_itemNew( NULL );
            HB_TYPE type = 0;

            hb_gtnap_area_set_row(gtarea, pos->row + 1);
            SELF_GETVALUE(area, (HB_USHORT)(pos->col + 1), pItem);
            type = HB_ITEM_TYPE(pItem);
            temp[0] = '\0';

            if (type == HB_IT_STRING)
            {
                hb_itemCopyStrUTF8(pItem, temp, temp_size);
            }
            else if (type == HB_IT_DATE)
            {
                char date[16];
                hb_itemGetDS(pItem, date);
                hb_dateFormat(date, temp, "DD/MM/YYYY");
            }
            else if (type == HB_IT_DOUBLE)
            {
                double value = hb_itemGetND(pItem);
                bstd_sprintf(temp, temp_size, "%12.4f", value);
            }

            cell->text = temp;
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
    GtNapArea *gtarea = hb_gtnap_new_area(view);
    i_create_columns(view, gtarea);
    tableview_OnNotify(view, listener(gtarea, i_OnTableNotify, GtNapArea));
    tableview_update(view);
}
