/*
    This is part of gtnap
*/

#include "hbgtnap.h"
#include "nappgui.h"

#include "hbapi.h"
#include "hbdate.h"
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

static AREAP pAREA = NULL;
static uint32_t pCurRecord = UINT32_MAX;

/*---------------------------------------------------------------------------*/

static void i_create_columns(TableView *view)
{
    HB_USHORT i, uiFields = 0;
    cassert_no_null(view);
    cassert_no_null(pAREA);
    SELF_FIELDCOUNT(pAREA, &uiFields);
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
static char_t TEMP[256];

static void i_OnTableNotify(void *data, Event *e)
{
    uint32_t etype = event_type(e);
    unref(data);

    switch(etype) {
    case ekEVTBLNROWS:
    {
        uint32_t *n = event_result(e, uint32_t);

        if (pAREA != NULL)
        {
            HB_ULONG ulRecCount = 0;
            SELF_RECCOUNT(pAREA, &ulRecCount);
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

        if (pAREA != NULL)
        {
            const EvTbPos *pos = event_params(e, EvTbPos);
            PHB_ITEM pItem = hb_itemNew( NULL );
            HB_TYPE type = 0;

            if (pCurRecord != pos->row + 1)
            {
                SELF_GOTO(pAREA, (HB_ULONG)(pos->row + 1));
                pCurRecord = pos->row + 1;
            }

            SELF_GETVALUE(pAREA, (HB_USHORT)(pos->col + 1), pItem);
            type = HB_ITEM_TYPE(pItem);
            TEMP[0] = '\0';

            if (type == HB_IT_STRING)
            {
                hb_itemCopyStrUTF8(pItem, TEMP, sizeof(TEMP));
            }
            else if (type == HB_IT_DATE)
            {
                char date[16];
                hb_itemGetDS(pItem, date);
                hb_dateFormat(date, TEMP, "DD/MM/YYYY");
            }
            else if (type == HB_IT_DOUBLE)
            {
                double value = hb_itemGetND(pItem);
                bstd_sprintf(TEMP, sizeof(TEMP), "%12.4f", value);
            }

            cell->text = TEMP;
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

    pAREA = hb_rddGetCurrentWorkAreaPointer();
    if (pAREA != NULL)
    {
        SELF_GOTO(pAREA, 1);
        pCurRecord = 1;
    }

    i_create_columns(view);
    tableview_OnNotify(view, listener(NULL, i_OnTableNotify, void));
    tableview_update(view);
}
