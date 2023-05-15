/*
    This is part of gtnap
    TODO: More info
*/

#include "gui.hxx"

__EXTERN_C

extern Font *hb_gtnap_font(void);

extern uint32_t hb_gtnap_cell_height(void);

//extern String *hb_gtnap_utf8_to_cp(const char_t *cp_str);

extern String *hb_block_to_utf8(HB_ITEM *item);



/* Must be deleted */
const char_t *hb_gtnap_parText(const uint32_t iParam);

/* Must be deleted */
String *hb_gtnap_parstr(const uint32_t iParam);

/* Must be deleted */
extern Window *hb_gtnap_cualib_current_window(void);

__END_C

