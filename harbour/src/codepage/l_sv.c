/*
 * $Id$
 */

/* Klas Engwall <klas dot engwall at engwall dot com> */

/* NOTE: Following collations contains extra characters
         compared to pure Swedish alphabet.
         Quoting Klas:
         "the philosophy behind the extended collation strings [...]
         was to allow *all* alphabetical characters provided by the
         [Windows ANSI] codepage, including those not normally used
         in Swedish words and names, to be sorted according to their
         alphabetical context rather than an arbitrary Asc() value
         - as suggested by the Swedish Language Council."
 */

#define HB_CP_UPPER     "A~Á~À~Â~ÃBC~ÇD~ÐE~É~È~Ê~ËFGHI~Í~Ì~Î~ÏJKLMN~ÑO~Ó~Ò~Ô~ÕPQRS~ŠTU~Ú~Ù~ÛV~WXY~Ý~Ÿ~ÜZ~ŽÅÄ~ÆÖ~Ø~Œ"
#define HB_CP_LOWER     "a~á~à~â~ãbc~çd~ðe~é~è~ê~ëfghi~í~ì~î~ïjklmn~ño~ó~ò~ô~õpqrs~štu~ú~ù~ûv~wxy~ý~ÿ~üz~žåä~æö~ø~œ"
