/*
 * $Id$
 */

// ; Donated to the public domain by Victor Szakats <info@szelvesz.hu>

REQUEST HB_LANG_HU852

func main()

? "Prev:", hb_langselect()
? hb_langName()
? NationMsg( 1 )
? CMonth( Date() )
? CDOW( Date() )
? "---------"

? "Prev:", hb_langSelect( "HU852" )
? hb_langName()
? NationMsg( 1 )
? CMonth( Date() )
? CDOW( Date() )
? "---------"

? "Prev:", hb_langSelect( "NOTHERE" )
? hb_langName()
? NationMsg( 1 )
? CMonth( Date() )
? CDOW( Date() )
? "---------"

return nil

