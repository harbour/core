/*
 * $Id$
 */

// ; Donated to the public domain by Victor Szakats <info@szelvesz.hu>

REQUEST HB_LANG_HU852

func main()

// hb_langselect( "EN" )

? NationMsg( 1 )
? CMonth( Date() )
? CDOW( Date() )
? "---------"

hb_langSelect( "HU852" )

? NationMsg( 1 )
? CMonth( Date() )
? CDOW( Date() )
? "---------"

hb_langSelect( "NOTHERE" )

? NationMsg( 1 )
? CMonth( Date() )
? CDOW( Date() )
? "---------"

return nil

