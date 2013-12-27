// Tests for stringify match markers
// Preproces and compile only
PROCEDURE Main()
LOCAL b := ""
MEMVAR a
MEMVAR c
PRIVATE a

#command _REGULAR_(<z>) => ? <z>    //REGULAR
#command _NORMAL_(<z>) => ? <"z">   //NORMAL
#command _SMART_(<z>) => ? <(z)>    //SMART
#command _DUMB_(<z>) => ? #<z>      //DUMB

#command _REGULAR_L(<z,...>) => s( <z> )
#command _NORMAL_L(<z,...>) => s( <"z"> ) //NORMAL
#command _SMART_L(<z,...>) => s( <(z)> )  //SMART
#command _DUMB_L(<z,...>) => s( #<z> )    //DUMB

USE &b ALIAS &a.1 INDEX &a.aa, &b, &c, &b.

? "< > REGULAR"
_REGULAR_(a)
_REGULAR_("a")
_REGULAR_('a')
_REGULAR_(["'a'"])
_REGULAR_(&a.1)
_REGULAR_(&a)
_REGULAR_(&a.)
_REGULAR_(&(a))
_REGULAR_(&a[1])
_REGULAR_(a[1])
_REGULAR_("['']")


? '< " " > NORMAL'
_NORMAL_(a)
_NORMAL_("a")
_NORMAL_('a')
_NORMAL_(["'a'"])
_NORMAL_(&a.1)
_NORMAL_(&a)
_NORMAL_(&a.)
_NORMAL_(&(a))
_NORMAL_(&a[1])
_NORMAL_(a[1])
_NORMAL_("['']")


? '< ( ) > SMART'
_SMART_(a)
_SMART_("a")
_SMART_('a')
_SMART_(["'a'"])
_SMART_(&a.1)
_SMART_(&a)
_SMART_(&a.)
_SMART_(&(a))
_SMART_(&a[1])
_SMART_(a[1])
_SMART_("['']")


? '# < > DUMB'
_DUMB_(a)
_DUMB_("a")
_DUMB_('a')
_DUMB_(["'a'"])
_DUMB_(&a.1)
_DUMB_(&a)
_DUMB_(&a.)
_DUMB_(&(a))
_DUMB_(&a[1])
_DUMB_(a[1])
_DUMB_("['']")


? "< > REGULAR list"
_REGULAR_L(a,"a",'a',["'a'"],"['a']",'["a"]',&a.1,&a,&a.,&a.  ,&(a),&a[1],&a.[1],&a.  [2],&a&a, &a.a)
? "< > NORMAL list"
_NORMAL_L(a,"a",'a',["'a'"],"['a']",'["a"]',&a.1,&a,&a.,&a.  ,&(a),&a[1],&a.[1],&a.  [2],&a&a, &.a, &a.a)
? "< > SMART list"
_SMART_L(a,"a",'a',["'a'"],"['a']",'["a"]',&a.1,&a,&a.,&a.  ,&(a),&a[1],&a.[1],&a.  [2],&a&a, &.a, &a.a)
? "< > DUMB list"
_DUMB_L(a,"a",'a',["'a'"],"['a']",'["a"]',&a.1,&a,&a.,&a.  ,&(a),&a[1],&a.[1],&a.  [2],&a&a, &.a, &a.a)

//a := [[,,]]
//? [[,,]]

RETURN
