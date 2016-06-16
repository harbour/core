//NOTEST

PROCEDURE Main()
LOCAL a, b

  TEST1() ; TEST2()
  Abs( 4 )

  TEST3()

  TEST4() ; TEST5()
  TEST6()

  TEST7 ;
  ()
//  TEST7 ; () // In Clipper: Incomplete statement or unbalanced delimiters

  TEST8( ;
  )
//  TEST8( ; ) // In Clipper: Incomplete statement or...

  TEST9( a ;
)
//  TEST9( a ; ) // In Clipper: Incomplete statement or...

  TEST10( a, ;
)
//  TEST10( a, ; ) // In Clipper: Incomplete statement or...

  TEST11( a, b ; //////////test
)

  a ;
:=b

  a :=;
b

  a ;
= ;
b

  a ;
+= ;
b

  TEST12( a := ;
b ;
)

  a :=b[ 1 ;
]
  a :=b[ 1 ; ]

  a :=b[ ;
1 , 2 ;
]
  a :=b[ ; 1, 2 ; ]

  a :=TEST13()[ 1 ]
  a :=TEST13()[ ;
 1 ]

  a :=b[ ;
1;
,;
2;
];
[ ;
 3 ]

 a :=TEST1(); TEST2()

// a :=TEST1()+ ; TEST2() // In Clipper: Incomplete statement ...

 a :=;
; /////// comment
; /* another comment */
;
 55

RETURN

FUNCTION TEST1() ; LOCAL n; IF n==5; n =4; END; RETURN n

FUNCTION TEST2(); LOCAL n

   WHILE n<5; n++; END
   RETURN n
