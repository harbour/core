//NOTEST
FUNCTION MAIN()
LOCAL a, b

  TEST1() ; TEST2()
  ABS( 4 )

  TEST3()

  TEST4() ; TEST5()
  TEST6()

  TEST7 ;
  ()
//  TEST7 ; () //In Clipper: Incomplete statement or unbalanced delimiters

  TEST8( ;
  )
//  TEST8( ; ) //In Clipper: Incomplete statement or...

  TEST9( a ;
)
//  TEST9( a ; ) //In Clipper: Incomplete statement or...

  TEST10( a, ;
)
//  TEST10( a, ; ) //In Clipper: Incomplete statement or...

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
][ ;
 1 ]

 a :=TEST1(); TEST2()

 a :=TEST1()+ ; TEST2()

RETURN nil

