/*
 * Harbour Project source code:
 *
 * Rewritten from C: Viktor Szakats (vszakats.net/harbour)
 * www - http://harbour-project.org
 */

PROCEDURE Main()

   ? "SHA-2 FIPS 180-2 Validation tests"
   Test_SHA2()

   ? "HMAC-SHA-2 IETF Validation tests"
   Test_SHA2_HMAC()

   RETURN

STATIC PROCEDURE Test_SHA2()

   LOCAL cMsg1  := "abc"
   LOCAL cMsg2a := "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
   LOCAL cMsg2b := "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
   LOCAL cMsg3  := Replicate( "a", 1000000 )

   LOCAL results := {;
      {; /* SHA-224 */
         "23097d223405d8228642a477bda255b32aadbce4bda0b3f7e36c9da7" ,;
         "75388b16512776cc5dba5da1fd890150b0c6455cb4f58b1952522525" ,;
         "20794655980c91d8bbb4c1ea97618a4bf03f42581948b2ee4ee7ad67" },;
      {; /* SHA-256 */
        "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" ,;
        "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1" ,;
        "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0" },;
      {; /* SHA-384 */
        "cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7" ,;
        "09330c33f71147e83d192fc782cd1b4753111b173b3b05d22fa08086e3b0f712fcc7c71a557e2db966c3e9fa91746039" ,;
        "9d0e1809716474cb086e834e310a4a1ced149e9c00f248527972cec5704c2a5b07b8b3dc38ecc4ebae97ddd87f3d8985" },;
      {; /* SHA-512 */
        "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f" ,;
        "8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa17299aeadb6889018501d289e4900f7e4331b99dec4b5433ac7d329eeb6dd26545e96e55b874be909" ,;
        "e718483d0ce769644e2e42c7bc15b4638e1f98b13b2044285632a803afa973ebde0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b" } }

   ? Lower( hb_SHA224( cMsg1  ) ) == results[ 1 ][ 1 ]
   ? Lower( hb_SHA224( cMsg2a ) ) == results[ 1 ][ 2 ]
   ? Lower( hb_SHA224( cMsg3  ) ) == results[ 1 ][ 3 ]

   ? Lower( hb_SHA256( cMsg1  ) ) == results[ 2 ][ 1 ]
   ? Lower( hb_SHA256( cMsg2a ) ) == results[ 2 ][ 2 ]
   ? Lower( hb_SHA256( cMsg3  ) ) == results[ 2 ][ 3 ]

   ? Lower( hb_SHA384( cMsg1  ) ) == results[ 3 ][ 1 ]
   ? Lower( hb_SHA384( cMsg2b ) ) == results[ 3 ][ 2 ]
   ? Lower( hb_SHA384( cMsg3  ) ) == results[ 3 ][ 3 ]

   ? Lower( hb_SHA512( cMsg1  ) ) == results[ 4 ][ 1 ]
   ? Lower( hb_SHA512( cMsg2b ) ) == results[ 4 ][ 2 ]
   ? Lower( hb_SHA512( cMsg3  ) ) == results[ 4 ][ 3 ]

   RETURN

STATIC PROCEDURE Test_SHA2_HMAC()
   LOCAL aMsg := {;
      "Hi There",;
      "what do ya want for nothing?",;
      Replicate( hb_BChar( 0xdd ), 50 ),;
      Replicate( hb_BChar( 0xcd ), 50 ),;
      "Test With Truncation",;
      "Test Using Larger Than Block-Size Key - Hash Key First",;
      "This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm." }

   LOCAL results := {;
      "896fb1128abbdf196832107cd49df33f47b4b1169912ba4f53684b22",; /* HMAC-SHA-224 */
      "a30e01098bc6dbbf45690f3a7e9e6d0f8bbea2a39e6148008fd05e44",;
      "7fb3cb3588c6c1f6ffa9694d7d6ad2649365b0c1f65d69d1ec8333ea",;
      "6c11506874013cac6a2abc1bb382627cec6a90d86efc012de7afec5a",;
      "0e2aea68a90c8d37c988bcdb9fca6fa8",;
      "95e9a0db962095adaebe9b2d6f0dbce2d499f112f2d2b7273fa6870e",;
      "3a854166ac5d9f023f54d517d0b39dbd946770db9c2b95c9f6f565d1",;
      "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7",; /* HMAC-SHA-256 */
      "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843",;
      "773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe",;
      "82558a389a443c0ea4cc819899f2083a85f0faa3e578f8077a2e3ff46729665b",;
      "a3b6167473100ee06e0c796c2955552b",;
      "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54",;
      "9b09ffa71b942fcb27635fbcd5b0e944bfdc63644f0713938a7f51535c3a35e2",;
      "afd03944d84895626b0825f4ab46907f15f9dadbe4101ec682aa034c7cebc59cfaea9ea9076ede7f4af152e8b2fa9cb6",; /* HMAC-SHA-384 */
      "af45d2e376484031617f78d2b58a6b1b9c7ef464f5a01b47e42ec3736322445e8e2240ca5e69e2c78b3239ecfab21649",;
      "88062608d3e6ad8a0aa2ace014c8a86f0aa635d947ac9febe83ef4e55966144b2a5ab39dc13814b94e3ab6e101a34f27",;
      "3e8a69b7783c25851933ab6290af6ca77a9981480850009cc5577c6e1f573b4e6801dd23c4a7d679ccf8a386c674cffb",;
      "3abf34c3503b2a23a46efc619baef897",;
      "4ece084485813e9088d2c63a041bc5b44f9ef1012a2b588f3cd11f05033ac4c60c2ef6ab4030fe8296248df163f44952",;
      "6617178e941f020d351e2f254e8fd32c602420feb0b8fb9adccebb82461e99c5a678cc31e799176d3860e6110c46523e",;
      "87aa7cdea5ef619d4ff0b4241a1d6cb02379f4e2ce4ec2787ad0b30545e17cdedaa833b7d6b8a702038b274eaea3f4e4be9d914eeb61f1702e696c203a126854",; /* HMAC-SHA-512 */
      "164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea2505549758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737",;
      "fa73b0089d56a284efb0f0756c890be9b1b5dbdd8ee81a3655f83e33b2279d39bf3e848279a722c806b485a47e67c807b946a337bee8942674278859e13292fb",;
      "b0ba465637458c6990e5a8c5f61d4af7e576d97ff94b872de76f8050361ee3dba91ca5c11aa25eb4d679275cc5788063a5f19741120c4f2de2adebeb10a298dd",;
      "415fad6271580a531d4179bc891d87a6",;
      "80b24263c7c1a3ebb71493c1dd7be8b49b46d1f41b4aeec1121b013783f8f3526b56d037e05f2598bd0fd2215d6a1e5295e64f73f63f0aec8b915a985d786598",;
      "e37b6a775dc87dbaa4dfa9f96e5e3ffddebd71f8867289865df5a32d20cdc944b6022cac3c4982b10d5eeb55c3e4de15134676fb6de0446065c97440fa8c6a58"}

   LOCAL keys := {;
      Replicate( hb_BChar( 0x0b ),  20 ),;
      "Jefe",;
      Replicate( hb_BChar( 0xaa ),  20 ),;
      "",;
      Replicate( hb_BChar( 0x0c ),  20 ),;
      Replicate( hb_BChar( 0xaa ), 131 ),;
      Replicate( hb_BChar( 0xaa ), 131 ) }

   LOCAL tmp

   FOR tmp := 1 TO 25
      keys[ 4 ] += hb_BChar( tmp )
   NEXT

   FOR tmp := 1 TO 7
      IF tmp != 5
         ? Lower( hb_HMAC_SHA224( aMsg[ tmp ], keys[ tmp ] ) ) == results[ tmp ]
         ? Lower( hb_HMAC_SHA256( aMsg[ tmp ], keys[ tmp ] ) ) == results[ tmp + 7 ]
         ? Lower( hb_HMAC_SHA384( aMsg[ tmp ], keys[ tmp ] ) ) == results[ tmp + 14 ]
         ? Lower( hb_HMAC_SHA512( aMsg[ tmp ], keys[ tmp ] ) ) == results[ tmp + 21 ]
/* We don't support these MAC sizes */
/*    ELSE
         mac_224_size = 128 / 8;
         mac_256_size = 128 / 8;
         mac_384_size = 128 / 8;
         mac_512_size = 128 / 8; */
      ENDIF
   NEXT

   RETURN
