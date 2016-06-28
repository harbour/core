/* Copyright 2015 Przemyslaw Czerpak (druzus/at/poczta.onet.pl) */

#require "hbssl"

#define N_PORT          4001
#define EOL             e"\r\n"
#define PEM_CERT_FILE   "inetssl.pem"

STATIC s_lReady := .F.
STATIC s_lStop := .F.

STATIC s_lDelaySrv := .F.
STATIC s_lDelayCli := .F.

#if ! defined( __HBSCRIPT__HBSHELL )
REQUEST HB_MT  /* Require this to be built with '-mt' option */
#endif

PROCEDURE Main( delay )

   LOCAL thrd

   IF HB_ISSTRING( delay )
      s_lDelayCli := "C" $ Upper( delay )
      s_lDelaySrv := "S" $ Upper( delay )
   ENDIF

   /* initialize SSL library */
   SSL_init()
   RAND_seed( Time() + hb_TSToStr( hb_DateTime() ) + hb_DirBase() + NetName() )

   /* start server thread */
   IF Empty( thrd := hb_threadStart( @Server() ) )
      ? "Cannot start thread."
      RETURN
   ENDIF

   /* wait for server being ready to accept incoming connections */
   DO WHILE ! s_lReady
      hb_idleSleep( 0.01 )
   ENDDO

   /* start client */
   Client()

   /* inform server it should finish and wait for it */
   s_lStop := .T.
   hb_threadJoin( thrd )
   ?

   RETURN

STATIC PROCEDURE Client()

   LOCAL sock, ssl_ctx, ssl, nResult, nErr, cLine

   ssl_ctx := SSL_CTX_new()
   ssl := SSL_new( ssl_ctx )

   sock := hb_inetCreate()
   hb_inetTimeout( sock, 5000 )

   ? "CLIENT: connecting..."
   IF Empty( hb_inetConnectIP( "localhost", N_PORT, sock ) )
      ? "CLIENT: cannot connect to server."
   ELSE
      ? "CLIENT: connected to the server."
      hb_inetTimeout( sock, 3000 )

      IF s_lDelayCli
         ? "CLIENT: waiting..."
         hb_idleSleep( 1 )
      ENDIF

      ? "CLIENT: SSL CONNECT..."
      nResult := hb_inetSSL_CONNECT( sock, ssl )
      nErr := ERR_get_error()
      ?? hb_StrFormat( e"\nCLIENT: hb_inetSSL_CONNECT()=>%d (%d), '%s'\n", ;
                       nResult, nErr, ;
                       ERR_error_string( nErr ) )
      IF nResult == 1
         ? "CLIENT: connected with", SSL_get_cipher( ssl ), "encryption."
         DispCertInfo( ssl, "CLIENT:" )

         hb_inetSendAll( sock, hb_TSToStr( hb_DateTime() ) + EOL )
         DO WHILE ! HB_ISNULL( cLine := hb_inetRecvLine( sock ) )
            ? "CLIENT: RECV:", hb_ValToExp( cLine )
         ENDDO
      ENDIF
   ENDIF

   hb_inetClose( sock )

   RETURN

STATIC PROCEDURE Server()

   LOCAL sockSrv, sockConn, ssl_ctx, ssl, nResult, nErr, cLine

   ? "SERVER: create listen socket..."
   IF Empty( sockSrv := hb_inetServer( N_PORT ) )
      ? "SERVER: cannot create listen socket."
   ELSE
      ? "SERVER: loading certificates..."
      ssl_ctx := SSL_CTX_new()
      LoadCertificates( ssl_ctx, PEM_CERT_FILE, PEM_CERT_FILE )
      ssl := SSL_new( ssl_ctx )

      ? "SERVER: waiting for connections..."
      hb_inetTimeout( sockSrv, 100 )
      s_lReady := .T.
      DO WHILE ! s_lStop
         IF ! Empty( sockConn := hb_inetAccept( sockSrv ) )
            ? "SERVER: accepted new connection."
            hb_inetTimeout( sockConn, 3000 )

            IF s_lDelaySrv
               ? "SERVER: waiting..."
               hb_idleSleep( 1 )
            ENDIF

            ? "SERVER: SSL ACCEPT..."
            nResult := hb_inetSSL_ACCEPT( sockConn, ssl )
            nErr := ERR_get_error()
            ?? hb_StrFormat( e"\nSERVER: hb_inetSSL_ACCEPT()=>%d (%d), '%s'\n", ;
                             nResult, nErr, ;
                             ERR_error_string( nErr ) )

            IF nResult == 1
               cLine := hb_inetRecvLine( sockConn )
               ? "SERVER: RECV:", hb_ValToExp( cLine )
               hb_inetSendAll( sockConn, ;
                               "ECHO[ " + cLine + " ]" + EOL + ;
                               hb_TSToStr( hb_DateTime() ) + EOL + ;
                               OS() + EOL + ;
                               Version() + EOL + ;
                               EOL )
            ENDIF

            hb_inetClose( sockConn )
            sockConn := nil
         ENDIF
      ENDDO

      s_lReady := .F.
   ENDIF

   RETURN

STATIC PROCEDURE LoadCertificates( ssl_ctx, cCertFile, cKeyFile )

   /* Server using hb_inetSSL_ACCEPT() needs certificates,
      they can be generated using the following command:
         openssl req -x509 -nodes -days 365 -newkey rsa:2048 \
                 -sha256 -out <cCertFile> -keyout <cKeyFile>
    */
   IF ! hb_vfExists( cCertFile ) .AND. ! hb_vfExists( cKeyFile )
      ? "SERVER: generating certificates..."
      hb_run( "openssl req -x509 -nodes -days 365 -newkey rsa:2048 " + ;
              "-sha256 -out " + cCertFile + " -keyout " + cKeyFile )
   ENDIF

   /* set the local certificate from CertFile */
   IF SSL_CTX_use_certificate_file( ssl_ctx, cCertFile, HB_SSL_FILETYPE_PEM ) <= 0
      OutErr( hb_StrFormat( e"SERVER: SSL_CTX_use_certificate_file()=> '%s'\n", ;
                            ERR_error_string( ERR_get_error() ) ) )
      QUIT
   ENDIF

   /* set the private key from KeyFile (may be the same as CertFile) */
   IF SSL_CTX_use_PrivateKey_file( ssl_ctx, cKeyFile, HB_SSL_FILETYPE_PEM ) <= 0
      OutErr( hb_StrFormat( e"SERVER: SSL_CTX_use_PrivateKey_file()=> '%s'\n", ;
                            ERR_error_string( ERR_get_error() ) ) )
      QUIT
   ENDIF

   /* verify private key */
   IF ! SSL_CTX_check_private_key( ssl_ctx ) == 1
      OutErr( e"SERVER: Private key does not match the public certificate\n" )
      QUIT
   ENDIF

   RETURN

STATIC PROCEDURE DispCertInfo( ssl, cWho )

   LOCAL cert

   IF Empty( cert := SSL_get_peer_certificate( ssl ) )
      ? cWho, "No certificates."
   ELSE
      ? cWho, "Server certificates:"
      ? cWho, "Subject:", X509_name_oneline( X509_get_subject_name( cert ), 0, 0 )
      ? cWho, "Issuer:", X509_name_oneline( X509_get_issuer_name( cert ), 0, 0 )
   ENDIF

   RETURN
