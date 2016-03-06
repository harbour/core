#!/bin/sh

# Generate RSA keypair with encrypted private key

openssl=openssl

# Generate
$openssl genrsa -aes256 -passout pass:test -out privkey-pkcs1.pem 2048
$openssl pkcs8 -topk8 -inform pem -outform pem -passin pass:test -passout pass:test < privkey-pkcs1.pem > privkey-pkcs8.pem
$openssl rsa -inform PEM -passin pass:test -in privkey-pkcs8.pem -pubout > pubkey.pem

# Generate human-readable
$openssl rsa -inform PEM -passin pass:test -in privkey-pkcs1.pem -noout -text > privkey-pkcs1.pem.txt
$openssl rsa -inform PEM -passin pass:test -in privkey-pkcs8.pem -noout -text > privkey-pkcs8.pem.txt
$openssl rsa -inform PEM -passin pass:test -in pubkey.pem        -noout -text -pubin  > pubkey.pem.txt

cp privkey-pkcs1.pem     privkey.pem
cp privkey-pkcs1.pem.txt privkey.pem.txt
