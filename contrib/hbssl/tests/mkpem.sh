#!/bin/sh

# Copyright 2016 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.

# Generate RSA keypair with encrypted private key
# Requires: OpenSSL 1.x or upper
#           (install with `brew install openssl` on OS X)

case "$(uname)" in
   *Darwin*) alias openssl=/usr/local/opt/openssl/bin/openssl;;
esac

pass='pass:test'

# Generate
openssl genpkey -algorithm RSA -out privkey.pem -aes-256-cbc -pass "$pass" -pkeyopt rsa_keygen_bits:2048
openssl rsa -passin "$pass" -in privkey.pem -pubout > pubkey.pem

# Generate human-readable
openssl rsa -passin "$pass" -in privkey.pem -noout -text > privkey.pem.txt
openssl rsa -passin "$pass" -in pubkey.pem  -noout -text -pubin > pubkey.pem.txt
