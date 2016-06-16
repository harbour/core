#!/bin/sh

openssl genrsa -out privatekey.pem 2048
openssl req -new -subj "/C=LT/CN=localhost/O=My Company" -key privatekey.pem -out certrequest.csr
openssl x509 -req -sha256 -days 730 -in certrequest.csr -signkey privatekey.pem -out certificate.pem
openssl x509 -in certificate.pem -text -noout
