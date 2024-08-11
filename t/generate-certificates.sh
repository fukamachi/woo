#!/bin/sh

mkdir t/certs
cd t/certs

openssl genrsa -out localCA.key 2048
openssl req -batch -new -key localCA.key -out localCA.csr \
  -subj "/C=JP/ST=Tokyo/L=Chuo-ku/O=\"Woo\"/OU=Development/CN=localhost"
openssl x509 -req -days 3650 -signkey localCA.key -in localCA.csr -out localCA.crt
openssl x509 -text -noout -in localCA.crt
openssl genrsa -out localhost.key 2048
openssl req -batch -new -key localhost.key -out localhost.csr \
  -subj "/C=JP/ST=Tokyo/L=Chuo-ku/O=\"Woo\"/OU=Development/CN=localhost"
echo 'subjectAltName = DNS:localhost, DNS:localhost.localdomain, IP:127.0.0.1, DNS:app, DNS:app.localdomain' > localhost.csx
openssl x509 -req -days 1825 -CA localCA.crt -CAkey localCA.key -CAcreateserial -in localhost.csr -extfile localhost.csx -out localhost.crt
