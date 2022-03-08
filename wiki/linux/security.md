Security
========

## How to create a self-signed x509 certificate with both private and public keys?

    ## https://stackoverflow.com/questions/14464441/how-to-create-a-self-signed-x509-certificate-with-both-private-and-public-keys
    $ sudo apt install openssl
    $ sudo yum install openssl
    ## Generate private key
    $ openssl genrsa -out server.pem 2048
    ## Generate CSR: (In the "Common Name" set the domain of your service provider app)
    $ openssl req -new -key server.pem -out server.csr
    ## Generate Self Signed Cert
    $ openssl x509 -req -days 365 -in server.csr -signkey server.pem -out server.crt
    $ openssl req -new -x509 -key privateKey.pem -out cert.cer -days 365

## Generating a certificate and private key

    ## private key and self-signed certificate
    $ openssl req -x509 -sha256 -nodes -days 365 -newkey rsa:2048 -keyout privateKey.key -out certificate.crt

## Validate your P2 file

    ## Generate your private key and public certificate. Answer the questions and enter the Common Name when prompted.
    $ openssl req -newkey rsa:2048 -nodes -keyout key.pem -x509 -days 365 -out certificate.pem
    ## Review the created certificate
    $ openssl x509 -text -noout -in certificate.pem
    ## Combine your key and certificate in a PKCS#12 (P12) bundle
    $ openssl pkcs12 -inkey key.pem -in certificate.pem -export -out certificate.p12
    ## Validate your P2 file
    $ openssl pkcs12 -in certificate.p12 -noout -info
