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
