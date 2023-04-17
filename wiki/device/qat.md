QAT
===

## URLs

    ## https://developer.intel.com/quickassist
    ## https://github.com/intel/QAT_Engine
    ## https://github.com/intel/asynch_mode_nginx
    ## https://01.org/sites/default/files/downloads/336210qatswpg-013.pdf


## qatlib

    ## https://github.com/intel/qatlib
    ## libqat: user space library for QAT devices exposed via the vfio kernel driver
    ## libusdm: user space library for memory management
    ## qatmgr: user space daemon for device management
    ## Sample codes: applications to demo usage of the libs

## Installation and usage

    ## https://www.cnblogs.com/-xuan/p/14595856.html
    ## 1 Introduction
    ## https://01.org/intel-quickassist-technology
    ## https://01.org/sites/default/files/downloads/intelr-quickassist-technology/intelquickassisttechnologyopensslperformance.pdf
    ## http://www.intel.com/content/dam/www/public/us/en/documents/product-briefs/quickassist-adapter-8950-brief.pdf
    ## https://01.org/sites/default/files/downloads//337020-003-qatwcontaineranddocker.pdf

    ## 2 Install Software
    ## 2.1 Install QAT driver
    export ICP_ROOT=/opt/QAT
    mkdir /opt/QAT
    cd /opt/QAT
    wget https://downloadmirror.intel.com/30178/eng/QAT1.7.L.4.13.0-00009.tar.gz
    tar xf QAT1.7.L.4.13.0-00009.tar.gz
    ./configure
    make -j 40
    make install
    service qat_service status
    cpa_sample_code runTests=2

    ## 2.2 Install OpenSSL
    git clone https://github.com/openssl/openssl.git
    cd openssl/
    git checkout OpenSSL_1_1_1
    ./config --prefix=/usr/local/ssl -Wl,-rpath,/usr/local/ssl/lib
    make -j 40
    make install

    ## 2.3 Install QAT_engine
    git clone https://github.com/intel/QAT_Engine.git
    cd QAT_Engine/qat_contig_mem
    make    
    ## error: dereferencing pointer to incomplete type ‘struct task_struct’ 
    ## refer: http://www.voidcn.com/article/p-pwrzhtun-em.html
    vim qat_contig_mem.c
    #include <linux/sched.h>
    make load
    make test
    ……
    Hello world!
    ……

    cd ..
    ./autogen.sh
    ./configure --with-qat_hw-dir=/opt/QAT --with-openssl_install_dir=/usr/local/ssl

    ## 2.4 Install QATzip
    git clone https://github.com/intel/QATzip.git
    cd QATzip/
    ./configure --with-ICP_ROOT=$ICP_ROOT
    make clean
    make all install
    service qat_service restart

    ## 2.5 nginx + qat module
    git clone https://github.com/intel/asynch_mode_nginx.git
    cd asynch_mode_nginx/
    ./configure --prefix=/usr/local/nginx --conf-path=/etc/nginx/nginx.conf --without-http_rewrite_module --with-http_ssl_module --with-http_stub_status_module --with-http_v2_module --with-stream --with-stream_ssl_module --add-dynamic-module=modules/nginx_qatzip_module --add-dynamic-module=modules/nginx_qat_module/ --with-cc-opt="-DNGX_SECURE_MEM -I$OPENSSL_LIB/include -I$QZ_ROOT/include -I$ICP_ROOT/quickassist/include -I$ICP_ROOT/quickassist/include/dc -Wno-error=deprecated-declarations" --with-ld-opt="-Wl,-rpath=$OPENSSL_LIB/lib -L$OPENSSL_LIB/lib -L$QZ_ROOT/src -lqatzip -lz"

    ## Configure nginx
    cp /root/QAT_Engine/qat/config/dh895xcc/multi_process_optimized/dh895xcc_dev0.conf  /etc
    service qat_service restart

    # nginx config file
    events {
        worker_connections  102400;
        use epoll;
        accept_mutex off;
    }

    ssl_engine {
        use_engine qatengine;
        default_algorithms RSA,EC,DH,PKEY_CRYPTO;
        qat_engine {
            qat_offload_mode async;
            qat_notify_mode poll;
            qat_poll_mode heuristic;
            qat_sw_fallback on;
        }
    }

    http{
        server {
            listen      80;
            listen      443 ssl backlog=65534 reuseport deferred rcvbuf=8m sndbuf=8m asynch;  # add asynch
            server_name test.example.com;
            ssl_certificate     cert.pem;
            ssl_certificate_key cert_key.key;
            ssl_protocols TLSv1 TLSv1.1 TLSv1.2;
            ssl_ciphers ECDHE-RSA-AES256-SHA384:AES256-SHA256:RC4:HIGH:!MD5:!aNULL:!eNULL:!NULL:!DH:!EDH:!AESGCM;
            ssl_session_cache    shared:SSL_WS2:500m;
            ssl_session_timeout  10m;
            ssl_prefer_server_ciphers   on;

            #ssl_async  on;
            proxy_read_timeout 10;
            proxy_send_timeout 10;
            proxy_connect_timeout 10;

            add_header  Access-Control-Allow-Origin *;
            add_header  Access-Control-Allow-Methods HEAD,OPTIONS,GET,POST,PUT,DELETE;
            add_header  Access-Control-Allow-Headers Content-Type,Server,Date,Content-Length,Cache-Control,Keep-Alive,Connection,X-Requested-With,X-File-Name,Origin,Accept,X-CSRFToken;
            add_header  Access-Control-Max-Age 1728000;


            location / {
                expires off;
                proxy_cache off;
                proxy_http_version 1.1;
                proxy_set_header Connection "";
                proxy_next_upstream error non_idempotent;
                proxy_next_upstream_tries 4;
                proxy_next_upstream_timeout 10s;
                proxy_pass_header server;
                proxy_set_header host $host;
                proxy_redirect off;
                proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                proxy_set_header x-real-ip $remote_addr;
                proxy_set_header x-scheme $scheme;
                root /usr/share/nginx/html;
            }
        }
    }


## K8s QAT Device Plugin

    ## https://github.com/intel/intel-device-plugins-for-kubernetes#qat-device-plugin
    ## https://github.com/intel/intel-device-plugins-for-kubernetes/blob/main/demo/readme.md#intel-quickassist-technology-device-plugin-with-dpdk-demo-video
    ## Intel® QAT Device Plugin with DPDK:
    ## https://asciinema.org/a/PoWOz4q2lX4AF4K9A2AV1RtSA

## DSA & RSA

    ## ssh1 use RSA, ssh2 (latest) use DSA
    $ ssh-keygen -t rsa
    $ ssh-keygen -t dsa

    ## RSA: Public key is open, encode with PUBLIC key, private key only use decoce
    $ openssl genrsa -out private.key 1024
    $ openssl rsa -in private.key -pubout -out pub.key   ## get public key
    $ echo -n "123456" | openssl rsautl -encrypt -inkey pub.key -pubin
    $ cat encode.result | openssl rsautl -decrypt-inkey private.key

    ## DSA: only for digital sign (not for encode/decode/(exchange keys))
    $ openssl dsaparam -out dsaparam.pem 1024
    $ openssl gendsa -out privkey.pem dsaparam.pem
    ## DSA generate public key
    $ openssl dsa -in privkey.pem -out pubkey.pem -pubout
    $ rm -fr dsaparam.pem
    ## use private key to sign
    $ echo -n "123456" | openssl dgst -dss1 -sign privkey.pemsign.result
    ## use public key to verify
    $ echo -n "123456"| openssl dgst -dss1 -verify pubkey.pem -signature sign.result

    ## base64 & urlencode
    $ openssl enc -base64 -A
    $ openssl enc -d -base64 -A
