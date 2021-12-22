Nginx
=====

## URLs

    ## https://github.com/SimulatedGREG/nginx-cheatsheet
    ## https://linuxiac.com/reverse-proxy-with-nginx/

## Reverse Proxy with Nginx: A Step-by-Step Setup Guide

    ## Step 1: Installing Nginx
    $ sudo apt install nginx
    $ sudo dnf install nginx
    $ sudo pacman -S nginx
    $ nginx -v

    ## Step 2: Configuring Nginx
    $ unlink /etc/nginx/sites-enabled/default
    $ cd /etc/nginx/sites-available
    ## Nginx will redirect all incoming connections for blog.example.com on port 80 to the 127.0.1.10 server, listening on port 80
    $ sudo vim example.conf
    server {
        listen 80;
        server_name example.com www.example.com;

        location / {
                try_files $uri $uri/ /index.php?$query_string;
        }
    }

    server {
       listen 80;
       server_name blog.example.com;

       location / {
               proxy_pass http://127.0.1.10:80;
       }
    }
    $ sudo ln -s /etc/nginx/sites-available/example.conf /etc/nginx/sites-enabled/
    $ sudo nginx -t
    nginx: the configuration file /etc/nginx/nginx.conf syntax is ok
    nginx: configuration file /etc/nginx/nginx.conf test is successful
    $ sudo systemctl restart nginx

## Redirect on specific URL

    ## Can be permanent (301) or temporary (302).
    server {
      listen 80;
      server_name yourdomain.com;

      location /redirect-url {
        return 301 http://otherdomain.com;
      }
    }

## Reverse Proxy

    ## Useful for Node.js applications like express.
    ## Basic
    server {
      listen 80;
      server_name yourdomain.com;

      location / {
        proxy_pass http://0.0.0.0:3000;
        # where 0.0.0.0:3000 is your Node.js Server bound on 0.0.0.0 listing on port 3000
      }
    }
    ## Basic+
    upstream node_js {
      server 0.0.0.0:3000;
      # where 0.0.0.0:3000 is your Node.js Server bound on 0.0.0.0 listing on port 3000
    }

    server {
      listen 80;
      server_name yourdomain.com;

      location / {
        proxy_pass http://node_js;
      }
    }

## Nginx forward proxy http/https and proxy mail service

    ## https://programmer.group/nginx-forward-proxy-http-https-and-proxy-mail-service.html
    ## https://github.com/chobits/ngx_http_proxy_connect_module
    ## Nginx itself does not support https protocol request forwarding.
    ## In order for nginx to achieve this effect, need to use the third-party module ngx_http_proxy_connect_module.
    server {

        resolver 114.114.114.114;  #DNS resolution address
        listen 10080;              #Monitor address
        resolver_timeout 10s;      #Timeout
        proxy_connect;             #Enable connection http method support
        proxy_connect_allow            443 563;  #Ports that agents can connect to
        proxy_connect_connect_timeout  10s;      #Agent connection time out
        proxy_connect_read_timeout     10s;
        proxy_connect_send_timeout     10s;
        access_log  /weblogs/nginx/proxy.access.log;
        error_log   /weblogs/nginx /proxy.error.log;

        location / {
            proxy_pass $scheme://$http_host$request_uri;
            proxy_set_header Host $http_host;

            proxy_buffers 256 4k;
            proxy_max_temp_file_size 0;

            proxy_connect_timeout 30s;

            #allow 127.0.0.1;  #ip restrictions
            #deny all ;
        }
    }

    ## Test command (on the client)
    $ curl -I http://www.baidu.com -v -x 10.10.11.93:10080
    $ curl -I https://www.baidu.com -v -x 10.10.11.93:10080
