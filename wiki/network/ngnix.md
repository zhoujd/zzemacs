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
