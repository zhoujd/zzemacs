RabbitMQ
========

1. Install RabbitMQ on Ubuntu 20.04 LTS
   https://idroot.us/install-rabbitmq-ubuntu-20-04/

        $ sudo apt update
        $ sudo apt install erlang
        $ sudo apt install rabbitmq-server
        $ sudo systemctl enable rabbitmq-server
        $ sudo systemctl start rabbitmq-server
        $ sudo rabbitmq-plugins enable rabbitmq_management
        $ sudo ufw allow proto tcp from any to any port 5672,15672

        ## Input guest/guest for http://localhost:15672
        $ rabbitmqctl add_user admin StrongPassword
        $ rabbitmqctl set_user_tags admin administrator

        $ sudo usermod -aG rabbitmq $USER
