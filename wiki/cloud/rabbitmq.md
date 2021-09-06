RabbitMQ
========

## Install RabbitMQ on Ubuntu 20.04 LTS

    ## https://idroot.us/install-rabbitmq-ubuntu-20-04/
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
    $ rabbitmqctl set_permissions -p / admin ".*" ".*" ".*"

    $ sudo usermod -aG rabbitmq $USER

## Install Latest RabbitMQ Server on Ubuntu 20.04 LTS

    ## https://computingforgeeks.com/how-to-install-latest-rabbitmq-server-on-ubuntu-linux/
    ## RabbitMQ User Management Commands
    ## Delete User:
    $ rabbitmqctl delete_user user

    ## Change User Password:
    $ rabbitmqctl change_password user strongpassword

    ## Create new Virtualhost:
    $ rabbitmqctl add_vhost /my_vhost

    ## List available Virtualhosts:
    $ rabbitmqctl list_vhosts

    ## Delete a virtualhost:
    $ rabbitmqctl delete_vhost /myvhost

    ## Grant user permissions for vhost:
    $ rabbitmqctl set_permissions -p /myvhost user ".*" ".*" ".*"

    ## List vhost permissions:
    $ rabbitmqctl list_permissions -p /myvhost

    ## To list user permissions:
    $ rabbitmqctl list_user_permissions user

    ## Delete user permissions:
    $ rabbitmqctl clear_permissions -p /myvhost user

## Tutorials "Hello World!"

    ## https://www.rabbitmq.com/tutorials/tutorial-one-go.html
    ## https://www.rabbitmq.com/tutorials/tutorial-one-python.html
