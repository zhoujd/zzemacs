Docker Swarm
============

## Remove service

    $ docker stack rm hub

## How to disable docker swarm mode?

    ## worker node to leave from swarm
    $ docker swarm leave
    ## manager node to leave the swarm.
    $ docker swarm leave --force
