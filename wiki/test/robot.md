Robot
=====

## Robot Framework SSH tutorial with example

    ## https://medium.com/@avi.mehenwal/robot-framework-ssh-tutorial-with-example-348907cd871
    $ cat test_ssh_rf_demo.robot <<EOF
    *** Settings ***
    Documentation      Robot Framework test script
    Library            SSHLibrary

    *** Variables ***
    ${host}            127.0.0.1
    ${username}        root
    ${password}        ${EMPTY}
    ${alias}           remote_host_1

    *** Test Cases ***
    Test SSH Connection
        Open Connection     ${host}        alias=${alias}
        Login               ${username}    ${password}    delay=1
        Execute Command     hostname
        Close All Connections
    EOF

    $ robot test_ssh_rf_demo.robot

## Robot Framework - SSH Library - Login With Public Key

    ## https://stackoverflow.com/questions/68001504/robot-framework-ssh-library-login-with-public-key
    cat > test_ssh_keys_demo.robot <<EOF
    *** Settings ***
    Documentation      Robot Framework test script
    Library            SSHLibrary

    *** Variables ***
    ${hostname}    localhost
    ${username}    %{USER}

    *** Test Cases ***
    ConnectionToJump
        Open Connection        ${hostname}    port=22
        Login With Public Key  ${username}    %{HOME}/.ssh/id_rsa
        Execute Command        hostname
        Close All Connections
    EOF

    $ robot test_ssh_keys_demo.robot
