helm
====

## How to set multiple values with helm

    ## helm install [NAME] [CHART] [flags]
    ## field:
    ##  - value1
    ##  - value2
    ##  - value3
    ## --set field[0]=value1 --set field[1]=value2 --set field[2]=value3
    ## --set field={value1,value2,value3}
