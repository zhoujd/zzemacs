pkg-config
===========

## Compile with pkg-config in command line

    $ g++ $(pkg-config --cflags --libs libva libdrm) main.cpp
