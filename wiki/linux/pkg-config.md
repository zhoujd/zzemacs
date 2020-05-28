pkg-config
===========

1. Compile with pkg-config in command line

        $ g++ $(pkg-config --cflags --libs libva libdrm) main.cpp 
