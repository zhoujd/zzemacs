CPP
===

## URLs

```
https://en.cppreference.com/
https://coliru.stacked-crooked.com/
https://gcc.gnu.org/onlinedocs/gcc-4.6.3/libstdc++/api/a01137.html
```

## std::this_thread::yield

```
## The exact behavior of this function depends on the implementation
## https://en.cppreference.com/w/cpp/thread/yield

#include <chrono>
#include <iostream>
#include <thread>

// "busy sleep" while suggesting that other threads run
// for a small amount of time
void little_sleep(std::chrono::microseconds us)
{
    auto start = std::chrono::high_resolution_clock::now();
    auto end = start + us;
    do
    {
        std::this_thread::yield();
    }
    while (std::chrono::high_resolution_clock::now() < end);
}

int main()
{
    auto start = std::chrono::high_resolution_clock::now();

    little_sleep(std::chrono::microseconds(100));

    auto elapsed = std::chrono::high_resolution_clock::now() - start;
    std::cout << "waited for "
              << std::chrono::duration_cast<std::chrono::microseconds>(elapsed).count()
              << " microseconds\n";
}
```

## std::chrono::high_resolution_clock

```
std::chrono::time_point<std::chrono::high_resolution_clock> t[3];
t[0] = std::chrono::high_resolution_clock::now();
t[1] = std::chrono::high_resolution_clock::now();
t[2] = std::chrono::high_resolution_clock::now();
for (int i = 0; i < 2; i++) {
    auto elapsed = std::chrono::duration_cast<std::chrono::microseconds>(t[i+1] - t[i]);
    std::cout << " t" << i << "=" << elapsed.count() << "us" << std::endl;
}
```

## Sleep for milliseconds

```
## https://stackoverflow.com/questions/4184468/sleep-for-milliseconds
## C++ 11
#include <chrono>
#include <thread>
std::this_thread::sleep_for(std::chrono::milliseconds(x));

## Unix
#include <unistd.h>
unsigned int microseconds;
usleep(microseconds);

## Boost::Thread
## https://www.boost.org/doc/libs/1_44_0/doc/html/thread.html
#include <boost/thread/thread.hpp>
//waits 2 seconds
boost::this_thread::sleep(boost::posix_time::seconds(1));
boost::this_thread::sleep(boost::posix_time::milliseconds(1000));

## C++ 14
#include <chrono>
#include <thread>
using namespace std::chrono_literals;
std::this_thread::sleep_for(123ms);

## `select` function (POSIX, Linux, and Windows)
#include <sys/time.h>
void sleep(unsigned long msec) {
    timeval delay = {msec / 1000, msec % 1000 * 1000};
    int rc = ::select(0, NULL, NULL, NULL, &delay);
    if(-1 == rc) {
        // Handle signals by continuing to sleep or return immediately.
    }
}
```

## Iterating Over a Vector

```
## https://siddharthqs.com/the-essential-c-stl-cheat-sheet
// Using Index-Based Loop
for (size_t i = 0; i < vec.size(); ++i) {
        cout << vec[i] << " ";
    }
// Using Range-Based For Loop
for (int val : vec) {
    cout << val << " ";
}
// Using Iterators
for (auto it = vec.begin(); it != vec.end(); ++it) {
    cout << *it << " ";
}
// Using for_each Algorithm
std::for_each(vec.begin(), vec.end(), [](int val) {
        cout << val << " ";
});
```

## Reading json files in C++

```
## https://github.com/open-source-parsers/jsoncpp
$ sudo apt install libjsoncpp-dev
$ cat > cfg.json <<EOF
{
   "Note" : "This is a cofiguration file",
   "Config" : {
       "server-ip"     : "10.10.10.20",
       "server-port"   : "5555",
       "buffer-length" : 5000
   }
}
EOF

$ cat > ReadJsonCfg.cpp <<EOF
#include <iostream>
#include <json/value.h>
#include <jsoncpp/json/json.h>
#include <fstream>

void
displayCfg(const Json::Value &cfg_root);

int
main()
{
    Json::Reader reader;
    Json::Value cfg_root;
    std::ifstream cfgfile("cfg.json");
    cfgfile >> cfg_root;

    std::cout << "______ cfg_root : start ______" << std::endl;
    std::cout << cfg_root << std::endl;
    std::cout << "______ cfg_root : end ________" << std::endl;

    displayCfg(cfg_root);
}

void
displayCfg(const Json::Value &cfg_root)
{
    std::string serverIP = cfg_root["Config"]["server-ip"].asString();
    std::string serverPort = cfg_root["Config"]["server-port"].asString();
    unsigned int bufferLen = cfg_root["Config"]["buffer-length"].asUInt();

    std::cout << "______ Configuration ______" << std::endl;
    std::cout << "server-ip     :" << serverIP << std::endl;
    std::cout << "server-port   :" << serverPort << std::endl;
    std::cout << "buffer-length :" << bufferLen<< std::endl;
}
EOF

$ cat /usr/lib/x86_64-linux-gnu/pkgconfig/jsoncpp.pc
$ g++ -g -O0 -std=c++11 -I. -I/usr/include/jsoncpp -c -o ReadJsonCfg.o ReadJsonCfg.cpp
$ g++ ReadJsonCfg.o -L/usr/lib/x86_64-linux-gnu -ljsoncpp -o readjsoncfg
$ ./readjsoncfg
```
