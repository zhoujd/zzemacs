CPP
===

## URLs

```
## https://en.cppreference.com/
## https://coliru.stacked-crooked.com/
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
