CPP
===

## URLs

    ## https://en.cppreference.com/
    ## https://coliru.stacked-crooked.com/

## std::this_thread::yield

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
