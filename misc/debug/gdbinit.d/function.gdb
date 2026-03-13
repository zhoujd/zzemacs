## function.gdb

define skip-std-cpp
  ## Function(s) std::.* will be skipped when stepping
  skip -rfu std::.*
  ## Skipping standard C++ library
  skip -gfi /usr/include/c++/*/*/*
  skip -gfi /usr/include/c++/*/*
  skip -gfi /usr/include/c++/*
end
