#pragma once

#define FWD(x) std::forward<decltype(x)>(x)

#define OVERLOAD_SET(f)                                                        \
  [](auto&&... args) noexcept(noexcept(f(FWD(args)...)))                       \
      ->decltype(f(FWD(args)...)) {                                            \
    return f(FWD(args)...);                                                    \
  }
