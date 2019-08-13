#pragma once

#include <tl/expected.hpp>

#include <exception>

namespace fcpp {

template <typename Function, typename Return = std::invoke_result_t<Function>>
tl::expected<Return, std::exception_ptr> mtry(Function&& f) {
  try {
    return std::invoke(std::forward<Function>(f));
  } catch (...) {
    return tl::unexpected(std::current_exception());
  }
}

}  // namespace fcpp
