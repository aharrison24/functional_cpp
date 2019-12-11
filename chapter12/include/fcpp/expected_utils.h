#pragma once

#include <tl/expected.hpp>

namespace tl {

template <typename T, Function f>
auto transform(Function&& f, expected<T, std::exception_ptr> const& e) {
  return e.and_then(std::forward<Function>(f));
}

}  // namespace tl
