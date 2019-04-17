#include <utility>

// Implementing some common algorithms in terms of std::accumulate

namespace fcpp {

template <typename T, typename ForwardIter, typename BinaryOperation>
constexpr auto foldl(ForwardIter first,
                     ForwardIter last,
                     T init_value,
                     BinaryOperation op) -> T {
  for (; first != last; ++first) {
    init_value = op(std::move(init_value), *first);
  }
  return init_value;
}

}  // namespace fcpp
