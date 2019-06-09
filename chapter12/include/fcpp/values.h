#pragma once

#include <algorithm>
#include <initializer_list>
#include <vector>

template <typename...>
struct Print;

namespace fcpp {
template <typename Message>
class values {
 public:
  using value_type = Message;

  values(std::initializer_list<Message> vals) : vals_{std::move(vals)} {}

  void set_message_handler(std::function<void(Message&&)> emit) {
    std::for_each(cbegin(vals_), cend(vals_),
                  [&emit](auto val) { emit(std::move(val)); });
  }

 private:
  std::vector<Message> vals_;
};

}  // namespace fcpp
