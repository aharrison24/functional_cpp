#pragma once

#include "traits.h"

#include <functional>

namespace fcpp {

namespace detail {
template <typename Sender,
          typename Function,
          typename InputMessage = typename Sender::value_type>
class sink_impl {
 public:
  explicit sink_impl(Sender&& sender, Function f)
      : sender_(std::move(sender)), function_(std::move(f)) {
    sender_.set_message_handler([this](InputMessage&& message) {
      process_message(std::move(message));
    });
  }

  void process_message(InputMessage&& message) const {
    std::invoke(function_, std::move(message));
  }

 private:
  Sender sender_;
  Function function_;
};

}  // namespace detail

template <typename Sender, typename Function>
auto sink(Sender&& sender, Function&& f) {
  return detail::sink_impl<remove_cvref_t<Sender>, std::decay_t<Function>>(
      std::forward<Sender>(sender), std::forward<Function>(f));
}
}  // namespace fcpp
