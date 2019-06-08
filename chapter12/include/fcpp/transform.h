#pragma once

#include "traits.h"

#include <functional>
#include <utility>

namespace fcpp {

namespace detail {
template <typename Sender,
          typename Function,
          typename InputMessage = typename Sender::value_type,
          typename OutputMessage = std::invoke_result_t<Function, InputMessage>>
class transform_impl {
 public:
  using value_type = OutputMessage;

  transform_impl(Sender&& sender, Function f)
      : sender_(std::move(sender)), function_(std::move(f)) {}

  void set_message_handler(std::function<void(OutputMessage&&)> emit) {
    emit_ = std::move(emit);
    sender_.set_message_handler([this](InputMessage&& message) {
      process_message(std::move(message));
    });
  }

  void process_message(InputMessage&& message) const {
    emit_(std::invoke(function_, std::move(message)));
  }

 private:
  Sender sender_;
  Function function_;
  std::function<void(OutputMessage&&)> emit_;
};
}  // namespace detail

template <typename Sender, typename Function>
auto transform(Sender&& sender, Function&& f) {
  return detail::transform_impl<remove_cvref_t<Sender>, std::decay_t<Function>>(
      std::forward<Sender>(sender), std::forward<Function>(f));
}
}  // namespace fcpp
