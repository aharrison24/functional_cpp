#pragma once

#include "traits.h"

#include <functional>
#include <utility>

namespace fcpp {

namespace detail {

template <typename Sender,
          typename Predicate,
          typename Message = typename Sender::value_type>
class filter_impl {
 public:
  using value_type = Message;

  filter_impl(Sender&& sender, Predicate p)
      : sender_(std::move(sender)), predicate_(std::move(p)) {}

  void set_message_handler(std::function<void(Message&&)> emit) {
    emit_ = std::move(emit);
    sender_.set_message_handler(
        [this](Message&& message) { process_message(std::move(message)); });
  }

  void process_message(Message&& message) {
    if (predicate_(message)) {
      emit_(std::move(message));
    }
  }

 private:
  Sender sender_;
  Predicate predicate_;
  std::function<void(Message&&)> emit_;
};
}  // namespace detail

template <typename Sender, typename Predicate>
auto filter(Sender&& sender, Predicate&& p) {
  return detail::filter_impl<remove_cvref_t<Sender>, std::decay_t<Predicate>>(
      std::forward<Sender>(sender), std::forward<Predicate>(p));
}

}  // namespace fcpp
