#pragma once

#include "traits.h"

#include <vector>

namespace fcpp {
namespace detail {

template <typename Sender,
          typename InputMessage = typename Sender::value_type,
          typename OutputMessage = typename InputMessage::value_type>
class join_impl {
 public:
  using value_type = OutputMessage;

  explicit join_impl(Sender&& sender) : sender_(std::move(sender)) {}

  void set_message_handler(std::function<void(OutputMessage&&)> emit) {
    emit_ = std::move(emit);

    sender_.set_message_handler([this](InputMessage&& message) {
      process_message(std::move(message));
    });
  }

  void process_message(InputMessage&& message) {
    sources_.push_back(std::move(message));
    sources_.back().set_message_handler(emit_);
  }

 private:
  Sender sender_;
  std::vector<InputMessage> sources_;
  std::function<void(OutputMessage&&)> emit_;
};

}  // namespace detail

template <typename Sender>
auto join(Sender&& sender) {
  return detail::join_impl<remove_cvref_t<Sender>>(
      std::forward<Sender>(sender));
}

}  // namespace fcpp
