#pragma once
#include "views.h"
#include <picojson.h>
#include <string>
#include <vector>

namespace senn {
namespace fcitx {
namespace im {
namespace views {
namespace json {

void Parse(const std::string &string_content,
           senn::fcitx::im::views::Converting *output) {
  picojson::value content;
  picojson::parse(content, string_content);

  const picojson::array forms =
      content.get<picojson::object>()["forms"].get<picojson::array>();
  for (picojson::array::const_iterator it = forms.begin(); it != forms.end();
       ++it) {
    output->forms.push_back(it->get<std::string>());
  }

  output->cursor_form_index =
      content.get<picojson::object>()["cursor-form-index"].get<double>();

  const picojson::array candidates =
      content.get<picojson::object>()["cursor-form"]
          .get<picojson::object>()["candidates"]
          .get<picojson::array>();
  for (picojson::array::const_iterator it = candidates.begin();
       it != candidates.end(); ++it) {
    output->cursor_form_candidates.push_back(it->get<std::string>());
  }

  output->cursor_form_candidate_index =
      content.get<picojson::object>()["cursor-form"]
          .get<picojson::object>()["candidate-index"]
          .get<double>();
}

void Parse(const std::string &string_content,
           senn::fcitx::im::views::Editing *output) {
  picojson::value content;
  picojson::parse(content, string_content);

  output->cursor_pos =
      content.get<picojson::object>()["cursor-pos"].get<double>();
  output->input = content.get<picojson::object>()["input"].get<std::string>();
  output->committed_input =
      content.get<picojson::object>()["committed-input"].get<std::string>();

  const picojson::array predictions =
      content.get<picojson::object>()["predictions"].get<picojson::array>();
  for (picojson::array::const_iterator it = predictions.begin();
       it != predictions.end(); ++it) {
    output->predictions.push_back(it->get<std::string>());
  }

  output->prediction_index =
      content.get<picojson::object>()["prediction-index"].get<double>();
}

} // namespace json
} // namespace views
} // namespace im
} // namespace fcitx
} // namespace senn
