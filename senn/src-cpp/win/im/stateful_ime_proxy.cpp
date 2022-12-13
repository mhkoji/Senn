#include "stateful_ime_proxy.h"
#include "../../../../third-party/picojson/picojson.h"
#include <sstream>

namespace senn {
namespace win {
namespace im {

namespace {

int ToWString(const std::string &char_string, std::wstring *output) {
  WCHAR buf[1024] = {'\0'};
  int size = MultiByteToWideChar(CP_UTF8, 0, char_string.c_str(),
                                 static_cast<int>(char_string.length()), buf,
                                 static_cast<int>(sizeof(buf)));
  *output = buf;
  return size;
}

} // namespace

void StatefulIMEProxy::ToggleInputMode() {
  std::string response;
  {
    std::stringstream ss;
    ss << "{"
       << "\"op\": \"toggle-input-mode\""
       << "}";
    Request(ss.str(), &response);
  }
  // It seems to need to consume output buffer...
  std::istringstream iss(response);
  std::string ok;
  iss >> ok;
}

InputMode StatefulIMEProxy::GetInputMode() {
  std::string response;
  {
    std::stringstream ss;
    ss << "{"
       << "\"op\": \"get-input-mode\""
       << "}";
    Request(ss.str(), &response);
  }

  std::istringstream iss(response);
  std::string mode;
  iss >> mode;
  if (mode == "DIRECT") {
    return kDirect;
  }
  if (mode == "HIRAGANA") {
    return kHiragana;
  }

  return kUnknown;
}

bool StatefulIMEProxy::CanProcess(uint64_t keycode) {
  std::string response;
  {
    std::stringstream ss;
    ss << "{"
       << "\"op\": \"can-process\","
       << "\"args\": {"
       << "\"keycode\": " << keycode << "}"
       << "}";
    Request(ss.str(), &response);
  }

  std::istringstream iss(response);
  bool can_process;
  iss >> can_process;
  return can_process;
}

bool StatefulIMEProxy::ProcessInput(
    uint64_t keycode, BYTE *modifiers,
    std::function<void(const views::Editing &)> on_editing,
    std::function<void(const views::Converting &)> on_converting,
    std::function<void(const views::Committed &)> on_committed) {
  std::string response;
  {
    std::stringstream ss;
    ss << "{"
       << "\"op\": \"process-input\","
       << "\"args\": {"
       << "\"keycode\": " << keycode << ","
       << "\"shift\": " << (modifiers[VK_SHIFT] ? "true" : "false") << "}"
       << "}";
    Request(ss.str(), &response);
  }

  std::istringstream iss(response);
  bool eaten;
  iss >> eaten;

  if (eaten) {
    std::string type;
    iss >> type;

    if (type == "EDITING") {
      std::string content;
      std::getline(iss, content);

      views::Editing editing;
      {
        picojson::value v;
        picojson::parse(v, content);

        const std::string input =
            v.get<picojson::object>()["input"].get<std::string>();
        ToWString(input, &editing.input);

        const picojson::array predictions =
            v.get<picojson::object>()["predictions"].get<picojson::array>();
        for (picojson::array::const_iterator it = predictions.begin();
             it != predictions.end(); ++it) {
          std::wstring prediction;
          ToWString(it->get<std::string>(), &prediction);
          editing.predictions.push_back(prediction);
        }
      }

      on_editing(editing);
    } else if (type == "CONVERTING") {
      std::string content;
      std::getline(iss, content);

      views::Converting converting;
      {
        picojson::value v;
        picojson::parse(v, content);

        const picojson::array forms =
            v.get<picojson::object>()["forms"].get<picojson::array>();
        for (picojson::array::const_iterator it = forms.begin();
             it != forms.end(); ++it) {
          std::wstring form;
          ToWString(it->get<std::string>(), &form);
          converting.forms.push_back(form);
        }

        converting.cursor_form_index = static_cast<size_t>(
            v.get<picojson::object>()["cursor-form-index"].get<double>());

        {
          const picojson::array candidates =
              v.get<picojson::object>()["cursor-form"]
                  .get<picojson::object>()["candidates"]
                  .get<picojson::array>();
          for (picojson::array::const_iterator it = candidates.begin();
               it != candidates.end(); ++it) {
            std::wstring str;
            ToWString(it->get<std::string>(), &str);
            converting.cursor_form_candidates.push_back(str);
          }
        }

        converting.cursor_form_candidate_index =
            static_cast<int>(v.get<picojson::object>()["cursor-form"]
                                 .get<picojson::object>()["candidate-index"]
                                 .get<double>());
      }

      on_converting(converting);
    } else if (type == "COMMITTED") {
      std::string content;
      std::getline(iss, content);

      views::Committed committed;
      {
        picojson::value v;
        picojson::parse(v, content);

        const std::string char_input =
            v.get<picojson::object>()["input"].get<std::string>();
        ToWString(char_input, &committed.input);
      }

      on_committed(committed);
    }
  }

  return eaten;
};

} // namespace im
} // namespace win
} // namespace senn
