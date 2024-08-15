#include "test.h"
#include <cassert>
#include <picojson.h>
#include <sstream>

namespace senn {
namespace fcitx {
namespace im {

void Test(std::istream &in, senn::fcitx::im::StatefulIME *ime) {
  std::string line;
  while (std::getline(in, line)) {
    picojson::value req, expected;
    {
      std::istringstream ss(line);
      picojson::parse(req, ss);
      picojson::parse(expected, ss);
    }

    std::string op = req.get<picojson::object>()["op"].get<std::string>();
    std::string expected_type =
        expected.get<picojson::object>()["type"].get<std::string>();
    if (op == "process-input") {
      uint32_t sym = req.get<picojson::object>()["sym"].get<double>();
      bool called = false;
      ime->ProcessInput(
          sym, 0, 0,
          [&](const senn::fcitx::im::views::Converting *view) {
            called = true;
            assert("c" == expected_type);

            std::string input;
            for (size_t i = 0; i < view->forms.size(); i++) {
              if (0 < i) {
                input += " ";
              }
              input += view->forms[i];
            }
            std::cerr << sym << " -> " << input << std::endl;

            assert(
                input ==
                expected.get<picojson::object>()["input"].get<std::string>());
          },
          [&](const senn::fcitx::im::views::Editing *view) {
            called = true;
            assert("e" == expected_type);

            std::string input = view->input;
            std::cerr << sym << " -> " << input << std::endl;

            assert(
                input ==
                expected.get<picojson::object>()["input"].get<std::string>());
          });
      assert(called);
    }
  }
}

} // namespace im
} // namespace fcitx
} // namespace senn
