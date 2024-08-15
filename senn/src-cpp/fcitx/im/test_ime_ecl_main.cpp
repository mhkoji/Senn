#include "stateful_ime_ecl.h"
#include "test.h"

int main(int argc, char *argv[]) {
  if (argc < 2) {
    return 1;
  }

  senn::fcitx::im::StatefulIMEEcl::ClBoot();
  senn::fcitx::im::StatefulIMEEcl::EclInitModule();

  senn::fcitx::im::StatefulIME *ime =
      senn::fcitx::im::StatefulIMEEcl::Create(std::string(argv[1]));

  senn::fcitx::im::Test(std::cin, ime);

  delete ime;

  senn::fcitx::im::StatefulIMEEcl::ClShutdown();

  return 0;
}
