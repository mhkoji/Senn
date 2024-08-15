#include "stateful_ime_ecl.h"
#include "test.h"

int main(int argc, char *argv[]) {
  if (argc < 2) {
    return 1;
  }

  senn::ibus::im::StatefulIMEEcl::ClBoot();
  senn::ibus::im::StatefulIMEEcl::EclInitModule();

  senn::ibus::im::StatefulIME *ime =
      senn::ibus::im::StatefulIMEEcl::Create(std::string(argv[1]));

  senn::ibus::im::Test(std::cin, ime);

  delete ime;

  senn::ibus::im::StatefulIMEEcl::ClShutdown();

  return 0;
}
