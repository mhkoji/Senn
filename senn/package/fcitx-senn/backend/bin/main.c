#include <ecl/ecl.h>


int main(int argc, char **argv) {
  char* dummy = "dummy";
  char *argv2[1] = {dummy};
  cl_boot(0, argv2);

  extern void init_senn(cl_object);
  ecl_init_module(NULL, init_senn);

  // gcc `~/.roswell/impls/x86-64/linux/ecl/21.2.1/bin/ecl-config --cflags` -o main main.c senn-bin-fcitx-lib--all-systems.a  `~/.roswell/impls/x86-64/linux/ecl/21.2.1/bin/ecl-config --ldflags` -lecl
  cl_object ime = cl_eval(c_string_to_object("(senn.bin.fcitx-lib:make-ime)"));

  cl_object result = cl_funcall(
      4,
      cl_eval(c_string_to_object("'senn.bin.fcitx-lib:process-input")),
      ime,
      ecl_make_fixnum(97),
      ecl_make_fixnum(0));
  ecl_print(result, ECL_T);

  cl_shutdown();
  return 0;
}
  
