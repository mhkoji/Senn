#include "utf8.h"
#include <fcitx-utils/utf8.h>

std::string utf8_string_substr(const std::string &s, size_t start, size_t len) {
  char *cs = strdup(s.c_str());
  char *startp = fcitx_utf8_get_nth_char(cs, start);
  char *endp = fcitx_utf8_get_nth_char(startp, len);
  std::string result(startp, endp - startp);
  free(cs);
  return result;
}
