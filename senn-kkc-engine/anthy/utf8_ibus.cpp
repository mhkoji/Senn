#include "utf8.h"
#include <glib-object.h>

std::string utf8_string_substr(const std::string &s, size_t start, size_t len) {
  gchar* substr = g_utf8_substring(s.c_str(), start, start + len);
  std::string result(substr);
  g_free(substr);
  return result;
}
