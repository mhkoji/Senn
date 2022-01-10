// Under construction
#include <anthy/anthy.h>
#include <fcitx-utils/utf8.h>
#include <iostream>
#include <string.h>

std::string utf8_string_substr(const std::string &s, size_t start, size_t len) {
  char *cs = strdup(s.c_str());
  char *startp = fcitx_utf8_get_nth_char(cs, start);
  char *endp = fcitx_utf8_get_nth_char(startp, len);
  std::string result(startp, endp - startp);
  free(cs);
  return result;
}

// sudo apt install -y libanthy-dev
// g++ engine.cpp -lanthy -lfcitx-utils
// ./a.out
int main(void) {
  anthy_init();

  anthy_context_t anthy_context = anthy_create_context();

  std::string pron = "きょうhaよいてんきです。";
  anthy_set_string(anthy_context, pron.c_str());

  struct anthy_conv_stat conv_stat;
  anthy_get_stat(anthy_context, &conv_stat);

  unsigned int seg_start_in_pron = 0;
  for (int nth_seg = 0; nth_seg < conv_stat.nr_segment; nth_seg++) {
    struct anthy_segment_stat seg_stat;
    anthy_get_segment_stat(anthy_context, nth_seg, &seg_stat);

    std::string sub_pron =
        utf8_string_substr(pron, seg_start_in_pron, seg_stat.seg_len);

    std::string form;
    {
      int nth_cand = 0;
      int len = anthy_get_segment(anthy_context, nth_seg, nth_cand, NULL, 0);
      char buf[len + 1];
      anthy_get_segment(anthy_context, nth_seg, nth_cand, buf, len + 1);
      buf[len + 1] = '\0';
      form = buf;
    }

    std::cout << form << "(" << sub_pron << ")" << std::endl;

    seg_start_in_pron += seg_stat.seg_len;
  }

  anthy_release_context(anthy_context);
  anthy_quit();
}
