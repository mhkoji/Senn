// Under construction
#include <anthy/anthy.h>
#include <fcitx-utils/utf8.h>
#include <iostream>
#include <string.h>
#include <vector>

std::string utf8_string_substr(const std::string &s, size_t start, size_t len) {
  char *cs = strdup(s.c_str());
  char *startp = fcitx_utf8_get_nth_char(cs, start);
  char *endp = fcitx_utf8_get_nth_char(startp, len);
  std::string result(startp, endp - startp);
  free(cs);
  return result;
}

struct Segment {
  std::string pron;
  std::vector<std::string> candidate_forms;
};

void AnthyConvert(anthy_context_t anthy_context, const std::string &pron,
                  std::vector<Segment> *segs) {
  anthy_set_string(anthy_context, pron.c_str());

  struct anthy_conv_stat conv_stat;
  anthy_get_stat(anthy_context, &conv_stat);

  segs->resize(conv_stat.nr_segment);

  unsigned int seg_start_in_pron = 0;
  for (int nth_seg = 0; nth_seg < conv_stat.nr_segment; nth_seg++) {
    struct anthy_segment_stat seg_stat;
    anthy_get_segment_stat(anthy_context, nth_seg, &seg_stat);

    std::string sub_pron =
        utf8_string_substr(pron, seg_start_in_pron, seg_stat.seg_len);
    (*segs)[nth_seg].pron = sub_pron;

    (*segs)[nth_seg].candidate_forms.resize(seg_stat.nr_candidate);
    for (int nth_cand = 0; nth_cand < seg_stat.nr_candidate; nth_cand++) {
      int len = anthy_get_segment(anthy_context, nth_seg, nth_cand, NULL, 0);
      char buf[len + 1];
      anthy_get_segment(anthy_context, nth_seg, nth_cand, buf, len + 1);
      buf[len + 1] = '\0';
      (*segs)[nth_seg].candidate_forms[nth_cand] = buf;
    }

    seg_start_in_pron += seg_stat.seg_len;
  }
}

// sudo apt install -y libanthy-dev
// g++ engine.cpp -lanthy -lfcitx-utils
// ./a.out
int main(void) {
  anthy_init();

  anthy_context_t anthy_context = anthy_create_context();

  std::vector<Segment> segs;
  AnthyConvert(anthy_context, "きょうhaよいてんきです。", &segs);

  for (size_t s = 0; s < segs.size(); s++) {
    std::cout << segs[s].pron << ":";
    for (size_t c = 0; c < segs[s].candidate_forms.size(); c++) {
      std::cout << " " << segs[s].candidate_forms[c];
    }
    std::cout << std::endl;
  }

  anthy_release_context(anthy_context);

  anthy_quit();
}
