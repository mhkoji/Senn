#include "utf8.h"
#include <anthy/anthy.h>
#include <iostream>
#include <picojson/picojson.h>
#include <stdexcept>
#include <vector>

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

void Loop(anthy_context_t anthy_context) {
  std::vector<Segment> segments;

  while (true) {
    std::string req_string;
    std::getline(std::cin, req_string);

    picojson::value req;
    picojson::parse(req, req_string);

    std::string op = req.get<picojson::object>()["op"].get<std::string>();

    if (op == "CONVERT") {
      segments.clear();
      std::string pron = req.get<picojson::object>()["args"]
                             .get<picojson::object>()["pron"]
                             .get<std::string>();
      AnthyConvert(anthy_context, pron, &segments);

      picojson::array items;
      for (size_t s = 0; s < segments.size(); s++) {
        picojson::object item;
        item["pron"] = picojson::value(segments[s].pron);

        picojson::array candidates;
        for (size_t c = 0; c < segments[s].candidate_forms.size(); c++) {
          picojson::object cand;
          cand["form"] = picojson::value(segments[s].candidate_forms[c]);
          candidates.push_back(picojson::value(cand));
        }
        item["candidates"] = picojson::value(candidates);

        items.push_back(picojson::value(item));
      }
      picojson::value resp(items);
      std::cout << resp << std::endl;
    } else if (op == "LIST_CANDIDATES") {
      // Return the empty array.
      picojson::array cands;
      picojson::value resp(cands);
      std::cout << resp << std::endl;
    } else {
      throw std::runtime_error("Invalid op: " + op);
    }
  }
}

class AnthyReleaer {
public:
  ~AnthyReleaer() { anthy_quit(); }
};

class AnthyContextReleaser {
public:
  AnthyContextReleaser(anthy_context_t anthy_context)
      : anthy_context_(anthy_context) {}

  ~AnthyContextReleaser() { anthy_release_context(anthy_context_); }

private:
  const anthy_context_t anthy_context_;
};

// sudo apt install -y libanthy-dev
// g++ engine.cpp -lanthy -lfcitx-utils -I ../../senn/third-party/
// ./a.out
int main(void) {
  anthy_init();
  AnthyReleaer anthy_releaser();

  anthy_context_t anthy_context = anthy_create_context();
  AnthyContextReleaser anthy_context_releaser(anthy_context);

  Loop(anthy_context);
}
