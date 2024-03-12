#include <fcitx/addonfactory.h>
#include <fcitx/addonmanager.h>
#include <fcitx/inputcontextmanager.h>
#include <fcitx/inputmethodengine.h>
#include <fcitx/inputpanel.h>

#include "fcitx/im/stateful_ime_ecl.h"
const std::string kKkcEnginePath = "/usr/lib/senn/kkc-engine";

namespace fcitx5_senn {

class UI {
public:
  class SennCandidateWord : public fcitx::CandidateWord {
  public:
    class Handler {
    public:
      virtual ~Handler() {}
      virtual void OnSelectCandidate(fcitx::InputContext *, int) = 0;
    };

    SennCandidateWord(Handler *handler, const std::string &str, int index)
        : handler_(handler), index_(index) {
      setText(fcitx::Text(str));
    }

    void select(fcitx::InputContext *ic) const override {
      handler_->OnSelectCandidate(ic, index_);
    }

  private:
    Handler *handler_;
    const int index_;
  };

  UI(SennCandidateWord::Handler *handler) : handler_(handler) {}

  void Show(fcitx::InputContext *ic,
            const senn::fcitx::im::views::Converting *view) {
    ic->inputPanel().reset();

    fcitx::Text text;
    {
      int i = 0, cursor_pos = 0;
      int cursor_form_index = view->cursor_form_index;
      for (std::vector<std::string>::const_iterator it = view->forms.begin();
           it != view->forms.end(); ++it, ++i) {
        fcitx::TextFormatFlags flags;
        if (i == cursor_form_index) {
          flags = fcitx::TextFormatFlag::HighLight;
        }
        text.append(*it, flags);

        // TODO: Maybe StatefulIME should calculate cursor_pos because StatefulIME
        // is designed to do such computation.
        if (i < cursor_form_index) {
          cursor_pos += it->size();
        }
      }

      text.setCursor(cursor_pos);
    }
    SetPreedit(ic, text);

    // candidate windowを表示
    if (0 < view->cursor_form_candidates.size()) {
      ic->inputPanel().setCandidateList(std::move(MakeCandidateWordList(
          view->cursor_form_candidates, view->cursor_form_candidate_index)));
    } else {
      ic->inputPanel().setCandidateList(nullptr);
    }

    ic->updateUserInterface(fcitx::UserInterfaceComponent::InputPanel);
  }

  void Show(fcitx::InputContext *ic,
            const senn::fcitx::im::views::Editing *view) {
    ic->inputPanel().reset();

    // 入力を確定
    if (view->committed_input != "") {
      ic->commitString(view->committed_input);
    }

    fcitx::Text text;
    text.append(view->input, fcitx::TextFormatFlag::Underline);
    text.setCursor(view->cursor_pos);
    SetPreedit(ic, text);

    // candidate windowを表示
    if (0 < view->predictions.size()) {
      ic->inputPanel().setCandidateList(std::move(
          MakeCandidateWordList(view->predictions, view->prediction_index)));
    } else {
      ic->inputPanel().setCandidateList(nullptr);
    }

    ic->updateUserInterface(fcitx::UserInterfaceComponent::InputPanel);
  }

  void Clear(fcitx::InputContext *ic) {
    ic->inputPanel().reset();
    // Don't use fcitx::Text(""), otherwise the ja window is shown forever.
    SetPreedit(ic, fcitx::Text());
    ic->inputPanel().setCandidateList(nullptr);
    ic->updateUserInterface(fcitx::UserInterfaceComponent::InputPanel);
  }

private:
  SennCandidateWord::Handler *handler_;

  std::unique_ptr<fcitx::CommonCandidateList>
  MakeCandidateWordList(const std::vector<std::string> &word_strings,
                        const int index) {
    const int PAGE_SIZE = 7;

    std::unique_ptr<fcitx::CommonCandidateList> cand_list =
        std::make_unique<fcitx::CommonCandidateList>();
    cand_list->setLayoutHint(fcitx::CandidateLayoutHint::Vertical);
    cand_list->setPageSize(PAGE_SIZE);

    int i = 0;
    for (std::vector<std::string>::const_iterator it = word_strings.begin();
         it != word_strings.end(); ++it, ++i) {
      cand_list->append<SennCandidateWord>(handler_, *it, i);
    }

    if (0 <= index && index < word_strings.size()) {
      cand_list->setGlobalCursorIndex(index);
      cand_list->setPage(index / PAGE_SIZE);
    }

    return cand_list;
  }

  void SetPreedit(fcitx::InputContext *ic, const fcitx::Text &text) {
    if (ic->capabilityFlags().test(fcitx::CapabilityFlag::Preedit)) {
      ic->inputPanel().setClientPreedit(text);
    } else {
      ic->inputPanel().setPreedit(text);
    }
    ic->updatePreedit();
  }
};

class SennState : public fcitx::InputContextProperty {
public:
  SennState(fcitx::InputContext *ic) : input_context(ic){};

  fcitx::InputContext *input_context;
};

class SennEngine : public fcitx::InputMethodEngineV2,
                   public UI::SennCandidateWord::Handler {
public:
  SennEngine(fcitx::Instance *instance)
      : instance_(instance), factory_([this](fcitx::InputContext &ic) {
          return new SennState(&ic);
        }) {
    // Because fcitx5-anthy initializes an anthy object here, we also
    // initialize ecl here.
    senn::fcitx::im::StatefulIMEEcl::ClBoot();
    senn::fcitx::im::StatefulIMEEcl::EclInitModule();
    ime_ = senn::fcitx::im::StatefulIMEEcl::Create(kKkcEnginePath);

    ui_ = new UI(this);
  }

  ~SennEngine() {
    delete ui_;
    delete ime_;
    senn::fcitx::im::StatefulIMEEcl::ClShutdown();
  }

  void keyEvent(const fcitx::InputMethodEntry &,
                fcitx::KeyEvent &keyEvent) override {
    if (keyEvent.isRelease()) {
      return;
    }

    uint32_t sym = keyEvent.rawKey().sym();
    uint32_t keycode = keyEvent.rawKey().code();
    uint32_t state = keyEvent.rawKey().states();
    // std::cout << sym << " " << keycode << " " << state << std::endl;

    fcitx::InputContext *ic = keyEvent.inputContext();
    bool consumed = ime_->ProcessInput(
        sym, keycode, state,
        [&](const senn::fcitx::im::views::Converting *view) {
          ui_->Show(ic, view);
        },
        [&](const senn::fcitx::im::views::Editing *view) {
          ui_->Show(ic, view);
        });

    if (consumed) {
      keyEvent.filterAndAccept();
    }
  }

  // SennCandidateWord::Handler
  void OnSelectCandidate(fcitx::InputContext *ic, int index) override {
    ime_->SelectCandidate(
        index,
        [&](const senn::fcitx::im::views::Converting *view) {
          ui_->Show(ic, view);
        },
        [&](const senn::fcitx::im::views::Editing *view) {
          ui_->Show(ic, view);
        });
  }

  void deactivate(const fcitx::InputMethodEntry &entry,
                  fcitx::InputContextEvent &event) override {
    reset(entry, event);
  }

  void reset(const fcitx::InputMethodEntry &,
             fcitx::InputContextEvent &event) override {
    ime_->ResetIM();
    ui_->Clear(event.inputContext());
  }

private:
  fcitx::Instance *instance_;
  fcitx::FactoryFor<SennState> factory_;
  senn::fcitx::im::StatefulIME *ime_;
  UI *ui_;
};

class SennFactory : public fcitx::AddonFactory {
  fcitx::AddonInstance *create(fcitx::AddonManager *manager) override {
    return new SennEngine(manager->instance());
  }
};

} // namespace fcitx5_senn

FCITX_ADDON_FACTORY(fcitx5_senn::SennFactory);
