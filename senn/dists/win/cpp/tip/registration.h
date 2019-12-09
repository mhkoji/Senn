#pragma once

#include <windows.h>

#include "win/registry.h"
#include "win/text-service/registration.h"
#include "win/text-service/class_factory.h"
#include "senn.h"

namespace senn {
namespace senn_win {
namespace registration {

class COMServerSettingsProvider
  : public ::senn::win::registry::com_server::SettingsProvider {
public:
  COMServerSettingsProvider(HINSTANCE module_handle) :
      module_handle_(module_handle) {
  }

  BOOL Get(::senn::win::registry::com_server::Settings *output) const override {
    DWORD size = ARRAYSIZE(output->module_file_name.content);
    DWORD num_chars = GetModuleFileName(
        module_handle_, output->module_file_name.content, size);
    if (num_chars == 0) {
      return FALSE;
    }
    output->module_file_name.size_including_null_termination =
      num_chars < (size - 1) ? num_chars + 1 : size;

    output->description.content = (const BYTE*)kDescription;
    output->description.bytes = (_countof(kDescription)) * sizeof(WCHAR);

    output->threading_model.content = (const BYTE*)kThreadingModel;
    output->threading_model.bytes = (_countof(kThreadingModel)) * sizeof(WCHAR);

    return TRUE;
  }

private:
  const HINSTANCE module_handle_;
};

class TextServiceSettingsProvider
  : public ::senn::win::text_service::registration::SettingsProvider {
public:
  void Get(::senn::win::text_service::registration::Settings *output) const override {
    output->langid = MAKELANGID(LANG_JAPANESE, SUBLANG_JAPANESE_JAPAN);

    output->profile.guid = kProfileGuid;
    output->profile.description = kProfileDescription;

    output->icon.file = NULL;

    size_t category_count =
        static_cast<size_t>(sizeof(kCategories) / sizeof(kCategories[0]));
    for (size_t i = 0; i < category_count; i++) {
      output->categories.push_back(kCategories[i]);
    }
  }
};

} // registration
} // win
} // senn
