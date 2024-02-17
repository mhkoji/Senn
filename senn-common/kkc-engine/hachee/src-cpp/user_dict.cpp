#include "user_dict.h"
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <set>

#define DLL_PUBLIC __attribute__ ((visibility ("default")))

namespace {

struct Entry {
  std::string form;
  std::string pron;
};

struct UserDict {
  std::vector<Entry> entries;
};
}

DLL_PUBLIC user_dict_t user_dict_load(const char *path) {
  std::ifstream in(path);
  if (!in) {
    return NULL;
  }

  std::vector<Entry> entries;
  std::set<std::string> added_forms;
  std::string line;

  while (std::getline(in, line)) {
    if (line.size() == 0) {
      continue;
    }
    if (line[0] == '#') {
      continue;
    }

    std::stringstream ss(line);
    std::string form, pron;
    ss >> form >> pron;

    if (form.size() == 0 || pron.size() == 0) {
      continue;
    } else if (added_forms.find(form) != added_forms.end()) {
      continue;
    }

    entries.push_back(Entry({form, pron}));
    added_forms.insert(form);
  }
  return new UserDict({entries});
}

DLL_PUBLIC void user_dict_destroy(user_dict_t p) {
  UserDict *ud = (UserDict *)p;
  delete ud;
}

DLL_PUBLIC int user_dict_count(user_dict_t p) {
  UserDict *ud = (UserDict *)p;
  return ud->entries.size();
}

DLL_PUBLIC entry_t user_dict_entry(user_dict_t p, int index) {
  UserDict *ud = (UserDict *)p;
  return &(ud->entries[index]);
}

DLL_PUBLIC const char *entry_pron(entry_t p) {
  Entry *e = (Entry *)p;
  return e->pron.c_str();
}

DLL_PUBLIC const char *entry_form(entry_t p) {
  Entry *e = (Entry *)p;
  return e->form.c_str();
}
