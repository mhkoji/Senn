#include <ShlObj_core.h>
#include <sstream>
#include <win/im/stateful_ime_ecl.h>

using namespace senn::win::im;

std::vector<std::wstring> g_texts;

LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam,
                            LPARAM lParam) {
  switch (uMsg) {
  case WM_DESTROY:
    PostQuitMessage(0);
    return 0;

  case WM_PAINT: {
    PAINTSTRUCT ps;
    HDC hdc = BeginPaint(hwnd, &ps);

    FillRect(hdc, &ps.rcPaint, (HBRUSH)(COLOR_WINDOW + 1));

    for (int i = 0; i < g_texts.size(); i++) {
      RECT r = {5, 5 + (i * 30), 500, 5 + ((1 + i) * 30)};
      DrawText(hdc, g_texts[i].c_str(), -1, &r, DT_LEFT);
    }

    EndPaint(hwnd, &ps);
  }
    return 0;
  }

  return DefWindowProc(hwnd, uMsg, wParam, lParam);
}

void GetHomeDir(std::wstring *out) {
  PWSTR homedir_path = nullptr;
  if (SHGetKnownFolderPath(FOLDERID_Profile, KF_FLAG_DEFAULT_PATH, NULL,
                           &homedir_path) == S_OK) {
    *out = homedir_path;
    *out += L"\\";
  }
  CoTaskMemFree(homedir_path);
}

INT WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, PSTR lpCmdLine,
                   INT nCmdShow) {

  std::wstring home_dir = L"";
  {
    GetHomeDir(&home_dir);
    if (home_dir == L"") {
      return 0;
    }
  }

  std::wstring senn_dir = home_dir + L".senn\\";
  std::wstring ecl_dir = senn_dir + L"ecl\\";
  std::wstring kkc_engine_path = senn_dir + L"kkc-engine.exe";

  _wputenv_s(L"ECLDIR", ecl_dir.c_str());

  StatefulIMEEcl::ClBoot();
  StatefulIMEEcl::EclInitModule();

  StatefulIME *ime;
  {
    size_t size;
    char buf[256];
    wcstombs_s(&size, buf, sizeof(buf), kkc_engine_path.c_str(),
               sizeof(buf) - 1);
    ime = StatefulIMEEcl::Create(buf);
  }

  std::vector<uint64_t> syms = {84, 79, 85, 75, 89, 79, 85, 78, 73, 73,
                                75, 73, 77, 65, 83, 73, 84, 65, 32};
  ime->ToggleInputMode();
  for (size_t i = 0; i < syms.size(); i++) {
    uint64_t sym = syms[i];
    BYTE modifiers = 0;
    std::wstringstream ss;
    ss << "Sym: " << sym << " -> ";
    ime->ProcessInput(
        sym, &modifiers,
        [&](const views::Editing &v) { ss << "Editing: " << v.input; },
        [&](const views::Converting &v) {
          ss << "Converting:";
          for (size_t i = 0; i < v.forms.size(); i++) {
            ss << " " << v.forms[i];
          }
        },
        [&](const views::Committed &v) { ss << "Committed: " << v.input; });
    g_texts.push_back(ss.str());
  }

  const wchar_t CLASS_NAME[] = L"stateful-ime-ecl-main Window Class";
  WNDCLASS wc = {};
  wc.lpfnWndProc = WindowProc;
  wc.hInstance = hInstance;
  wc.lpszClassName = CLASS_NAME;
  RegisterClass(&wc);

  HWND hwnd =
      CreateWindowEx(0,                        // Optional window styles.
                     CLASS_NAME,               // Window class
                     L"stateful-ime-ecl-main", // Window text
                     WS_OVERLAPPEDWINDOW,      // Window style

                     // Size and position
                     CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,

                     NULL,      // Parent window
                     NULL,      // Menu
                     hInstance, // Instance handle
                     NULL       // Additional application data
      );

  if (hwnd == NULL) {
    return 0;
  }

  ShowWindow(hwnd, nCmdShow);

  MSG msg = {};
  while (GetMessage(&msg, NULL, 0, 0) > 0) {
    TranslateMessage(&msg);
    DispatchMessage(&msg);
  }

  delete ime;
  StatefulIMEEcl::ClShutdown();

  return 0;
}
