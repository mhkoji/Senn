# fcitx5-senn

```
% sudo apt -y install cmake ecl libfcitx5core-dev

% mkdir -p dep-kkc
% ros use sbcl-bin
% ros dump executable ../../../senn-kkc-engine/hachee/ros/main-lm.ros -o dep-kkc/kkc-engine

% mkdir -p dep-ecl
% ros use ecl
% ros -s senn-lib-fcitx -e '(asdf:make-build :senn-lib-fcitx :type :static-library :monolithic t :init-name "init_senn" :move-here #P"dep-ecl/")' -q

% cd <fcitx5-senn dir>
% mkdir build
% cd build
% cmake -DCMAKE_BUILD_TYPE=Release .. && make
% sudo make install
```
