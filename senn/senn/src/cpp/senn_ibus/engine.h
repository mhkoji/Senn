#pragma once
#include "stateful_im.h"

namespace senn {
namespace ibus {
namespace engine {

struct IBusSennEngine {
  IBusEngine parent;
  StatefulIM *im;
}


} // engine
} // ibus
} // senn
