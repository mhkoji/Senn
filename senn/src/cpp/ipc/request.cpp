#include "request.h"

namespace senn {
namespace ipc {

Requester::Requester(ConnectionFactory *factory)
  : factory_(factory), conn_(nullptr) {
}

Requester::~Requester() {
  if (conn_) {
    conn_->Close();
  }
}

void Requester::Request(const std::string &req, std::string *res) {
  if (!conn_) {
    conn_ = factory_->Create();
  }

  conn_->Write(req);
  if (!conn_->ReadLine(1000, res)) {
    std::cerr << "Failed to request" << std::endl;
    std::exit(1);
  }
}

} // ipc
} // senn
