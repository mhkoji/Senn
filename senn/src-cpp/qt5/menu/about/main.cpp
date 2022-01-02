#include <QApplication>
#include <QFont>
#include <QLabel>
#include <QVBoxLayout>
#include <QWidget>

// sudo apt install -y qt5-default
// qmake && make
int main(int argc, char **argv) {
  QApplication app(argc, argv);

  QWidget window;
  window.setFixedSize(464, 200);
  window.setWindowTitle("About Senn");

  const QFont &font = window.font();

  QVBoxLayout *layout = new QVBoxLayout(&window);

  QLabel *name = new QLabel(&window);
  name->setFont(QFont(font.family(), 36, QFont::Bold));
  name->setText("Senn");
  name->show();
  layout->addWidget(name);

  QLabel *desc = new QLabel(&window);
  desc->show();
  desc->setText("Senn is an input method editor for the Japanese language.");
  layout->addWidget(desc);

  window.show();

  return app.exec();
}
