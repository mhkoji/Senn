#include <gdk/gdk.h>
#include <gtk/gtk.h>

// g++ about.cpp `pkg-config --cflags --libs gtk+-3.0`
static void activate(GtkApplication *app, gpointer user_data) {
  const int width = 464, height = 200;

  GtkWidget *window = gtk_application_window_new(app);
  gtk_window_set_title(GTK_WINDOW(window), "About Senn");
  gtk_window_set_default_size(GTK_WINDOW(window), width, height);

  {
    GdkGeometry geom;
    geom.base_width = geom.min_width = geom.max_width = width;
    geom.base_height = geom.min_height = geom.max_height = height;
    GdkWindowHints hints = GdkWindowHints(
        GDK_HINT_BASE_SIZE | GDK_HINT_MIN_SIZE | GDK_HINT_MAX_SIZE);
    gtk_window_set_geometry_hints(GTK_WINDOW(window), nullptr, &geom, hints);
  }

  {
    GtkWidget *fixed = gtk_fixed_new();
    GtkWidget *name = gtk_label_new (nullptr);
    gtk_label_set_markup(GTK_LABEL(name), "<span weight=\"bold\" size=\"36000\">Senn</span>");
    gtk_fixed_put(GTK_FIXED(fixed), name, 20, 20);

    GtkWidget *desc = gtk_label_new (nullptr);
    gtk_label_set_text(GTK_LABEL(desc), "Senn is an input method editor for the Japanese language.");
    gtk_fixed_put(GTK_FIXED(fixed), desc, 20, 100);

    gtk_container_add(GTK_CONTAINER(window), fixed);
  }

  gtk_widget_show_all(window);
}

int main(int argc, char **argv) {
  GtkApplication *app =
      gtk_application_new("org.gtk.example", G_APPLICATION_FLAGS_NONE);
  g_signal_connect(app, "activate", G_CALLBACK(activate), NULL);

  int status = g_application_run(G_APPLICATION(app), argc, argv);
  g_object_unref(app);
  return status;
}
