(defpackage :senn.gui.menu.about
  (:use :cl
        :gtk
        :gdk
        :gdk-pixbuf
        :gobject
        :glib
        :gio
        :pango
        :cairo)
  (:export :show))
(in-package :senn.gui.menu.about)

(defun show ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :resizable nil
                                 :type :toplevel
                                 :title "About Senn")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Set the fixed window size.
      (let* ((w 464)
             (h 200)
             (geom (make-gdk-geometry
                    :max-width w :max-height h
                    :min-width w :min-height h
                    :base-width w :base-height h))
             (geom-mask (logior (ash 1 1)    ;; GDK_HINT_MIN_SIZE
                                (ash 1 2)    ;; GDK_HINT_MAX_SIZE
                                (ash 1 3)))) ;; GDK_HINT_BASE_SIZE
        (gtk-window-set-geometry-hints window nil geom geom-mask))

      (let ((fixed (make-instance 'gtk-fixed)))
        (gtk-fixed-put fixed
         (make-instance 'gtk-label
          :use-markup t
          :label "<span weight=\"bold\" size=\"36000\">Senn</span>")
         20 20)
        (gtk-fixed-put fixed
         (make-instance 'gtk-label
          :label
          "Senn is an input method editor for the Japanese language.")
         20 100)
        (gtk-container-add window fixed))

      (gtk-widget-show-all window))))
