(in-package :senn.win.im)

(defmethod senn.win.ime:can-process ((state t)
                                     (ime senn.win.im:ime)
                                     (key senn.win.keys:key)
                                     (mode t))
  (if (senn.win.im.process-input:dry-run state ime key mode)
      t nil))
