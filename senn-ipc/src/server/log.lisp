(defpackage :senn-ipc.server.log
  (:export :info :warn)
  (:import-from :log
                :info
                :warn))

(log:config
 :pattern
 "%D{%Y-%m-%d %H:%M:%S} %<%;<;;>;p [%t] %g{}{}{:downcase}%:; ;F (%C{}{ }{:downcase})%2.2N - %m%>%n")
