(require 'server)
(unless (server-running-p)
  (server-start))
(setq ns-pop-up-frames nil)

