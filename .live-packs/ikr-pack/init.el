;; User pack init file
;;
;; User this file to initiate the pack configuration.
;; See README for more information.

(live-load-config-file "bindings.el")
(live-load-config-file "assoc.el")

(live-add-pack-lib "pi-php-mode")
(require 'pi-php-mode)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
