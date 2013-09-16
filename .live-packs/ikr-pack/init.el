;; User pack init file
;;
;; User this file to initiate the pack configuration.
;; See README for more information.

;; Load bindings config
(live-load-config-file "bindings.el")

(live-add-pack-lib "pi-php-mode")
(require 'pi-php-mode)
