;; highlight nicknames
(require 'erc-highlight-nicknames)
(add-to-list 'erc-modules 'highlight-nicknames)
(erc-update-modules)

;; erc: hide join part messages
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(provide 'init-erc)
