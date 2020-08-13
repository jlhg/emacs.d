(when (fboundp 'native-compile-async)
  (if (yes-or-no-p "async compile?")
      (setq comp-deferred-compilation t)
    (setq comp-deferred-compilation nil)))

(provide 'init-native-comp)
