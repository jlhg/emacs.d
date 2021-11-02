(when (fboundp 'native-compile-async)
  (if (yes-or-no-p "async compile? ")
      (progn
        (setq package-native-compile t)
        (native-compile-async "~/.emacs.d/package" 'recursively)
        ;; Block until native compilation has finished.
        (while (or comp-files-queue
                   (> (comp-async-runnings) 0))
          (sleep-for 1)))))

(provide 'init-native-comp)
