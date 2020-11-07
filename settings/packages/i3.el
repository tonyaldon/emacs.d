(defvar i3-font-lock-keywords
  '(;; for ~/.config/i3/config
    ("bindsym" . font-lock-keyword-face)
    ("^set" . font-lock-keyword-face)
    ("^mode" . font-lock-keyword-face)
    ("^exec" . font-lock-keyword-face)
    ("^workspace" . font-lock-keyword-face)
    ("^bar" . font-lock-builtin-face)
    ("^assign" . font-lock-keyword-face)
    ("^for_window" . font-lock-keyword-face)
    ("\\(colors\\) [{]+" 1 font-lock-builtin-face t)
    ("^font" . font-lock-builtin-face)
    ("^floating_modifier" . font-lock-builtin-face)
		("bindsym \\([^ ]*\\)" 1 font-lock-function-name-face t)
    ;; for ~/.i3status.conf file
    ("^[ \t]*\\(.+?\\)\\(?:\\[\\(.*?\\)\\]\\)?[ \t]*[^+]=" 1 font-lock-variable-name-face t)
    ("\\(^order\\) \\(?:[+][=]+\\)" 1 font-lock-keyword-face t)
    ("\\(general\\) [{]+" 1 font-lock-builtin-face t)
    ("\\(run_watch VPN\\) [{]+" 1 font-lock-builtin-face t)
    ("\\(run_watch DHCP\\) [{]+" 1 font-lock-builtin-face t)
    ("\\(wireless _first_\\) [{]+" 1 font-lock-builtin-face t)
    ("\\(disk\\) .* [{]+" 1 font-lock-builtin-face t)
    ("\\(load\\) [{]+" 1 font-lock-builtin-face t)
    ("\\(cpu_usage\\) [{]+" 1 font-lock-builtin-face t)
    ("\\(battery 0\\) [{]+" 1 font-lock-builtin-face t)
    ("\\(tztime local\\) [{]+" 1 font-lock-builtin-face t)
    )
  "Font lock keywords to use in i3-mode.

These keywords are used in the i3 configuration file (~/.config/i3/config)
and i3status configuration file (~/.i3status.conf).")


(define-derived-mode i3-mode sh-mode "i3-mode"
  "Major mode for i3 configuration files."
  (font-lock-add-keywords nil i3-font-lock-keywords))


(provide 'i3)
