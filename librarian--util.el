;;; librarian--util.el -*- lexical-binding: t; -*-

(defvar librarian--util-local-var-skip-regexp (rx (or "-map"
                                          "keymap"
                                          "display-table"
                                          "imenu-generic-expression"
                                          "font-lock-keywords"))
  )

(defvar librarian--util-xwidget-webkit-last-session-buffer nil)

(defvar librarian--util-refocus-target  "iTerm")

(defvar librarian--util--buffer-display-fn #'+popup-buffer)

(defun librarian--util-get (&optional thing prompt arg)
  "Grab the current selection, THING at point, or xref identifier at point.
returns a str, potentially with text properties"
  (interactive "i\ni\np")
  (declare (side-effect-free t))
  (let ((result (cond ((stringp thing)
                       thing)
                      ((and (fboundp 'evil-visual-state-p) (evil-visual-state-p))
                       (buffer-substring-no-properties evil-visual-beginning evil-visual-end))
                      ((region-active-p)
                       (buffer-substring-no-properties (region-beginning) (region-end)))
                      ((and (not (null thing)) (symbolp thing))
                       (thing-at-point thing t))
                      ((memq (xref-find-backend) '(eglot elpy nox))
                       ;; note from doom: eglot, elpy, nox have different returns for xref, so just use thing at point
                       (thing-at-point 'symbol t))
                      (t
                       (xref-backend-identifier-at-point (xref-find-backend)))
                      )
                ))
    (when arg
      (message "Thing: %s" result))
    result
    )
  )

(defun librarian--util-xwidget-webkit-open-url-fn (url &optional new-session)
  (if (not (display-graphic-p))
      (browse-url url)
    (unless (featurep 'xwidget-internal)
      (user-error "Your build of Emacs lacks Xwidgets support and cannot open Xwidget WebKit browser"))
    (let ((orig-last-session-buffer (if (boundp 'xwidget-webkit-last-session-buffer)
                                        xwidget-webkit-last-session-buffer
                                      nil)))
      (setq xwidget-webkit-last-session-buffer librarian--util-xwidget-webkit-last-session-buffer)
      (save-window-excursion
        (xwidget-webkit-browse-url url new-session))
      (pop-to-buffer xwidget-webkit-last-session-buffer)
      (setq librarian--util-xwidget-webkit-last-session-buffer xwidget-webkit-last-session-buffer
            xwidget-webkit-last-session-buffer orig-last-session-buffer))))

(defun librarian--util--regain-focus ()
  " utility to regain focus when a command will
change focus to something else (preview, firefox)
force it back to the terminal
"
  (when (eq system-type 'darwin)
    (call-process "osascript" nil nil nil
                  "-e" (format "tell application \"%s\"" librarian--util-refocus-target)
                  "-e" "activate"
                  "-e" "end tell"
                  )
    )
  )

;;;###autoload
(defun librarian-describe-class()
  " use cl-describe-struct  "
  (interactive)
  (ivy-read "Describe class: "
            obarray
            :predicate #'cl-find-class
            :action #'(lambda (x) (cl-describe-type (intern x)))
            )
  )

(defun librarian--util-pop-to-xref (result)
  " Given a string | xref (item?)
Display the result
 "
  (if (stringp result)
      (message result)
    (let* ((carousel-suppress-adding t)
           (xrefs  (list))
           (marker (save-excursion (xref-location-marker (xref-item-location (cl-first xrefs)))))
           (buf    (marker-buffer marker))
           )
      (funcall librarian--util--buffer-display-fn buf)
      (with-current-buffer buf
        (xref--goto-char marker))
      )
    )
  )

;;;###autoload
(defun librarian-buffer-locals ()
  (interactive)
  (let ((vars (buffer-local-variables))
        (buf (buffer-name (current-buffer)))
        )
    (with-temp-buffer-window (format "*Buffer Locals: %s" buf)
        'display-buffer-pop-up-window
        (lambda (wind val) (with-selected-window wind
                        (emacs-lisp-mode))
          val)
      (cl-loop for x in vars do
               (if (or (string-match librarian--util-local-var-skip-regexp
                                     (symbol-name (car x)))
                        (< 40 (length (format "%s" (cdr x)))))
                   (princ (format "(%s : Skipped)" (car x)))
                 (princ x))
               (princ "\n")
               )
      )
    )
  )

;;;###autoload
(defun librarian-system-config ()
  (interactive)
  (with-temp-buffer-window "*Emacs Build Configuration*" 'display-buffer-pop-up-window nil
    (princ "Emacs Built with: \n")
    (princ system-configuration-features)
    (cl-loop for line in (s-split " -" system-configuration-options)
             do
             (princ "\n-")
             (princ line)
             )
    )
  )

(defun librarian--util-fix-ivy-xrefs (fn fetcher alist)
  "HACK Fix #4386: `ivy-xref-show-xrefs' calls `fetcher' twice, which has
  side effects that breaks in some cases (i.e. on `dired-do-find-regexp').
originally from doom
"
  (when (functionp fetcher)
    (setf (alist-get 'fetched-xrefs alist)
          (funcall fetcher)))
  (funcall fn fetcher alist))

(provide 'librarian--util)
;;; librarian--util.el ends here
