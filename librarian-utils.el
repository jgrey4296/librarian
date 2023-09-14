;;; tools/lookup/autoload/xwidget.el -*- lexical-binding: t; -*-

(defvar librarian--xwidget-webkit-last-session-buffer nil)

(defvar librarian-refocus-target  "iTerm")

(defun librarian-get (&optional thing prompt arg)
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

(defun librarian-xwidget-webkit-open-url-fn (url &optional new-session)
  (if (not (display-graphic-p))
      (browse-url url)
    (unless (featurep 'xwidget-internal)
      (user-error "Your build of Emacs lacks Xwidgets support and cannot open Xwidget WebKit browser"))
    (let ((orig-last-session-buffer (if (boundp 'xwidget-webkit-last-session-buffer)
                                        xwidget-webkit-last-session-buffer
                                      nil)))
      (setq xwidget-webkit-last-session-buffer librarian--xwidget-webkit-last-session-buffer)
      (save-window-excursion
        (xwidget-webkit-browse-url url new-session))
      (pop-to-buffer xwidget-webkit-last-session-buffer)
      (setq librarian--xwidget-webkit-last-session-buffer xwidget-webkit-last-session-buffer
            xwidget-webkit-last-session-buffer orig-last-session-buffer))))

(defun librarian--regain-focus ()
  " utility to regain focus when a command will
change focus to something else (preview, firefox)
force it back to the terminal
"
  (when (eq system-type 'darwin)
    (call-process "osascript" nil nil nil
                  "-e" (format "tell application \"%s\"" librarian-refocus-target)
                  "-e" "activate"
                  "-e" "end tell"
                  )
    )
  )

(defun librarian-describe-class()
  " use cl-describe-struct  "
  (interactive)
  (ivy-read "Describe class: "
            obarray
            :predicate #'cl-find-class
            :action #'(lambda (x) (cl-describe-type (intern x)))
            )
  )

(defun librarian-pop-to-xref (result)
  (if (stringp result)
      (message result)
    (let* ((carousel-suppress-adding t)
           (xrefs (list)) ;; (anaconda-mode-make-xrefs result))
           (marker (save-excursion (xref-location-marker (xref-item-location (cl-first xrefs)))))
           (buf (marker-buffer marker))
           )
      (+popup-buffer buf)
      (with-current-buffer buf
        (xref--goto-char marker))
      )
    )
  )

(defun librarian--jump-to (prop identifier &optional display-fn arg)
  (let ((origin (point-marker))
        (result (librarian--run-handler prop identifier))
        )
    ;; Deal with result
    (unwind-protect
        (when (cond ((null result)
                     (message "No lookup handler could find %S" identifier)
                     nil)
                    ((markerp result)
                     (funcall (or display-fn #'switch-to-buffer)
                              (marker-buffer result))
                     (goto-char result)
                     result)
                    (result))
          (with-current-buffer (marker-buffer origin)
            (better-jumper-set-jump (marker-position origin)))
          result)
      (set-marker origin nil))
    )
  )

(defun librarian-file (&optional path)
  "Figure out PATH from whatever is at point and open it.

Each function in `librarian-file-functions' is tried until one changes the point
or the current buffer.

Otherwise, falls back on `find-file-at-point'."
  (interactive)
  (cond ((and path
              buffer-file-name
              (file-equal-p path buffer-file-name)
              (user-error "Already here")))
        ((librarian--jump-to :file path))
        ((user-error "Couldn't find any files here")))
  )

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
               (if (or (string-match jg-help-local-var-skip-regexp
                                     (symbol-name (car x)))
                        (< 40 (length (format "%s" (cdr x)))))
                   (princ (format "(%s : Skipped)" (car x)))
                 (princ x))
               (princ "\n")
               )
      )
    )
  )

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

(evil-define-command evil-librarian-online (query &optional bang)
  "Look up QUERY online. Will prompt for search engine the first time, then
reuse it on consecutive uses of this command. If BANG, always prompt for search
engine."
  (interactive "<a><!>")
  (librarian-online query (librarian--online-provider bang 'evil-ex)))

(evil-define-command evil-librarian-dash (query &optional bang)
  "Look up QUERY in your dash docsets. If BANG, prompt to select a docset (and
install it if necessary)."
  (interactive "<a><!>")
  (let (selected)
    (when bang
      (setq selected (helm-dash-read-docset "Select docset" (helm-dash-official-docsets)))
      (unless (dash-docs-docset-path selected)
        (librarian-install-docset selected)))
    (librarian-in-docsets query selected)))

(defun librarian--fix-ivy-xrefs (fn fetcher alist)
  "HACK Fix #4386: `ivy-xref-show-xrefs' calls `fetcher' twice, which has
  side effects that breaks in some cases (i.e. on `dired-do-find-regexp')."
  (when (functionp fetcher)
    (setf (alist-get 'fetched-xrefs alist)
          (funcall fetcher)))
  (funcall fn fetcher alist))

(advice-add #'ivy-xref-show-xrefs :around #'librarian--fix-ivy-refs)

(provide 'librarian-utils)
