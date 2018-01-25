;;; Environment setup
(defun psv/fs ()
  (make-directory "~/Books" t)
  (make-directory "~/Distributives" t)
  (make-directory "~/Downloads" t)
  (make-directory "~/Dropbox" t)
  (make-directory "~/bin" t)
  (make-directory "~/src" t)
  (make-directory "~/workspace" t))

(defun psv/personal-fs ()
  (make-directory "~/Audioworks" t)
  (make-directory "~/Documents" t)
  (make-directory "~/Music" t)
  (make-directory "~/Music/heap" t)
  (make-directory "~/Music/lossy" t)
  (make-directory "~/Music/lossless" t)
  (make-directory "~/Music/soundtracks" t)
  (make-directory "~/Videos" t)
  (make-directory "~/Videos/audio" t)
  (make-directory "~/Videos/emacs" t)
  (make-directory "~/Videos/guitar" t)
  (make-directory "~/Videos/movies" t)
  (make-directory "~/Videos/programming" t))

(defvar psv/executables '("bash" "git" "ag" "go" "global" "wget" "curl"
			  "gocode" "godef" "goimports" "golint" ; some golang tools
			  "plantuml.jar" "ditaa.jar" "dot"
			  "tmux" "ssh" "sshpass"
			  "exercism")
  "List of executables that should be installed for comfortable work")

(defun psv/check-executable (program)
  (unless (executable-find program)
    (warn "'%s' is not in $PATH or not installed, try ~/.emacs.d/sh/%s.sh" program program)))

(defun psv/check-executables ()
  (mapcar 'psv/check-executable psv/executables))

;; (psv/fs)
;; (psv/check-executables)

(provide 'base-env)
