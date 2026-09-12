;;; tatrel.el --- Emacs interface for tatr -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Menderes Sabaz
;;
;; Author: Menderes Sabaz <sabazmenderes@proton.me>
;; Maintainer: Menderes Sabaz <sabazmenderes@proton.me>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, tasks, convenience
;; URL: https://github.com/byfanes/tatrel
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; An Emacs interface for the tatr task manager.
;;
;;; Code:

(require 'subr-x)
(require 'tabulated-list)

(defgroup tatrel nil
  "Emacs interface for the tatr task manager."
  :group 'tools
  :prefix "tatrel-")

(defcustom tatrel-executable "tatr"
  "Path to the tatr executable."
  :type 'string
  :group 'tatrel)

(defvar-local tatrel-show-closed nil
  "Whether to show closed tasks in the current Tatrel buffer.")

(defvar-local tatrel-tags ""
  "Tags to filter by in the current Tatrel buffer.")

(defun tatrel--ensure-executable ()
  "Ensure the tatr executable is available."
  (unless (executable-find tatrel-executable)
    (user-error "Executable '%s' not found. Is tatr installed and in your PATH?" tatrel-executable)))

;;;###autoload
(defun tatrel-find (id)
  "Find and open the TASK.md file for the given task ID."
  (interactive "sTask ID: ")

  (when (string-empty-p id)
    (user-error "Task ID cannot be empty"))

  (let ((root (locate-dominating-file default-directory "tasks")))
    (unless root
      (user-error "Could not find tasks directory"))

    (let ((task-file (expand-file-name (format "tasks/%s/TASK.md" id) root)))
      (unless (file-exists-p task-file)
        (user-error "Task ID '%s' does not exist" id))

      (find-file task-file)
      (message "Opening task ID: %s" id))))

(defun tatrel-open-task-at-point (&optional button)
  "Open the tatr task at point or button."
  (interactive)
  (let ((task-id (if button
                     (button-get button 'task-id)
                   (tabulated-list-get-id))))
    (if task-id
        (tatrel-find task-id)
      (user-error "No task at point"))))

;; Help me. I hate this.
(defun tatrel--get-entries ()
  "Generate entries for `tatrel-list-mode' by running tatr."
  (let* ((root (locate-dominating-file default-directory "tasks"))
         (tasks-dir (when root (expand-file-name "tasks" root)))
         (command
          (concat
           (shell-quote-argument tatrel-executable) " ls"
           (if tatrel-show-closed " -c" "")
           (if (not (string-empty-p tatrel-tags))
               (concat " " (shell-quote-argument tatrel-tags))
             ""))))
    (if (not tasks-dir)
        (progn
          (message "Could not find tasks directory")
          nil)
      (let ((default-directory tasks-dir))
        (delq nil
              (mapcar
               (lambda (line)
                 (when (string-match
                        "^\\./\\([^/]+\\)/TASK\\.md:1: \\([^ ]+\\) \\[PRIORITY: *\\([0-9]+\\)\\] \\(.*\\)$"
                        line)
                   (let ((id       (match-string 1 line))
                         (status   (match-string 2 line))
                         (priority (match-string 3 line))
                         (title    (match-string 4 line)))
                     (list id
                           (vector
                            (list id
                                  'action #'tatrel-open-task-at-point
                                  'task-id id
                                  'follow-link t
                                  'help-echo "Click to open this task")
                            status
                            priority
                            title)))))
               (split-string (shell-command-to-string command) "\n" t)))))))

(define-derived-mode tatrel-list-mode tabulated-list-mode "Tatrel"
  "Major mode for listing tatr tasks."
  (setq tabulated-list-format
        [("Time ID" 17 t)
         ("Status" 6 t)
         ("Priority" 6 t)
         ("Title" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'tatrel--get-entries))

(define-key tatrel-list-mode-map (kbd "RET") #'tatrel-open-task-at-point)
(define-key tatrel-list-mode-map (kbd "o") #'tatrel-open-task-at-point)

(defun tatrel--get-existing-tags ()
  "Parse 'tatr summary' to return a list of known tags for autocomplete."
  (let* ((root (locate-dominating-file default-directory "tasks"))
         (tags nil))
    (when root
      (let* ((default-directory root)
             (output (ignore-errors (shell-command-to-string (format "%s summary" (shell-quote-argument tatrel-executable))))))
        (when output
          (with-temp-buffer
            (insert output)
            (goto-char (point-min))
            (while (re-search-forward "^[ \t]+\\([^ \t\n]+\\)[ \t]+=>" nil t)
              (push (match-string 1) tags))))))
    tags))

;;;###autoload
(defun tatrel-list (show-closed tags)
  "List tatr tasks.
If SHOW-CLOSED is non-nil, closed tasks are included.
TAGS is an optional query string to filter the list."
  (interactive
   (list
    ;; I am still think i should have RET as a valid thing which will be no
    (y-or-n-p "Show closed tasks? ")
    (completing-read "Tags Query (RET for all): " (tatrel--get-existing-tags))))

  (tatrel--ensure-executable)

  (let ((root (locate-dominating-file default-directory "tasks")))
    (unless root
      (user-error "Could not find tasks directory"))

    (let ((buf (get-buffer-create "*tatrel-list*"))
          (dir default-directory))
      (with-current-buffer buf
        (setq default-directory dir)
        (tatrel-list-mode)
        (setq tatrel-show-closed show-closed)
        (setq tatrel-tags tags)
        (tabulated-list-print t))
      (switch-to-buffer buf))))

;;;###autoload
(defun tatrel-new (&optional title tags priority suffix)
  "Create a new tatr task.
TITLE is the task title. TAGS is a list of strings.
PRIORITY determines sorting order (default 100).
SUFFIX is an optional specific directory suffix."
  (interactive
   (let* ((read-title (read-string "Title (default 'New Task'): " nil nil "New Task"))
          (read-tags-str (read-string "Tags (comma-separated, optional): "))
          (read-tags (when (not (string-empty-p read-tags-str))
                       (mapcar #'string-trim (split-string read-tags-str "," t))))
          (read-pri-str (read-string "Priority (default 100): " nil nil "100"))
          (read-pri (if (string-empty-p read-pri-str) 100 (string-to-number read-pri-str)))
          (read-suf (read-string "Suffix (optional): ")))
     (list read-title
           read-tags
           read-pri
           (if (string-empty-p read-suf) nil read-suf))))

  (tatrel--ensure-executable)

  (let* ((actual-title (if (or (null title) (string-empty-p title)) "New Task" title))
         (actual-pri (if priority priority 100))
         (args (list tatrel-executable "new" "-p" (number-to-string actual-pri))))

    (when (and suffix (not (string-empty-p suffix)))
      (setq args (append args (list "-s" suffix))))

    (when tags
      (dolist (tag tags)
        (setq args (append args (list "-t" tag)))))

    (setq args (append args (list actual-title)))

    (let* ((command (mapconcat #'shell-quote-argument args " "))
           (output (shell-command-to-string command))
           (path (when (string-match "^\\([^ \n]+/TASK\\.md\\):" output)
                   (match-string 1 output))))
      (if path
          (find-file path)
        (message "Could not find created task path in output:\n%s"
                 output)))))

;;;###autoload
(defun tatrel-summary ()
  "Show a summary of tasks grouped by tags."
  (interactive)
  (tatrel--ensure-executable)
  (let ((dir default-directory)
        (root (locate-dominating-file default-directory "tasks")))
    (unless root
      (user-error "Could not find tasks directory"))

    (let ((buf (get-buffer-create "*tatrel-summary*"))
          (output (shell-command-to-string (format "%s summary" (shell-quote-argument tatrel-executable)))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq default-directory dir)
          (insert output)
          (goto-char (point-min))

          (while (re-search-forward "^[ \t]+\\([^ \t\n]+\\)[ \t]+=>" nil t)
            (let ((tag-name (match-string 1))
                  (start (match-beginning 1))
                  (end (match-end 1)))

              (make-text-button start end
                                'action (lambda (btn)
                                          (tatrel-list nil (concat ":" (button-get btn 'tatrel-tag))))
                                'tatrel-tag tag-name
                                'help-echo (format "Click to list tasks with tag '%s'" tag-name)
                                'follow-link t))))

        (goto-char (point-min))
        (special-mode))

      (switch-to-buffer buf))))

;;;###autoload
(defun tatrel-version ()
  "Display the current tatrel and tatr version."
  (interactive)
  (tatrel--ensure-executable)
  ;; Idk i put tatrel version too
  (message (concat "tatrel 0.1.0\n"
                   (shell-command-to-string (format "%s version" (shell-quote-argument tatrel-executable))))))

;;;###autoload
(defun tatrel-log (&optional id)
  "Show the git log for the task corresponding to ID.
If called from a TASK.md buffer, default to the current task ID."
  (interactive
   (let ((file (buffer-file-name))
         (current-id nil))

     (when (and file (string-match "/tasks/\\([^/]+\\)/TASK\\.md$" file))
       (setq current-id (match-string 1 file)))

     (list (if current-id
               current-id
             (read-string "Task ID for git log: ")))))

  (when (or (null id) (string-empty-p id))
    (user-error "Task ID cannot be empty"))

  (let ((root (locate-dominating-file default-directory "tasks")))
    (unless root
      (user-error "Could not find tasks directory"))

    (let* ((task-rel-dir (format "tasks/%s" id))
           (task-abs-dir (expand-file-name task-rel-dir root))
           (buf (get-buffer-create (format "*tatrel-log: %s*" id))))

      (unless (file-exists-p task-abs-dir)
        (user-error "Task ID '%s' does not exist" id))

      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (default-directory root))
          (erase-buffer)

          (insert (shell-command-to-string
                   (format "git log -- %s" (shell-quote-argument task-rel-dir))))

          (when (= (buffer-size) 0)
            (insert (format "No git history found for task: %s\n" id)))

          (goto-char (point-min))
          (special-mode)))

      (switch-to-buffer buf))))


;;; Task Minor Mode

(defvar tatrel-task-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") #'tatrel-log)
    map)
  "Keymap for `tatrel-task-minor-mode'.")

;;;###autoload
(define-minor-mode tatrel-task-minor-mode
  "Minor mode for editing tatr TASK.md files.
Provides convenient keybindings for task operations."
  :lighter " Tatrel"
  :keymap tatrel-task-minor-mode-map)

;;;###autoload
(add-hook 'find-file-hook
          (lambda ()
            (when (and buffer-file-name
                       (string-match-p "/tasks/[^/]+/TASK\\.md$" buffer-file-name))
              (tatrel-task-minor-mode 1))))

(provide 'tatrel)
;;; tatrel.el ends here
