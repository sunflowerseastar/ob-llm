;;; org-llm.el --- Use `llm` in org mode -*- lexical-binding: t -*-

;; Copyright (C) 2025 Grant Surlyn

;; Author: Grant Surlyn <grant@sunflowerseastar.com>
;; URL: https://github.com/sunflowerseastar/org-llm
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: llm org-mode tools convenience org babel
;; SPDX-License-Identifier: GPL-3.0-or-later

;; IMPORTANT: Also requires `llm', version 0.26
;; Please see https://llm.datasette.io for installation

;; This file is not part of GNU Emacs.

;; org-llm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; org-llm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with org-llm.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; org-llm provides integration between Org-mode and Simon Willison's LLM
;; command-line tool (further referred to simply as `llm`), enabling interaction
;; with large language models directly from Org documents. If you don't have
;; `llm` installed or configured, please see https://llm.datasette.io to follow
;; installation instructions and orient yourself first.
;;
;; The idea:
;; - interface with Simon Willison's `llm` tool via Emacs through org mode
;;   - code block header arguments pass through to `llm` as flags, so you can
;;     specify model, file, website, "continue", conversation id, database, etc.
;; - execute prompts in babel src code blocks with C-c C-c
;;   - results stream back into RESULTS
;;   - final streamed result is converted by pandoc into org mode by default
;;     - (disable this with code block header argument `:results silent`)
;;
;; Usage:
;; (require 'org-llm)
;; (org-llm-mode 1)
;;
;; Org babel code blocks with a "language" of 'llm' shell out to `llm`:
;;
;; #+begin_src llm
;; Explain autopoiesis
;; #+end_src
;;
;; When "executing" this code block with C-c C-c, it will stream the results
;; into a RESULTS block, and then convert the finalized output into org-mode
;; using pandoc. If the code block has a header argument of `:results silent`,
;; the finalized streamed result will not be converted into org-mode.
;;
;; Header arguments that make sense for the `llm` shell command are passed
;; along. For example, to use a model called "my-model" and continue the most
;; recent conversation:
;;
;; #+begin_src llm :model my-model :continue
;; What criticism and opposition has it faced over time, and how has it fared
;; (as a concept)
;; #+end_src

;;; Code:

(require 'org)
(require 'ob)
(require 'cl-lib)
(require 'json)

(defvar org-src-lang-modes)
(defvar org-babel-load-languages)

(defgroup org-llm nil
  "Options for Org-mode's LLM (Large Language Model) integration."
  :tag "Org LLM"
  :group 'org-babel)

(defun org-llm--string-to-bool (value)
  "Convert VALUE to a boolean.
If VALUE is nil, \"nil\", \"false\", or \"no\", return nil.
Otherwise, return non-nil."
  (not (or (null value)
           (and (stringp value)
                (member (downcase value) '("nil" "false" "no" "0"))))))

(defun org-llm--process-header-args (params)
  "Process header arguments in PARAMS and return categorized results.
Returns a plist with keys:
- :org-code-block-header-args - standard org babel header arguments
- :custom-params - custom parameters for special handling
- :llm-flags - all remaining header arguments go to `llm' command"
  (let (llm-flags org-code-block-header-args custom-params)
    ;; Process each parameter
    (dolist (param params)
      (let ((key (car param)))
        (cond
         ;; org babel code block header arguments - don't include these as flags
         ;; when constructing the `llm' command to shell out; instead, these
         ;; should handle code block execution as usual...
         ((memq key '(:results :exports :cache :noweb :session :tangle
                      :hlines :colname-names :rowname-names :result-type :result-params :wrap))
          (push param org-code-block-header-args))

         ;; ...special header arguments used as "params" for org-llm application logic...
         ((memq key '(:database :preserve-stream :no-conversion))
          (push param custom-params))

         ;; ...and then all other header arguments are going to be passed to the
         ;; `llm' command as flags.
         (t (push param llm-flags)))))

    ;; Return categorized parameters
    (list :org-code-block-header-args org-code-block-header-args
          :custom-params custom-params
          :llm-flags llm-flags)))

(defcustom org-llm-output-buffer "*org-babel-llm-output*"
  "Buffer name prefix for displaying LLM streaming output.
Each LLM process gets its own uniquely named buffer based on this prefix."
  :type 'string
  :group 'org-llm)

(defcustom org-llm-line-indicator "🦆"
  "Text to display in the mode line when an LLM process is running.
Set to nil to disable the indicator."
  :type '(choice (string :tag "Indicator text")
          (const :tag "No indicator" nil))
  :group 'org-llm)

(defcustom org-llm-post-process-auto-convert-p t
  "Generally speaking, the text result from `llm` is going to be
 either json (if it's a schema prompt) or markdown. If it's json,
 then we probably want to post-process that json to make it look
 nice. If it's markdown, then we probably want to post-process
 that to convert it to org syntax. This setting, `t' by default,
 will automatically attempt to make those conversions when the
 response is finished coming back."
  :type 'boolean
  :group 'org-llm)

(defcustom org-llm-pandoc-additional-org-mode-conversion-flags nil
  "Additional flags to pass to pandoc when converting markdown to org format.
This should be a list of strings, each string being a single flag or flag with value.
For example: '(\"--lua-filter=/path/to/filter.lua\")"
  :type '(repeat string)
  :group 'org-llm)

;; ---------------------------------------------------------------------------
;; Post-processing helpers (markdown → Org, insertion, etc.)
;; ---------------------------------------------------------------------------

(defun org-llm--remove-trailing-backslashes (text)
  "Remove trailing double backslashes from TEXT that appear after pandoc conversion."
  (replace-regexp-in-string "\\\\\\\\$" "" text))

(defun org-llm--markdown->org (text)
  "Convert markdown TEXT to Org with Pandoc, or return TEXT if conversion fails."
  (if (executable-find "pandoc")
      (with-temp-buffer
        (insert text)
        (let ((args (append '("--from" "markdown+hard_line_breaks"
                              "--to" "org"
                              "--sandbox=true"
                              "--wrap=none")
                            org-llm-pandoc-additional-org-mode-conversion-flags)))
          (if (zerop (apply #'call-process-region
                            (point-min) (point-max)
                            "pandoc" t t nil
                            args))
              (org-llm--remove-trailing-backslashes (buffer-string))
            text)))
    text))

(defun org-llm--postprocess-result (final-output params)
  "Apply all requested post-processing steps to FINAL-OUTPUT according to PARAMS."
  (let* ((custom-params (plist-get (org-llm--process-header-args params) :custom-params))
         (no-conversion-p (assq :no-conversion custom-params))
         ;; don't convert if :no-conversion is present (regardless of its value)...
         (apply-post-processing-p (if no-conversion-p nil
                                    ;; ...otherwise, convert according to `org-llm-post-process-auto-convert-p'
                                    org-llm-post-process-auto-convert-p)))
    ;; (message ":: custom-params %s" custom-params)
    ;; (message ":: no-conversion-p %s" no-conversion-p)
    ;; (message ":: apply-post-processing-p %s" apply-post-processing-p)
    (if apply-post-processing-p
        (org-llm--markdown->org final-output)
      final-output)))

(defun org-llm--finalize-result (final-output all-params p-src-buffer p-src-position silent-p)
  "Post-process FINAL-OUTPUT and insert it unless SILENT-P.  Return the processed text."
  (condition-case err
      (with-current-buffer p-src-buffer
        (save-excursion
          (goto-char p-src-position)
          (let ((processed (org-llm--postprocess-result final-output all-params)))
            ;; (message ":: processed %S ::" processed)
            ;; (message "::: all-params %s" all-params)
            (unless silent-p
              (with-current-buffer p-src-buffer
                (save-excursion
                  (goto-char p-src-position)
                  (org-babel-remove-result all-params)
                  (org-babel-insert-result processed '("raw"))))
              processed))))
    (error (message "Error in LLM process sentinel: %S" err))))

(defvar org-babel-default-header-args:llm '((:results . "raw")))

(defvar org-llm-active-processes nil
  "List of currently running LLM processes.")

(defun org-llm-extract-database-param (plist)
  "Extract the value associated with :db from the property list PLIST."
  (let ((db-pair (assoc :database plist)))
    (when db-pair
      (cdr db-pair))))

;; Helper functions for org-babel-execute:llm

(defun org-llm-filter-params (params)
  "Filter PARAMS to include relevant ones for LLM command line.
Converts Org Babel header arguments to command line flags for the LLM tool."
  (let ((processed-params (org-llm--process-header-args params)))
    (mapconcat
     (lambda (param)
       (let ((key (symbol-name (car param)))
             (value (cdr param)))
         ;; Check if the key is valid for 'llm' and if value is non-nil.
         (cond
          ;; Handle parameters that should be translated directly to flags
          ((null value)  ;; Boolean flags
           (let ((flag (substring key 1))) ;; Remove leading ':' from key
             (if (= (length flag) 1)
                 (format " -%s" flag)
               (format " --%s" flag))))
          ((and (string-prefix-p ":" key) value)
           (let ((flag (substring key 1))) ;; Remove leading ':' from key
             (if (= (length flag) 1)
                 (format " -%s %s" flag (shell-quote-argument (format "%s" value)))
               (format " --%s %s" flag (shell-quote-argument (format "%s" value))))))
          ;; Treat single-dash flags separately
          ((member key '("-s"))
           (format "%s" (shell-quote-argument (format "%s" value))))
          (t ""))))
     (plist-get processed-params :llm-flags) "")))

(defun org-llm--prepare-command (body params)
  "Build command string for LLM execution based on BODY and PARAMS."
  (let* ((flags (org-llm-filter-params params))
         (logs-database-path (org-llm-extract-database-param params)))
    (format (concat (when logs-database-path
                      (format "LLM_USER_PATH=%s " logs-database-path))
                    "llm %s %s")
            (shell-quote-argument body)
            flags)))

(defun org-llm--create-output-buffer (buffer-name llm-shell-command)
  "Create and setup output buffer named BUFFER-NAME for command LLM-SHELL-COMMAND."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "LLM Query started at %s\n\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format "Command: %s\n\n" llm-shell-command))
      (insert "Output:\n\n"))
    buffer))

(defun org-llm--start-process (llm-shell-command output-buffer)
  "Start LLM process running LLM-SHELL-COMMAND with output to OUTPUT-BUFFER."
  (let ((proc (start-process-shell-command "org-babel-llm" output-buffer llm-shell-command)))
    ;; Add to active processes list
    (push proc org-llm-active-processes)
    proc))

(defun org-llm--setup-mode-line ()
  "Setup the mode line indicator for active LLM processes."
  (when org-llm-line-indicator
    (unless (member '(:eval (when org-llm-active-processes org-llm-line-indicator))
                    global-mode-string)
      (setq global-mode-string
            (append global-mode-string
                    (list '(:eval (when org-llm-active-processes org-llm-line-indicator)))))))
  (force-mode-line-update t))

(defun org-llm--prepare-org-result-placeholder (src-buffer src-position params)
  "Create result placeholder in SRC-BUFFER at SRC-POSITION for streaming.
PARAMS are the source block parameters."
  (when (buffer-live-p src-buffer)
    (with-current-buffer src-buffer
      (save-excursion
        (goto-char src-position)
        ;; Check if results should be silent
        (let* ((result-params (cdr (assq :results params)))
               (silent-p (and result-params
                              (stringp result-params)
                              (string-match "silent" result-params))))
          ;; Create a placeholder result for non-silent results
          (when (not silent-p)
            (org-babel-insert-result "" '(:results "replace")))

          ;; For streaming, we need a marker even for silent results
          (when (not silent-p)
            (save-excursion
              (if (not silent-p)
                  ;; If not silent, use the results block position
                  (progn
                    (re-search-forward "^[ \t]*#\\+RESULTS:" nil t)
                    ;; Skip the header and `#+begin_example` line to stream
                    ;; directly *inside* the example block.
                    (forward-line 2)
                    (point-marker))
                ;; For silent results, create a temporary marker at the end of the block
                (goto-char src-position)
                (if (re-search-forward "^[ \t]*#\\+END_SRC" nil t)
                    (progn
                      (end-of-line)
                      ;; Insert two newlines after the end of the block to ensure proper formatting
                      (insert "\n\n")
                      (point-marker))
                  (point-marker))))))))))

(defun org-llm--stream-output (output proc-buffer proc-mark result-marker)
  "Stream OUTPUT to multiple destinations.
PROC-BUFFER is process buffer with PROC-MARK as marker.
SRC-BUFFER and RESULT-MARKER define where to stream in org buffer."
  (let ((clean-output (replace-regexp-in-string "\r" "" output)))
    ;; Stream to process buffer
    (with-current-buffer proc-buffer
      (let ((moving (= (point) proc-mark)))
        (save-excursion
          (goto-char proc-mark)
          (insert clean-output)
          (set-marker proc-mark (point)))
        (when moving (goto-char proc-mark))))

    ;; Stream to org buffer result if marker is available and it's not silent
    (let* ((proc (get-buffer-process proc-buffer))
           (params (when proc (process-get proc 'all-params)))
           (result-params (cdr (assq :results params)))
           (silent-p (and result-params
                          (stringp result-params)
                          (string-match "silent" result-params))))
      (when (and (not silent-p) result-marker (marker-buffer result-marker))
        (with-current-buffer (marker-buffer result-marker)
          (save-excursion
            (goto-char result-marker)
            (insert clean-output)
            (set-marker result-marker (point))))))))

(defun org-llm--create-process-filter ()
  "Create process filter function for streaming output."
  (lambda (process output)
    (let ((res-marker (process-get process 'result-marker)))
      (org-llm--stream-output
       output
       (process-buffer process)
       (process-mark process)
       res-marker))))

(defun org-llm--create-process-sentinel ()
  "Create process sentinel function for handling completion."
  (lambda (process event)
    (let* ((all-params (process-get process 'all-params))
           (p-src-buffer (process-get process 'src-buffer))
           (p-src-position (process-get process 'src-position))
           (p-start-time (process-get process 'start-time))
           (status (substring event 0 -1)) ; Remove trailing newline
           (duration (float-time (time-subtract (current-time) p-start-time)))
           (final-output ""))

      (message ":: all-params %s" all-params)

      ;; Update the output buffer with completion status
      (with-current-buffer (process-buffer process)
        (goto-char (point-max))
        (insert (format "\n\n--- Process %s after %s seconds ---\n"
                        status duration))
        ;; Extract only the actual output, not the metadata
        (save-excursion
          (goto-char (point-min))
          (if (search-forward "Output:\n\n" nil t)
              (let ((start (point)))
                (goto-char (point-max))
                (if (search-backward "\n\n--- Process " nil t)
                    (setq final-output (buffer-substring-no-properties start (point)))
                  (setq final-output (buffer-substring-no-properties start (point-max)))))
            (setq final-output (buffer-string)))))

      ;; Handle process completion (streaming has finished)
      (when (string-match "finished" event)
        (let* ((result-params (cdr (assq :results all-params)))
               (silent-p (and result-params
                              (stringp result-params)
                              (string-match "silent" result-params))))

          ;; 1 - remove streamed output from org buffer (before finalizing result)
          ;; (message "::: before remove streamed output: %S" silent-p)
          (let* ((custom-params (plist-get (org-llm--process-header-args all-params) :custom-params))
                 (preserve-stream-param (assq :preserve-stream custom-params))
                 (default-preserve-stream-p silent-p)
                 (preserve-stream-p (org-llm--string-to-bool (if preserve-stream-param
                                                                 (cdr preserve-stream-param)
                                                               default-preserve-stream-p)))
                 (result-marker (process-get process 'result-marker))
                 (stream-start (process-get process 'stream-start-marker)))

            ;; (message "::: before delete %S %S %S %S %S" custom-params preserve-stream-param preserve-stream-p result-marker stream-start)
            ;; (message "::::: %S1 " result-marker (marker-buffer result-marker))
            ;; (message "::::: %S2 " stream-start (marker-buffer stream-start))

            ;; Clean up streamed content unless preserved
            (when (and (not preserve-stream-p)
                       result-marker (marker-buffer result-marker)
                       stream-start (marker-buffer stream-start))
              (with-current-buffer (marker-buffer result-marker)
                (delete-region stream-start result-marker))))

          ;; 2 - insert output into org-buffer (if not silent)
          (when (and (buffer-live-p p-src-buffer) (not silent-p))
            (org-llm--finalize-result final-output all-params p-src-buffer p-src-position silent-p))))

      ;; Remove from active processes list
      (setq org-llm-active-processes (delq process org-llm-active-processes))
      ;; (message "::: org-llm-active-processes after %S" org-llm-active-processes)

      ;; Update mode line
      (when (and org-llm-line-indicator (null org-llm-active-processes))
        (force-mode-line-update t))

      ;; Notify user
      (message "LLM process %s in %s seconds" status duration))))

(defun org-babel-execute:llm (body params)
  "Execute a block of LLM code with Babel, with streaming output.
This function runs asynchronously, allowing editing to continue while
the LLM process runs in the background.
With prefix argument, display the output buffer in a split window."
  (message "Executing org-babel block: llm (waiting for output...)")
  (message "database path %s" (org-llm-extract-database-param params))

  ;; Set up the environment
  (let* ((llm-shell-command (org-llm--prepare-command body params))
         (buffer-name (format "%s-%s" org-llm-output-buffer (make-temp-name "")))
         (buffer (org-llm--create-output-buffer buffer-name llm-shell-command))
         (src-buffer (current-buffer))
         (src-position (point))
         (start-time (current-time))
         (proc nil)
         (result-marker nil)
         (stream-start-marker nil))

    (message "Running LLM command: %s" llm-shell-command)

    ;; Display the buffer in a window only when prefix arg is provided
    (when current-prefix-arg
      (unless (get-buffer-window buffer)
        (let ((new-window (split-window-below)))
          (set-window-buffer new-window buffer))))

    ;; Prepare a place in the org buffer for streaming results
    (setq result-marker (org-llm--prepare-org-result-placeholder src-buffer src-position params))
    ;; Record the immutable start of the streaming region
    (when result-marker
      (setq stream-start-marker (copy-marker result-marker nil)))

    ;; Set up and start the process
    (setq proc (org-llm--start-process llm-shell-command buffer))

    ;; Update the mode line
    (org-llm--setup-mode-line)

    ;; Store context information as process properties
    (process-put proc 'all-params params)
    (process-put proc 'start-time start-time)
    (process-put proc 'src-buffer src-buffer)
    (process-put proc 'src-position src-position)
    (process-put proc 'result-marker result-marker)
    (process-put proc 'stream-start-marker stream-start-marker)

    ;; Store the original source block location to clean up properly later
    (process-put proc 'source-block-end
                 (save-excursion
                   (goto-char src-position)
                   (when (re-search-forward "^[ \t]*#\\+END_SRC" nil t)
                     (progn (forward-line) (line-end-position)))))

    ;; Set up the process filter to handle streaming output
    (set-process-filter proc (org-llm--create-process-filter))

    ;; Set up the process sentinel to handle completion
    (set-process-sentinel proc (org-llm--create-process-sentinel))

    ;; Return a placeholder - the real result will be inserted by the sentinel
    nil))

;;;###autoload
(define-minor-mode org-llm
  "Minor mode to handle `llm` blocks in Org-mode. This mode
 enables execution of LLM (Large Language Model) code blocks in
 Org-mode documents. When enabled, you can create source blocks
 with the 'language' set to `llm` and execute them with C-c C-c.
 In this sense, execution means \"send this code block content to
 Simon Willison's `llm` command line tool, and pass the org babel
 code block header arguments (that are relevant to `llm`) to the
 `llm` command."
  :lighter " LLM"
  :keymap (let ((map (make-sparse-keymap))) map)
  :group 'org-llm
  (if org-llm
      (progn
        (add-to-list 'org-src-lang-modes '("llm" . fundamental))
        (add-to-list 'org-babel-load-languages '(llm . t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
        (message "Org LLM mode enabled"))
    (message "Org LLM mode disabled")))

(defun org-llm-list-active-processes ()
  "Display information about currently running LLM processes."
  (interactive)
  (if org-llm-active-processes
      (let ((buf (get-buffer-create "*LLM Processes*")))
        (with-current-buffer buf
          (erase-buffer)
          (insert (format "%d active LLM processes:\n\n" (length org-llm-active-processes)))
          (dolist (proc org-llm-active-processes)
            (let ((start-time (process-get proc 'start-time))
                  (duration (float-time (time-subtract (current-time)
                                                       (process-get proc 'start-time))))
                  (buffer (process-buffer proc)))
              (insert (format "Process: %s\n" (process-name proc)))
              (insert (format "Started: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S" start-time)))
              (insert (format "Running for: %.1f seconds\n" duration))
              (insert (format "Output buffer: %s\n\n" (buffer-name buffer))))))
        (display-buffer buf))
    (message "No active LLM processes")))

(defun org-llm-kill-process ()
  "Kill the most recently started LLM process."
  (interactive)
  (if org-llm-active-processes
      (let ((proc (car org-llm-active-processes)))
        (when (yes-or-no-p (format "Kill process %s? " (process-name proc)))
          (delete-process proc)
          (message "Process %s killed" (process-name proc))))
    (message "No active LLM processes to kill")))

(defun org-llm-kill-all-processes ()
  "Kill all running LLM processes."
  (interactive)
  (when org-llm-active-processes
    (dolist (proc org-llm-active-processes)
      (delete-process proc))
    (setq org-llm-active-processes nil)
    (message "All LLM processes killed")
    (message "No active LLM processes to kill")))

(defcustom org-llm-models nil
  "List of available LLM models. This list is populated by
 `org-llm-refresh-models` from the output of `llm models`."
  :type '(repeat string)
  :group 'org-llm)

(defun org-llm--output-to-model-identifier (line)
  "Return the model token found in LINE produced by 'llm models'.
The token is everything after the first ": " up to the next space
or the end of the line.  If LINE does not contain such a token,
return nil."
  (when (string-match ": \\([^ ]+\\)" line)
    (match-string 1 line)))

(defun org-llm-refresh-models ()
  "Update `org-llm-models' with the current output of 'llm models'."
  (interactive)
  (message "org-llm-models are being refreshed via the `llm models` command...")
  (let* ((raw    (shell-command-to-string "llm models"))
         (lines  (split-string raw "\n" t))      ; drop empty strings
         (models (delq nil                       ; remove any nils
                       (mapcar #'org-llm--output-to-model-identifier lines))))
    ;; Save models to the customization variable
    (setq org-llm-models models)
    (customize-save-variable 'org-llm-models models)
    (message "org-llm-models have been refreshed. There are %d models available."
             (length models))
    org-llm-models))

(defun org-llm--initialize-models ()
  "Initialize models if needed.
If `org-llm-models' is empty, refresh the models."
  (when (null org-llm-models)
    (org-llm-refresh-models)))

;; Initialize models when the package loads
(add-hook 'after-init-hook 'org-llm--initialize-models)

(defun org-llm-yank-a-model-name ()
  "Select a model and yank it into the current buffer."
  (interactive)
  (let ((selection (completing-read "Paste (yank) a model name: " org-llm-models)))
    (insert selection)
    (message "Yanked: %s" selection)))

(defun org-llm-copy-a-model-name ()
  "Select a model and copy (kill) it."
  (interactive)
  (let ((selection (completing-read "Copy (kill) a model name: " org-llm-models)))
    (kill-new selection)  ; Copy the selected item to the clipboard
    (message "Copied: %s" selection)))

(defun org-llm-change-default-model ()
  "Set a new default model. With a prefix argument, see the
 current default model."
  (interactive)
  (if current-prefix-arg (shell-command "llm models default")
    (let ((selection (completing-read "Set new default model: " org-llm-models)))
      (let ((command (format "llm models default %s" selection)))
        (shell-command command)
        (message "Executed command: %s" command)))))

(defun org-llm-browse-conversations ()
  "Browse LLM conversations and yank the ID of the selected
 conversation."
  (interactive)
  (let* ((output (shell-command-to-string "llm logs -t --json"))
         (conversations (json-read-from-string output))
         (choices (mapcar (lambda (conversation)
                            (format "%s | %s | %s | %s"
                                    (cdr (assoc 'conversation_name conversation))
                                    (cdr (assoc 'prompt conversation))
                                    (cdr (assoc 'conversation_model conversation))
                                    (cdr (assoc 'id conversation))))
                          conversations))
         (selected (completing-read "Select conversation: " choices)))
    ;; extract the corresponding ID from the selected conversation
    (let ((conversation-parts (split-string selected " | ")))
      (when (>= (length conversation-parts) 4)
        (let ((conversation-id (nth 3 conversation-parts)))
          (kill-new conversation-id)
          (message "Yanked conversation ID: %s" conversation-id))))))

(defun org-llm-query-logs-candidates (json-data)
  "Build an association list of candidates from JSON-DATA.
Each candidate is a cons cell where the car is a formatted string that
displays conversation_name, prompt, conversation_model, and id in columns,
and the cdr is the corresponding id."
  (cl-loop for entry across json-data
           for conv    = (alist-get 'conversation_name entry)
           for prompt  = (alist-get 'prompt entry)
           for model   = (alist-get 'conversation_model entry)
           for id      = (alist-get 'conversation_id entry)
           collect
           (cons (format "%-40s %-60s %-20s %s"
                         (or conv "")
                         (or prompt "")
                         (or model "")
                         (or id ""))
                 id)))

(defun org-llm-query-logs ()
  "Prompt for a search string (in minibuffer via `read-string'),
 query the llm logs by assembling and shelling out to
 `llm logs -t --json -q <search-string>`, put those results into a
 `completing-read', and then kill the conversation id of the
 chosen one."
  (interactive)
  (let* ((search-str (read-string "Enter search string: "))
         (llm-logs-shell-command (format "llm logs -n 0 -t --json -q \"%s\"" search-str))
         (json-output (shell-command-to-string llm-logs-shell-command))
         ;; Ensure JSON arrays are read as vectors
         (json-array-type 'vector)
         (data (ignore-errors (json-read-from-string json-output))))
    (if (not data)
        (message "No data returned from command: %s" llm-logs-shell-command)
      (let* ((candidates (org-llm-query-logs-candidates data))
             ;; completing-read expects a collection of strings; our
             ;; candidates list is an alist of (display . id).
             (choice (completing-read "Select log: " candidates nil t)))
        (when choice
          (let ((conversation-id (cdr (assoc choice candidates))))
            (kill-new conversation-id)
            (message "Copied (killed) conversation ID: %s" conversation-id)))))))

(provide 'org-llm)

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-llm.el ends here
