;;; ob-llm.el --- Use `llm' as an Org Babel language -*- lexical-binding: t -*-

;; Copyright (C) 2025 Grant Surlyn

;; Author: Grant Surlyn <grant@sunflowerseastar.com>
;; URL: https://github.com/sunflowerseastar/ob-llm
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: llm org-mode tools convenience org babel
;; SPDX-License-Identifier: GPL-3.0-or-later

;; IMPORTANT: Also requires `llm' CLI tool, version 0.26
;; Please see https://llm.datasette.io for installation

;; This file is not part of GNU Emacs.

;; ob-llm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ob-llm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ob-llm.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ob-llm provides integration between Org-mode and Simon Willison's LLM
;; command-line tool (further referred to simply as `llm'), enabling interaction
;; with large language models directly from Org documents. If you don't have
;; `llm' installed or configured, please see https://llm.datasette.io to follow
;; setup instructions and orient yourself first.
;;
;; The main idea:
;;
;; - interface with Simon Willison's `llm' tool via Emacs through Org mode
;;   - code block header arguments pass through to `llm' as flags, so you can
;;     specify model, file, website, "continue", conversation id, database, etc.
;; - execute prompts in Babel code blocks with `C-c C-c'
;;   - results stream back into #+RESULTS
;;   - completed response is converted to Org mode syntax or prettified JSON
;;
;;; Usage:
;;
;; Org Babel code blocks with a "language" of llm shell out to `llm', with the
;; code block's <body> serving as the prompt:
;;
;; #+begin_src llm
;; Explain autopoiesis
;; #+end_src
;;
;; When executing this code block with `C-c C-c', it will stream the response
;; into #+RESULTS, and then optionally convert the finalized output into Org
;; mode syntax using Pandoc (or prettified JSON if the code block is using
;; `:schema' or `:schema-multi'). If the code block has a header argument of
;; `:no-conversion', the finalized streamed result will not be converted.
;;
;; If the code block has a header argument of `:results silent', then the
;; response will still go into an output buffer (called the process buffer or
;; `proc-buffer' in the code), but not stream into or be placed into the Org
;; mode buffer where the code block is (aka source buffer or `src-buffer').
;;
;; Code block header arguments that make sense for the `llm' shell command are
;; passed along. For example, to use a model called "my-model" and continue the
;; most recent conversation:
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

(defgroup ob-llm nil
  "Options for ob-llm, a wrapper of the `llm' command line tool."
  :tag "ob-llm"
  :group 'org-babel)

(defvar org-babel-load-languages)
(defvar ob-llm-active-processes nil
  "List of currently running LLM processes.")

(defcustom ob-llm-line-indicator "★"
  "Text to display in the mode line when an LLM process is running.
Set to nil to disable the indicator."
  :type '(choice (string :tag "Indicator text")
          (const :tag "No indicator" nil))
  :group 'ob-llm)

(defcustom ob-llm-post-process-auto-convert-p t
  "Whether to format (json) & convert (md) responses.

    chat (markdown) -> Org mode, schema (JSON) -> prettified JSON

Generally speaking, the text result from `llm' is going to be
either json (if it's a schema prompt) or markdown. If it's json,
then format (prettify) the json. If it's markdown, then convert
it to Org syntax. This setting, t by default, will automatically
attempt to make those conversions when the response is finished
coming back.

When this is t, you can override the conversion on any code block
with the header argument `:no-conversion'."
  :type 'boolean
  :group 'ob-llm)

(defcustom ob-llm-pandoc-additional-org-mode-conversion-flags nil
  "Additional flags to pass to Pandoc when converting from md.

This should be a list of strings, each string being a single flag
or flag with value. For example:

    (\"--lua-filter=/path/to/filter.lua\")"
  :type '(repeat string)
  :group 'ob-llm)

(defcustom ob-llm-models nil
  "List of available LLM models.

This list is populated with `ob-llm-refresh-models' by
converting and inserting the output from the shell command `llm
models'. You should not have to adjust `ob-llm-models' manually,
but you will want to run `ob-llm-refresh-models' whenever you
update the `llm' plugins or otherwise change the models that you
know will be available to `llm'."
  :type '(repeat string)
  :group 'ob-llm)

;;  ---------------------------------------------------------------------------
;;; General helpers
;;  ---------------------------------------------------------------------------

(defun ob-llm--process-header-args (params)
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
         ;; Org Babel code block header arguments - don't include these as flags
         ;; when constructing the `llm' command to shell out; instead, these
         ;; should handle code block execution as usual...
         ((memq key '(:results :exports :cache :noweb :session :tangle
                      :hlines :colname-names :rowname-names :result-type :result-params :wrap))
          (push param org-code-block-header-args))

         ;; ...special header arguments used as "params" for ob-llm application logic...
         ((memq key '(:database :no-conversion))
          (push param custom-params))

         ;; ...and then all other header arguments are going to be passed to the
         ;; `llm' command as flags.
         (t (push param llm-flags)))))

    ;; Return categorized parameters
    (list :org-code-block-header-args org-code-block-header-args
          :custom-params custom-params
          :llm-flags llm-flags)))

(defun ob-llm--filter-params (raw-params)
  "Filter RAW-PARAMS; keep relevant ones for `llm' command line.

Converts Org Babel header arguments to command line flags for `llm'."
  (let ((processed-params (ob-llm--process-header-args raw-params)))
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

(defun ob-llm--prepare-command (body raw-params)
  "Build `llm' shell command based on BODY and RAW-PARAMS."
  (let* ((flags (ob-llm--filter-params raw-params))
         (logs-database-path (alist-get :database raw-params)))
    (format (concat (when logs-database-path
                      (format "LLM_USER_PATH=%s " logs-database-path))
                    "llm %s %s")
            (shell-quote-argument body)
            flags)))

(defun ob-llm--create-output-buffer (buffer-name llm-shell-command)
  "Create and setup output buffer named BUFFER-NAME for command LLM-SHELL-COMMAND."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "LLM Query started at %s\n\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format "Command: %s\n\n" llm-shell-command))
      (insert "Output:\n\n"))
    buffer))

(defun ob-llm--setup-mode-line ()
  "Setup the mode line indicator for active LLM processes."
  (when ob-llm-line-indicator
    (unless (member '(:eval (when ob-llm-active-processes ob-llm-line-indicator))
                    global-mode-string)
      (setq global-mode-string
            (append global-mode-string
                    (list '(:eval (when ob-llm-active-processes ob-llm-line-indicator)))))))
  (force-mode-line-update t))

;; ---------------------------------------------------------------------------
;; Post-processing
;; ---------------------------------------------------------------------------

(defun ob-llm--remove-trailing-backslashes (text)
  "Remove trailing double backslashes from TEXT."
  (replace-regexp-in-string "\\\\\\\\$" "" text))

(defun ob-llm--prettify-json-response (final-output)
  "Pretty-print FINAL-OUTPUT using jq if available."
  (if (executable-find "jq")
      (with-temp-buffer
        (insert final-output)
        (if (zerop (call-process-region (point-min) (point-max)
                                        "jq" t t nil "."))
            (string-trim (buffer-string))
          final-output))
    final-output))

(defun ob-llm--convert-markdown-response-to-org-mode (text)
  "Convert markdown TEXT to Org with Pandoc, or return TEXT if conversion fails."
  (if (executable-find "pandoc")
      (with-temp-buffer
        (insert text)
        (let ((pandoc-args (append '("--from" "markdown+hard_line_breaks"
                                     "--to" "org"
                                     "--sandbox=true"
                                     "--wrap=none")
                                   ob-llm-pandoc-additional-org-mode-conversion-flags)))
          (if (zerop (apply #'call-process-region
                            (point-min) (point-max)
                            "pandoc" t t nil
                            pandoc-args))
              (ob-llm--remove-trailing-backslashes (buffer-string))
            text)))
    text))

(defun ob-llm--post-process-result (final-output schema-p no-conversion-p)
  "Post-process FINAL-OUTPUT.

SCHEMA-P and NO-CONVERSION-P are used to determine how to
process."
  (cond
   ;; 1 - do not perform any post-process conversion
   ((or no-conversion-p (not ob-llm-post-process-auto-convert-p)) final-output)

   ;; 2 - it's a schema, so prettify the JSON response
   (schema-p
    (let ((final-output-after-pp-json (ob-llm--prettify-json-response final-output)))
      (format "#+begin_src json\n%s\n#+end_src" final-output-after-pp-json)))

   ;; 3 - otherwise, convert the markdown response to Org mode syntax
   (t (ob-llm--convert-markdown-response-to-org-mode final-output))))

;;  ---------------------------------------------------------------------------
;;; Streams and processes
;;  ---------------------------------------------------------------------------

(defun ob-llm--insert-output (output src-buffer src-position)
  "Insert OUTPUT into SRC-BUFFER at SRC-POSITION."
  (condition-case err
      (with-current-buffer src-buffer
        (save-excursion
          (goto-char src-position)
          (with-current-buffer src-buffer
            (save-excursion
              (goto-char src-position)
              (org-babel-insert-result output '("raw"))))))
    (error (message "Error in LLM process sentinel: %S" err))))

(defun ob-llm--start-process (llm-shell-command output-buffer)
  "Start `llm' process with LLM-SHELL-COMMAND, out to OUTPUT-BUFFER."

  (let ((proc (start-process-shell-command "org-babel-llm" output-buffer llm-shell-command)))
    ;; Add to active processes list
    (push proc ob-llm-active-processes)
    proc))

(defun ob-llm--prepare-org-result-placeholder (src-buffer src-position params)
  "Create result placeholder in SRC-BUFFER at SRC-POSITION for streaming.

PARAMS are the code block header arguments."
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
                    ;; Skip the header and `#+begin_example' line to stream
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

(defun ob-llm--stream-output (output proc-buffer proc-mark result-marker)
  "Stream OUTPUT into the process buffer & src buffer.

PROC-BUFFER is process buffer with PROC-MARK as marker.
RESULT-MARKER define where to stream in src Org buffer."
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
           (raw-params (when proc (process-get proc 'raw-params)))
           (result-params (cdr (assq :results raw-params)))
           (silent-p (and result-params
                          (stringp result-params)
                          (string-match "silent" result-params))))
      (when (and (not silent-p) result-marker (marker-buffer result-marker))
        (with-current-buffer (marker-buffer result-marker)
          (save-excursion
            (goto-char result-marker)
            (insert clean-output)
            (set-marker result-marker (point))))))))

(defun ob-llm--create-process-filter ()
  "Create process filter function for streaming output."
  (lambda (process output)
    (let ((res-marker (process-get process 'result-marker)))
      (ob-llm--stream-output
       output
       (process-buffer process)
       (process-mark process)
       res-marker))))

(defun ob-llm--create-process-sentinel ()
  "Create process sentinel function for handling completion."
  (lambda (process event)
    (let* ((raw-params (process-get process 'raw-params))
           (src-buffer (process-get process 'src-buffer))
           (src-position (process-get process 'src-position))
           (start-time (process-get process 'start-time))
           (status (substring event 0 -1)) ; remove trailing newline
           (duration (float-time (time-subtract (current-time) start-time)))
           (final-output ""))

      ;; update the output buffer with completion status
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

      ;; handle process completion
      (when (or (string-match "finished" event)
                (string-match "exited" event))
        (let* ((finished-p (string-match "finished" event))
               (exited-p (string-match "exited" event))
               (result-params (cdr (assq :results raw-params)))
               (silent-p (and result-params
                              (stringp result-params)
                              (string-match "silent" result-params))))

          ;; 1 - remove streamed output from org buffer (before finalizing result)
          (if (not silent-p)
              (let* ((stream-start (process-get process 'stream-start-marker))
                     (result-marker (process-get process 'result-marker)))
                (when (and result-marker (marker-buffer result-marker)
                           stream-start (marker-buffer stream-start))
                  (with-current-buffer (marker-buffer result-marker)
                    (delete-region stream-start result-marker)))))

          ;; 2 - with error or non-zero exit, insert result no matter what
          (when (and (buffer-live-p src-buffer) exited-p)
            (ob-llm--insert-output final-output src-buffer src-position))

          ;; 3 - with regular finish, insert output into org-buffer (if not silent)
          (when (and (buffer-live-p src-buffer) (not silent-p) finished-p)
            (let* ((processed-params (ob-llm--process-header-args raw-params))
                   (llm-flags (plist-get processed-params :llm-flags))
                   (schema-p (or (assq :schema llm-flags)
                                 (assq :schema-multi llm-flags)))
                   (custom-params (plist-get processed-params :custom-params))
                   (no-conversion-p (assq :no-conversion custom-params))
                   (processed-final-output
                    (ob-llm--post-process-result final-output schema-p no-conversion-p)))
              (ob-llm--insert-output processed-final-output src-buffer src-position)))))

      ;; remove from active processes list
      (setq ob-llm-active-processes (delq process ob-llm-active-processes))

      ;; update mode line
      (when (and ob-llm-line-indicator (null ob-llm-active-processes))
        (force-mode-line-update t))

      ;; notify user
      (message "LLM process %s in %s seconds" status duration))))

(defun ob-llm-list-active-processes ()
  "Display information about currently running LLM processes."
  (interactive)
  (if ob-llm-active-processes
      (let ((buf (get-buffer-create "*LLM Processes*")))
        (with-current-buffer buf
          (erase-buffer)
          (insert (format "%d active LLM processes:\n\n" (length ob-llm-active-processes)))
          (dolist (proc ob-llm-active-processes)
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

(defun ob-llm-kill-process ()
  "Kill the most recently started LLM process."
  (interactive)
  (if ob-llm-active-processes
      (let ((proc (car ob-llm-active-processes)))
        (when (yes-or-no-p (format "Kill process %s? " (process-name proc)))
          (delete-process proc)
          (message "Process %s killed" (process-name proc))))
    (message "No active LLM processes to kill")))

(defun ob-llm-kill-all-processes ()
  "Kill all running LLM processes."
  (interactive)
  (when ob-llm-active-processes
    (dolist (proc ob-llm-active-processes)
      (delete-process proc))
    (setq ob-llm-active-processes nil)
    (message "All LLM processes killed")
    (message "No active LLM processes to kill")))

;; ---------------------------------------------------------------------------
;; Model handling
;; ---------------------------------------------------------------------------

(defun ob-llm--output-to-model-identifier (line)
  "Return the model identifier found per LINE from `llm models'.

The identifier is everything after the first `: ' up to the next
space or the end of the line. If LINE does not contain such a
identifier, return nil."
  (when (string-match ": \\([^ ]+\\)" line)
    (match-string 1 line)))

(defun ob-llm-refresh-models ()
  "Update `ob-llm-models' with `llm models' shell output."
  (interactive)
  (message "ob-llm-models are being refreshed via the `llm models' command...")
  (let* ((raw (shell-command-to-string "llm models"))
         (lines (split-string raw "\n" t)) ; drop empty strings
         (models (delq nil ; remove nils
                       (mapcar #'ob-llm--output-to-model-identifier lines))))
    (setq ob-llm-models models)
    (customize-save-variable 'ob-llm-models models)
    (message "ob-llm-models have been refreshed. There are now %d models available."
             (length models))
    ob-llm-models))

(defun ob-llm-yank-a-model-name ()
  "Select a model and yank (paste) it into the current buffer."
  (interactive)
  (if (null ob-llm-models)
      (message "ob-llm doesn't know what models are available. Please M-x `ob-llm-refresh-models'.")
    (let ((selection (completing-read "Yank a model name: " ob-llm-models)))
      (insert selection)
      (message "Yanked: %s" selection))))

(defun ob-llm-kill-a-model-name ()
  "Select a model and kill (copy) it."
  (interactive)
  (if (null ob-llm-models)
      (message "ob-llm doesn't know what models are available. Please M-x `ob-llm-refresh-models'.")
    (let ((selection (completing-read "Kill a model name: " ob-llm-models)))
      (kill-new selection)
      (message "Copied: %s" selection))))

(defun ob-llm-echo-default-model ()
  "Echo out the default model so you can see what's set."
  (interactive)
  (shell-command "llm models default"))

(defun ob-llm-change-default-model ()
  "Set a new default model.

With a prefix argument, see the current default model."
  (interactive)
  (if current-prefix-arg (shell-command "llm models default")
    (if (null ob-llm-models)
        (message "ob-llm doesn't know what models are available. Please M-x `ob-llm-refresh-models'.")
      (let ((selection (completing-read "Set new default model: " ob-llm-models)))
        (let ((command (format "llm models default %s" selection)))
          (shell-command command)
          (message "Executed command: %s" command))))))

;;  ---------------------------------------------------------------------------
;;; Database, conversation, logs
;;  ---------------------------------------------------------------------------

(defun ob-llm-browse-conversations ()
  "Browse LLM conversations to yank an ID."
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

(defun ob-llm-query-logs-candidates (json-data)
  "Build an association list of candidates from JSON-DATA.

Each candidate is a cons cell where the car is a formatted string
that displays conversation_name, prompt, conversation_model, and
id in columns, and the cdr is the corresponding id."
  (cl-loop for entry across json-data
           for conv = (alist-get 'conversation_name entry)
           for prompt = (alist-get 'prompt entry)
           for model = (alist-get 'conversation_model entry)
           for id = (alist-get 'conversation_id entry)
           collect
           (cons (format "%-40s %-60s %-20s %s"
                         (or conv "")
                         (or prompt "")
                         (or model "")
                         (or id ""))
                 id)))

(defun ob-llm-query-logs ()
  "Search the llm logs with a query string.

Prompt for a search string (in minibuffer via `read-string'),
query the llm logs by assembling and shelling out to `llm logs -t
--json -q <search-string>', put those results into a
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
      (let* ((candidates
              (ob-llm-query-logs-candidates data)) ; alist of (display . id)
             ;; completing-read expects a collection of strings;
             (selection (completing-read "Select log: " candidates nil t)))
        (when selection
          (let ((conversation-id (cdr (assoc selection candidates))))
            (kill-new conversation-id)
            (message "Killed conversation ID: %s" conversation-id)))))))

;;  ---------------------------------------------------------------------------
;;; Main minor mode setup
;;  ---------------------------------------------------------------------------

;;;###autoload
(defun org-babel-execute:llm (body raw-params)
  "Pass an llm code block to `llm'.

The BODY is the prompt that will be passed to `llm' as a shell
command. RAW-PARAMS (the code block header arguments) will be
processed such that Babel gets the ones it expects, ob-llm gets
the ones it wants (ex. `:no-conversion'), and the rest are passed
to the `llm' shell command as flags."

  ;; Set up the environment
  (let* ((llm-shell-command (ob-llm--prepare-command body raw-params))
         (buffer-name (format "*ob-llm-output-%s*" (make-temp-name "")))
         (buffer (ob-llm--create-output-buffer buffer-name llm-shell-command))
         (src-buffer (current-buffer))
         (src-position (point))
         (start-time (current-time))
         (proc nil)
         (result-marker nil)
         (stream-start-marker nil))

    (message "llm shell command: %s" llm-shell-command)

    ;; with a prefix argument, split and show the output buffer
    (when current-prefix-arg
      (unless (get-buffer-window buffer)
        (let ((new-window (split-window-below)))
          (set-window-buffer new-window buffer))))

    ;; record the place in the src buffer for streaming results
    (setq result-marker (ob-llm--prepare-org-result-placeholder src-buffer src-position raw-params))
    (when result-marker
      (setq stream-start-marker (copy-marker result-marker nil)))

    ;; start the process
    (setq proc (ob-llm--start-process llm-shell-command buffer))

    ;; update mode line
    (ob-llm--setup-mode-line)

    ;; store context as process properties
    (process-put proc 'raw-params raw-params)
    (process-put proc 'start-time start-time)
    (process-put proc 'src-buffer src-buffer)
    (process-put proc 'src-position src-position)
    (process-put proc 'result-marker result-marker)
    (process-put proc 'stream-start-marker stream-start-marker)

    ;; store the original source block location to clean up properly later
    (process-put proc 'source-block-end
                 (save-excursion
                   (goto-char src-position)
                   (when (re-search-forward "^[ \t]*#\\+END_SRC" nil t)
                     (progn (forward-line) (line-end-position)))))

    ;; set the process filter to handle the received stream
    (set-process-filter proc (ob-llm--create-process-filter))

    ;; set the process sentinel for completion/exit handling
    (set-process-sentinel proc (ob-llm--create-process-sentinel))

    ;; return a placeholder -- filter & sentinel handle everything
    nil))

;;;###autoload
(define-minor-mode ob-llm-mode
  "Minor mode to handle llm Babel code blocks in Org-mode.

Execution will send the code block content to Simon Willison's
`llm' command line tool along with relevant header arguments as
flags."
  :lighter " LLM"
  :keymap (let ((map (make-sparse-keymap))) map)
  :group 'ob-llm
  (if ob-llm-mode
      (progn
        (unless (assq 'llm org-babel-load-languages)
          (add-to-list 'org-babel-load-languages '(llm . t)))
        (org-babel-do-load-languages 'org-babel-load-languages
                                     org-babel-load-languages)
        (message "Org LLM mode enabled"))
    (setq org-babel-load-languages (assq-delete-all 'llm org-babel-load-languages))
    (message "Org LLM mode disabled")))

(provide 'ob-llm)

;;; ob-llm.el ends here
