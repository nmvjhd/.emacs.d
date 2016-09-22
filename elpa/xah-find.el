;;; xah-find.el --- find replace in pure emacs lisp. Purpose similar to unix grep/sed.

;; Copyright © 2012-2015 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Version: 2.0.1
;; Created: 02 April 2012
;; Keywords: convenience, extensions, files, tools, unix

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; Provides emacs commands for find/replace. Similar to {grep, sed}, but entirely written emacs lisp.

;; This package provides the follow functions:

;; xah-find-text                → like grep
;; xah-find-text-regex          → like regex grep
;; xah-find-count               → like grep count
;; xah-find-replace-text        → like sed
;; xah-find-replace-text-regex  → like sed

;; This package is most useful when:

;; • On Windows and don't have unix find/grep/sed utils installed.

;; • Process lots Unicode chars. See  http://xahlee.info/comp/unix_uniq_unicode_bug.html and http://ergoemacs.org/emacs/emacs_grep_problem.html

;; • Find/Replace string that contains newline chars.

;; • You want to use emacs regex, not shell's regex.

;; These commands treat a file as sequence of chars, not as lines as in sed, so it's much more easier to find or replace char sequences.

;; The printed report is also not based on lines. Instead, visual separator are used for easy reading.

;; For each occurance or replacement, n chars will be printed before and after. The number of chars to show is defined by `xah-find-print-before' and `xah-find-print-after'

;; each “block of text” in output is one occurrence.
;; for example, if a line in a file has 2 occurrences, then the same line will be reported twice, as 2 “blocks”.
;; so, the number of blocks corresponds exactly to the number of occurrences.

;; Ignore directories.
;; Add the following in your init:

;; (setq
;;  xah-find-dir-ignore-regex-list
;;  [
;;   "\\.git/"
;;    ; more path regex here
;;   ])

;; TODO:
;; The output isn't beautiful. May be hard to read.
;; File path in output isn't clickable. (use M-x `ffap' for now.)
;; Highlighting is clunky (am using `highlight-lines-matching-regexp' instead of coding my own text properties)

;; I've been using this for 2 years, about every week, on linux (and Windows), on 5 thousand HTML files.

;; Do you find it useful? Help me make it better.
;; Buy Xah Emacs Tutorial
;; http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html

;;; INSTALL

;; To install manually, place this file in the directory 〔~/.emacs.d/lisp/〕

;; Then, place the following code in your emacs init file

;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (autoload 'xah-find-text "xah-find" "find replace" t)
;; (autoload 'xah-find-text-regex "xah-find" "find replace" t)
;; (autoload 'xah-find-replace-text "xah-find" "find replace" t)
;; (autoload 'xah-find-replace-text-regex "xah-find" "find replace" t)
;; (autoload 'xah-find-count "xah-find" "find replace" t)

;;; HISTORY

;; 2015-05-20 changes won't be logged here anymore, unless incompatible change.
;; version 2.0.0, 2015-05-20 • major rewrite and rename. prepare for MELPA
;; version 1.6.9, 2014-05-29 • turned on undo in output buffer
;; version 1.6.8, 2013-07-05 • More options added to “xah-find-text”, “xah-find-text-regex”, “xah-find-replace-text”. Output format improved. Much code refactoring.
;; version 1.6.7, 2013-06-17 • WARNING the argument for case search is reversed for xah-find-replace-text-regex. • added a case search option for xah-find-text-regex
;; version 1.6.6, 2012-12-16 Now, the backup file's suffix is same for all backup files created during one command call. Before, each backup file has timestamp when the backup file is created, that is, their seconds will differ.
;; version 1.6.5, 2012-12-08 improved the prompt for “xah-find-count” and also its output.
;; version 1.6.4, 2012-12-06 Backup file name now has this format: 「~‹x›~‹datetimestamp›~」 where ‹x› is 「t」 for plain text replace and 「r」 for regex replace. e.g. 「x.html~r~20121206_095642~」 Also, modified the prompt for 「xah-find-replace-text-regex」 so it is consistent with the function's argument.
;; version 1.6.3, 2012-11-30 fixed a bug: when one of the find or find/replace is called, and the temp output buffer already exits, the highlighting doesn't work. Now it does work.
;; version 1.6.2, 2012-11-29 trival change. Changed output file names to consistently start with “•” instead of some “◆”
;; version 1.6.1, 2012-11-20 improved the highlighting for xah-find-replace-text. It now highlighting the replaced text, instead of the find text.
;; version 1.6, 2012-08-12 added xah-find-count.
;; version 1.5, 2012-07-24 minor modification to the output format, made more consistent, added a utf-8 header.
;; version 1.4, 2012-07-21 added prompt for a dir on “xah-find-text” and all others.
;; version 1.3, 2012-07-19 added “xah-find-replace-text-regex”
;; version 1.2, 2012-07-14 added “xah-find-replace-text”
;; version 1.1, 2012-05-11 modified xah-find-text so that same line are not printed.
;; version 1.0, 2012-04-02 First version.

;;; TODO:
;; 2015-05-20 the feeble find-lisp-find-files is becoming a efficiency pain. It uses one regex to list all files, then you have to filter dir. And, there's no alternative except some “modern” API third-party shiny thing


;;; Code:

(require 'find-lisp) ; in emacs
(require 'hi-lock) ; in emacs

(defcustom xah-find-print-before 100 "Number of characters to print before search string."
  :group 'xah-find
  )
(setq xah-find-print-before 100)

(defcustom xah-find-print-after 30 "Number of characters to print after search string."
  :group 'xah-find
  )
(setq xah-find-print-after 30)

(defcustom xah-find-dir-ignore-regex-list nil "A list or vector of regex patterns, if match, that directory will be ignored. Case is dependent on current value of `case-fold-search'"
  :group 'xah-find
  )
(setq
 xah-find-dir-ignore-regex-list
 [
  "\\.git/"

  "xahlee_info/php-doc/"
  "xahlee_info/node_api/"
  "xahlee_info/java8_doc/"
  "xahlee_info/css_transitions/"
  "xahlee_info/css3_spec_bg/"
  "xahlee_info/css_3_color_spec/"
  "xahlee_info/REC-SVG11-20110816/"
  "xahlee_info/python_doc_3.3.3/"
  "xahlee_info/python_doc_2.7.6/"
  "xahlee_info/jquery_doc/"
  "xahlee_info/javascript_ecma-262_5.1_2011/"
  "xahlee_info/git-bottomup/"
  "xahlee_info/dom-whatwg/"
  "xahlee_info/css_2.1_spec/"
  "xahlee_info/clojure-doc-1.6/"

  ])

(defcustom xah-find-separator nil "A string that act as separator."
  :group 'xah-find
  )
(setq
 xah-find-separator
 "---88--------------------------------------------------\n")



(defun xah-find--filter-list (φpredicate φsequence)
  "Return a new list such that φpredicate is true on all members of φsequence.
URL `http://ergoemacs.org/emacs/elisp_filter_list.html'
Version 2015-05-23"
  (delete
   "e3824ad41f2ec1ed"
   (mapcar
    (lambda (ξx)
      (if (funcall φpredicate ξx)
          ξx
        "e3824ad41f2ec1ed" ))
    φsequence)))

(defun xah-find--ignore-dir-p (φpath)
  "Return true if φpath should be ignored. Else, nil."
  (catch 'catch25001
    (mapc
     (lambda (x)
       (when (string-match x φpath) (throw 'catch25001 x)))
     xah-find-dir-ignore-regex-list)
    nil
    ))



(defun xah-find--backup-suffix (φs)
  "Return a string of the form 「~‹φs›~‹date time stamp›~」"
  (concat "~" φs (format-time-string "%Y%m%d%H%M%S") "~"))

(defun xah-find--current-date-time-string ()
  "Returns current date-time string in full ISO 8601 format.
Example: 「2012-04-05T21:08:24-07:00」.

Note, for the time zone offset, both the formats 「hhmm」 and 「hh:mm」 are valid ISO 8601. However, Atom Webfeed spec seems to require 「hh:mm」."
  (concat
   (format-time-string "%Y-%m-%dT%T")
   ((lambda (ξx) (format "%s:%s" (substring ξx 0 3) (substring ξx 3 5))) (format-time-string "%z"))))

(defun xah-find--print-header (φinput-dir φpath-regex φsearch-str &optional φreplace-str )
  "Print things"
  (interactive)
  (princ
   (concat
    "-*- coding: utf-8 -*-" "\n"
    "Datetime: " (xah-find--current-date-time-string) "\n"
    "Result of: " (symbol-name real-this-command) "\n"
    (format "Directory ❮%s❯\n" φinput-dir )
    (format "Path regex ❮%s❯\n" φpath-regex )
    (format "Search string ❮%s❯\n" φsearch-str )
    (when φreplace-str
      (format "Replace string ❮%s❯\n" φreplace-str))
    xah-find-separator
    )))

(defun xah-find--print-text-block (φstring9462)
  "print string9462"
  (princ (format "\n❮%s❯\n\n" φstring9462)))

(defun xah-find--print-file-count (φfilepath4287 φcount8086)
  "Print file path and count"
  (princ (format "• %d %s\n" φcount8086 φfilepath4287 )))

(defun xah-find--switch-and-highlight (φbuffer φhi-str &optional φuse-regex-p)
  "switch to φbuffer and highlight stuff"
  (interactive)
  (progn
    (switch-to-buffer φbuffer)
    (delete-other-windows)
    (fundamental-mode)
    (hi-lock-mode) ; todo: implement my own coloring
    (buffer-enable-undo)
    (when (not (string= φhi-str ""))
      (highlight-phrase
       (if φuse-regex-p
           φhi-str
         (regexp-quote φhi-str))
       (quote hi-yellow)))
    (highlight-lines-matching-regexp "^• " (quote hi-pink))))



;;;###autoload
(defun xah-find-text (φsearch-str1 φinput-dir φpath-regex φfixed-case-search-p φprintContext-p)
  "Report files that contain string.
By default, not case sensitive, and print surrounding text.
If `universal-argument' is called first, prompt to ask."
  (interactive
   (let (
         (ξdefault-input
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word))))
     (list
      (read-string (format "Search string (default %s): " ξdefault-input) nil 'query-replace-history ξdefault-input)
      (ido-read-directory-name "Directory: " default-directory default-directory "MUSTMATCH")
      (read-from-minibuffer "Path regex: " nil nil nil 'dired-regexp-history)
      (if current-prefix-arg (y-or-n-p "Fixed case in search?") nil )
      (if current-prefix-arg (y-or-n-p "Print surrounding Text?") t ))))

  (let (
        (case-fold-search (not φfixed-case-search-p))
        (ξcount 0)
        (ξoutputBuffer "*xah-find output*")
        ξp1 ; context begin position
        ξp2 ; context end position
        )

    (setq φinput-dir (file-name-as-directory φinput-dir)) ; normalize dir path

    (with-output-to-temp-buffer ξoutputBuffer
      (xah-find--print-header φinput-dir φpath-regex φsearch-str1 )
      (mapc
       (lambda (ξpath)
         (setq ξcount 0)
         (with-temp-buffer
           (insert-file-contents ξpath)
           (while (search-forward φsearch-str1 nil "NOERROR")
             (setq ξcount (1+ ξcount))
             (setq ξp1 (max 1 (- (match-beginning 0) xah-find-print-before )))
             (setq ξp2 (min (point-max) (+ (match-end 0) xah-find-print-after )))
             (when φprintContext-p (xah-find--print-text-block (buffer-substring-no-properties ξp1 ξp2 ))))
           (when (> ξcount 0)
             (xah-find--print-file-count ξpath ξcount))))

       (xah-find--filter-list
        (lambda (x)
          (not (xah-find--ignore-dir-p x)))
        (find-lisp-find-files φinput-dir φpath-regex)))

      (xah-find--switch-and-highlight ξoutputBuffer φsearch-str1))))

;;;###autoload
(defun xah-find-text-regex (φsearch-regex φinput-dir φpath-regex φfixed-case-search-p φprint-context-level )
  "Report files that contain a string pattern, similar to unix grep."
  (interactive
   (list
    (read-string (format "Search regex (default %s): " (current-word)) nil 'query-replace-history (current-word))
    (ido-read-directory-name "Directory: " default-directory default-directory "MUSTMATCH")
    (read-from-minibuffer "Path regex: " nil nil nil 'dired-regexp-history)
    (y-or-n-p "Fixed case search?")
    (ido-completing-read "Print context level (0=none, 1=matched pattern, 2=neighboring string) " '("0" "1" "2"))))

  (let (
        (ξcount 0)
        (ξoutputBuffer "*xah-find output*")
        (ξpos1 1) ; beginning of line
        (ξpos2 1))

    (setq φinput-dir (file-name-as-directory φinput-dir)) ; add ending slash

    (with-output-to-temp-buffer ξoutputBuffer
      (princ (format "-*- coding: utf-8 -*-
%s
xah-find-text-regex result.
Search regex 「%s」
Directory 「%s」
Path Regex 「%s」

" (xah-find--current-date-time-string) φsearch-regex φinput-dir φpath-regex))
      (mapc
       (lambda (ξfp)
         (setq ξcount 0)
         (with-temp-buffer
           (insert-file-contents ξfp)
           (setq case-fold-search (not φfixed-case-search-p))
           (while (search-forward-regexp φsearch-regex nil t)
             (setq ξcount (1+ ξcount))
             (cond
              ((equal φprint-context-level "0") nil)
              ((equal φprint-context-level "1") (xah-find--print-text-block (match-string 0)))
              ((equal φprint-context-level "2")
               (progn
                 (setq ξpos1 (max 1 (- (match-beginning 0) xah-find-print-before )))
                 (setq ξpos2 (min (point-max) (+ (match-end 0) xah-find-print-after )))
                 (xah-find--print-text-block (buffer-substring-no-properties ξpos1 ξpos2 ))))))
           (when (> ξcount 0)
             (xah-find--print-file-count ξfp ξcount))))
       (xah-find--filter-list
        (lambda (x)
          (not (xah-find--ignore-dir-p x)))
        (find-lisp-find-files φinput-dir φpath-regex)))

      (xah-find--switch-and-highlight ξoutputBuffer φsearch-regex t))))

;;;###autoload
(defun xah-find-replace-text (φsearch-str φreplace-str φinput-dir φpath-regex φwrite-to-file-p φfixed-case-search-p φfixed-case-replace-p &optional φbackup-p)
  "Find/Replace string in all files of a directory.
Search string can span multiple lines.
No regex."
  (interactive
   (list
    (read-string (format "Search string (default %s): " (current-word)) nil 'query-replace-history (current-word))
    (read-string (format "Replace string: ") nil 'query-replace-history)
    (ido-read-directory-name "Directory: " default-directory default-directory "MUSTMATCH")
    (read-from-minibuffer "Path regex: " nil nil nil 'dired-regexp-history)
    (y-or-n-p "Write changes to file?")
    (y-or-n-p "Fixed case in search?")
    (y-or-n-p "Fixed case in replacement?")
    (y-or-n-p "Make backup?")))

  (let (
        (ξoutputBuffer "*xah-find output*")
        (ξbackupSuffix (xah-find--backup-suffix "xfrt")))

    (with-output-to-temp-buffer ξoutputBuffer
     (xah-find--print-header φinput-dir φpath-regex φsearch-str φreplace-str )
     (mapc
      (lambda (ξf)
        (let ( (case-fold-search (not φfixed-case-search-p))
               (ξcount 0))
          (with-temp-buffer
            (insert-file-contents ξf)
            (while (search-forward φsearch-str nil t)
              (replace-match φreplace-str φfixed-case-replace-p "literalreplace")
              (setq ξcount (1+ ξcount))
              (xah-find--print-text-block
               (buffer-substring-no-properties
                (max 1 (- (match-beginning 0) xah-find-print-before ))
                (min (point-max) (+ (point) xah-find-print-after )))))

            (when (> ξcount 0)
              (when φwrite-to-file-p
                (when φbackup-p (copy-file ξf (concat ξf ξbackupSuffix) t))
                (write-region 1 (point-max) ξf))
              (xah-find--print-file-count ξf ξcount )))))
      (xah-find--filter-list
       (lambda (x)
         (not (xah-find--ignore-dir-p x)))
       (find-lisp-find-files φinput-dir φpath-regex)))
     (princ "Done"))

    (xah-find--switch-and-highlight ξoutputBuffer φreplace-str)))

;;;###autoload
(defun xah-find-replace-text-regex (φregex φreplace-str φinput-dir φpath-regex φwrite-to-file-p φfixed-case-search-p φfixed-case-replace-p)
  "Find/Replace by regex in all files of a directory.

φregex is a regex pattern.
φreplace-str is replacement string.
φinput-dir is input directory to search (includes all nested subdirectories).
φpath-regex is a regex to filter file paths.
φwrite-to-file-p, when true, write to file, else, print a report of changes only.
φfixed-case-search-p sets `case-fold-search' for this operation.
φfixed-case-replace-p, if true, then the letter-case in replacement is literal. (this is relevant only if φfixed-case-search-p is true.)
"
  (interactive
   (list
    (read-regexp "regex: " )
    (read-string (format "Replace string: ") nil 'query-replace-history)
    (ido-read-directory-name "Directory: " default-directory default-directory "MUSTMATCH")
    (read-from-minibuffer "Path regex: " nil nil nil 'dired-regexp-history)
    (y-or-n-p "Write changes to file?")
    (y-or-n-p "Fixed case in search?")
    (y-or-n-p "Fixed case in replacement?")))

  (let (
        (ξoutputBuffer "*xah-find output*")
        (ξbackupSuffix (xah-find--backup-suffix "xfrtr")))
    (with-output-to-temp-buffer ξoutputBuffer
      (xah-find--print-header φinput-dir φpath-regex φregex φreplace-str )
      (mapc
       (lambda (ξfp)
         (let (
               (ξcount 0)
               ξmatchStrFound ξmatchStrReplaced )

           (with-temp-buffer
             (insert-file-contents ξfp)
             (setq case-fold-search (not φfixed-case-search-p))
             (while (re-search-forward φregex nil t)
               (setq ξmatchStrFound (match-string 0))
               (replace-match φreplace-str φfixed-case-replace-p)
               (setq ξmatchStrReplaced (match-string 0))
               (setq ξcount (1+ ξcount))
               (princ (format "「%s」\n" ξmatchStrFound))
               (princ (format "『%s』\n" ξmatchStrReplaced)))

             (when (> ξcount 0)
               (when φwrite-to-file-p
                 (copy-file ξfp (concat ξfp ξbackupSuffix) t)
                 (write-region 1 (point-max) ξfp))
               (princ (format "• %d %s\n" ξcount ξfp))))))
       (xah-find--filter-list
        (lambda (x)
          (not (xah-find--ignore-dir-p x)))
        (find-lisp-find-files φinput-dir φpath-regex)))
      (princ "Done"))

    (xah-find--switch-and-highlight ξoutputBuffer φreplace-str)

    ;; (progn
    ;;   (when (not (string= φreplace-str ""))
    ;;     (highlight-phrase (regexp-quote φregex) (quote hi-yellow)))
    ;;   )

    ))

;;;###autoload
(defun xah-find-count (φsearch-str φcount-expr φcount-number φinput-dir φpath-regex)
  "Report how many occurances of a string, of a given dir.
Similar to grep, written in elisp.

Case sensitivity is determined by `case-fold-search'. Call `toggle-case-fold-search' to change."
  (interactive
   (let* ( ξoperator)
     (list
      (read-string (format "Search string (default %s): " (current-word)) nil 'query-replace-history (current-word))
      (setq ξoperator
            (ido-completing-read
             "Report on:"
             '("greater than" "greater or equal to" "equal" "not equal" "less than" "less or equal to" )))
      (read-string (format "Count %s: "  ξoperator) "0")
      (ido-read-directory-name "Directory: " default-directory default-directory "MUSTMATCH")
      (read-from-minibuffer "Path regex: " nil nil nil 'dired-regexp-history))))

  (let* (
         (ξoutputBuffer "*xah-find output*")
         (ξcountOperator
          (cond

           ((string-equal "less than" φcount-expr ) '<)
           ((string-equal "less or equal to" φcount-expr ) '<=)
           ((string-equal "greater than" φcount-expr ) '>)
           ((string-equal "greater or equal to" φcount-expr ) '>=)
           ((string-equal "equal" φcount-expr ) '=)
           ((string-equal "not equal" φcount-expr ) '/=)
           (t (error "your count expression 「%s」 is wrong!" φcount-expr ))))
         (ξcountNumber (string-to-number φcount-number)))

    (with-output-to-temp-buffer ξoutputBuffer
      (princ (format "-*- coding: utf-8 -*-
Date: %s
Command “xah-find-count” result.
Search string: 「%s」
Count expression: 「%s %s」
Input dir: 「%s」
Path regex: 「%s」
"
                     (xah-find--current-date-time-string) φsearch-str φcount-expr φcount-number φinput-dir φpath-regex))
      (mapc
       (lambda (ξf)
         (let ((ξcount 0))
           (with-temp-buffer
             (insert-file-contents ξf)
             (goto-char 1)
             (while (search-forward φsearch-str nil "NOERROR if not found")
               ;; (princ (format "「%s」\n" (buffer-substring-no-properties (line-beginning-position) (line-end-position) )))
               (setq ξcount (1+ ξcount)))

             ;; report if the occurance is not n times
             (when
                 (funcall ξcountOperator ξcount ξcountNumber)
               (princ (format "• %d %s\n" ξcount ξf))))))
       (xah-find--filter-list
        (lambda (x)
          (not (xah-find--ignore-dir-p x)))
        (find-lisp-find-files φinput-dir φpath-regex)))
      (princ "Done"))

    (xah-find--switch-and-highlight ξoutputBuffer φsearch-str)))

(provide 'xah-find)

;;; xah-find.el ends here
