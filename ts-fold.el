;;; ts-fold.el --- Code folding using tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Junyi Hou
;; Copyright (C) 2021  Shen, Jen-Chieh

;; Created date 2021-08-11 14:12:37

;; Author: Junyi Hou <junyi.yi.hou@gmail.com>
;;         Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Code folding using tree-sitter
;; Keyword: folding tree-sitter
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (tree-sitter "0.15.1") (s "1.9.0") (fringe-helper "1.0.1"))
;; URL: https://github.com/jcs090218/ts-fold

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides a code-folding mechanism based on tree-sitter
;; package.  Turn on the minor-mode `ts-fold-mode' to enable
;; this mechanism.  Note that all functionalities provided here based on the
;; `tree-sitter-mode', and thus it should be enabled before
;; `ts-fold-mode' can properly fold codes.

;;; Code:

(require 'seq)
(require 'subr-x)

(require 's)
(require 'tree-sitter)

(require 'ts-fold-util)
(require 'ts-fold-parsers)
(require 'ts-fold-summary)

;;
;; (@* "Customization" )
;;

(defgroup ts-fold nil
  "Code folding using tree-sitter."
  :group 'tree-sitter
  :prefix "ts-fold-")

(setq ts-fold-range-alist
  `((java-mode       . ,ts-fold-queries-java)))

(defcustom ts-fold-range-alist
  ;; `((agda-mode       . ,(ts-fold-parsers-agda))
  ;;   (sh-mode         . ,(ts-fold-parsers-bash))
  ;;   (c-mode          . ,(ts-fold-parsers-c))
  ;;   (c++-mode        . ,(ts-fold-parsers-c++))
  ;;   (csharp-mode     . ,(ts-fold-parsers-csharp))
  ;;   (css-mode        . ,(ts-fold-parsers-css))
  ;;   (ess-r-mode      . ,(ts-fold-parsers-r))
  ;;   (go-mode         . ,(ts-fold-parsers-go))
  ;;   (html-mode       . ,(ts-fold-parsers-html))
  ;;   (java-mode       . ,(ts-fold-parsers-java))
  ;;   (javascript-mode . ,(ts-fold-parsers-javascript))
  ;;   (js-mode         . ,(ts-fold-parsers-javascript))
  ;;   (js2-mode        . ,(ts-fold-parsers-javascript))
  ;;   (js3-mode        . ,(ts-fold-parsers-javascript))
  ;;   (json-mode       . ,(ts-fold-parsers-json))
  ;;   (jsonc-mode      . ,(ts-fold-parsers-json))
  ;;   (nix-mode        . ,(ts-fold-parsers-nix))
  ;;   (php-mode        . ,(ts-fold-parsers-php))
  ;;   (python-mode     . ,(ts-fold-parsers-python))
  ;;   (rjsx-mode       . ,(ts-fold-parsers-javascript))
  ;;   (ruby-mode       . ,(ts-fold-parsers-ruby))
  ;;   (rust-mode       . ,(ts-fold-parsers-rust))
  ;;   (rustic-mode     . ,(ts-fold-parsers-rust))
  ;;   (scala-mode      . ,(ts-fold-parsers-scala))
  ;;   (swift-mode      . ,(ts-fold-parsers-swift))
  ;;   (typescript-mode . ,(ts-fold-parsers-typescript)))
  `((java-mode       . ,ts-fold-queries-java))
  "An alist of (major-mode . (tree-sitter-query . function)).

FUNCTION is used to determine where the beginning and end for FOLDABLE-NODE-TYPE
in MAJOR-MODE.  It should take a single argument (the syntax node with type
FOLDABLE-NODE-TYPE) and return the buffer positions of the beginning and end of
the fold in a cons cell.  See `ts-fold-range-python' for an example."
  :type '(alist :key-type symbol :value-type (alist :key-type symbol :value-type alist))
  :set (lambda (symbol value)
         (set-default symbol value))
  :group 'ts-fold)

(defcustom ts-fold-mode-hook nil
  "Hook to run when enabling `ts-fold-mode`."
  :type 'hook
  :group 'ts-fold)

(defcustom ts-fold-replacement "..."
  "Show this string instead of the folded text."
  :type 'string
  :group 'ts-fold)

(defface ts-fold-replacement-face
  '((t :foreground "#808080" :box '(:line-width -1 :style 'pressed-button)))
  "Face used to display the fold replacement text."
  :group 'ts-fold)

(defface ts-fold-fringe-face
  '((t ()))
  "Face used to display fringe contents."
  :group 'ts-fold)

;;
;; (@* "Externals" )
;;

(declare-function ts-fold-indicators-refresh "ts-fold-indicators.el")

;;
;; (@* "Entry" )
;;

(defun ts-fold--enable ()
  "Start folding minor mode."
  (setq-local line-move-ignore-invisible t)
  (add-to-invisibility-spec '(ts-fold . t))

  ;; evil integration
  (when (bound-and-true-p evil-fold-list)
    (add-to-list 'evil-fold-list
                 '((ts-fold-mode)
		   :toggle ts-fold-toggle
                   :open ts-fold-open
                   :close ts-fold-close
                   :open-rec ts-fold-open-recursively
                   :open-all ts-fold-open-all
                   :close-all ts-fold-close-all)))

  (run-hooks 'ts-fold-mode-hook))

(defun ts-fold--disable ()
  "Stop folding minor mode."
  (remove-from-invisibility-spec '(ts-fold . t))
  (let ((tree-sitter-mode t))
    (ts-fold-open-all)))

(defun ts-fold--invalidate (&optional old-tree)
  "Recalculate fold ranges after a text change.
Installed on `tree-sitter-after-change-functions'.
OLD-TREE is the tree before the edit."
  (when (or (hash-table-empty-p ts-fold--fold-ranges)
            (not (and old-tree (zerop (length (tsc-changed-ranges old-tree tree-sitter-tree))))))
    (let* ((root-node (tsc-root-node tree-sitter-tree))
           (matches (tsc-query-matches ts-fold--query root-node #'tsc--buffer-substring-no-properties ts-fold--query-cursor)))
      (clrhash ts-fold--fold-ranges)
      (mapc (lambda (match) (ts-fold--match-to-fold-range match ts-fold--patterns-to-handlers-alist ts-fold--fold-ranges)) matches)))
  ts-fold--fold-ranges)

(defvar-local ts-fold--default-patterns nil
  "Default code folding patterns.
This should be set by major modes that want to integrate with `ts-fold'.
It is either a string, or a vector of S-expressions. For more details on the
syntax, see https://emacs-tree-sitter.github.io/syntax-highlighting/queries/.")

(defvar ts-fold--patterns-to-handlers-alist nil
  "Additional language-specific code folding patterns, keyed by language symbol.")
(put 'ts-fold--patterns-to-handlers-alist 'risky-local-variable t)

(defvar-local ts-fold--extra-patterns-list nil
  "Additional buffer-local vode folding patterns.")

(defvar-local ts-fold--query nil
  "Tree query used for code folding, compiled from patterns.")

(defvar-local ts-fold--query-cursor nil)

(defvar-local ts-fold--patterns-to-handlers-alist nil
  "A map of fold patterns to their handler functions for the current buffer.
Handler functions should take the arguments (beg end offset),
where beg and end are numeric character offsets in the buffer,
and offset is a pair (start-offset end-offset) which determines
how much to offset the start and end of the fold.")

(defun ts-fold--ensure-query ()
  "Return the tree query to be used for syntax highlighting in this buffer."
  (unless ts-fold--query
    (setq ts-fold--query
          (when ts-fold--default-patterns
            (tsc-make-query
             tree-sitter-language
             (mapconcat #'tsc--stringify-patterns
                        (append ts-fold--extra-patterns-list
                                (alist-get (tsc--lang-symbol tree-sitter-language)
                                           ts-fold--patterns-to-handlers-alist)
                                (list ts-fold--default-patterns))
                        "\n")))))
  ts-fold--query)

(defun ts-fold--setup ()
  "Set up `ts-fold' in the current buffer.
This assumes `tree-sitter-mode' was already enabled."
  (setq ts-fold--patterns-to-handlers-alist (alist-get major-mode ts-fold-range-alist))
  (setq ts-fold--default-patterns (mapcar #'car ts-fold--patterns-to-handlers-alist))
  (setq ts-fold--fold-ranges (make-hash-table :test 'equal))
  (when (ts-fold--ensure-query)
    (unless ts-fold--query-cursor
      (setq ts-fold--query-cursor (tsc-make-query-cursor))
      ;; Invalidate the buffer, only if we were actually disabled previously.
      (ts-fold--invalidate))
    (add-hook 'tree-sitter-after-change-functions #'ts-fold--invalidate nil :local)))

(defun ts-fold--teardown ()
  "Tear down `ts-fold' in the current buffer."
  (remove-hook 'tree-sitter-after-change-functions
               #'ts-fold--invalidate
               :local)
  (when ts-fold--query-cursor
    ;; Open all folds only if we're enabled
    (ts-fold-open-all)
    (setq ts-fold--query-cursor nil))
  (when ts-fold--query
    (setq ts-fold--query nil))
  (when ts-fold--fold-ranges
    (setq ts-fold--fold-ranges nil)))

;;;###autoload
(define-minor-mode ts-fold-mode
  "Toggle code folding based on Tree-sitter's syntax tree.
Enabling this automatically enables `tree-sitter-mode' in the buffer.
To enable this automatically whenever `tree-sitter-mode' is enabled:
 (add-hook 'tree-sitter-after-on-hook #'ts-fold-mode)"
  :init-value nil
  :group 'tree-sitter
  (tree-sitter--handle-dependent ts-fold-mode
    #'ts-fold--setup
    #'ts-fold--teardown))

;;
;; (@* "Core" )
;;

(defun ts-fold--foldable-node-at-pos (&optional pos)
  "Return the smallest foldable node at POS.  If POS is nil, use `point'.

Raise `user-error' if no foldable node is found.

This function is borrowed from `tree-sitter-node-at-point'."
  (tree-sitter-node-at-pos nil pos))

(defvar-local ts-fold--fold-ranges nil)

(defun ts-fold--match-to-fold-range (match fold-alist ranges-table)
  "For a single MATCH, return an alist mapping all nodes in it to the match range and the action to perform, pulled from FOLD-ALIST."
  (let* ((idx (car match)) ;; Which part of the query this match corresponds to we've got (numbered from 0)
         (nodes (cdr match)) ;; All nodes covered by the match
         (fold-nodes (seq-filter (lambda (el) (eq 'fold (car el))) nodes)) ;; All captures from the match annotated with @fold
         (first-node (cdr (elt fold-nodes 0))) ;; The first node in the match
         (last-node (cdr (elt fold-nodes (1- (length fold-nodes)))))) ;; The last node in the match
    ;; An alist of all matched nodes to the larger match they're a part of, in the form:
    ;;   ((node . (start end fold-action))
    (mapcar
     (lambda (node)
       (puthash
        (ts-fold--node-to-alist-key (cdr node))
        (list (tsc-node-start-position first-node) (tsc-node-end-position last-node) (cdr (elt fold-alist idx)))
        ranges-table))
     nodes)))

(defun ts-fold--get-fold-ranges ()
  "Return all fold ranges in the document."
  ts-fold--fold-ranges)

(defun ts-fold--node-to-alist-key (node)
  "Create an interned symbol encoding a node's information for quicker lookup"
  (list (tsc-node-type node) (tsc-node-start-byte node) (tsc-node-end-byte node)))

(defun ts-fold--get-fold-range (node)
  "Return the beginning (as buffer position) of fold for NODE."
  (if-let* ((node node)
            (range-info (gethash (ts-fold--node-to-alist-key node) (ts-fold--get-fold-ranges)))
            (start (nth 0 range-info))
            (end (nth 1 range-info))
            (handler (nth 2 range-info)))
      (cond ((functionp handler) (funcall handler start end (cons 0 0)))
            ((listp handler) (funcall (nth 0 handler) start end (cons (nth 1 handler) (nth 2 handler))))
            (t (user-error "Invalid handler %S for node %S `ts-fold-range-alist' in %s" handler node major-mode)))
    (when-let* ((parent (tsc-get-parent node)))
      (ts-fold--get-fold-range parent))))

;;
;; (@* "Overlays" )
;;

(defun ts-fold--create-overlay (range)
  "Create invisible overlay in RANGE."
  (when range
    (let* ((beg (car range)) (end (cdr range)) (ov (make-overlay beg end)))
      (overlay-put ov 'creator 'ts-fold)
      (overlay-put ov 'invisible 'ts-fold)
      (overlay-put ov 'display (or (and ts-fold-summary-show
                                        (ts-fold-summary--get (buffer-substring beg end)))
                                   ts-fold-replacement))
      (overlay-put ov 'face 'ts-fold-replacement-face)
      (overlay-put ov 'isearch-open-invisible #'ts-fold--isearch-open))))

(defun ts-fold--isearch-open (ov)
  "Open overlay OV during `isearch' session."
  (delete-overlay ov))

(defun ts-fold-overlay-at (node)
  "Return the ts-fold overlay at NODE if NODE is foldable and folded.
Return nil otherwise."
  (when-let* ((range (ts-fold--get-fold-range node)))
    (thread-last (overlays-in (car range) (cdr range))
                 (seq-filter (lambda (ov)
                               (and (eq (overlay-get ov 'invisible) 'ts-fold)
                                    (= (overlay-start ov) (car range))
                                    (= (overlay-end ov) (cdr range)))))
                 car)))

;;
;; (@* "Commands" )
;;

(defmacro ts-fold--ensure-ts (&rest body)
  "Run BODY only if `tree-sitter-mode` is enabled."
  (declare (indent 0))
  `(if (bound-and-true-p tree-sitter-mode)
       (progn ,@body)
     (user-error "Ignored, tree-sitter-mode is not enabled in the current buffer")))

;;;###autoload
(defun ts-fold-close (&optional node)
  "Fold the syntax node at `point` if it is foldable.

Foldable nodes are defined in `ts-fold-foldable-node-alist' for the
current `major-mode'.  If no foldable NODE is found in point, do nothing."
  (interactive)
  (ts-fold--ensure-ts
    (let ((node (or node (ts-fold--foldable-node-at-pos))))
      ;; make sure I do not create multiple overlays for the same fold
      (when-let* ((ov (ts-fold-overlay-at node)))
        (delete-overlay ov))
      (ts-fold--create-overlay (ts-fold--get-fold-range node)))))

;;;###autoload
(defun ts-fold-open ()
  "Open the fold of the syntax node in which `point' resides.
If the current node is not folded or not foldable, do nothing."
  (interactive)
  (ts-fold--ensure-ts
    (when-let* ((node (ts-fold--foldable-node-at-pos))
                (ov (ts-fold-overlay-at node)))
      (delete-overlay ov))))

;;;###autoload
(defun ts-fold-open-recursively ()
  "Open recursively folded syntax NODE that are contained in the node at point."
  (interactive)
  (ts-fold--ensure-ts
    (when-let* ((node (ts-fold--foldable-node-at-pos))
                (beg (tsc-node-start-position node))
                (end (tsc-node-end-position node)))
      (thread-last (overlays-in beg end)
                   (seq-filter (lambda (ov) (eq (overlay-get ov 'invisible) 'ts-fold)))
                   (mapc #'delete-overlay)))))

;;;###autoload
(defun ts-fold-close-all ()
  "Fold all foldable syntax nodes in the buffer."
  (interactive)
  (ts-fold--ensure-ts
    (let* ((node (tsc-root-node tree-sitter-tree))
           (patterns (seq-mapcat (lambda (type) `(,(list type) @name))
                                 (alist-get major-mode ts-fold-foldable-node-alist)
                                 'vector))
           (query (tsc-make-query tree-sitter-language patterns))
           (nodes-to-fold (tsc-query-captures query node #'ignore)))
      (thread-last nodes-to-fold
                   (mapcar #'cdr)
                   (mapc #'ts-fold-close)))))

;;;###autoload
(defun ts-fold-open-all ()
  "Unfold all syntax nodes in the buffer."
  (interactive)
  (ts-fold--ensure-ts
    (thread-last (overlays-in (point-min) (point-max))
                 (seq-filter (lambda (ov) (eq (overlay-get ov 'invisible) 'ts-fold)))
                 (mapc #'delete-overlay))))

;;;###autoload
(defun ts-fold-toggle ()
  "Toggle the syntax node at `point'.
If the current syntax node is not foldable, do nothing."
  (interactive)
  (ts-fold--ensure-ts
    (if-let* ((node (ts-fold--foldable-node-at-pos (point)))
              (ov (ts-fold-overlay-at node)))
        (progn (delete-overlay ov) t)
      (ts-fold-close))))

(defun ts-fold--after-command (&rest _)
  "Function call after interactive commands."
  (ts-fold-indicators-refresh))

(let ((commands '(ts-fold-close
                  ts-fold-open
                  ts-fold-open-recursively
                  ts-fold-close-all
                  ts-fold-open-all
                  ts-fold-toggle)))
  (dolist (command commands)
    (advice-add command :after #'ts-fold--after-command)))

;;
;; (@* "Rule Helpers" )
;;

(defun ts-fold--next-prev-node (node next)
  "Return previous/next sibling node starting from NODE.

If NEXT is non-nil, return next sibling.  Otherwirse, return previouse sibling."
  (if next (tsc-get-next-sibling node) (tsc-get-prev-sibling node)))

(defun ts-fold--continuous-node-prefix (node prefix next)
  "Iterate through node starting from NODE and compare node-text to PREFIX;
then return the last iterated node.

Argument NEXT is a boolean type.  If non-nil iterate forward; otherwise iterate
in backward direction."
  (let ((iter-node node) (last-node node)
        (last-line (car (tsc-node-start-point node))) line text break
        (line-range 1) (last-line-range 1) max-line-range)
    (while (and iter-node (not break))
      (setq text (tsc-node-text iter-node)
            line (car (tsc-node-start-point iter-node))
            line-range (1+ (s-count-matches "\n" text))
            max-line-range (max line-range last-line-range))
      (if (and (ts-fold-util--in-range-p line (- last-line max-line-range) (+ last-line max-line-range))
               (string-prefix-p prefix text))
          (setq last-node iter-node last-line line
                last-line-range (1+ (s-count-matches "\n" text)))
        (setq break t))
      (setq iter-node (ts-fold--next-prev-node iter-node next)))
    last-node))

(defun ts-fold-range-seq (beg end offset)
  "Return the fold range in sequence starting from NODE.

Argument OFFSET can be used to tweak the final beginning and end position."
  (let ((beg (1+ beg))
        (end (1- end)))
    (ts-fold-util--cons-add (cons beg end) offset)))

(defun ts-fold-range-line-comment (beg end offset prefix)
  "Define fold range for line comment.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information.

Argument PREFIX is the comment prefix in string."
  (when-let* ((first-node (ts-fold--continuous-node-prefix node prefix nil))
              (last-node (ts-fold--continuous-node-prefix node prefix t))
              (prefix-len (length prefix))
              (beg (+ beg prefix-len)))
    (ts-fold-util--cons-add (cons beg end) offset)))

(defun ts-fold-range-block-comment (beg end offset)
  "Define fold range for block comment.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (ts-fold-range-seq node (ts-fold-util--cons-add '(1 . -1) offset)))

(defun ts-fold-range-c-like-comment (node offset)
  "Define fold range for C-like comemnt."
  (let ((text (tsc-node-text node)))
    (if (and (string-match-p "\n" text) (string-prefix-p "/*" text))
        (ts-fold-range-block-comment node offset)
      (if (string-prefix-p "///" text)
          (ts-fold-range-line-comment node offset "///")
        (ts-fold-range-line-comment node offset "//")))))

;;
;; (@* "Languages" )
;;

(defun ts-fold-range-c-preproc-if (node offset)
  "Define fold range for `if' preprocessor."
  (let* ((named-node (tsc-get-child-by-field node :condition))
         (else (tsc-get-child-by-field node :alternative))
         (beg (tsc-node-end-position named-node))
         (end (1- (tsc-node-start-position else))))
    (ts-fold-util--cons-add (cons beg end) offset)))

(defun ts-fold-range-c-preproc-ifdef (node offset)
  "Define fold range for `ifdef' and `ifndef' preprocessor."
  (when-let* ((named-node (tsc-get-child-by-field node :name))
              (else (tsc-get-child-by-field node :alternative))
              (beg (tsc-node-end-position named-node))
              (end (1- (tsc-node-start-position else))))
    (ts-fold-util--cons-add (cons beg end) offset)))

(defun ts-fold-range-c-preproc-elif (node offset)
  "Define fold range for `elif' preprocessor."
  (when-let* ((named-node (tsc-get-child-by-field node :condition))
              (else (tsc-get-child-by-field node :alternative))
              (beg (tsc-node-end-position named-node))
              (end (1- (tsc-node-start-position else))))
    (ts-fold-util--cons-add (cons beg end) offset)))

(defun ts-fold-range-c-preproc-else (node offset)
  "Define fold range for `else' preprocessor."
  (when-let* ((target "#else")
              (len (length target))
              (beg (+ (tsc-node-start-position node) len))
              (end (tsc-node-end-position node)))
    (ts-fold-util--cons-add (cons beg end) offset)))

(defun ts-fold-range-html (node offset)
  "Define fold range for tag in HTML."
  (let* ((beg (tsc-node-end-position (tsc-get-nth-child node 0)))
         (end-node (tsc-get-nth-child node (1- (tsc-count-children node))))
         (end (tsc-node-start-position end-node)))
    (ts-fold-util--cons-add (cons beg end) offset)))

(defun ts-fold-range-python (node offset)
  "Define fold range for `function_definition' and `class_definition'.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (when-let* ((named-node (or (tsc-get-child-by-field node :superclasses)
                              (tsc-get-child-by-field node :return_type)
                              (tsc-get-child-by-field node :parameters)
                              (tsc-get-child-by-field node :name)))
              ;; the colon is an anonymous node after return_type or parameters node
              (beg (tsc-node-end-position (tsc-get-next-sibling named-node)))
              (end (tsc-node-end-position node)))
    (ts-fold-util--cons-add (cons beg end) offset)))

(defun ts-fold-range-ruby (node offset)
  "Define fold range for `method' and `class' in Ruby.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (when-let* ((named-node (or (tsc-get-child-by-field node :superclass)
                              (tsc-get-child-by-field node :parameters)
                              (tsc-get-child-by-field node :name)))
              (beg (tsc-node-end-position named-node))
              (end (tsc-node-end-position node)))
    (ts-fold-util--cons-add (cons beg end) offset)))

(defun ts-fold-range-rust-macro (node offset)
  "Return the fold range for `macro_definition' NODE in Rust.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (when-let* ((children (tsc-count-children node))
              (last_bracket (tsc-get-nth-child node (- children 1)))
              (first_bracket (tsc-get-nth-child node 2))
              (beg (tsc-node-start-position first_bracket))
              (end (1+ (tsc-node-start-position last_bracket))))
    (ts-fold-util--cons-add (cons beg end) offset)))

(provide 'ts-fold)
;;; ts-fold.el ends here
