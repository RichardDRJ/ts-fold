;;; tree-sitter-fold-parsers.el --- Adapter layer to Tree-Sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-10-04 17:45:48

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
;; Adapter layer to Tree-Sitter
;;
;; This isn't a real parser implementation, but records down the rule
;; in order to let the Tree-Sitter to parse things correctly.  Think of
;; the rule sets!
;;

;;; Code:

;;
;; (@* "Externals" )
;;

(declare-function tree-sitter-fold-range-seq "tree-sitter-fold.el")
(declare-function tree-sitter-fold-range-python "tree-sitter-fold.el")
(declare-function tree-sitter-fold-range-nix-function "tree-sitter-fold.el")
(declare-function tree-sitter-fold-range-go-type-declaration "tree-sitter-fold.el")
(declare-function tree-sitter-fold-range-go-method "tree-sitter-fold.el")

;;
;; (@* "Parsers" )
;;

(defun tree-sitter-fold-parsers-c ()
  "Rule sets for C."
  '((compound_statement     . tree-sitter-fold-range-seq)
    (declaration_list       . tree-sitter-fold-range-seq)
    (enumerator_list        . tree-sitter-fold-range-seq)
    (field_declaration_list . tree-sitter-fold-range-seq)
    (comment                . (tree-sitter-fold-range-seq 1 -1))))

(defun tree-sitter-fold-parsers-c++ ()
  "Rule sets for C++."
  (append (tree-sitter-fold-parsers-c)))

(defun tree-sitter-fold-parsers-csharp ()
  "Rule sets for C#."
  '((block                                . tree-sitter-fold-range-seq)
    (accessor_list                        . tree-sitter-fold-range-seq)
    (enum_member_declaration_list         . tree-sitter-fold-range-seq)
    (declaration_list                     . tree-sitter-fold-range-seq)
    (switch_body                          . tree-sitter-fold-range-seq)
    (anonymous_object_creation_expression . tree-sitter-fold-range-seq)
    (initializer_expression               . tree-sitter-fold-range-seq)
    (comment                              . (tree-sitter-fold-range-seq 1 -1))))

(defun tree-sitter-fold-parsers-css ()
  "Rule sets for CSS."
  '((keyframe_block_list . tree-sitter-fold-range-seq)
    (block               . tree-sitter-fold-range-seq)))

(defun tree-sitter-fold-parsers-go ()
  "Rule sets for Go."
  '((type_declaration     . tree-sitter-fold-range-go-type-declaration)
    (function_declaration . tree-sitter-fold-range-go-method)
    (method_declaration   . tree-sitter-fold-range-go-method)))

(defun tree-sitter-fold-parsers-html ()
  "Rule sets for HTML."
  '((style_start_tag  . tree-sitter-fold-range-seq)
    (script_start_tag . tree-sitter-fold-range-seq)
    (comment          . (tree-sitter-fold-range-seq 1 -1))))

(defun tree-sitter-fold-parsers-java ()
  "Rule sets for Java."
  '((switch_block                    . tree-sitter-fold-range-seq)
    (block                           . tree-sitter-fold-range-seq)
    (element_value_array_initializer . tree-sitter-fold-range-seq)
    (module_body                     . tree-sitter-fold-range-seq)
    (enum_body                       . tree-sitter-fold-range-seq)
    (class_body                      . tree-sitter-fold-range-seq)
    (constructor_body                . tree-sitter-fold-range-seq)
    (annotation_type_body            . tree-sitter-fold-range-seq)
    (interface_body                  . tree-sitter-fold-range-seq)
    (array_initializer               . tree-sitter-fold-range-seq)
    (comment                         . (tree-sitter-fold-range-seq 1 -1))))

(defun tree-sitter-fold-parsers-javascript ()
  "Rule sets for JavaScript."
  '((export_clause   . tree-sitter-fold-range-seq)
    (statement_block . tree-sitter-fold-range-seq)))

(defun tree-sitter-fold-parsers-json ()
  "Rule sets for JSON."
  '((object . tree-sitter-fold-range-seq)
    (array  . tree-sitter-fold-range-seq)))

(defun tree-sitter-fold-parsers-nix ()
  "Rule sets for Nix."
  '((attrset  . tree-sitter-fold-range-seq)
    (function . tree-sitter-fold-range-nix-function)))

(defun tree-sitter-fold-parsers-python ()
  "Rule sets for Python."
  '((function_definition . tree-sitter-fold-range-python)
    (class_definition    . tree-sitter-fold-range-python)))

(defun tree-sitter-fold-parsers-r ()
  "Rule sets for R."
  '((brace_list . tree-sitter-fold-range-seq)))

(defun tree-sitter-fold-parsers-rust ()
  "Rule sets for Rust."
  '((declaration_list       . tree-sitter-fold-range-seq)
    (enum_variant_list      . tree-sitter-fold-range-seq)
    (field_declaration_list . tree-sitter-fold-range-seq)
    (use_list               . tree-sitter-fold-range-seq)
    (field_initializer_list . tree-sitter-fold-range-seq)
    (match_block            . tree-sitter-fold-range-seq)
    (block                  . tree-sitter-fold-range-seq)
    (block_comment          . (tree-sitter-fold-range-seq 1 -1))))

(defun tree-sitter-fold-parsers-typescript ()
  "Rule sets for TypeScript."
  (append (tree-sitter-fold-parsers-javascript)))

(provide 'tree-sitter-fold-parsers)
;;; tree-sitter-fold-parsers.el ends here