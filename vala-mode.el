;;; vala-mode.el --- Vala mode derived mode

;; Author:     2005 Dylan R. E. Moonfire
;;	       2008 Étienne BERSAC
;; Maintainer: Étienne BERSAC <bersace03@laposte.net>
;; Modifier:   Kentaro NAKAZAWA <kentaro.nakazawa@nifty.com>
;; Created:    2008 May the 4th
;; Modified:   April 2011
;; Version:    0.1
;; Keywords:   vala languages oop

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;    See http://live.gnome.org/Vala for details about Vala language.
;;
;;    This is a separate mode to implement the Vala constructs and
;;    font-locking. It is mostly the csharp-mode from
;;    http://mfgames.com/linux/csharp-mode with vala specific keywords
;;    and filename suffixes.
;;
;;    Note: The interface used in this file requires CC Mode 5.30 or
;;    later.

;;; .emacs (don't put in (require 'vala-mode))
;; (autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
;; (setq auto-mode-alist
;;    (append '(("\\.vala$" . vala-mode)) auto-mode-alist))

;;; Versions:
;;
;;	0.1	: Initial version based on csharp-mode
;;

;; This is a copy of the function in cc-mode which is used to handle
;; the eval-when-compile which is needed during other times.
(require 'cc-defs)
(defun c-filter-ops (ops opgroup-filter op-filter &optional xlate)
  ;; See cc-langs.el, a direct copy.
  (unless (listp (car-safe ops))
    (setq ops (list ops)))
  (cond ((eq opgroup-filter t)
	 (setq opgroup-filter (lambda (opgroup) t)))
	((not (functionp opgroup-filter))
	 (setq opgroup-filter `(lambda (opgroup)
				 (memq opgroup ',opgroup-filter)))))
  (cond ((eq op-filter t)
	 (setq op-filter (lambda (op) t)))
	((stringp op-filter)
	 (setq op-filter `(lambda (op)
			    (string-match ,op-filter op)))))
  (unless xlate
    (setq xlate 'identity))
  (c-with-syntax-table (c-lang-const c-mode-syntax-table)
    (delete-duplicates
     (mapcan (lambda (opgroup)
	       (when (if (symbolp (car opgroup))
			 (when (funcall opgroup-filter (car opgroup))
			   (setq opgroup (cdr opgroup))
			   t)
		       t)
		 (mapcan (lambda (op)
			   (when (funcall op-filter op)
			     (let ((res (funcall xlate op)))
			       (if (listp res) res (list res)))))
			 opgroup)))
	     ops)
     :test 'equal)))

;; This inserts the bulk of the code.
(require 'cc-mode)

(require 'imenu)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-mode" nil t)
    (load "cc-fonts" nil t)
    (load "cc-langs" nil t)))

(eval-and-compile
  ;; Make our mode known to the language constant system.  Use Java
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'vala-mode 'java-mode))

;; Java uses a series of regexes to change the font-lock for class
;; references. The problem comes in because Java uses Pascal (leading
;; space in names, SomeClass) for class and package names, but
;; Camel-casing (initial lowercase, upper case in words,
;; i.e. someVariable) for variables.
;;(error (byte-compile-dest-file))
;;(error (c-get-current-file))
(c-lang-defconst c-opt-after-id-concat-key
  vala (if (c-lang-const c-opt-identifier-concat-key)
	   (c-lang-const c-symbol-start)))

(c-lang-defconst c-basic-matchers-before
  vala `(
;;;; Font-lock the attributes by searching for the
;;;; appropriate regex and marking it as TODO.
	 ;;,`(,(concat "\\(" vala-attribute-regex "\\)")
	 ;;   0 font-lock-function-name-face)	   

	 ;; Put a warning face on the opener of unclosed strings that
	 ;; can't span lines.  Later font
	 ;; lock packages have a `font-lock-syntactic-face-function' for
	 ;; this, but it doesn't give the control we want since any
	 ;; fontification done inside the function will be
	 ;; unconditionally overridden.
	 ,(c-make-font-lock-search-function
	   ;; Match a char before the string starter to make
	   ;; `c-skip-comments-and-strings' work correctly.
	   (concat ".\\(" c-string-limit-regexp "\\)")
	   '((c-font-lock-invalid-string)))
	   
	 ;; Fontify keyword constants.
	 ,@(when (c-lang-const c-constant-kwds)
	     (let ((re (c-make-keywords-re nil
			 (c-lang-const c-constant-kwds))))
	       `((eval . (list ,(concat "\\<\\(" re "\\)\\>")
			       1 c-constant-face-name)))))
	   
	 ;; Fontify all keywords except the primitive types.
	 ,`(,(concat "\\<" (c-lang-const c-regular-keywords-regexp))
	    1 font-lock-keyword-face)

	 ;; Fontify leading identifiers in fully
	 ;; qualified names like "Foo.Bar".
	 ,@(when (c-lang-const c-opt-identifier-concat-key)
	     `((,(byte-compile
		  `(lambda (limit)
		     (while (re-search-forward
			     ,(concat "\\(\\<" ; 1
				      "\\(" (c-lang-const c-symbol-key)
				      "\\)" ; 2
				      "[ \t\n\r\f\v]*"
				      (c-lang-const
				       c-opt-identifier-concat-key)
				      "[ \t\n\r\f\v]*"
				      "\\)"
				      "\\("
				      (c-lang-const
				       c-opt-after-id-concat-key)
				      "\\)")
			     limit t)
		       (unless (progn
				 (goto-char (match-beginning 0))
				 (c-skip-comments-and-strings limit))
			 (or (get-text-property (match-beginning 2) 'face)
			     (c-put-font-lock-face (match-beginning 2)
						   (match-end 2)
						   c-reference-face-name))
			 (goto-char (match-end 1)))))))))
	 ))

;; Vala does not allow a leading qualifier operator. It also doesn't
;; allow the ".*" construct of Java. So, we redo this regex without
;; the "\\|\\*" regex.
(c-lang-defconst c-identifier-key
  vala (concat "\\(" (c-lang-const c-symbol-key) "\\)" ; 1
	       (concat "\\("
		       "[ \t\n\r\f\v]*"
		       (c-lang-const c-opt-identifier-concat-key)
		       "[ \t\n\r\f\v]*"
		       (concat "\\("
			       "\\(" (c-lang-const c-symbol-key) "\\)"
			       "\\)")
		       "\\)*")))

;; Vala has a few rules that are slightly different than Java for
;; operators. This also removed the Java's "super" and replaces it
;; with the Vala's "base".
(c-lang-defconst c-operators
  vala `((prefix "base")))

;; Vala directives
(c-lang-defconst c-opt-cpp-prefix
  vala "\\s *#\\s *")


;; Vala uses the following assignment operators
(c-lang-defconst c-assignment-operators
  vala '("=" "*=" "/=" "%=" "+=" "-=" ">>=" "<<="
	 "&=" "^=" "|=" "++" "--"))

;; This defines the primative types for Vala
(c-lang-defconst c-primitive-type-kwds
  vala '("void" "bool" "char" "uchar" "short" "ushort" "int" "uint" "long" "ulong"
	 "size_t" "ssize_t" "int8" "uint8" "int16" "uint16" "int32" "uint32" "int64" "uint64"
	 "unichar" "float" "double" "string"))

;; The keywords that define that the following is a type, such as a
;; class definition.
(c-lang-defconst c-type-prefix-kwds
  vala '("class" "interface" "struct" "enum" "signal"))

;; Type modifier keywords. They appear anywhere in types, but modifiy
;; instead create one.
(c-lang-defconst c-type-modifier-kwds
  vala '("const"))

;; Structures that are similiar to classes.
(c-lang-defconst c-class-decl-kwds
  vala '("class" "interface"))

;; The various modifiers used for class and method descriptions.
(c-lang-defconst c-modifier-kwds
  vala '("public" "partial" "private" "const" "abstract"
	 "protected" "ref" "in" "out" "static" "virtual"
	 "override" "params" "internal" "weak" "owned"
	 "unowned" "async" "yield"))

;; We don't use the protection level stuff because it breaks the
;; method indenting. Not sure why, though.
(c-lang-defconst c-protection-kwds
  vala nil)

;; Define the keywords that can have something following after them.
(c-lang-defconst c-type-list-kwds
  vala '("struct" "class" "interface" "is" "as"
	 "delegate" "event" "set" "get" "add" "remove"
	 "callback" "signal" "var" "default"))

;; This allows the classes after the : in the class declartion to be
;; fontified. 
(c-lang-defconst c-typeless-decl-kwds
  vala '(":"))

;; Sets up the enum to handle the list properly
(c-lang-defconst c-brace-list-decl-kwds
  vala '("enum" "errordomain"))

;; We need to remove Java's package keyword
(c-lang-defconst c-ref-list-kwds
  vala '("using" "namespace" "construct"))

;; Follow-on blocks that don't require a brace
(c-lang-defconst c-block-stmt-2-kwds
  vala '("for" "if" "switch" "while" "catch" "foreach" "lock"))

;; Statements that break out of braces
(c-lang-defconst c-simple-stmt-kwds
  vala '("return" "continue" "break" "throw"))

;; Statements that allow a label
;; TODO?
(c-lang-defconst c-before-label-kwds
  vala nil)

;; Constant keywords
(c-lang-defconst c-constant-kwds
  vala '("true" "false" "null"))

;; Keywords that start "primary expressions."
(c-lang-defconst c-primary-expr-kwds
  vala '("this" "base"))

;; We need to treat namespace as an outer block to class indenting
;; works properly.
(c-lang-defconst c-other-block-decl-kwds
  vala '("namespace" "extern"))

;; We need to include the "in" for the foreach
(c-lang-defconst c-other-kwds
  vala '("in" "sizeof" "typeof"))

(require 'cc-awk)

(c-lang-defconst c-at-vsemi-p-fn
  vala 'c-awk-at-vsemi-p)


;; (defcustom vala-font-lock-extra-types nil
;;   "*List of extra types (aside from the type keywords) to recognize in Vala mode.
;; Each list item should be a regexp matching a single identifier.")

(defconst vala-font-lock-keywords-1 (c-lang-const c-matchers-1 vala)
  "Minimal highlighting for Vala mode.")

(defconst vala-font-lock-keywords-2 (c-lang-const c-matchers-2 vala)
  "Fast normal highlighting for Vala mode.")

(defconst vala-font-lock-keywords-3 (c-lang-const c-matchers-3 vala)
  "Accurate normal highlighting for Vala mode.")

(defvar vala-font-lock-keywords vala-font-lock-keywords-3
  "Default expressions to highlight in Vala mode.")

(defvar vala-mode-syntax-table
  nil
  "Syntax table used in vala-mode buffers.")
(or vala-mode-syntax-table
    (setq vala-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table vala))))

(defvar vala-mode-abbrev-table nil
  "Abbreviation table used in vala-mode buffers.")
(c-define-abbrev-table 'vala-mode-abbrev-table
  ;; Keywords that if they occur first on a line
  ;; might alter the syntactic context, and which
  ;; therefore should trig reindentation when
  ;; they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)))

(defvar vala-mode-map (let ((map (c-make-inherited-keymap)))
			;; Add bindings which are only useful for Vala
			map)
  "Keymap used in vala-mode buffers.")

;;(easy-menu-define vala-menu vala-mode-map "Vala Mode Commands"
;;		  ;; Can use `vala' as the language for `c-mode-menu'
;;		  ;; since its definition covers any language.  In
;;		  ;; this case the language is used to adapt to the
;;		  ;; nonexistence of a cpp pass and thus removing some
;;		  ;; irrelevant menu alternatives.
;;		  (cons "Vala" (c-lang-const c-mode-menu vala)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))

;; Custom variables
(defcustom vala-mode-hook nil
  "*Hook called by `vala-mode'."
  :type 'hook
  :group 'c)

(defcustom vala-want-imenu t
  "*Whether to generate a buffer index via imenu for Vala buffers."
  :type 'boolean :group 'vala)

;;; The entry point into the mode
;;;###autoload
(defun vala-mode ()
  "Major mode for editing Vala code.
This is a simple example of a separate mode derived from CC Mode
to support a language with syntax similar to
C#/C/C++/ObjC/Java/IDL/Pike.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `vala-mode-hook'.

Key bindings:
\\{vala-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table vala-mode-syntax-table)
  (setq major-mode 'vala-mode
	mode-name "Vala"
	local-abbrev-table vala-mode-abbrev-table
	abbrev-mode t)
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars vala-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'vala-mode)
  ;;(easy-menu-add vala-menu)
  (c-set-style "linux")
  (setq indent-tabs-mode t)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (c-toggle-auto-newline -1)
  (c-toggle-hungry-state -1)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'vala-mode-hook)
  (c-update-modeline)

  ;; maybe do imenu scan after hook returns
  (when vala-want-imenu
    (vala--setup-imenu)))


; ==================================================================
;;; imenu stuff (based on csharp-mode.el)

(defconst vala--imenu-expression
  (let* ((single-space                   "[ \t\n\r\f\v]")
         (optional-space                 (concat single-space "*"))
         (bol                            "^[ \t]*") ;; BOL shouldn't accept lineshift.
         (space                          (concat single-space "+"))
         (access-modifier (regexp-opt '( "public" "private" "protected" "internal"
                                         "static" "override" "virtual"
                                         "abstract" "async" "new")))
         ;; this will allow syntactically invalid combinations of modifiers
         ;; but that's a compiler problem, not a imenu-problem
         (access-modifier-list           (concat "\\(?:" access-modifier space "\\)"))
         (access-modifiers (concat access-modifier-list "*"))
         (basic-type                     (concat
                                          ;; typename
                                          "\\(?:[A-Za-z_][[:alnum:]_]*\\.\\)*"
                                          "[A-Za-z_][[:alnum:]_]*"
                                          ))
         (type                           (concat
                                          basic-type
                                          ;; simplified, optional generic constraint.
                                          ;; handles generic sub-types.
                                          "\\(?:<[[:alnum:],<> \t\n\f\v\r]+>\\)?"))
         (return-type                    (concat
                                          type
                                          ;; optional array-specifier
                                          "\\(?:\\[\\]\\)?"))
         (interface-prefix               (concat "\\(?:" type "\\.\\)"))
         ;; param-list with parens
         (parameter-list "\\(?:\([^!\)]*\)\\)")
         (inheritance-clause (concat "\\(?:"
                                     optional-space
                                     ":"
                                     optional-space type
                                     "\\(?:" optional-space "," optional-space type "\\)*"
                                     "\\)?")))

    (list (list "namespace"
                (concat bol "namespace" space
                        "\\(" basic-type "\\)") 1)
          ;; not all these are classes, but they can hold other
          ;; members, so they are treated uniformly.
          (list "class"
                (concat bol
                        access-modifiers
                        "\\("
                        (regexp-opt '("class" "struct" "interface")) space
                        type inheritance-clause "\\)")  1)
          (list "enum"
                (concat bol
                        access-modifiers
                        "\\(" "enum" space
                        basic-type "\\)")  1)
          (list "ctor"
                (concat bol
                        ;; ctor MUST have access modifiers, or else we pick
                        ;; every if statement in the file...
                        access-modifier-list "+"
                        "\\("
                        basic-type
                        optional-space
                        parameter-list
                        "\\)"
                        "\\(?:"
                        optional-space
                        ":"
                        optional-space
                        "\\(?:this\\|base\\)"
                        optional-space
                        parameter-list
                        "\\)?"
                        optional-space "{") 1)
          (list "method"
                (concat bol
                        ;; we MUST require modifiers, or else we cannot reliably
                        ;; identify declarations, without also dragging in lots of
                        ;; if statements and what not.
                        access-modifier-list "+"
                        return-type space
                        "\\("
                        type
                        optional-space
                        parameter-list
                        "\\)"
                        ;; optional // or /* comment at end
                        "\\(?:[ \t]*/[/*].*\\)?"
                        optional-space
                        "{") 1)
          (list "method-inf"
                (concat bol
                        return-type space
                        "\\("
                        interface-prefix
                        type
                        optional-space
                        parameter-list
                        "\\)"
                        ;; optional // or /* comment at end
                        "\\(?:[ \t]*/[/*].*\\)?"
                        optional-space
                        "{") 1)
          (list "method-abs-ext"
                (concat bol
                        access-modifier-list "+"
                        (regexp-opt '("extern" "abstract")) space
                        return-type space
                        "\\("
                        type
                        optional-space
                        parameter-list
                        "\\)"
                        optional-space
                        ;; abstract/extern methods are terminated with ;
                        ";") 1)
          ;; delegates are almost like abstract methods, so pick them up here
          (list "delegate"
                (concat bol
                        access-modifiers
                        "delegate" space
                        return-type space
                        "\\("
                        type
                        "\\)"
                        optional-space
                        parameter-list
                        ;; optional // or /* comment at end
                        optional-space
                        ";") 1)
          (list "prop"
                (concat bol
                        ;; must require access modifiers, or else we
                        ;; pick up pretty much anything.
                        access-modifiers
                        return-type space
                        "\\("
                        type
                        "\\)"
                        optional-space "{" optional-space
                        ;; unless we are super-specific and expect the accessors,
                        ;; lots of weird things gets slurped into the name.
                        ;; including the accessors themselves.
                        (regexp-opt '("get" "set"))
                        ) 1)
          (list "prop-inf"
                (concat bol
                        return-type space
                        "\\("
                        interface-prefix
                        type
                        "\\)"
                        optional-space "{" optional-space
                        ;; unless we are super-specific and expect the accessors,
                        ;; lots of weird things gets slurped into the name.
                        ;; including the accessors themselves.
                        (regexp-opt '("get" "set"))
                        ) 1)
          ;; adding fields... too much?
          (list "field"
                (concat bol
                        access-modifier-list "+"
                        ;; fields can be readonly/const/volatile
                        "\\(?:" (regexp-opt '("readonly" "const" "volatile")) space "\\)?"
                        return-type space
                        "\\("
                        type
                        "\\)"
                        optional-space
                        ;; optional assignment
                        "\\(?:=[^;]+\\)?"
                        ";") 1)
          (list "indexer"
                (concat bol
                        access-modifiers
                        return-type space
                        "this" optional-space
                        "\\("
                        ;; opening bracket
                        "\\[" optional-space
                        ;; type
                        "\\([^\]]+\\)" optional-space
                        type
                        ;; closing brackets
                        "\\]"
                        "\\)"
                        optional-space "{" optional-space
                        ;; unless we are super-specific and expect the accessors,
                        ;; lots of weird things gets slurped into the name.
                        ;; including the accessors themselves.
                        (regexp-opt '("get" "set"))) 1))))

(defun vala--imenu-get-pos (pair)
  "Return `position' from a (title . position) cons-pair `PAIR'.
   The position may be a integer, or a marker (as returned by
   imenu-indexing).  This function ensures what is returned is an
   integer which can be used for easy comparison."
  (let ((pos (cdr pair)))
    (if (markerp pos)
        (marker-position pos)
      pos)))

(defun vala--imenu-get-container (item containers previous)
  "Return the container which `ITEM' belongs to.
   `ITEM' is a (title . position) cons-pair.  `CONTAINERS' is a
   list of such.  `PREVIOUS' is the name of the previous
   container found when recursing through `CONTAINERS'.
   The final result is based on item's position relative to those
   found in `CONTAINERS', or nil if none is found."
  (if (not containers)
      previous
    (let* ((item-pos (vala--imenu-get-pos item))
           (container (car containers))
           (container-pos (vala--imenu-get-pos container))
           (rest      (cdr containers)))
      (if (and container-pos
               (< item-pos container-pos))
          previous
        (vala--imenu-get-container item rest container)))))

(defun vala--imenu-get-container-name (item containers)
  "Return the name of the container which `ITEM' belongs to.
   `ITEM' is a (title . position) cons-pair.
   `CONTAINERS' is a list of such.
   The name is based on the results from
   `vala--imenu-get-container'."
  (let ((container (vala--imenu-get-container item containers nil)))
    (if (not container)
        nil
      (let ((container-p1 (car (split-string (car container))))   ;; namespace
            (container-p2 (cadr (split-string (car container))))) ;; class/interface
        ;; use p1 (namespace) when there is no p2
        (if container-p2
            container-p2
          container-p1)))))

(defun vala--imenu-sort (items)
  "Sort an imenu-index list `ITEMS' by the string-portion."
  (sort items (lambda (item1 item2)
                (string< (car item1) (car item2)))))

(defun vala--imenu-get-class-name (class namespaces)
  "Gets a name for a imenu-index `CLASS'.
   Result is based on its own name and `NAMESPACES' found in the same file."
  (let ((namespace (vala--imenu-get-container-name class namespaces))
        (class-name (car class)))
    (if (not namespace)
        class-name
      ;; reformat to include namespace
      (let* ((words (split-string class-name))
             (type  (car words))
             (name  (cadr words)))
        (concat type " " namespace "." name)))))

(defun vala--imenu-get-class-nodes (classes namespaces)
  "Create a new alist with CLASSES as root nodes with NAMESPACES added.
   Each class will have one imenu index-entry \"( top)\" added by
   default."

  (mapcar (lambda (class)
            (let ((class-name (vala--imenu-get-class-name class namespaces))
                  (class-pos  (cdr class)))
              ;; construct a new alist-entry where value is itself
              ;; a list of alist-entries with -1- entry which the top
              ;; of the class itself.
              (cons class-name
                    (list
                     (cons "( top )" class-pos)))))
          classes))

(defun vala--imenu-get-class-node (result item classes namespaces)
  "Get the class-node in `RESULT' which an `ITEM' should be inserted into.
   For this calculation, the original index items `CLASSES' and `NAMESPACES'
   is needed."
  (let* ((class-item (vala--imenu-get-container item classes nil))
         (class-name (vala--imenu-get-class-name class-item namespaces)))
    (assoc class-name result)))

(defun vala--imenu-format-item-node (item type)
  "Format an ITEM with a specified TYPE as an imenu item to be inserted into the index."
  (cons
   (concat "(" type ") " (car item))
   (cdr item)))

(defun vala--imenu-append-items-to-menu (result key name index classes namespaces)
  "Formats the imenu-index using the provided values.
This is done by modifying the contents of `RESULT' in place."
  ;; items = all methods, all events, etc based on "type"
  (let* ((items (cdr (assoc key index))))
    (dolist (item items)
      (let ((class-node (vala--imenu-get-class-node result item classes namespaces))
            (item-node  (vala--imenu-format-item-node item name)))
        (nconc class-node (list item-node))))))

(defun vala--imenu-transform-index (index)
  "Transform an imenu INDEX based on `IMENU-GENERIC-EXPRESSION'.
  The resulting structure should be based on full type-names, with
  type-members nested hierarchially below its parent."
  (let* ((result nil)
         (namespaces (cdr (assoc "namespace" index)))
         (classes    (cdr (assoc "class"     index)))
         (class-nodes (vala--imenu-get-class-nodes classes namespaces)))
    ;; be explicit about collection variable
    (setq result class-nodes)
    (dolist (type '(("ctor")
                    ("method")
                    ("method-inf" "method")
                    ("method-abs-ext" "method")
                    ("prop")
                    ("prop-inf" "prop")
                    ("field")
                    ("indexer")))
      (let* ((key (car type))
             (name (car (last type))))
        (vala--imenu-append-items-to-menu result key name index classes namespaces)))

    ;; add enums and delegates to main result list, as own items.
    ;; We don't support nested types. EOS.
    ;;
    ;; This has the issue that they get reported as "function" in
    ;; `helm-imenu', but there's nothing we can do about that.
    ;; The alternative is making it a menu with -1- submenu which
    ;; says "( top )" but that will be very clicky...

    ;; before adding delegates, we need to pad the entry so that it
    ;; matches the "<type> <name>" signature used by all the other
    ;; imenu entries
    (let ((delegates (cdr (assoc "delegate" index))))
      (dolist (delegate delegates)
        (setf (car delegate) (concat "delegate " (car delegate)))))

    (dolist (type '("enum" "delegate"))
      (dolist (item (cdr (assoc type index)))
        (let ((item-name (vala--imenu-get-class-name item namespaces)))
          (setq result (cons (cons item-name (cdr item))
                             result)))))

    ;; sort individual sub-lists
    (dolist (item result)
      (when (listp (cdr item))
        (setf (cdr item) (vala--imenu-sort (cdr item)))))

    ;; sort main list
    ;; (Enums always sort last though, because they don't have
    ;; sub-menus)
    (vala--imenu-sort result)))

(defun vala--imenu-create-index-function ()
  "Create an imenu index."
  (vala--imenu-transform-index
   (imenu--generic-function vala--imenu-expression)))

(defun vala--setup-imenu ()
  "Set up `imenu' for `vala-mode'."

  ;; There are two ways to do imenu indexing. One is to provide a
  ;; function, via `imenu-create-index-function'.  The other is to
  ;; provide imenu with a list of regexps via
  ;; `imenu-generic-expression'; imenu will do a "generic scan" for you.
  ;;
  ;; We use both.
  ;;
  ;; First we use the `imenu-generic-expression' to build a index for
  ;; us, but we do so inside a `imenu-create-index-function'
  ;; implementation which allows us to tweak the results slightly
  ;; before returning it to Emacs.
  (setq imenu-create-index-function #'vala--imenu-create-index-function)
  (imenu-add-menubar-index))

(provide 'vala-mode)

;;; vala-mode.el ends here
