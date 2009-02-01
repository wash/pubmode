;; pub.el - PubMode an interface to PubMed for Emacs
;;
;; Copyright (C) 2008, Stefan Washietl
;;
;; <wash@tbi.univie.ac.at>
;;
;; This file is part of PubMode.
;;
;; PubMode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; PubMode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with PubMode.  If not, see <http://www.gnu.org/licenses/>.


(defgroup pub nil
  "PubMode -- A PubMed interface for Emacs"
  :tag "PubMode"
  :link '(url-link :tag "Home Page" 
                   "http://www.tbi.univie.ac.at/~wash/pubmacs")
  :prefix "pub-"
  :group 'emacs
)

(defcustom pub-auto-save t
"Save BibTeX-file automatically when selection window is closed
and items have been imported."
  :group 'pub
  :type 'boolean)

(defcustom pub-key-template "%N:%Y"  "\
Define the pattern of the BibTeX key. 

You can access the following special fields:

%N Last name of first author (first letter uppercase), eg. Smith
#N Last name of first author (all lowercase), eg. smith
@N Last name of first author (all uppercase), eg. SMITH

%J Journal abbreviation (first letters uppercase), eg. J_Mol_Biol
#J Journal abbreviation (all lowercase), eg. j_mol_biol
@J Journal abbreviation (all uppercase), eg. J_MOL_BIOL

%P PubMed ID, eg. 12621444

%Y Year (four digits), eg. 2008
#Y Year (two digits), eg. 08


The length of the author field can be limited by appending the
number of characters to the N, eg. @N3 would give SMI instead of
SMITH. 

The default key is %N:%Y, which gives keys of the form Smith:2008.
"
  :group 'pub
  :type 'string)


(defcustom pub-auto-save t
  "Save BibTeX-file automatically after items were added."
  :group 'pub
  :type 'boolean)

(defcustom pub-citation-template "\\cite{%K}"  "\
Define the citation command for LaTeX.  

You can set here the command that is used to include a citation
in your LaTex document. %K is substituted for the BibTeX
key(s). By default this variable is set to `\\cite{%K}'

"
  :group 'pub
  :type 'string)


(defcustom pub-limit 15
  "Number of items shown on one page."
  :group 'pub
  :type 'integer)

(defcustom pub-bibtex-file "" 
"Default destination file for imported BibTeX entries. 

If this variable is empty, the BibTeX file is determined as
follows: The current file is used if PubMode is called from a
BibTeX file itself. If a \\bibliography LaTeX command can be
found this file is used. If none of both applies, the user is
asked to set a destination manually."
  :group 'pub
  :type 'string)

(defcustom pub-bibtex-abstract nil
  "Include abstract in BibTeX."
  :group 'pub
  :type 'boolean)

(defcustom pub-bibtex-double-brace nil
"Enclose title in BibTex entry in double braces or not.

If this is option is non-nil, the title field in the BibTeX entry
will be enclosed in an additional set of curly brackets {}. This
makes BibTeX preserving upper and lowercase. In general you
should let the style decide which letters to print upper or
lowercase, which, however, may require manually insertion of
braces to fix words like DNA or IGF-2.
"
  :group 'pub
  :type 'boolean)


;; Choice selection does not work for some reason
(defcustom pub-default-detail 2 
"Set your default detail level 1,2 or 3 of the PubMode selection window.

   1: brief view with only the first Author and the reference without title.
   2: Summary with full reference and all authors. 
   3: Summary with abstract 
"
  :group 'pub
  :type 'integer)


(defcustom pub-line-width 65 
"Line width of the text in the PubMode selection window. 

Lines longer than that value are broken"
  :group 'pub
  :type 'integer)


(defvar pub-help-prompt "[ ] select [ENTER] paste+quit [123] details [b]ibtex import [c]ite [q]uit [h]elp"
  "Help prompt for minibuffer"
  )

(defvar pub-help-bibtex-prompt "[ ] select [ENTER] cite+quit [123] details [b]ibtex import [c]ite [q]uit [h]elp"
  "Help prompt for minibuffer"
  )



(defvar pub-help-screen 

"
  [Up]/[Down]    Select previous/next item in the list.
\[Left]/[Right]   Switch to previous/next page.

     [1]         \`brief view\'
     [2]         \`summary view\' 
     [3]         \`abstract view\'
     [d]         Toggle detail level of current item.

   [Space]       Mark/un-mark current item.
   [a]/[u]       Mark/un-mark all items on the current page.

     [t]         Import current item/marked items as text.

     [b]         Import current item/marked items to BibTeX file.
     [c]         Insert citation for current item/marked items. Item 
                 has to be imported before it can be cited. 
     [w]         Copy current item/selected items to the kill-ring 
                 (\`clipboard\') for yanking (\`pasting\') into another
                 buffer.
     [p]         New PubMed query.
     [r]         Search related articles for current item.

     [f]         Choose BibTex file.

     [o]         Open full-text (if available) in external browser.

     [h]         Show this help screen.
     [q]         Exit the PubMed selection window.
   [ENTER]       Import and cite item/selected items and exit ([b]+[c]+[q])
"

"Content of help screen"
)


(defvar pub-keymap nil "Key bindings for PubMode selection buffer.")
(defvar pub-help-keymap nil "Key bindings for PubMode help screen.")

(setq pub-keymap (make-keymap))

(setq pub-help-keymap (make-keymap))

(define-key pub-help-keymap "q" 'pub-close-helpscreen)

(define-key pub-keymap [(down)] 'pub-next-entry)
(define-key pub-keymap [(up)] 'pub-prev-entry)

(define-key pub-keymap "q" 'pub-close-selection-buffer)

(define-key pub-keymap "d" 'pub-toggle-level)

(define-key pub-keymap [(right)] 'pub-next-page)
(define-key pub-keymap [(left)] 'pub-prev-page)

(define-key pub-keymap "b" 'pub-export-bibtex)

(define-key pub-keymap "c" 'pub-cite)

(define-key pub-keymap "\r" 'pub-action)

(define-key pub-keymap "p" 'pub-med)

(define-key pub-keymap "a" 'pub-select-all)
(define-key pub-keymap "u" 'pub-deselect-all)

(define-key pub-keymap "t" 'pub-export-text)

(define-key pub-keymap "o" 'pub-browse-entry)

(define-key pub-keymap "w" 'pub-copy-text)

(define-key pub-keymap "1" 'pub-detail-brief)
(define-key pub-keymap "2" 'pub-detail-summary)
(define-key pub-keymap "3" 'pub-detail-abstract)

(define-key pub-keymap "f" 'pub-set-bibtex-export)

(define-key pub-keymap "r" 'pub-related-articles)

(define-key pub-keymap "h" 'pub-help)

(define-key pub-keymap " " 'pub-select-toggle)

(make-face 'pub-author-face)
(copy-face 'font-lock-function-name-face 'pub-author-face)

(make-face 'pub-citation-face)
(copy-face 'font-lock-comment-face 'pub-citation-face)

(make-face 'pub-highlighter-face)
(copy-face 'highlight 'pub-highlighter-face)

(make-face 'pub-selection-face)
(copy-face 'region 'pub-selection-face)

(make-face 'pub-header-face)
(copy-face 'font-lock-type-face 'pub-header-face)

(make-face 'pub-title-face)
(copy-face 'default 'pub-title-face)
(set-face-attribute 'pub-title-face nil :weight 'bold)

(require 'cl)
(require 'pub-coding)
(require 'pub-ui)
(require 'pub-ncbi)
(require 'pub-format)
(require 'pub-helper)
(require 'pub-export)



(defun pub-med (term) "Retrieve PubMed entries"
  (interactive "MSearch PubMed: ")

  (let ((already-running (get-buffer "*PubMed Results*"))
        (result nil)
        (query-term)
        (spell-suggestions))

    (setq query-term (replace-regexp-in-string "[[:blank:]]+" "%20" term))
    (setq result (pub-search query-term))
    (setq spell-suggestions (pub-spell-suggestions term))

    (cond ((eq result 'nohits)
           (if (not spell-suggestions)
               (progn 
                 (message "No hits found.")
                 (sit-for 3)
                 )
             (if (y-or-n-p (format "No hits found. Did you mean \"%s\" and want to repeat the search? " spell-suggestions))
                 (pub-med spell-suggestions)
               (message nil)
               )
             )
           )
          
          ((eq result 'communication-problem)
           (message "Error while contacting NCBI")
           (sit-for 3)
           )

          (t (if (not already-running)
                 (pub-init-selection-buffer)
               (set-buffer "*PubMed Results*")
               )
             
             (setq pub-web-env (nth 0 result))
             (setq pub-query-key (nth 1 result))
             (setq pub-num-hits (nth 2 result))
             
             (if (= 0 (% pub-num-hits pub-limit))
                 (setq pub-num-pages (/ pub-num-hits pub-limit))
               (setq pub-num-pages (+ (/ pub-num-hits pub-limit) 1))
               )
    
              ; initialize pub-data vector
             (setq pub-data (make-vector pub-num-hits nil))

             (setq pub-curr-index 0)
             (setq pub-curr-page 0)

             (setq pub-curr-query-term term)

             (pub-fetch pub-curr-page)

             ; If there is a buffer and a window is showing it, switch to this window.
             ; If there is a buffer and no window is showin is, than show in new window
             ; If there was not buffer previously show in new window

             (if (and already-running (get-buffer-window "*PubMed Results*"))
                 (select-window (get-buffer-window "*PubMed Results*"))
               (switch-to-buffer-other-window "*PubMed Results*"))

            
             (pub-show-page)
             

             (if spell-suggestions
                 (if (y-or-n-p (format "Did you mean \"%s\" and want to repeat the search? " spell-suggestions))
                     (pub-med spell-suggestions)
                   (message nil)
                   )
               (pub-message (format "%d items found." pub-num-hits))
               )
                            
             )
          )
    )
  )


(provide 'pub)