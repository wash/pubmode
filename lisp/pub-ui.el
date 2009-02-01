
;; pub-ui.el --- PubMode an interface to PubMed for Emacs
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



(defun pub-update-modeline () ""

  (let ((export-file "-")
        (export-buffer)
        )

   
    (setq export-file pub-bibtex-export)

    (if pub-bibtex-export
        (progn
          (setq export-buffer (get-file-buffer pub-bibtex-export))
          (setq export-file (file-name-nondirectory export-file))
          (if export-buffer
              (if (buffer-modified-p export-buffer)
                  (setq export-file (concat export-file "*"))))))

             
    (setq mode-line-format
          (list "-"
                'mode-line-mule-info
                'mode-line-frame-identification
                "%b "
                "  [Item "
                (number-to-string (+(+ (* pub-curr-page pub-limit) pub-curr-index) 1))
                "/"
                (number-to-string pub-num-hits)
                " for"
                " \""
                pub-curr-query-term
                "\"]"
                "  [Page "
                (number-to-string (+ 1 pub-curr-page))
                "/"
                (number-to-string pub-num-pages)
                "]  ("
                export-file
                ")"
                "-%-"))

    )

  (force-mode-line-update)

  )

(defun pub-ask-duplicate (key keyNew)
  "Ask what to do with duplicates"
  (let ((input))
    (catch 'done
      (while 1
        (message "There is already an entry %s: (r)eplace, (s)kip or (a)dd with key %s" key keyNew)
        (setq input (read-event))
        (cond ((or (equal input ?s) (equal input ?S)) (throw 'done ?s))
              ((or (equal input ?r) (equal input ?R)) (throw 'done ?r))
              ((or (equal input ?a) (equal input ?A)) (throw 'done ?a))
              (t (ding))
              )
        )
      )
    )
  )


(defun pub-show-page () ""

  (setq pub-curr-index 0)

  (pub-draw-page)
 
  (goto-char 1)

 )

(defun pub-draw-page () ""

   (let* ((buffer-read-only nil) 
        (count)
        (i)
        (currEntry)
        (from)
        (data)
        (mem-point)
        (headerString)
        )
     (pub-compare-with-bibtex)

    (setq mem-point (point))
    (setq data (pub-get-data pub-curr-page))

    (erase-buffer)

    (setq pub-entries-min '())
    (setq pub-entries-max '())

    (setq from (+ 1 (* pub-limit pub-curr-page)))
    
    (setq to (* pub-limit (+ 1 pub-curr-page)))

;    (setq headerString (format  "\n%d articles found. Page %d of %d.\n\n"  
;                                pub-num-hits (+ 1 pub-curr-page) pub-num-pages))

    (put-text-property 0 (length headerString) 'face 'pub-header-face headerString)

;    (insert headerString)
    
    (insert "\n")
    
    (setq count (* pub-limit pub-curr-page))
    (setq i 0)

    (while (< i (length data))
    
      (setq currEntry (nth i data))

      (setq count (+ count 1))
      (setq i (+ i 1))

      (setq pub-entries-min (append pub-entries-min (list (point))))
       
      (pub-print-entry-text currEntry count)
        
      (setq pub-entries-max (append pub-entries-max (list (- (point) 1))))
        

      )
    (setq pub-num-entries i)
    
    (goto-char mem-point)

    
    (pub-highlight-entry pub-curr-index)

    (pub-update-modeline)
    
    )

)

(defun pub-init-selection-buffer () "Init selection buffer"
 
  (let ((buf (current-buffer))
        )
    
;    (if (get-buffer "*Pubmacs*")
;        (kill-buffer "*Pubmacs*")
;      )


    (get-buffer-create "*PubMed Results*")
    (set-buffer "*PubMed Results*")

     ;; Remember calling buffer
    (setq pub-calling-buffer buf)
    
    (kill-all-local-variables)

    
    (make-local-variable 'pub-entries-min)
    (make-local-variable 'pub-entries-max)
    (make-local-variable 'pub-num-entries)
    (make-local-variable 'pub-num-hits)
    (make-local-variable 'pub-num-pages)
    (make-local-variable 'pub-curr-index)
    (make-local-variable 'pub-highlighter)
    (make-local-variable 'pub-calling-buffer)
    (make-local-variable 'pub-bibtex-export)
    (make-local-variable 'pub-detail-level)
    (make-local-variable 'pub-curr-page)
    (make-local-variable 'pub-web-env)
    (make-local-variable 'pub-query-key)
    (make-local-variable 'pub-current-query-term)
    (make-local-variable 'pub-data)
    (make-local-variable 'pub-showing-helpscreen)
    (make-local-variable 'pub-bibtex-mode)

    (setq pub-highlighter (make-overlay 0 0))
    (overlay-put pub-highlighter 'face 'pub-highlighter-face) 


    (use-local-map pub-keymap)
    (setq pub-entries-min '())
    (setq pub-entries-max '())
    (setq buffer-read-only t)
    (setq cursor-type nil)
    (setq pub-calling-buffer buf)
    (setq pub-detail-level 1)
    (setq pub-showing-helpscreen nil)
    (setq pub-bibtex-mode nil)

    ;; Try to find a bibtex file to export to

    (setq pub-bibtex-export nil)
 
   ; If calling buffer visits a file...
    (if (buffer-file-name pub-calling-buffer)
       ;... that has a \bibliograpy{}...
        (let ((buf pub-calling-buffer)
              (export nil))
          (with-temp-buffer
            (insert-buffer-substring buf)
            (goto-char (point-min))
            (if (re-search-forward "\\\\bibliography{\\(.*?\\)}" nil t)
                (progn 
                  (setq export (match-string 1))
                  (setq export (concat (file-name-directory (buffer-file-name buf)) export ".bib"))
                  ; ... that also exists => use it
                  (if (not (file-exists-p export))
                      (progn 
                        ;(message "\\bibliography tag found but file %s does not exist." export)
                        (setq export nil)
                        )
                    )
                  )
              )
            
            )

          (setq pub-bibtex-export export)
          
          )
      )

     ; If calling buffer visits a file...
    (if (buffer-file-name pub-calling-buffer)
       ;... that ends in .bib => use it and set bib-tex-mode
        (if (string-match "\.bib$" (buffer-file-name pub-calling-buffer))
            (progn
              (setq pub-bibtex-export (buffer-file-name pub-calling-buffer))
              (setq pub-bibtex-mode t)
              )
          )
      )

     ; If calling buffer visits a file...
    (if (buffer-file-name pub-calling-buffer)
       ;... that ends in .tex => set bibtex mode
        (if (string-match "\.tex$" (buffer-file-name pub-calling-buffer))
            (setq pub-bibtex-mode t)
          )
      )
    
    



    )

   ; If the user has set manually a target file => override other
   ; choices and use it

  (if pub-bibtex-file 
      (if (not (equal pub-bibtex-file ""))
          (setq pub-bibtex-export pub-bibtex-file)
        )
    )

  (add-hook 'post-command-hook 'pub-post-command-hook nil t)

  )


(defun pub-post-command-hook () ""

  (let ((prompt))

    (if pub-bibtex-mode
        (setq prompt pub-help-bibtex-prompt)
      (setq prompt pub-help-prompt))


    (if (not pub-showing-helpscreen)
        (message prompt)
      (message "Press [q] to exit this help-screen."))

    (pub-update-modeline)
    
    )
   
  )
 
(defun pub-message (msg) ""

  (message msg)

  (if (sit-for 2)
      (pub-post-command-hook))
 
  )


(defun pub-close-selection-buffer () "Close selection"
  (interactive)
  (let ((buffer))

    (if pub-showing-helpscreen
        (pub-draw-page)
      (progn
        (if pub-bibtex-export
            (progn
              (setq buffer (find-file-noselect pub-bibtex-export))
              (if (buffer-modified-p buffer)
                  (progn 
                    (set-buffer buffer)
                    (if pub-auto-save (save-buffer))
                    )
                (set-buffer "*PubMed Results*")
                )
              )
          )
        (kill-selection-buffer "*PubMed Results*")
        (setq pub-buffer nil)
        (delete-window)
        )
      )
    )
  (message nil)
  )

(defun pub-close-helpscreen () ""
  (interactive)

  (pub-draw-page)
  (setq pub-showing-helpscreen nil)
  
  (use-local-map pub-keymap)

  )


(defun pub-next-entry () "Next entry"
  (interactive)

  (if (< pub-curr-index (- pub-num-entries 1))
      (progn (setq pub-curr-index (+ pub-curr-index 1))
             (pub-highlight-entry pub-curr-index))
    (if (< (+ pub-curr-page 1) pub-num-pages)
        (pub-message "Last item on this page. Show next page with \`right\'")
      (pub-message "Last item."))
    )

  )

(defun pub-prev-entry () "Prev Entry"
  (interactive)

  (if (> pub-curr-index 0)
      (progn (setq pub-curr-index (- pub-curr-index 1))
             (pub-highlight-entry pub-curr-index))
    (if (> pub-curr-page 0)
        (pub-message "First item on this page. Show previous page with \`left\'")
      (pub-message "First item.")
      )
    )
  )



(defun pub-highlight-entry (index) "Select entry"

  (let ((buffer-read-only nil)
        (minPos (nth index pub-entries-min))
        (maxPos (nth index pub-entries-max))
        (selected (gethash "selected" (nth pub-curr-index (pub-get-data pub-curr-page))))
        )

    (if selected
        (move-overlay pub-highlighter (+ minPos 5) maxPos)
      (move-overlay pub-highlighter minPos maxPos)
      )
    

    
    (goto-char minPos)

    ; Scroll to top if first entry
    (if (= pub-curr-index 0)
        (goto-char (point-min))
      )

    )
  )



(defun pub-toggle-level () ""
  (interactive)

  (let ((data (nth pub-curr-index (pub-get-data pub-curr-page)))
        (level)
        )
  
    (setq level (gethash "detail" data))
    
    (if (= level 1)
        (progn
          (puthash "detail" 2 data)
          (pub-draw-page)
          (pub-message "Summary view.")
          )
      )

    (if (= level 2)
        (progn 
          (puthash "detail" 3 data)
          (pub-draw-page)
          (pub-message "Abstract view.")
          )
      )
        
    (if (= level 3)
        (progn 
          (puthash "detail" 1 data)
          (pub-draw-page)
          (pub-message "Brief view.")
          )
      )
    )
  )

(defun pub-next-page () ""
  (interactive)

  (setq pub-curr-page (+ pub-curr-page 1))
    
  (if (> pub-curr-page (- pub-num-pages 1))
      (progn
        (setq pub-curr-page (- pub-curr-page 1))
        (pub-message "Already on last page.")
        )
    (progn 
      (setq pub-curr-index 0)
      (pub-highlight-entry 0)
      (pub-draw-page)
      (pub-message (format "Showing page %d of %d." (+ pub-curr-page 1) pub-num-pages))
      )
    )
  )


(defun pub-prev-page () ""

  (interactive)

  (setq pub-curr-page (- pub-curr-page 1))

  (if (< pub-curr-page 0)
      (progn
        (setq pub-curr-page 0)
        (pub-message "Already on first page.")
        )
    (progn 
      (setq pub-curr-index 0)
      (pub-highlight-entry 0)
      (pub-draw-page)
      (pub-message (format "Showing page %d of %d." (+ pub-curr-page 1) pub-num-pages))
      )
    )
  )


(defun pub-reprint-entry (index) ""

  (let ((buffer-read-only nil)
        (min (nth index pub-entries-min))
        (max (nth index pub-entries-max))
        (count (+ (+ (* pub-limit pub-curr-page) index) 1))
        )


    (delete-region min (+ 1 max)) 

    (goto-char min)

    (pub-print-entry-text (nth index (pub-get-data pub-curr-page)) count)
    
    )

  (pub-highlight-entry pub-curr-index)
  (pub-update-modeline)

  )


(defun pub-set-bibtex-export (file) "Sets export file for current query"
  (interactive "FEnter file-name for BibTeX-export: ")

  ; set global variable here
  (setq pub-bibtex-file file)
  
  (setq pub-bibtex-export file)

  (pub-draw-page)

  (pub-update-modeline)

  )

(defun pub-select-toggle () ""
  
  (interactive)

  (let ((data (nth pub-curr-index (pub-get-data pub-curr-page))))
    
    (if (gethash "selected" data)
        (progn
          (puthash "selected" nil data)
          (pub-reprint-entry pub-curr-index)
          (pub-message (format "Item %d de-selected" (+ 1 (+ (* pub-curr-page pub-limit) pub-curr-index))))
          )
      (progn 
        (puthash "selected" t data)
        (pub-reprint-entry pub-curr-index)
        (pub-message (format "Item %d selected" (+ 1 (+ (* pub-curr-page pub-limit) pub-curr-index))))
        )
      )

    )
  
  )

(defun pub-select-all () ""
  (interactive)
  (let* ((data (pub-get-data pub-curr-page))
         (i 0)
         currEntry)

    (while (< i (length data))
      (setq currEntry (nth i data))
      (puthash "selected" t currEntry)
      (setq i (+ i 1))
      )
    (pub-draw-page)
    (pub-message "All items selected.")
    )
  )

(defun pub-deselect-all () ""
  (interactive)
  (let* ((data (pub-get-data pub-curr-page))
         (i 0)
         currEntry)

    (while (< i (length data))
      (setq currEntry (nth i data))
      (puthash "selected" nil currEntry)
      (setq i (+ i 1))
      )
    (pub-draw-page)
    (pub-message "All items deselected.")
    )
  )

(defun pub-import-cite-exit () ""

  (pub-export-bibtex t)
  (pub-cite t)
  (pub-close-selection-buffer)
  
  )

(defun pub-import-text-exit () ""
  (interactive)
  (pub-export-text t)
  (pub-close-selection-buffer)
  
  )


(defun pub-action () ""
  (interactive)

  (if pub-bibtex-mode
      (pub-import-cite-exit)
    (pub-import-text-exit))
  )




(defun pub-set-global-detail-level (level) ""
  (let ((i 0)
        (j 0)
        (pub-hash)
        ) 

    (while (< i pub-num-pages)
      (setq page-list (aref pub-data i)) 
      (setq j 0)
      (while (< j (length page-list))
        (setq pub-hash (nth j page-list))
        (puthash "detail" level pub-hash)
        (setq j (+ 1 j))
        )
      (setq i (+ 1 i))
      )
    )
  (pub-draw-page)
  )

(defun pub-detail-brief () ""
  (interactive)
  (pub-set-global-detail-level 1)
  (setq pub-default-detail 1)
  (pub-message "Brief view.")
  )

(defun pub-detail-summary () ""
  (interactive)
  (pub-set-global-detail-level 2)
  (setq pub-default-detail 2)
  (pub-message "Summary view.")
  )

(defun pub-detail-abstract () ""
  (interactive)
  (pub-set-global-detail-level 3)
  (setq pub-default-detail 3)
  (pub-message "Abstract view.")
  )


(defun kill-selection-buffer (buffer)
  ;; Kill buffer if it exists.
  (and (setq buffer (get-buffer buffer))
       (kill-buffer buffer)))


(defun pub-browse-entry () ""
  (interactive)
  
  (browse-url (pub-linkout (gethash "PMID" (nth pub-curr-index (pub-get-data pub-curr-page)))))

  (pub-message "Article is being opened in external browser.")

  )

(defun pub-related-articles () ""
  (interactive)
  
  (let ((pmid (gethash "PMID" (nth pub-curr-index (pub-get-data pub-curr-page)))))
    (if (pub-get-related-articles pmid)
        (pub-message (format "Showing related hits for entry %s." pmid))
      (pub-message (format "No related articles found for entry %s." pmid))
      )
    )
  )



(defun pub-help () ""
  (interactive)

  (let ((input)
        (buffer-read-only nil))
    (erase-buffer)

    (insert pub-help-screen)

    (goto-char (point-min))

    (use-local-map pub-help-keymap)
    
    (setq pub-showing-helpscreen t)


    )

  )

(provide 'pub-ui)
