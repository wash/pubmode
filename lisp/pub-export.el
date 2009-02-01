;; pub-export.el --- PubMode an interface to PubMed for Emacs
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


(defun pub-cite (&optional silent) ""
  (interactive)
  
  (let ((selectedEntries '())
        (i 0)
        (j 0)
        (pub-hash)
        (selectedFound nil)
        (key)
        (keys '())
        ) 

    (pub-compare-with-bibtex)

    (while (< i pub-num-pages)
      (setq page-list (aref pub-data i)) 
      (setq j 0)
      (while (< j (length page-list))
        (setq pub-hash (nth j page-list))
        (if (gethash "selected" pub-hash)
            (progn
              (setq selectedFound t)
              (setq key (gethash "indatabase" pub-hash))
              (if key 
                  (push key keys)
                (pub-message "Use `b' to import item first.")
                )
              )
          )
        (setq j (+ 1 j))
        )
      (setq i (+ 1 i))
      )

    
    (if (not selectedFound)
        (progn
          (setq key (gethash "indatabase" (nth pub-curr-index (pub-get-data pub-curr-page))))
          (if key
              (progn
                (pub-send-to-calling-buffer  (pub-format-latex-citation key))
                (if (not silent)
                    (pub-message (format "Citation inserted to buffer %s." (buffer-name pub-calling-buffer)))
                  )
                )
            (pub-message "Use `b' to import item first.")
            )
          )
      (if keys
          (progn
            (pub-send-to-calling-buffer (pub-format-latex-citation (mapconcat (function (lambda (x) x)) keys ",")))
            (if (not silent)
                (pub-message (format "Citation inserted to buffer %s." (buffer-name pub-calling-buffer)))
              )
            )
        )
      )
    )
  )


(defun pub-copy-text () ""
  (interactive)
  
  (let ((selectedEntries '())
        (i 0)
        (j 0)
        (pub-hash)
        (selectedFound nil)
        (text "")
        ) 

    (kill-new "")

    (while (< i pub-num-pages)
      (setq page-list (aref pub-data i)) 
      (setq j 0)
      (while (< j (length page-list))
        (setq pub-hash (nth j page-list))
        (if (gethash "selected" pub-hash)
            (progn
              (setq selectedFound t)
              (with-temp-buffer
                (pub-print-entry-text pub-hash nil t)
                (kill-append (buffer-substring (point-min) (point-max)) nil)
                )
              )
          )
        (setq j (+ 1 j))
        )
      (setq i (+ 1 i))
      )
    
    (if (not selectedFound)
        (progn
          (setq pub-hash (nth pub-curr-index (pub-get-data pub-curr-page)))
          (with-temp-buffer
            (pub-print-entry-text pub-hash nil t)
            (kill-append (buffer-substring (point-min) (point-max)) nil)
            )
          )
      )
    )
  
  (pub-message "Citation(s) saved to the kill-ring. Use \`C-y\' to yank it.")

  )




(defun pub-export-text (&optional silent) ""
  (interactive)
  
  (let ((selectedEntries '())
        (i 0)
        (j 0)
        (pub-hash)
        (selectedFound nil)
        (text "")
        ) 

    (while (< i pub-num-pages)
      (setq page-list (aref pub-data i)) 
      (setq j 0)
      (while (< j (length page-list))
        (setq pub-hash (nth j page-list))
        (if (gethash "selected" pub-hash)
            (progn
              (setq selectedFound t)
              (with-temp-buffer
                (pub-print-entry-text pub-hash nil t)
                (pub-send-to-calling-buffer (buffer-substring (point-min) (point-max)))
                )
              )
          )
        (setq j (+ 1 j))
        )
      (setq i (+ 1 i))
      )
    
    (if (not selectedFound)
        (progn
          (setq pub-hash (nth pub-curr-index (pub-get-data pub-curr-page)))
          (with-temp-buffer
            (pub-print-entry-text pub-hash nil t)
            (pub-send-to-calling-buffer (buffer-substring (point-min) (point-max)))
            )
          )
      )
    )
  
  (if (not silent)
      (pub-message (format "Citation(s) have been imported to buffer %s" (buffer-name pub-calling-buffer))))

  )




(defun pub-export-bibtex (&optional silent) ""
  (interactive)
  

  (let ((selectedEntries '())  
        (i 0)
        (j 0)
        (pub-hash)
        (selectedFound nil)
        (counter 0)
        (key)
        )
         
  ; If no bibtex file is set or found so far, ask the user      

    (if (not pub-bibtex-export)
        (progn
          (call-interactively 'pub-set-bibtex-export)
          (pub-draw-page)
          )
      )

    (while (< i pub-num-pages)

      (setq page-list (aref pub-data i)) 

      (setq j 0)

      (while (< j (length page-list))
            
        (setq pub-hash (nth j page-list))
            
        (if (gethash "selected" pub-hash)
            (progn
              (setq selectedFound t)
              (if (pub-export-bibtex-entry pub-hash)
                  (setq counter (+ counter 1))
                )
              (if (= pub-curr-page i)
                  (pub-reprint-entry j)
                )
              )
          )
        (setq j (+ 1 j))
        )
      (setq i (+ 1 i))
      )
    
    (if (not selectedFound)
        (progn
          (if (pub-export-bibtex-entry (nth pub-curr-index (pub-get-data pub-curr-page)))
              (setq counter (+ counter 1))
            )
          (pub-reprint-entry pub-curr-index)
          )
      )


    (if (not silent)
        (if (> counter 0)
            (if (= counter 1)
                (pub-message "Imported one item.")
              (pub-message (format "Imported %d items" counter)))
          )
      )
    )
  )

(defun pub-export-bibtex-entry (entry) ""
  
  (let* ((index pub-curr-index)
         (keyList '())
         (hash)
         (key (pub-generate-key entry))
         (maxSuffix 0)
         (newSuffix 0)
         (foundExisting nil)
         (currKey "")
         (newKey "")
         (decision)
         (point-start)
         (point-end)
         (suffix nil)
         (finalKey nil) ; non-nil indicates that entry was exported
         (i 0)
         )


    (setq hash  (pub-scan-bib-file pub-bibtex-export))

    (save-excursion
      (set-buffer (find-file-noselect pub-bibtex-export))

      (goto-char (point-max))

      (setq keyList (pub-hash-keys hash))

      (while (< i (length keyList))
        
        (setq currKey (nth i keyList))

        (if (string-match (concat "^" (regexp-quote key) "\\([[:alpha:]]\\)?$") currKey)
            (progn 
              (setq foundExisting t)
              (setq suffix (match-string 1 currKey))

              (if suffix 
                  (progn 
                    (setq suffix (string-to-char suffix))
                
                    (if (> suffix maxSuffix)
                        (setq maxSuffix suffix))
                    
                    )
                )
              )
          )

        (setq i (+ i 1))

        )

      (if foundExisting 
          (progn
            (save-window-excursion
              (switch-to-buffer-other-window (current-buffer))
                            
              (if (= maxSuffix 0) (setq newSuffix ?a)
                (setq newSuffix (+ 1 maxSuffix)))
            
              (setq newKey (concat key (char-to-string newSuffix)))
              
              (if (= maxSuffix 0) (setq currKey key)
                (setq currKey (concat key (char-to-string maxSuffix)))
                )

              (setq start-point (gethash currKey hash))
              (goto-char start-point)
              (set-mark (point))

              (re-search-forward "{[[:blank:]]*\\(.*\\)[[:blank:]]*,")
            

              (setq decision (pub-ask-duplicate currKey newKey))
              
             ;;         ;append
               (cond ((= decision ?a)
                      
                      (progn 
                        (goto-char (point-max))
                        (pub-print-entry-bibtex entry newKey)
                        (setq finalKey newKey)
                        )
                      )
                     ; Skip
                     ((= decision ?s)
                      (message "Skipped %s" key))
                  
                     ; Replace
                     ((= decision ?r)
                      (progn 
                       ; Goto next entry or if there is none to end of buffer
                        (if (not (re-search-forward "@article{" nil t))
                            (goto-char (point-max)))
                     
                        (setq end-point (re-search-backward "}" nil t))                       
                        (forward-char 1)
                     
                        (delete-region start-point (point))
                        
                        (pub-print-entry-bibtex entry currKey)

                        (setq finalKey currKey)                

                        )
                      )      
                     )
              )
            )
        ;else 
        (progn
          (pub-print-entry-bibtex entry key)
          (setq finalKey key)
          )
        )
      )
    
    (if finalKey
        (puthash "indatabase" finalKey entry)
      )

    finalKey
    
    )
  )

(provide 'pub-export)