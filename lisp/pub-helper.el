
;; pub-helper.el --- PubMode an interface to PubMed for Emacs
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


(defun pub-get-data (page) ""

  (if (not (aref pub-data pub-curr-page))
      (pub-fetch pub-curr-page)
    )

  (aref pub-data pub-curr-page)

  )

(defun pub-compare-with-bibtex () ""
  (let* ((bib-hash)
         (page-list)
         (pub-hash)
         (key)
         (pmid)
         (i 0)
         (j 0))

    (if pub-bibtex-export
        (progn
          (setq bib-hash (pub-scan-bib-file-pmid pub-bibtex-export))
          
          (while (< i pub-num-pages)
            (setq page-list (aref pub-data i)) 
            (setq j 0)
            (if page-list
                (while (< j (length page-list))
                  (setq pub-hash (nth j page-list))
              
                  (if pub-hash
                      (progn
                        (setq pmid (gethash "PMID" pub-hash))
                        
                        (if (gethash pmid bib-hash)
                            (puthash "indatabase" (gethash pmid bib-hash) pub-hash)
                          (puthash "indatabase" nil pub-hash)
                          )
                        )
                    )
                  (setq j (+ 1 j))
                  )
              )
              
            (setq i (+ 1 i))
     
            )
          )
      )
    )
  )


(defun pub-break-line (line n indent &optional first) ""

  (with-temp-buffer
    (let ((fill-column n))
      (insert line)
      (fill-region (point-min) (point-max))
      (setq line (delete-and-extract-region (point-min) (point-max)))
      )
    )
  (setq line (replace-regexp-in-string "[\n]" (concat "\n" (make-string indent ?\ )) line))

  (if (not first)
      (setq line (concat (make-string indent ?\ ) line ))
    )

  line

  )




(defun pub-hash-keys (hash) "Return all keys in hashtable."
  
  (let (allkeys)
    (maphash (lambda (kk vv) (setq allkeys (cons kk allkeys)))
             hash)
    allkeys
    )
  ) 

(defun pub-send-to-calling-buffer (text) ""
  
  
  (set-buffer pub-calling-buffer)
    
  (insert-before-markers text)
  
  (set-buffer "*PubMed Results*")

  )




(defun pub-scan-bib-file (file) ""
  (interactive)


  (let ((hash))
  
    (setq hash (make-hash-table :test 'equal)) 

    (save-excursion

      (set-buffer (find-file-noselect file))

      (goto-char (point-min))
    
      (while (setq match (re-search-forward "@article{[[:blank:]]*\\(.*\\)[[:blank:]]*," nil t))
        
        (puthash (match-string 1) (match-beginning 0) hash)

        )
      )
    hash
    )
  )

(defun pub-scan-bib-file-pmid (file) ""
  (interactive)

  (let ((hash)
        (pmid)
        (key))
    
    (setq hash (make-hash-table :test 'equal)) 
    
    (save-excursion

      (set-buffer (find-file-noselect file))

      (goto-char (point-min))
    
      (while (setq match (re-search-forward "PMID[[:blank:]]*=[[:blank:]\"{]*\\([[:digit:]]+\\)" nil t))

        (setq pmid (match-string 1))

        (save-excursion
          (re-search-backward "@article{[[:blank:]]*\\(.*\\)[[:blank:]]*," nil t)
          (setq key (match-string 1))
          )
                
        (puthash pmid key hash)
        
       
        )
      )
    hash
    )
  )





(provide 'pub-helper)