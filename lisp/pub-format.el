
;; pub-format.el --- PubMode an interface to PubMed for Emacs
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

(defun pub-print-entry-text (data &optional count plain) "Print entry"

  (let* ((buffer-read-only nil) 
         (authors (gethash "authors" data))
         (firstAuthor (car (nth 0 authors)))
         (title (gethash "title" data))
         (journal (gethash "journal" data))
         (year (gethash "year" data))
         (month (gethash "month" data))
         (volume (gethash "volume" data))
         (number (gethash "number" data))
         (pages (gethash "pages" data))
         (abstract (gethash "abstract" data))
         (PMID (gethash "PMID" data))
         (selected (gethash "selected" data))
         (indatabase (gethash "indatabase" data))         
         (detail (gethash "detail" data))
         (authorString "")
         (countString "")
         (idString "")
         (firstInitials)
         (isCollective nil)
         (citation "")
         (indent 6)
         )

    (if (not authors)
        (progn 
          (setq authors (list (list "[No authors listed]" "")))
          (setq firstAuthor "[No authors listed]")
          )
      )
    
    (if (not title)
        (setq title "[No title]")
      )
        
    
    (if (not abstract)
        (setq abstract "No abstract available.")
      )

;    (setq title (encode-coding-string title 'mule-utf-8))
;    (setq firstAuthor (encode-coding-string firstAuthor 'mule-utf-8))
;    (setq abstract (encode-coding-string abstract 'mule-utf-8))

    (setq authorString (mapconcat 'pub-format-author-text authors ", "))

    (setq firstInitials  (nth 1 (nth 0 authors)))

    (setq isCollective (equal firstInitials "COLLECTIVE"))

    (if isCollective
        (setq firstInitials "")
      (setq firstInitials (concat " " firstInitials))
      )



    (if count
        (progn
          (setq countString (format "%d. " count))
          (if indatabase
              (setq countString (format "*%d. " count))
            )
          (setq countString (format "%6s" countString))
          (if selected 
              (if (not plain)
                  (put-text-property 0 (- (length countString) 1) 'face 'pub-selection-face countString)
                )
            )
          (insert countString)
          )

      (setq indent 0)

      )
   

    (if (= detail 1)
        (progn 
         
          (if (and (not isCollective) (> (length authors) 1))
              (setq firstAuthor (concat firstAuthor " et al. "))
            (setq firstAuthor (concat firstAuthor firstInitials ". "))
            )

          (setq citation (pub-format-citation data 2))

          (if (not plain)
              (progn
                (put-text-property 0 (length firstAuthor) 'face 'pub-author-face firstAuthor)
                (put-text-property 0 (length citation) 'face 'pub-citation-face citation)
                )
            )

          
          (insert firstAuthor)
          (insert citation)
          (insert "\n\n")
          
          )
      )

    (if (>= detail 2)
        (progn 

          (setq authorString (concat (pub-break-line authorString pub-line-width indent t) "\n"))
          (setq title (concat (pub-break-line title pub-line-width indent) "\n"))
          (setq citation (concat (pub-break-line (pub-format-citation data 1) pub-line-width indent) "\n"))

          (if (not plain)
              (progn
                (put-text-property 0 (length authorString) 'face 'pub-author-face authorString)
                (put-text-property 0 (length title) 'face 'pub-title-face title)
                (put-text-property 0 (length citation) 'face 'pub-citation-face citation)
                ))
          
          ;(insert (format "%3d. " count))
          (insert authorString)
          (insert title)
          (insert citation)
          (insert "\n")
             
          ))
          
    (if (= detail 3)
        (progn 

          (setq abstract (concat (pub-break-line abstract pub-line-width indent) "\n"))
          (setq idString (concat "PMID: " PMID))
          (setq idString (pub-break-line idString pub-line-width indent))

          (insert abstract)
          (insert "\n")
          (insert idString)
          (insert "\n\n")
          )
      )
    )
  )

(defun pub-format-citation (data style) ""

  (let* ((journal (gethash "journal" data))
         (year (gethash "year" data))
         (month (gethash "month" data))
         (volume (gethash "volume" data))
         (number (gethash "number" data))
         (pages (gethash "pages" data))
         (pubstatus (gethash "pubstatus" data))
         (citation)
         )
    
    (setq journal (format "%s. " journal))

    (if (equal pubstatus "aheadofprint")
        (setq pubstatus "[Epub ahead of print]"))

    (if year 
        (setq year (format "%s" year))
      (setq year ""))
    
    (if month 
        (setq month (format " %s" month))
      (setq month ""))
    
    (if number (setq number (format "(%s)" number))
      (setq number "")
      )

    (if volume (setq volume (format "%s:" volume))
      (setq number "")
      )
    
    (if pages (setq pages (format "%s" pages))
      (setq pages pubstatus)
      )


    (if (= style 1)
        (setq citation (concat journal year month "; " volume number pages))
      )
    
    (if (= style 2)
        (setq citation (concat journal year "; " volume pages))
     )

    citation

    )
  )


(defun pub-generate-key (data)

  (let* ((authors (gethash "authors" data))
         (firstAuthor (car (nth 0 authors)))
         (title (gethash "title" data))
         (journal (gethash "journal" data))
         (year (gethash "year" data))
         (month (gethash "month" data))
         (volume (gethash "volume" data))
         (number (gethash "number" data))
         (pages (gethash "pages" data))
         (abstract (gethash "abstract" data))
         (PMID (gethash "PMID" data))
         (selected (gethash "selected" data))
         (detail (gethash "detail" data))
         (authorString "")
         (short-year "")
         (len nil)
         (key "")
         )
    
    (setq journal (replace-regexp-in-string "[[:blank:]]+" "_" journal))        


    (if (not year)
        (progn
          (setq year "")
          (setq short-year "")
          )
      (progn
        (setq short-year (concat (string (aref year 2)) (string (aref year 3))))
        )
      )
    
    (if (not firstAuthor)
        (setq firstAuthor journal)
      )

    (setq key pub-key-template)

    (if (string-match "N\\([[:digit:]]+\\)" key)
        (setq len (match-string 1 key)))

    (if len
        (progn
          (setq len (string-to-number len))
          (if (>= (length firstAuthor) len)
              (setq firstAuthor (substring firstAuthor 0 len))
            )
          )
      )
 
    (setq key (replace-regexp-in-string "%N[[:digit:]]*" firstAuthor key t))
    (setq key (replace-regexp-in-string "#N[[:digit:]]*" (downcase firstAuthor) key t))
    (setq key (replace-regexp-in-string "@N[[:digit:]]*" (upcase firstAuthor) key t))

    (setq key (replace-regexp-in-string "%Y" year key t))
    (setq key (replace-regexp-in-string "#Y" short-year key t))

    (setq key (replace-regexp-in-string "%P" PMID key t))

    (setq key (replace-regexp-in-string "%J" journal key t))
    (setq key (replace-regexp-in-string "#J" (downcase journal) key t))
    (setq key (replace-regexp-in-string "@J" (upcase journal) key t))

    
    (setq key (replace-regexp-in-string "[[:blank:]]+" "_" key))

    
    (setq key (pub-unicode-string-to-ascii key))

    key
    
    )

  )


(defun pub-print-entry-bibtex (data key) "Print entry as bibtex"

  (let* ((authors (gethash "authors" data))
         (firstAuthor (car (nth 0 authors)))
         (title (gethash "title" data))
         (journal (gethash "journal" data))
         (year (gethash "year" data))
         (month (gethash "month" data))
         (volume (gethash "volume" data))
         (number (gethash "number" data))
         (pages (gethash "pages" data))
         (abstract (gethash "abstract" data))
         (PMID (gethash "PMID" data))
         (doi (gethash "doi" data))
         (selected (gethash "selected" data))
         (detail (gethash "detail" data))
         (authorString "")
         )

    (if authors
        (progn
          (setq authorString (mapconcat 'pub-format-author-bibtex authors " and "))
          (setq authorString (pub-break-line authorString 50 17 t))
          )
      (progn 
        (setq authors (list (list "unknown" "")))
        (setq firstAuthor "unknown")
        )
      )



    (setq title (pub-escape-tex title))
    (setq journal (pub-escape-tex journal))
    (setq volume (pub-escape-tex volume))
    (setq number (pub-escape-tex number))
    (setq abstract (pub-escape-tex abstract))
    (setq pages (pub-escape-tex pages))
    (setq doi (pub-escape-tex doi))

   
    (if (not year) (setq year ""))
    (if (not title) (setq title ""))
    (if (not month) (setq month ""))
    (if (not number) (setq number ""))
    (if (not volume) (setq volume ""))
    (if (not pages) (setq pages ""))
    (if (not abstract) (setq abstract ""))
    (if (not doi) (setq doi ""))


    (if pub-bibtex-double-brace
        (setq title (concat "{" title "}"))
      )


    (setq title (pub-break-line title 50 17 t))
    (setq abstract (pub-break-line abstract 50 17 t))




    (insert (concat "@Article{" key ",\n"))
    (insert (concat "  author =       {" authorString "},\n"))
    (insert (concat "  title =        {" title "},\n"))
    (insert (concat "  journal =      {" journal "},\n"))
    (insert (concat "  year =         {" year "},\n"))
    (insert (concat "  month =        {" month "},\n"))
    (insert (concat "  number =       {" number "},\n"))
    (insert (concat "  volume =       {" volume "},\n"))
    (insert (concat "  pages =        {" pages "},\n"))
    
    (insert (concat "  doi =          {" doi "},\n"))

    (if (and (not (equal abstract "")) pub-bibtex-abstract)
        (insert (concat "  abstract =     {" abstract "},\n"))
      )
    
    (insert (concat "  PMID =         {" PMID "}}\n\n"))
  

    )

  )

(defun pub-format-author-bibtex (author) ""

  (let ( (lastName (nth 0 author))
         (initials (nth 1 author))
         (suffix (nth 2 author))
         )

    (if ( = (length initials) 2)
        (setq initials (concat (string (aref initials 0)) " " (string (aref initials 1)))))


    (setq lastName (pub-escape-tex lastName))
    (setq initials (pub-escape-tex initials))

    
    (if (equal initials "COLLECTIVE")
        (concat "{" lastName "}")

      (if suffix
          (progn
            (setq suffix (pub-escape-tex suffix))
            (concat (pub-unicode-string-to-tex lastName) ", " suffix ", " initials)
            )
        (concat (pub-unicode-string-to-tex lastName) ", " initials)
        )
      )
    )
  )


(defun pub-format-author-text (author) ""

  (let ( (lastName (nth 0 author))
         (initials (nth 1 author))
         (suffix (nth 2 author))
         )

    (if suffix (setq initials (concat initials " " suffix)))
    
    (if (equal initials "COLLECTIVE")
        lastName
      (concat lastName " " initials)
      )
    )

)


(defun pub-format-latex-citation (key) ""
  
  (replace-regexp-in-string "%K" key pub-citation-template t)        
 
  )


(provide 'pub-format)