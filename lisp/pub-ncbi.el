
;; pub-ncbi.el --- PubMode an interface to PubMed for Emacs
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

(defvar esearch 
  "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=PubMed&usehistory=y&retmax=1&term="
  "URL for esearch tool of the NCBI API.")

(defvar efetch  
  "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?retmode=xml&db=PubMed"
  "URL for efetch tool of the NCBI API."
  )

(defvar elink_linkout
  "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?retmode=ref&cmd=prlinks&db=PubMed&"
  "URL for elink tool of the NCBI API with settings for getting LinkOut URL."
  )

(defvar elink_related
  "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&db=pubmed&id="
  "URL for elink tool of the NCBI API with settings for getting related articles."
  )

(defvar espell
  "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/espell.fcgi?&db=PubMed&term="
  "URL for espell tool of the NCBI API."
  )

(defvar epost
  "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/epost.fcgi?db=pubmed&id="
  "URL for epost tool of the NCBI API."
  )




(defun pub-search (term) "Search pubmed"

  (catch 'exit

    (let ((url)
          (i 0)
          (data-buffer)
          (status)
          (web-env)
          (query-key)
          (num-hits))
    
      (save-excursion

        (setq url (concat esearch term))
        (setq data-buffer (url-retrieve-synchronously url))

        (if (not data-buffer)
            (throw 'exit 'communication-problem))

        (switch-to-buffer data-buffer)
        (goto-char (point-min))
        (if (not (search-forward "<?xml" nil t))
            (progn
              (kill-buffer (current-buffer))
              (throw 'exit 'communication-problem)
              )
          )
        (backward-char 5)
        (setq root (car (xml-parse-region (point) (point-max))))
        (kill-buffer (current-buffer))
      
        )
    
      (setq web-env (car (xml-node-children (car (xml-get-children root 'WebEnv)))))
      (setq query-key (car (xml-node-children (car (xml-get-children root 'QueryKey)))))
      (setq num-hits  (car (xml-node-children (car (xml-get-children root 'Count)))))
      
      (setq num-hits (string-to-number num-hits))
      
      (if (= num-hits 0)
          (progn
            (setq status 'nohits)
            (throw 'exit 'nohits)
            )
        )

     
      (throw 'exit (list web-env query-key num-hits))

      )

   
    )
  )


(defun pub-fetch (page) "Retrieve PubMed entries"

  (let ((url)
        (retstart (* page pub-limit)))
    (save-excursion

      (setq url (concat efetch "&query_key=" pub-query-key 
                        "&WebEnv=" pub-web-env "&retstart=" 
                        (number-to-string retstart) "&retmax=" 
                        (number-to-string pub-limit)))
    


      (switch-to-buffer (url-retrieve-synchronously url))

      ;(save-buffer)


      (goto-char (point-min))
      (search-forward "<?xml")
      (backward-char 5)
      
      (setq root (car (xml-parse-region (point) (point-max))))
    
      (kill-buffer (current-buffer))

      )

    (aset pub-data page (pub-xml-parse-PubMedArticleSet root))

    )

  (pub-compare-with-bibtex)


  )


(defun pub-linkout (pmid) ""

  (concat elink_linkout "id=" pmid)

  )



(defun pub-xml-parse-PubMedArticleSet (root) ""

  (let ((returnList '())
        (articles)
        )

    (setq articles (xml-get-children root 'PubmedArticle))

    (setq )

    (while articles
    
      (setq currArticle (pop articles))
  
      (setq data (pub-xml-parse-PubMedArticle currArticle))

      (setq returnList (append returnList (list data)))

      )

    returnList
  
    
    )
  )


(defun pub-xml-parse-PubMedArticle (article) "Parses article XML"
  (let* ((citationEntry (car (xml-get-children currArticle 'MedlineCitation)))
         (PMID (car (xml-node-children (car (xml-get-children citationEntry 'PMID)))))
         (articleEntry (car (xml-get-children citationEntry 'Article)))
         (journalEntry (car (xml-get-children articleEntry 'Journal)))
         (journalIssueEntry (car (xml-get-children journalEntry 'JournalIssue)))
         (volume (car (xml-node-children (car (xml-get-children journalIssueEntry 'Volume)))))
         (number (car (xml-node-children (car (xml-get-children journalIssueEntry 'Issue)))))
         (PubDateEntry (car (xml-get-children journalIssueEntry 'PubDate)))
         (year (car (xml-node-children (car (xml-get-children PubDateEntry 'Year)))))
         (MedlineDate (car (xml-node-children (car (xml-get-children PubDateEntry 'MedlineDate)))))
         (month (car (xml-node-children (car (xml-get-children PubDateEntry 'Month)))))
         (title (car (xml-node-children (car (xml-get-children articleEntry 'ArticleTitle)))))
         (medlineJournalInfoEntry (car (xml-get-children citationEntry 'MedlineJournalInfo)))
         (journal (car (xml-node-children (car (xml-get-children medlineJournalInfoEntry 'MedlineTA)))))
         (paginationEntry (car (xml-get-children articleEntry 'Pagination)))
         (pages (car (xml-node-children (car (xml-get-children paginationEntry 'MedlinePgn)))))
         (abstractEntry (car (xml-get-children articleEntry 'Abstract)))
         (abstract (car (xml-node-children (car (xml-get-children abstractEntry 'AbstractText)))))
         (authorList (xml-get-children (car (xml-get-children articleEntry 'AuthorList)) 'Author))
         (pubmedDataEntry (car (xml-get-children currArticle 'PubmedData)))
         (pubstatus (car (xml-node-children (car (xml-get-children pubmedDataEntry 'PublicationStatus)))))                          
         (articleId (xml-get-children (car (xml-get-children pubmedDataEntry 'ArticleIdList)) 'ArticleId))
         (authors '())
         (doi nil)
         (hash)
         )

    (while authorList (setq authors (append authors (list (pub-parse-xml-author (pop authorList))))))

    (setq hash (make-hash-table :test 'equal)) 

    (if journal (setq journal (decode-coding-string journal 'mule-utf-8)))
    (if title (setq title (decode-coding-string title 'mule-utf-8)))
    (if abstract (setq abstract (decode-coding-string abstract 'mule-utf-8)))

    (setq doi (pub-parse-xml-articleIdList articleId))

    
    ;; Non-standard date formats are stored in MedlineDate fields
    ;; We parse it for a year and ignore the rest.

    (if MedlineDate
        (if (string-match "\\([12][[:digit:]][[:digit:]][[:digit:]]\\)" MedlineDate)
            (setq year (match-string 1 MedlineDate)))
      )
    



    (puthash "PMID" PMID hash)
    (puthash "year" year hash)
    (puthash "volume" volume hash)
    (puthash "number" number hash)
    (puthash "month" month hash)
    (puthash "title" title hash)
    (puthash "pages" pages hash)
    (puthash "abstract" abstract hash)
    (puthash "journal" journal hash)
    (puthash "authors" authors hash)
    (puthash "doi" doi hash)
    (puthash "pubstatus" pubstatus hash)
    (puthash "selected" nil hash)
    (puthash "detail"  pub-default-detail hash)
    (puthash "indatabase" nil hash)

    hash

    )


  )

(defun pub-parse-xml-author (authorEntry) "Parses author entry"

  (let* ((lastName (car (xml-node-children (car (xml-get-children authorEntry 'LastName)))))
         (initials (car (xml-node-children (car (xml-get-children authorEntry 'Initials)))))
         (firstName (car (xml-node-children (car (xml-get-children authorEntry 'FirstName)))))
         (suffix (car (xml-node-children (car (xml-get-children authorEntry 'Suffix)))))
         (collectiveName (car (xml-node-children (car (xml-get-children authorEntry 'CollectiveName)))))
         (returnList '())
         )

    (if collectiveName
        (progn (setq lastName collectiveName)
               (setq initials "COLLECTIVE")))

    (if (not lastName)
        (setq lastName "")
      )

    (if (not initials)
        (setq initials "")
      )

   
    ;(print lastName)
    ;(print (find-coding-systems-string lastName))


    (setq lastName (decode-coding-string lastName 'mule-utf-8))
    (setq initials (decode-coding-string initials 'mule-utf-8))
    
    (if suffix 
        (setq suffix (decode-coding-string suffix 'mule-utf-8))
      )

    
    (list lastName initials suffix)
    )
  )


(defun pub-parse-xml-articleIdList (articleIdList) ""

  (let ((articleId t)
        (doi nil))

    (while articleId
      
      (setq articleId (pop articleIdList))

      (if (equal (cdr (assq 'IdType (xml-node-attributes articleId))) "doi")
          (setq doi (car (xml-node-children articleId)))
        )
      
      )

    doi
    
    )
  )


(defun pub-spell-suggestions (term) ""

  (interactive "MSearch PubMed: ")
  
  (setq term (replace-regexp-in-string "[[:blank:]]+" "%20" term))
  
  (let ((url)
        (corrected))
    (save-excursion

      (setq url (concat espell term))
    
      (switch-to-buffer (url-retrieve-synchronously url))

      (goto-char (point-min))
      (search-forward "<eSpellResult>")
      

      (backward-char 14)
      
      (setq corrected (car (xml-node-children (car (xml-get-children (car (xml-parse-region (point) (point-max))) 'CorrectedQuery)))))
      
      (kill-buffer (current-buffer))

      )

    corrected

    )

  )

(defun pub-get-related-articles (pmid) ""

  (interactive "MSearch PubMed: ")

  (let ((url)
        (idlist '())
        (web-env)
        (query-key)
        (num-hits)
        (counter 0)
        )

    (catch 'exit

    (save-excursion

      (setq url (concat elink_related pmid))

      (switch-to-buffer (url-retrieve-synchronously url))

      (goto-char (point-min))
      
      (while (re-search-forward "<Id>\\([[:digit:]]+\\)</Id>" (point-max) t)

        ;; Limit number of related articles to 500
        (if (< counter 500)
            ;; Skipt first one which is Id of the original entry
            (if (> counter 1)
                (push (match-string 1) idlist) 
              )
          )
      
        (setq counter (+ 1 counter))
        )

      ;; not tested because I did not find article without related articles
      ;; but they exist
      (if (= (length idlist) 0)
          (throw 'exit nil)
        )



      (setq idlist (reverse idlist))
      

      (kill-buffer (current-buffer))
            
      (setq url (concat epost (mapconcat (lambda (pmid) pmid) idlist ",")))

      (switch-to-buffer (url-retrieve-synchronously url))

      (goto-char (point-min))
      (if (not (search-forward "<?xml" nil t))
          (progn
            (kill-buffer (current-buffer))
            )
          )
      (backward-char 5)
      (setq root (car (xml-parse-region (point) (point-max))))
      (kill-buffer (current-buffer))

      (setq pub-num-hits (length idlist))
      (setq pub-web-env (car (xml-node-children (car (xml-get-children root 'WebEnv)))))
      (setq pub-query-key (car (xml-node-children (car (xml-get-children root 'QueryKey)))))
      

      (if (= 0 (% pub-num-hits pub-limit))
          (setq pub-num-pages (/ pub-num-hits pub-limit))
        (setq pub-num-pages (+ (/ pub-num-hits pub-limit) 1))
        )
    
      ; initialize pub-data vector
      (setq pub-data (make-vector pub-num-hits nil))

      (setq pub-curr-index 0)
      (setq pub-curr-page 0)

      (setq pub-curr-query-term (format "Related to %s" pmid))

      (pub-fetch pub-curr-page)
            
      (pub-show-page)

      )
    
    (throw 'exit t)
    
    )
    
    )
  )


(provide 'pub-ncbi)

