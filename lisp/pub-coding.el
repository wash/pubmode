
;; pub-coding.el --- PubMode an interface to PubMed for Emacs
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

(setq unicode-to-ascii-table 
      (list '(?\x00D8 "O")  ; Ø   LATIN CAPITAL LETTER O WITH STROKE
            '(?\x00E0 "a")  ; à   LATIN SMALL LETTER A WITH GRAVE
            '(?\x00E1 "a")  ; á   LATIN SMALL LETTER A WITH ACUTE
            '(?\x00E2 "a")  ; â   LATIN SMALL LETTER A WITH CIRCUMFLEX
            '(?\x00E3 "a")  ; ã   LATIN SMALL LETTER A WITH TILDE
            '(?\x00E4 "a")  ; ä   LATIN SMALL LETTER A WITH DIAERESIS
            '(?\x00E5 "a")  ; å   LATIN SMALL LETTER A WITH RING ABOVE
            '(?\x00E7 "c")  ; ç   LATIN SMALL LETTER C WITH CEDILLA
            '(?\x00E8 "e")  ; è   LATIN SMALL LETTER E WITH GRAVE
            '(?\x00E9 "e")  ; é   LATIN SMALL LETTER E WITH ACUTE
            '(?\x00EA "e")  ; ê   LATIN SMALL LETTER E WITH CIRCUMFLEX
            '(?\x00EB "e")  ; ë   LATIN SMALL LETTER E WITH DIAERESIS
            '(?\x00EC "i")  ; ì   LATIN SMALL LETTER I WITH GRAVE
            '(?\x00ED "i")  ; í   LATIN SMALL LETTER I WITH ACUTE
            '(?\x00EE "i")  ; î   LATIN SMALL LETTER I WITH CIRCUMFLEX
            '(?\x00EF "i")  ; ï   LATIN SMALL LETTER I WITH DIAERESIS
            '(?\x00F1 "n")  ; ñ   LATIN SMALL LETTER N WITH TILDE
            '(?\x00F2 "o")  ; ò   LATIN SMALL LETTER O WITH GRAVE
            '(?\x00F3 "o")  ; ó   LATIN SMALL LETTER O WITH ACUTE
            '(?\x00F4 "o")  ; ô   LATIN SMALL LETTER O WITH CIRCUMFLEX
            '(?\x00F5 "o")  ; õ   LATIN SMALL LETTER O WITH TILDE
            '(?\x00F6 "o")  ; ö   LATIN SMALL LETTER O WITH DIAERESIS
            '(?\x00F8 "o")  ; ø   LATIN SMALL LETTER O WITH STROKE
            '(?\x00F9 "u")  ; ù   LATIN SMALL LETTER U WITH GRAVE
            '(?\x00FA "u")  ; ú   LATIN SMALL LETTER U WITH ACUTE
            '(?\x00FB "u")  ; û   LATIN SMALL LETTER U WITH CIRCUMFLEX
            '(?\x00FC "u")  ; ü   LATIN SMALL LETTER U WITH DIAERESIS
            '(?\x00FD "y")  ; ý   LATIN SMALL LETTER Y WITH ACUTE
            '(?\x00FF "y")  ; ÿ   LATIN SMALL LETTER Y WITH DIAERESIS
            '(?\x0101 "a")  ; ā  LATIN SMALL LETTER A WITH MACRON
            '(?\x0103 "a")  ; ă   LATIN SMALL LETTER A WITH BREVE
            '(?\x0107 "c")  ; ć   LATIN SMALL LETTER C WITH ACUTE
            '(?\x0109 "c")  ; ĉ   LATIN SMALL LETTER C WITH CIRCUMFLEX
            '(?\x0113 "e")  ; ē  LATIN SMALL LETTER E WITH MACRON
            '(?\x0115 "e")  ; ĕ   LATIN SMALL LETTER E WITH BREVE
            '(?\x011D "g")  ; ĝ   LATIN SMALL LETTER G WITH CIRCUMFLEX
            '(?\x011F "g")  ; ğ   LATIN SMALL LETTER G WITH BREVE
            '(?\x0123 "g")  ; ģ   LATIN SMALL LETTER G WITH CEDILLA
            '(?\x0125 "h")  ; ĥ   LATIN SMALL LETTER H WITH CIRCUMFLEX
            '(?\x0129 "i")  ; ĩ   LATIN SMALL LETTER I WITH TILDE
            '(?\x012B "i")  ; ī  LATIN SMALL LETTER I WITH MACRON
            '(?\x012D "i")  ; ĭ   LATIN SMALL LETTER I WITH BREVE
            '(?\x0135 "j")  ; ĵ   LATIN SMALL LETTER J WITH CIRCUMFLEX
            '(?\x0137 "k")  ; ķ   LATIN SMALL LETTER K WITH CEDILLA
            '(?\x013A "l")  ; ĺ   LATIN SMALL LETTER L WITH ACUTE
            '(?\x013C "l")  ; ļ   LATIN SMALL LETTER L WITH CEDILLA
            '(?\x0141 "L")  ; Ł  LATIN CAPITAL LETTER L WITH STROKE
            '(?\x0142 "l")  ; ł  LATIN SMALL LETTER L WITH STROKE
            '(?\x0144 "n")  ; ń   LATIN SMALL LETTER N WITH ACUTE
            '(?\x0146 "n")  ; ņ   LATIN SMALL LETTER N WITH CEDILLA
            '(?\x014D "o")  ; ō  LATIN SMALL LETTER O WITH MACRON
            '(?\x014F "o")  ; ŏ   LATIN SMALL LETTER O WITH BREVE
            '(?\x0155 "r")  ; ŕ   LATIN SMALL LETTER R WITH ACUTE
            '(?\x0157 "r")  ; ŗ   LATIN SMALL LETTER R WITH CEDILLA
            '(?\x015B "s")  ; ś   LATIN SMALL LETTER S WITH ACUTE
            '(?\x015D "s")  ; ŝ   LATIN SMALL LETTER S WITH CIRCUMFLEX
            '(?\x015F "s")  ; ş   LATIN SMALL LETTER S WITH CEDILLA
            '(?\x0163 "t")  ; ţ   LATIN SMALL LETTER T WITH CEDILLA
            '(?\x0169 "u")  ; ũ   LATIN SMALL LETTER U WITH TILDE
            '(?\x016B "u")  ; ū  LATIN SMALL LETTER U WITH MACRON
            '(?\x016D "u")  ; ŭ   LATIN SMALL LETTER U WITH BREVE
            '(?\x016F "u")  ; ů   LATIN SMALL LETTER U WITH RING ABOVE
            '(?\x0175 "w")  ; ŵ   LATIN SMALL LETTER W WITH CIRCUMFLEX
            '(?\x0177 "y")  ; ŷ   LATIN SMALL LETTER Y WITH CIRCUMFLEX
            '(?\x017A "z")  ; ź   LATIN SMALL LETTER Z WITH ACUTE
            '(?\x1E81 "w")  ; ẁ   LATIN SMALL LETTER W WITH GRAVE
            '(?\x1E83 "w")  ; ẃ   LATIN SMALL LETTER W WITH ACUTE
            '(?\x1E85 "w")  ; ẅ   LATIN SMALL LETTER W WITH DIAERESIS
            '(?\x1EF3 "y")  ; ỳ   LATIN SMALL LETTER Y WITH GRAVE
            ))


(setq unicode-to-tex-table 
      (list '(?\x00D8 "{\\O}"    )  ; Ø   LATIN CAPITAL LETTER O WITH STROKE
            '(?\x00E0 "{\\`a}"   )  ; à   LATIN SMALL LETTER A WITH GRAVE
            '(?\x00E1 "{\\'a}"   )  ; á   LATIN SMALL LETTER A WITH ACUTE
            '(?\x00E2 "{\\^a}"   )  ; â   LATIN SMALL LETTER A WITH CIRCUMFLEX
            '(?\x00E3 "{\\~a}"   )  ; ã   LATIN SMALL LETTER A WITH TILDE
            '(?\x00E4 "{\\\"a}"  )  ; ä   LATIN SMALL LETTER A WITH DIAERESIS
            '(?\x00E5 "{\\aa}"   )  ; å   LATIN SMALL LETTER A WITH RING ABOVE
            '(?\x00E7 "{\\c c}"  )  ; ç   LATIN SMALL LETTER C WITH CEDILLA
            '(?\x00E8 "{\\`e}"   )  ; è   LATIN SMALL LETTER E WITH GRAVE
            '(?\x00E9 "{\\'e}"   )  ; é   LATIN SMALL LETTER E WITH ACUTE
            '(?\x00EA "{\\^e}"   )  ; ê   LATIN SMALL LETTER E WITH CIRCUMFLEX
            '(?\x00EB "{\\\"e}"  )  ; ë   LATIN SMALL LETTER E WITH DIAERESIS
            '(?\x00EC "{\\`i}"   )  ; ì   LATIN SMALL LETTER I WITH GRAVE
            '(?\x00ED "{\\'i}"   )  ; í   LATIN SMALL LETTER I WITH ACUTE
            '(?\x00EE "{\\^i}"   )  ; î   LATIN SMALL LETTER I WITH CIRCUMFLEX
            '(?\x00EF "{\\\"i}"  )  ; ï   LATIN SMALL LETTER I WITH DIAERESIS
            '(?\x00F1 "{\\~n}"   )  ; ñ   LATIN SMALL LETTER N WITH TILDE
            '(?\x00F2 "{\\`o}"   )  ; ò   LATIN SMALL LETTER O WITH GRAVE
            '(?\x00F3 "{\\'o}"   )  ; ó   LATIN SMALL LETTER O WITH ACUTE
            '(?\x00F4 "{\\^o}"   )  ; ô   LATIN SMALL LETTER O WITH CIRCUMFLEX
            '(?\x00F5 "{\\~o}"   )  ; õ   LATIN SMALL LETTER O WITH TILDE
            '(?\x00F6 "{\\\"o}"  )  ; ö   LATIN SMALL LETTER O WITH DIAERESIS
            '(?\x00F8 "{\\o}"    )  ; ø   LATIN SMALL LETTER O WITH STROKE
            '(?\x00F9 "{\\`u}"   )  ; ù   LATIN SMALL LETTER U WITH GRAVE
            '(?\x00FA "{\\'u}"   )  ; ú   LATIN SMALL LETTER U WITH ACUTE
            '(?\x00FB "{\\^u}"   )  ; û   LATIN SMALL LETTER U WITH CIRCUMFLEX
            '(?\x00FC "{\\\"u}"  )  ; ü   LATIN SMALL LETTER U WITH DIAERESIS
            '(?\x00FD "{\\'y}"   )  ; ý   LATIN SMALL LETTER Y WITH ACUTE
            '(?\x00FF "{\\\"y}"  )  ; ÿ   LATIN SMALL LETTER Y WITH DIAERESIS
            '(?\x0101 "{\\=a}"   )  ; ā  LATIN SMALL LETTER A WITH MACRON
            '(?\x0103 "{\\u a}"  )  ; ă   LATIN SMALL LETTER A WITH BREVE
            '(?\x0107 "{\\'c}"   )  ; ć   LATIN SMALL LETTER C WITH ACUTE
            '(?\x0109 "{\\^c}"   )  ; ĉ   LATIN SMALL LETTER C WITH CIRCUMFLEX
            '(?\x0113 "{\\=e}"   )  ; ē  LATIN SMALL LETTER E WITH MACRON
            '(?\x0115 "{\\u e}"  )  ; ĕ   LATIN SMALL LETTER E WITH BREVE
            '(?\x011D "{\\^g}"   )  ; ĝ   LATIN SMALL LETTER G WITH CIRCUMFLEX
            '(?\x011F "{\\u g}"  )  ; ğ   LATIN SMALL LETTER G WITH BREVE
            '(?\x0123 "{\\`g}"   )  ; ģ   LATIN SMALL LETTER G WITH CEDILLA
            '(?\x0125 "{\\^h}"   )  ; ĥ   LATIN SMALL LETTER H WITH CIRCUMFLEX
            '(?\x0129 "{\\~\\i}" )  ; ĩ   LATIN SMALL LETTER I WITH TILDE
            '(?\x012B "{\\=\\i}" )  ; ī  LATIN SMALL LETTER I WITH MACRON
            '(?\x012D "{\\u \\i}")  ; ĭ   LATIN SMALL LETTER I WITH BREVE
            '(?\x0135 "{\\^\\j}" )  ; ĵ   LATIN SMALL LETTER J WITH CIRCUMFLEX
            '(?\x0137 "{\\'k}"   )  ; ķ   LATIN SMALL LETTER K WITH CEDILLA
            '(?\x013A "{\\'l}"   )  ; ĺ   LATIN SMALL LETTER L WITH ACUTE
            '(?\x013C "{\\c l}"  )  ; ļ   LATIN SMALL LETTER L WITH CEDILLA
            '(?\x0141 "{\\L}"    )  ; Ł  LATIN CAPITAL LETTER L WITH STROKE
            '(?\x0142 "{\\l}"    )  ; ł  LATIN SMALL LETTER L WITH STROKE
            '(?\x0144 "{\\'n}"   )  ; ń   LATIN SMALL LETTER N WITH ACUTE
            '(?\x0146 "{\\c n}"  )  ; ņ   LATIN SMALL LETTER N WITH CEDILLA
            '(?\x014D "{\\=o}"   )  ; ō  LATIN SMALL LETTER O WITH MACRON
            '(?\x014F "{\\u o}"  )  ; ŏ   LATIN SMALL LETTER O WITH BREVE
            '(?\x0155 "{\\'r}"   )  ; ŕ   LATIN SMALL LETTER R WITH ACUTE
            '(?\x0157 "{\\c r}"  )  ; ŗ   LATIN SMALL LETTER R WITH CEDILLA
            '(?\x015B "{\\'s}"   )  ; ś   LATIN SMALL LETTER S WITH ACUTE
            '(?\x015D "{\\^s}"   )  ; ŝ   LATIN SMALL LETTER S WITH CIRCUMFLEX
            '(?\x015F "{\\c s}"  )  ; ş   LATIN SMALL LETTER S WITH CEDILLA
            '(?\x0163 "{\\c t}"  )  ; ţ   LATIN SMALL LETTER T WITH CEDILLA
            '(?\x0169 "{\\~u}"   )  ; ũ   LATIN SMALL LETTER U WITH TILDE
            '(?\x016B "{\\=u}"   )  ; ū  LATIN SMALL LETTER U WITH MACRON
            '(?\x016D "{\\u u}"  )  ; ŭ   LATIN SMALL LETTER U WITH BREVE
            '(?\x016F "u"        )  ; ů ? LATIN SMALL LETTER U WITH RING ABOVE
            '(?\x0175 "{\\^w}"   )  ; ŵ   LATIN SMALL LETTER W WITH CIRCUMFLEX
            '(?\x0177 "{\\^y}"   )  ; ŷ   LATIN SMALL LETTER Y WITH CIRCUMFLEX
            '(?\x017A "{\\'z}"   )  ; ź   LATIN SMALL LETTER Z WITH ACUTE
            '(?\x1E81 "{\\`w}"   )  ; ẁ   LATIN SMALL LETTER W WITH GRAVE
            '(?\x1E83 "{\\'w}"   )  ; ẃ   LATIN SMALL LETTER W WITH ACUTE
            '(?\x1E85 "{\\\"w}"  )  ; ẅ   LATIN SMALL LETTER W WITH DIAERESIS
            '(?\x1EF3 "{\\`y}"   )  ; ỳ   LATIN SMALL LETTER Y WITH GRAVE
            ))


(defun pub-escape-tex (text) ""
  
  ; Skip nil entries that might occur at this place
  (if text
      (progn
        (setq text (replace-regexp-in-string "\\\\" "\\\\textbackslash" text))
        (setq text (replace-regexp-in-string "%" "\\\\%" text))
        (setq text (replace-regexp-in-string "\\$" "\\\\$" text))
        (setq text (replace-regexp-in-string "_" "\\\\_" text))
        (setq text (replace-regexp-in-string "&" "\\\\&" text))
        (setq text (replace-regexp-in-string "#" "\\\\#" text))
        (setq text (replace-regexp-in-string "}" "\\\\}" text))
        (setq text (replace-regexp-in-string "{" "\\\\{" text))
        (setq text (replace-regexp-in-string ">" "$>$" text))
        (setq text (replace-regexp-in-string "<" "$<$" text))
        (setq text (replace-regexp-in-string "\\~" "\\\\textasciitilde" text))
        (setq text (replace-regexp-in-string "\\^" "\\\\textasciicircum" text))
        )
    
    )
    text
  
  )

(defun pub-unicode-string-to-ascii (inputString) " "


  (let ((i 0)
        (char nil)
        (returnValue ""))

    (loop for i from 0 to (- (length inputString) 1) do 
          (progn
            (setq char (aref inputString i))
            (setq char (pub-unicode-char-to-ascii char))
            (setq returnValue (concat returnValue char))
            )
          )
    
    returnValue
    
    )
  
  )

(defun pub-unicode-string-to-tex (inputString) " "


  (let ((i 0)
        (char nil)
        (returnValue ""))

    (loop for i from 0 to (- (length inputString) 1) do 
          (progn
            (setq char (aref inputString i))
            (setq char (pub-unicode-char-to-tex char))
            (setq returnValue (concat returnValue char))
            )
          )
    
    returnValue
    
    )
  )


(defun pub-unicode-char-to-ascii (utf) " "

   (let ((unicode nil)
         (ascii nil)
         (ret nil))

     ; get unicode code for utf-encoded character
     (setq unicode (encode-char utf 'ucs)) 

     ; skip characters for which no unicode is found
     (if unicode 

         ; if normal ascii characters return this
         (if (< utf 256)
             (setq ret (char-to-string utf))
           ; else try to get ascii representation from our table
           (progn 
             (setq ascii (nth 1 (assoc unicode unicode-to-ascii-table)))
             (if ascii (setq ret ascii))
             )
           )
       )

     ; if no character found so far, give up
     (if (not ret)
         (setq ret "")
       )

     ret
     
     )
   )

(defun pub-unicode-char-to-tex (utf) " "

  (let ((unicode nil)
        (tex nil)
        (ret nil))

     ; get unicode code for utf-encoded character
    (setq unicode (encode-char utf 'ucs)) 

     ; skip characters for which no unicode is found
    (if unicode 

         ; if normal ascii characters return this
        (if (< utf 256)
            (setq ret (char-to-string utf))
           ; else try to get tex representation from our table
          (progn 
            (setq tex (nth 1 (assoc unicode unicode-to-tex-table)))
            (if tex (setq ret tex))
            )
          )
      )

     ; if no character found so far, give up
    (if (not ret)
        (setq ret "?")
      )
    ret
    )
  )



(provide 'pub-coding)