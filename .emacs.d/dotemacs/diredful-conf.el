
("excel"
 "makefile_ext"
 "text"
 "exec"
 "header"
 "source"
 "pdf"
 "word")

(
  ("excel" (:inherit (font-lock-function-name-face)) "xlsx?" nil nil nil nil)
  ("makefile_ext" (:inherit (match)) "mk" nil nil nil nil)
  ("text" (:inherit (font-lock-preprocessor-face)) "txt" nil nil nil nil)
  ("exec" (:inherit (font-lock-type-face)) "exe dll bat sh rtp" nil nil nil nil)
  ("header" (:inherit (compilation-column-number)) "h hpp hh" nil nil nil nil)
  ("source" (:inherit (font-lock-variable-name-face)) "c cpp pl py el ahk" nil nil nil nil)
  ("pdf" (:inherit (font-lock-comment-face)) "pdf" nil nil nil nil)
  ("word" (:inherit (font-lock-constant-face)) "docx? dotx?" nil nil nil nil)
  )
