;;-------------------=={ UnFormat String }==------------------;;
;;                                                            ;;
;;  Returns a string with all MText formatting codes removed. ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  str - String to Process                                   ;;
;;  mtx - MText Flag (T if string is for use in MText)        ;;
;;------------------------------------------------------------;;
;;  Returns:  String with formatting codes removed            ;;
;;------------------------------------------------------------;;

(defun LM:UnFormat ( string / _replace rx )

    (defun _replace ( new old str )
        (vlax-put-property rx 'pattern old)
        (vlax-invoke rx 'replace str new)
    )
  
    (if (setq rx (vlax-get-or-create-object "VBScript.RegExp"))
        (progn
            (setq string
                (vl-catch-all-apply
                    (function
                        (lambda ( )
                            (vlax-put-property rx 'global     actrue)
                            (vlax-put-property rx 'multiline  actrue)
                            (vlax-put-property rx 'ignorecase acfalse) 
                          
                          
                            (foreach pair
                               '(
                                    ;; NEW        ;; OLD
                                    ("\032"    . "\\\\\\\\") ;; Remove to an "SUBSTITUTE" character
                                    (" "       . "\\\\P|\\n|\\t") ;; New Line or Tab to an espace (and paragraph too?)
                                    ("$1"      . "\\\\(\\\\[ACcFfHLlOopQTW])|\\\\[ACcFfHLlOopQTW][^\\\\;]*;|\\\\[ACcFfHLlOopQTW]")
                                    ("$1$2/$3" . "([^\\\\])\\\\S([^;]*)[/#\\^]([^;]*);")
                                    ("$1$2"    . "\\\\(\\\\S)|[\\\\](})|}")
                                    ("$1"      . "[\\\\]({)|{")
                                )
                                (setq string (_replace (car pair) (cdr pair) string))
                            )
                        )
                    )
                )
            )
          
            (vlax-release-object rx)
            (if (null (vl-catch-all-error-p string))
                string
            )
        )
    )
)

(vl-load-com)

(defun c:osm:tCount ( / *error* texts materials i currentENt currentEntText longestString)
  (defun *error* (msg)
    (or 
      (wcmatch (strcase msg t) "*break,*cancel*,*exit*") 
      (alert (strcat "ERROR: " msg "**"))
    )
  )

  (prompt "Select the disered texts or mtexts: ")
  (setq texts (ssget '((0 . "TEXT,MTEXT"))))
  
  (setq materials (list))
  ;; Usamos o conceito de "HASHMAP" aqui. Embora, não sei se a performance é a mesma
  
  ;; SPEED: O(n) 
  ;; Memory: O(n)
  
  (setq longestString 0)
  
  (repeat (setq i (sslength texts))
    (setq currentEnt (vlax-ename->vla-object (ssname texts (setq i (1- i)))))
    
    (if (> (strlen (vla-get-textstring currentEnt)) longestString) (setq longestString (strlen (vla-get-textstring currentEnt))))
    
    (if (= (vla-get-objectname currentEnt) "AcDbMText")
      (setq currentEntText (LM:UnFormat (vla-get-textstring currentEnt)))
      (setq currentEntText (vla-get-textstring currentEnt))
    )
    
    ;; Cria um tipo de dicionario
    (if (assoc currentEntText materials)
      ;; Atualiza o numero em textos iguais
      (setq materials
        (subst 
          ;; Valor novo          
          (cons currentEntText (+ (cdr (assoc currentEntText materials)) 1))
          
          ;; Valor antigo
          (assoc currentEntText materials)
          
          ;; Lista
          materials
        )
      )
      
      ;; Se não existir, adicionar no dicionario.
      (setq materials (append materials (list (cons currentEntText 1))))
    )
  )

  (setq longestString (+ 10 longestString))
  
  (repeat (fix (/ (- longestString 8) 2)) (princ "="))
  (princ " RESULT ")
  (repeat (fix (/ (- longestString 8) 2)) (princ "="))
  (prompt "TEXT")
  (repeat (- longestString 9) (princ "."))
  (princ "TOTAL")
  (terpri)
  
  (repeat longestString (princ "="))
  (terpri)
  
  (mapcar 
    (function
      (lambda (name)
        (prompt (car name))
        (repeat (- longestString (+ (strlen (car name)) (strlen (itoa (cdr name))))) (princ "."))
        (princ (itoa (cdr name)))
      )
    )  
      materials
  )
  
  (terpri)
  (repeat longestString (princ "="))
  
  (alert "Check the result on your prompt area! Press <F2> for a better view.")
  
  (princ)
)

(alert "Lisp carregada com sucesso! Digite \"TCount\" para comecar.")