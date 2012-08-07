;;; moneylog.el --- Provide a way to enter moneylog logs

;; Copyright (C) 2009-2012  Leslie Harlley Watter

;; Author: Leslie Harlley Watter <leslie@watter.org>
;; Keywords: moneylog, helper

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file should be used with moneylog (http://www.aurelio.net/moneylog)

;;; Code:



(defun i-register (&optional tag)
  "Inserts/format an account record. It should be used with moneylog
   2009.08.15	-29.00	visa	| Cicles Radar - Mesa para a bicicleta"
  (interactive)
;; use or not org function to prefill the date
  (set 'use-org "T")
;; pre-fills the today variable with today's date
  (if (string= use-org "T")
	(setq today (org-read-date)) ;; use org 
	(setq today (format-time-string "%Y-%m-%d"))  ;; do not use org
	)
  (setq rdata (read-string "Data (2009.06.15): " today ))
  (unless (equal rdata "")
  (setq rvalor (read-string "Valor (110.00 ou -110.00): " "-"))
  (unless (equal rvalor "-")
  (setq rtags (read-string "Tags (poupanca,visa,ccred): " tag))
  (setq rcomentario (read-string "Comentario: "))
  (unless (equal rcomentario "")
	(setq atual (point))
	(goto-char (point-max))
	(if (equal 
		 (search-backward (format "%s" rdata) (point-min) t) nil) 
		(goto-char atual)
	  )
	(beginning-of-line)
	(forward-line 1)
    (insert (format "%s\t%s\t%s| %s\n" rdata rvalor rtags rcomentario )) ;
))))


(defun i-reg-gas ()
  "Inserts/format a gas account record. It should be used with moneylog"
  (interactive)
;; prefills month variable to be concatenated to default tags
  (setq tdia (string-to-number (format-time-string "%d")))
;;  (i-prefill-date "posto, moto, ")
;; here starts the real gas function
  (setq tiporeg (read-string "tipo Registro ([C]|I) (Credito/Imediato): "))
;; set the default value if user simply press enter
  (if (equal tiporeg "") 
	  (setq tiporeg "C")
  )
  (if (equal tiporeg "C") 
	  (i-reg-ccredit "posto, moto")
	(i-register (concat "posto, moto"))
	)
  (setq odomtotal (read-string "Odometro Total: "))
  (unless (equal odomtotal "")
  (setq odomparc (read-string "Odometro Parcial: "))
  (unless (equal odomparc "")
  (setq litabast (read-string "Litros Abastecidos: "))
  (unless (equal litabast "")
  (setq precolitro (read-string "Preço por Litro: "))
  (unless (equal precolitro "")
  (setq kmporlitro (number-to-string (/ (string-to-number odomparc)(string-to-number litabast))))
  (setq tipogas (read-string "Tipo ( 1 aditivada / [comum] ): "))
  (if (equal tipogas "") 
	  (setq tipogas "comum")
	(setq tipogas "aditivada")
	)
	(backward-char 1)
    (insert (format " otot}%s} opar(%s) l{%s{ R/l$%s$ km/l _%s_ %s \n"  odomtotal odomparc litabast  precolitro (substring kmporlitro 0 6) tipogas )) ;
)))))


(defun i-atualiza-dados-dev ()
  "Atualiza os dados no arquivo moneylog.txt"
  (interactive)
  (goto-char (point-min))
  (search-forward "## começo")
   ; grab the current line
  (beginning-of-line)
  (setq myStartPos (line-beginning-position))

  (goto-char (point-min))
  (search-forward "## fim")
  ; grab the current line
  (end-of-line)
  (setq myEndPos (line-end-position))
  ; get the strings between start and end point
  (setq myStr (buffer-substring myStartPos myEndPos))
  (pop-to-buffer (find-file-noselect "./dev/moneylog.txt"))

  (goto-char (point-min))
  (search-forward "## começo")
   ; grab the current line
  (beginning-of-line)
  (setq myStartPos2 (line-beginning-position))

  (goto-char (point-min))
  (search-forward "## fim")
  ; grab the current line
  (end-of-line)
  (setq myEndPos2 (line-end-position))
  ;; delete the region
  (delete-region myStartPos2 myEndPos2)
  (goto-char myStartPos2)
  (insert myStr)
  (basic-save-buffer)
  (message "Arquivo Atualizado" )
)
 

(defun i-ordena-ccredit ()
"Ordena os registros de data do cartão de crédito."
(interactive)
;; discover point-min and point-max from the selected region, or use the next word
(let (pos1 pos2 bds)
  (if (and transient-mark-mode
           mark-active)
      (setq start (region-beginning) end (region-end))
    (progn
      (setq bds (bounds-of-thing-at-point 'symbol))
      (setq start (car bds) end (cdr bds))
	  ))
;; idiom for string replacement in region
;; restrict the atuation of the next commands to the selected region
(with-output-to-temp-buffer "*registros*"
  (save-restriction
	(narrow-to-region start end)
	(goto-char (point-min))
	(while (< (point) (point-max))
;; reads the buffer line
	  (setq linha (buffer-substring (line-beginning-position) (line-end-position)))
;; matches the date field 
	  (string-match "\\(@[0-9][0-9].[0-9][0-9]\\)" linha)
;; puts the date matched in the data variable
	  (setq data-compra (match-string 0 linha))
;; prints the line with the buy date's first
	  (princ (format "%s %s\n" data-compra linha))
	  (forward-line 1)
	  )
	;; repeat for other string pairs
	)
  )
;; creates a new buffer called *registros to manipulate the strings
(let ((oldbuf (current-buffer)))
  (set-buffer "*registros*")
;; let we modify the buffer
  (setq buffer-read-only nil)
;; as the buffer already has the lines in the right format, we sort them all
;; @12.21 2011-01-17	-36.00	ccred11_01, presente| @12.21 Camisetas 
  (sort-lines nil (point-min) (point-max))
;; we need to define the region that will be killed (the @mm.dd part at the front of the line)
;; that was inserted before
  (setq minr (point-min))
  (goto-char (point-max))
  (forward-line -1)
  (re-search-forward "\\(@[0-9][0-9].[0-9][0-9] \\)")
  (setq maxr (point))
;; kill the rectangle
  (kill-rectangle minr maxr)
;; now we kill the whole region to put it into the kill ring
  (kill-region (point-min) (point-max))
;; kill the temporary buffer
  (kill-buffer (current-buffer)) 
  (set-buffer oldbuf))
;; we'll kill the old region to put the new sorted lines
(kill-region (region-beginning) (region-end))
(goto-char (region-end))
(forward-line)
(insert (format "# ordenado\n"))
;; as we have killed the selected region, the ordered lines are in the 2nd position of the kill ring
(yank 2)
  )
)

;; recebe data da compra 
;; calcula data de vencimento 
;; acerta a tag de ccred/mes e inclui a data de compra automaticamente no comentário

(defun i-reg-ccredit (&optional tag)
;;(defun icc (&optional tag)
  "Inserts/format an account record. It should be used with moneylog
   2009.08.15	-29.00	visa	| Cicles Radar - Mesa para a bicicleta"
  (interactive)
;; pre-fills the today variable with today's date
;; fechamento is the date where the credit card closes it's bills
  (setq bdate (format-time-string "%Y-%m-%d"))
  (setq rdatacompra (read-string "Data compra (yyyy.mm.dd): " bdate))
  (unless (equal rdatacompra "")
	(setq diacompra (substring rdatacompra -2 nil))
	(setq mescompra (substring rdatacompra -5 -3))
	(setq anocompra (substring rdatacompra 0 4))
;;;;	(insert (format "Data Compra dia mes ano | %s\t%s\t%s\t%s\n" rdatacompra diacompra mescompra anocompra ))
	)
  (setq fechamento 3)
  (setq diaspgtoaposfechamento 14)
  (setq diacobranca (+  fechamento diaspgtoaposfechamento))
  (setq anocobranca (string-to-number anocompra))

;; lógica de cálculo da data da fatura
;; compra dia 21 jan 
;; compra dia 1 fev 
;; ambos pagam em fevereiro
;; compra dia 4 de fevereiro
;; compra dia 2 de março 
;; pagam em março
;;
;; se dia_compra > dia-fechamento
;; então 
;;    se mes < 12 
;;     então mes_cobrança = mes_compra + 1
;;    se mes = 12
;;     então  
;;	      mes_cobrança = 01
;;		  ano_cobrança = ano_compra +1 
;; senão
;;     mes_cobrança = mes
;; fim 
;;  dia_cobranca = dia_fechamento + diaspgtoaposfechamento

  (if (> (string-to-number diacompra) fechamento) 
	  (progn 
		(if (< (string-to-number mescompra) 12 )
			(setq mescobranca (+ (string-to-number mescompra) 1))
		;; mescompra = 12
		  (progn 
			(setq mescobranca 01)
			(setq anocobranca (+ (string-to-number anocompra) 1))
			)
		  )
		)
	;; mes_cobrança é o mes atual
	(setq mescobranca (string-to-number mescompra))
	)
;; AAAA-MM-DD (cobrança) valor tags | @(data_compra) comentário
  (setq pretags (concat (concat (concat (concat (concat "ccred" (substring (format "%s" anocobranca) 2 4) ) "_")  (format "%02d" mescobranca)) ", ") tag ))

  (setq rvalor (read-string "Valor (110.00 ou -110.00): " "-"))
  (unless (equal rvalor "-")
	(setq tags (read-string "Tags (ccred): " pretags))
	(setq rcomentario (read-string "Comentario: "))
	(unless (equal rcomentario "")
;; lógica da busca:
;; marca o ponto atual
;; vai ao final do arquivo
;; pesquisa na volta pela data de registro 
;; se encontrar, vai para esse ponto (ultimo registro com essa data)
;; senão insere no ponto atual na próxima linha

	  (setq atual (point))
	  (goto-char (point-max))
	  (if (equal 
		   (search-backward (format "%04d-%02d-%02d" anocobranca mescobranca diacobranca) (point-min) t) nil) 
		  (goto-char atual)
		)
	  (beginning-of-line)
	  (forward-line 1)
	  
	  (insert (format "%04d-%02d-%02d\t%s\t%s| @%s.%s %s\n" anocobranca mescobranca diacobranca rvalor tags mescompra diacompra rcomentario )) ;
	  )
	)
) ;; fim icc	

(provide 'moneylog)
;;; moneylog.el ends here
