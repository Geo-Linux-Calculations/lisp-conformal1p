;;;;   Graphical Helmert's transformation
;;;;   ----------------------------------
(defun C:conformal1p ( / lun lup aun aup ang pst ptt ps pt
                     q ib pms pmt pmd ir dts dsm
                     a1 a2 b1 2b c1 c2 sa1 sa2 sb1 sb2 sc1 sc2
                     at bt qt wt wtr wtd scl et)

  (setq lun (getvar "LUNITS"))
  (setq lup (getvar "LUPREC"))
  (setq aun (getvar "AUNITS"))
  (setq aup (getvar "AUPREC"))
  (setq ang (getvar "ANGDIR"))
 
  (setvar "LUNITS" 2)
  (setvar "LUPREC" 3)
  (setvar "AUNITS" 2)
  (setvar "AUPREC" 4)
  (setvar "ANGDIR" 1)

  (setq
    ib 0
    q "Y"
  )
  (while (= q "Y")
    (progn
      (princ "\n")
      (princ (+ 1 ib))
      (setq pst (getpoint " точка источника : "))
      (if pst
        (progn
          (princ "\n")
          (princ (+ 1 ib))
          (setq ptt (getpoint pst " точка цели : "))
        )
      )
      (setq q "N")
      (if (and pst ptt)
        (progn
          (setq
            ps (append ps (list pst))
            pt (append pt (list ptt))
            ib (+ 1 ib)
            q "Y"
          )
          (grdraw pst ptt 5)
        )
      )
      (if (< ib 2) (setq q "Y"))
    )
  )

  (setq
    pms (list 0.0 0.0 0.0)
    pmt (list 0.0 0.0 0.0)
  )
  (foreach p ps (setq pms (mapcar '+ pms p)))
  (foreach p pt (setq pmt (mapcar '+ pmt p)))
  (setq
    pms (mapcar '/ pms (list ib ib ib))
    pmt (mapcar '/ pmt (list ib ib ib))
  )
  (setq
    pmd (mapcar '- pmt pms)
  )
  
  (grdraw pms pmt 6)

  (setq
    ir 0
    dts nil
    dsm nil
  )
  (while (< ir ib)
    (setq
      dts (append dts (list (mapcar '- (nth ir pt) (nth ir ps))))
      dsm (append dsm (list (mapcar '- (nth ir ps) pms)))
      ir (+ 1 ir)
    )
  )
  
  (setq
    ir 0
    a1 nil
    a2 nil
    b1 nil
    b2 nil
    c1 nil
    c2 nil
  )
  (while (< ir ib)
    (setq
      a1 (append a1 (list (* (car (nth ir dts)) (car (nth ir dsm)))))
      a2 (append a2 (list (* (cadr (nth ir dts)) (cadr (nth ir dsm)))))
      b1 (append b1 (list (* (cadr (nth ir dts)) (car (nth ir dsm)))))
      b2 (append b2 (list (* (car (nth ir dts)) (cadr (nth ir dsm)))))
      c1 (append c1 (list (* (car (nth ir dsm)) (car (nth ir dsm)))))
      c2 (append c2 (list (* (cadr (nth ir dsm)) (cadr (nth ir dsm)))))
      ir (+ 1 ir)
    )
  )

  (setq
    ir 0
    sa1 0.0
    sa2 0.0
    sb1 0.0
    sb2 0.0
    sc1 0.0
    sc2 0.0
  )
  (while (< ir ib)
    (setq
      sa1 (+ sa1 (nth ir a1))
      sa2 (+ sa2 (nth ir a2))
      sb1 (+ sb1 (nth ir b1))
      sb2 (+ sb2 (nth ir b2))
      sc1 (+ sc1 (nth ir c1))
      sc2 (+ sc2 (nth ir c2))
      ir (+ 1 ir)
    )
  )
 
  (setq
    at (+ 1 (/ (+ sa1 sa2) (+ sc1 sc2)))
    bt (/ (- sb1 sb2) (+ sc1 sc2))
    qt (sqrt (+ (* at at) (* bt bt)))
    wt (atan bt at)
    wtr (* (1- 0) (/ (* 200.0 wt) pi))
    wtd (* (1- 0) (/ (* 180.0 wt) pi))
  )

  (setq et (ssget))

  (setq
    scl qt
    q "Y"
  )
  (initget "Y N U")
  (setq q (getkword "\n Применить масштабирование? Yes/No_(q=1)/User  <Y> : "))
  (if (or (= q "n") (= q "N"))
    (setq scl 1.0)
  )
  (if (or (= q "u") (= q "U"))
    (setq scl (getreal "User's scale : "))
  )

  (setq q "N")
  (initget "N Y")
  (setq q (getkword "\n Сохранить оригинал? No/Yes <N> : "))
  (if (or (= q "y") (= q "Y"))
    (command "_copy" et "" "0,0,0" "0,0,0") 
  )
  (command "_move" et "" "_none" pms "_none" pmt)
  (command "_rotate" et "" "_none" pmt wtr)
  (command "_scale" et "" "_none" pmt scl)

  (princ
    (strcat "\n  dY : " (rtos (car pmd) 2 3) "    dX : " (rtos (cadr pmd) 2 3)
     "\n  w  : " (rtos wtr 2 4) "g    " (rtos wtd 2 5) "°    q : " (rtos scl 2 8)
    )
  )  

  (setvar "LUNITS" lun)
  (setvar "LUPREC" lup)
  (setvar "AUNITS" aun)
  (setvar "AUPREC" aup)
  (setvar "ANGDIR" ang)

)
;======Konec
