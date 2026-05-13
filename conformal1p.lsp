;;;;   Graphical conformal 1power (Helmert's) transformation
;;;;   ----------------------------------
(defun C:conformal1p ( / lun lup aun aup ang pst ptt ps pt
                     q ib pms pmt pmd ir dts dsm
                     Xt Yt xs ys sXtxs sYtys sYtxs sXtys sxsxs sysys
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
      (setq pst (getpoint (strcat "\n" (rtos (+ 1 ib) 2 0) " point source : ")))
      (if pst
        (progn
          (setq ptt (getpoint pst (strcat "\n" (rtos (+ 1 ib) 2 0) " point target : ")))
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
    sXtxs 0.0
    sYtys 0.0
    sYtxs 0.0
    sXtys 0.0
    sxsxs 0.0
    sysys 0.0
  )
  (while (< ir ib)
    (setq
      Xt (car (nth ir dts))
      Yt (cadr (nth ir dts))
      xs (car (nth ir dsm))
      ys (cadr (nth ir dsm))
      sXtxs (+ sXtxs (* Xt xs))
      sYtys (+ sYtys (* Yt ys))
      sYtxs (+ sYtxs (* Yt xs))
      sXtys (+ sXtys (* Xt ys))
      sxsxs (+ sxsxs (* xs xs))
      sysys (+ sysys (* ys ys))
      ir (+ 1 ir)
    )
  )

  (setq
    at (+ 1 (/ (+ sXtxs sYtys) (+ sxsxs sysys)))
    bt (/ (- sYtxs sXtys) (+ sxsxs sysys))
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
  (setq q (getkword (strcat "\n Scale? Yes_(q=" (rtos scl 2 4) ")/No_(q=1)/User  <Y> : ")))
  (if (or (= q "n") (= q "N"))
    (setq scl 1.0)
  )
  (if (or (= q "u") (= q "U"))
    (setq scl (getreal "User's scale : "))
  )

  (setq q "N")
  (initget "N Y")
  (setq q (getkword "\n Save origin? No/Yes <N> : "))
  (if (or (= q "y") (= q "Y"))
    (command "_copy" et "" "0,0,0" "0,0,0") 
  )
  (command "_move" et "" "_none" pms "_none" pmt)
  (command "_rotate" et "" "_none" pmt wtr)
  (command "_scale" et "" "_none" pmt scl)

  (princ
    (strcat
      "\n  dY : " (rtos (car pmd) 2 3) "    dX : " (rtos (cadr pmd) 2 3) "    dH : " (rtos (caddr pmd) 2 3)
      "\n  w  : " (rtos wtr 2 4) "g    " (rtos wtd 2 5) "°    q : " (rtos scl 2 8)
    )
  )  

  (setvar "LUNITS" lun)
  (setvar "LUPREC" lup)
  (setvar "AUNITS" aun)
  (setvar "AUPREC" aup)
  (setvar "ANGDIR" ang)

)
;;;;   ----------------------------------
