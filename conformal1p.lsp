;;;;   Graphical Helmert's transformation
;;;;   ----------------------------------
(defun C:conformal1p ( / lun lup aun aup ang ib1 ib2 x1 y1 h1 X2 Y2 H2
		             q ib xt1 yt1 ht1 Xt2 Yt2 Ht2 t1 t2 dx dy dh ir Xx Yy xr yr
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

  (setq ib1 (getpoint "\n 1точка Источника : "))
  (setq ib2 (getpoint ib1 "\n 1точка Цели : "))
  (setq x1 (list (car ib1)))
  (setq y1 (list (cadr ib1)))
  (setq h1 (list (caddr ib1)))
  (setq X2 (list (car ib2)))
  (setq Y2 (list (cadr ib2)))
  (setq H2 (list (caddr ib2)))
  (grdraw ib1 ib2 5)

  (setq ib1 (getpoint "\n 2точка Источника : "))
  (setq ib2 (getpoint ib1 "\n 2точка Цели : "))
  (setq x1 (append x1 (list (car ib1))))
  (setq y1 (append y1 (list (cadr ib1))))
  (setq h1 (append h1 (list (caddr ib1))))
  (setq X2 (append X2 (list (car ib2))))
  (setq Y2 (append Y2 (list (cadr ib2))))
  (setq H2 (append H2 (list (caddr ib2))))
  (grdraw ib1 ib2 5)
  
  (setq q "Y"
	ib 2)
  (while (/= q "N")
    (initget "Y N")
    (setq q (getkword "\n Следующая точка? Yes/No <Y> : "))
    (if (or (= q "y") (= q "Y") (= q nil))
      (progn
        (setq ib1 (getpoint "\n Следующая точка Источника : "))
        (setq ib2 (getpoint ib1 "\n Следующая точка Цели : "))
        (setq x1 (append x1 (list (car ib1))))
        (setq y1 (append y1 (list (cadr ib1))))
        (setq h1 (append h1 (list (caddr ib1))))
        (setq X2 (append X2 (list (car ib2))))
        (setq Y2 (append Y2 (list (cadr ib2))))
        (setq H2 (append H2 (list (caddr ib2))))
	(setq ib (+ 1 ib))
        (grdraw ib1 ib2 5)
      )
    )
  )

  (setq xt1 0.0
	yt1 0.0
	ht1 0.0
	Xt2 0.0
	Yt2 0.0
	Ht2 0.0)
  (foreach p x1 (setq xt1 (+ xt1 p)))
  (foreach p y1 (setq yt1 (+ yt1 p)))
  (foreach p h1 (setq ht1 (+ ht1 p)))
  (foreach p X2 (setq Xt2 (+ Xt2 p)))
  (foreach p Y2 (setq Yt2 (+ Yt2 p)))
  (foreach p H2 (setq Ht2 (+ Ht2 p)))
  (setq xt1 (/ xt1 ib))
  (setq yt1 (/ yt1 ib))
  (setq ht1 (/ ht1 ib))
  (setq Xt2 (/ Xt2 ib))
  (setq Yt2 (/ Yt2 ib))
  (setq Ht2 (/ Ht2 ib))
  (setq t1 (list xt1 yt1 ht1))
  (setq t2 (list Xt2 Yt2 Ht2))
  (setq dx (- (nth 0 t2) (nth 0 t1)))
  (setq dy (- (nth 1 t2) (nth 1 t1)))
  (setq dh (- Ht2 ht1))
  
  (grdraw t1 t2 6)

  (setq ir 0)
  (setq Xx nil
	Yy nil
	xr nil
	yr nil)
  (while (< ir ib)
    (setq Xx (append Xx (list (- (nth ir X2) (nth ir x1)))))
    (setq Yy (append Yy (list (- (nth ir Y2) (nth ir y1)))))
    (setq xr (append xr (list (- (nth ir x1) xt1))))
    (setq yr (append yr (list (- (nth ir y1) yt1))))
    (setq ir (+ 1 ir))
  )
  
  (setq ir 0)
  (setq a1 nil
	a2 nil
	b1 nil
	b2 nil
	c1 nil
	c2 nil)
  (while (< ir ib)
    (setq a1 (append a1 (list (* (nth ir Xx) (nth ir xr)))))
    (setq a2 (append a2 (list (* (nth ir Yy) (nth ir yr)))))
    (setq b1 (append b1 (list (* (nth ir Yy) (nth ir xr)))))
    (setq b2 (append b2 (list (* (nth ir Xx) (nth ir yr)))))
    (setq c1 (append c1 (list (* (nth ir xr) (nth ir xr)))))
    (setq c2 (append c2 (list (* (nth ir yr) (nth ir yr)))))    
    (setq ir (+ 1 ir))
  )

  (setq ir 0)
  (setq sa1 0.0
	sa2 0.0
	sb1 0.0
	sb2 0.0
	sc1 0.0
	sc2 0.0)
  (while (< ir ib)
    (setq sa1 (+ sa1 (nth ir a1)))
    (setq sa2 (+ sa2 (nth ir a2)))
    (setq sb1 (+ sb1 (nth ir b1)))
    (setq sb2 (+ sb2 (nth ir b2)))
    (setq sc1 (+ sc1 (nth ir c1)))
    (setq sc2 (+ sc2 (nth ir c2)))
    (setq ir (+ 1 ir))    
  )
 
  (setq at (/ (+ sa1 sa2) (+ sc1 sc2)))
  (setq at (+ 1 at))
  (setq bt (/ (- sb1 sb2) (+ sc1 sc2)))
  (setq qt (sqrt (+ (* at at) (* bt bt))))
  (setq wt (atan bt at))
  (setq wtr (* (1- 0) (/ (* 200.0 wt) pi)))
  (setq wtd (* (1- 0) (/ (* 180.0 wt) pi)))  

  (setq q "Y")
  (initget "Y N")
  (setq q (getkword "\n Применить масштабирование? Yes/No_(q=1)/User  <Y> : "))
  (if (or (= q "y") (= q "Y") (= q nil))
    (setq scl qt)
  )
  (if (or (= q "n") (= q "N"))
    (setq scl 1.0)
  )
  (if (or (= q "u") (= q "U"))
    (setq scl (getreal "User's scale : "))
  )
  (setq et (ssget))
  (setq q "N")
  (initget "N Y")
  (setq q (getkword "\n Сохранить оригинал? No/Yes <N> : "))
  (if (or (= q "y") (= q "Y"))
    (command "_copy" et "" "0,0,0" "0,0,0") 
  )
  (command "_move" et "" "_none" t1 "_none" t2)
  (command "_rotate" et "" "_none" t2 wtr)
  (command "_scale" et "" "_none" t2 scl)

  (princ
    (strcat "\n  dY : " (rtos dx 2 3) "    dX : " (rtos dy 2 3)
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
