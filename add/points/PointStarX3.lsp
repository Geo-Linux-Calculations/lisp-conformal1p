(defun get-objects-by-mode (mode / ss)
  (cond
    ((eq mode "POINT") (ssget '((0 . "POINT"))))
    ((member mode '("CIRCLE" "WEIGHTED"))
     (ssget '((0 . "CIRCLE")))
    )
    ((eq mode "LOGGING") (ssget '((0 . "POINT"))))
    (T nil)
  )
)

(defun extract-coords-and-weights
       (ss mode / entList ptList weightList obj pt rad area w)
  (setq	ptList '()
	weightList
	 '()
  )
  (repeat (setq i (sslength ss))
    (setq i (1- i))
    (setq obj (vlax-ename->vla-object (ssname ss i)))
    (cond
      ((eq mode "POINT")
       (setq pt (vlax-get obj 'Coordinates))
       (setq ptList (cons pt ptList))
      )
      ((eq mode "CIRCLE")
       (setq pt (vlax-get obj 'Center))
       (setq ptList (cons pt ptList))
      )
      ((eq mode "WEIGHTED")
       (setq pt	  (vlax-get obj 'Center)
	     rad  (vlax-get obj 'Radius)
	     area (* pi rad rad)
	     w	  (/ 1.0 area)
       )
       (setq ptList (cons pt ptList))
       (setq weightList (cons w weightList))
      )
    )
  )
  (list ptList weightList)
)

(defun compute-midpoint	(ptList weightList mode / sum cnt wsum midpt)
  (cond
    ((eq mode "WEIGHTED")
     (setq sum	'(0 0 0)
	   wsum	0
     )
     (foreach pair (mapcar 'list ptList weightList)
       (setq sum
	      (mapcar '+
		      sum
		      (mapcar '(lambda (x) (* (cadr pair) x)) (car pair))
	      )
       )
       (setq wsum (+ wsum (cadr pair)))
     )
     (setq midpt (mapcar '/ sum (list wsum wsum wsum)))
    )
    (T
     (setq cnt	 (length ptList)
	   sum	 (apply 'mapcar (cons '+ ptList))
	   midpt (mapcar '/ sum (list cnt cnt cnt))
     )
    )
  )
  midpt
)

(defun draw-3dpolyline (ptList midpt)
  (setq allPts (cons midpt ptList))
  (entmakex
    (list
      '(0 . "POLYLINE")	'(100 . "AcDb3dPolyline") '(66 . 1) '(10 0.0 0.0 0.0)
      '(70 . 8))
  )

  (foreach pt allPts
    (entmakex (list '(0 . "VERTEX")
		    '(100 . "AcDb3dPolylineVertex")
		    (cons 10 pt)
		    '(70 . 32)
	      )
    )
    (entmakex (list '(0 . "VERTEX")
		    '(100 . "AcDb3dPolylineVertex")
		    (cons 10 midpt)
		    '(70 . 32)
	      )
    )
  )
  (entmakex '((0 . "SEQEND")))
)

(defun draw-weight-circle (midpt weightList / wm sm)
  (setq wm (apply '+ weightList))
  (setq sm (/ 1.0 wm))
  (setq rm (sqrt (/ sm pi)))		; радиус окружности
  (entmakex
    (list
      '(0 . "CIRCLE")
      (cons 10 midpt)
      (cons 40 rm)
      '(62 . 3)				; зелёный цвет
    )
  )
)

(defun c:PointStarX3 (/ mode ss ptList weightList midpt)
  (initget "POINT CIRCLE WEIGHTED LOGGING")
  (setq
    mode (getkword "\nSelect mode [POINT/CIRCLE/WEIGHTED]: ")
  )
	(setq data nil
		ptList nil
		weightList nil
		midpt nil
	)
  (setq ss (get-objects-by-mode mode))
  (if (not ss)
    (princ "\nNo valid objects selected.")
    (progn
      (setq data       (extract-coords-and-weights ss mode)
	    ptList     (car data)
	    weightList (cadr data)
	    midpt      (compute-midpoint ptList weightList mode)
      )
      (entmakex (list '(0 . "POINT") (cons 10 midpt)))
      (if (eq mode "WEIGHTED")
	(draw-weight-circle midpt weightList)
      )
      (draw-3dpolyline ptList midpt)
      (princ (strcat "\nCreated 3D polyline with "
		     (itoa (length ptList))
		     " points from midpoint."
	     )
      )
    )
  )
  (princ)
)
