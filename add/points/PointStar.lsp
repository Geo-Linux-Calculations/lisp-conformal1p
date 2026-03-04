(defun c:PointStar (/ ss ptList cnt sum midpt)
  (prompt "\nSelect POINT objects to connect: ")
  (setq ss (ssget '((0 . "POINT"))))
  
  (cond
    ((not ss) (princ "\nNo points selected."))
    ((< (sslength ss) 2) (princ "\nSelect at least 2 points."))
    (T
      (setq ptList 
        (vl-remove-if-not
          '(lambda (x) (eq (car x) 10))
          (apply 'append 
            (mapcar 'entget 
              (vl-remove-if 'listp 
                (mapcar 'cadr (ssnamex ss))
              )
            )
          )
        )
      )
      
      (setq ptList (mapcar 'cdr ptList)
            cnt (length ptList)
            sum (apply 'mapcar (cons '+ ptList))
            midpt (mapcar '/ sum (list cnt cnt cnt)))
      
      (entmakex (list '(0 . "POINT") (cons 10 midpt)))
      
      (foreach pt ptList
        (entmakex
          (list
            '(0 . "POLYLINE")
            '(100 . "AcDb3dPolyline")
            '(66 . 1)
            '(10 0.0 0.0 0.0)
            '(70 . 8)
          )
        )
        (entmakex (list '(0 . "VERTEX") '(100 . "AcDb3dPolylineVertex") (cons 10 pt) '(70 . 32)))
        (entmakex (list '(0 . "VERTEX") '(100 . "AcDb3dPolylineVertex") (cons 10 midpt) '(70 . 32)))
        (entmakex '((0 . "SEQEND")))
      )
      (princ (strcat "\nCreated " (itoa cnt) " connections to midpoint."))
    )
  )
  (princ)
)

