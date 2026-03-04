(defun C:RSHX (/ fname ext)
  (vl-load-com)
;;;Загрузить лисп и набрать в командной строке RSHX
;;;Remove missing SHX
;;;Отключения запроса на выбор несуществующего файла форм  
;;;Эдуард Смолянка shx
;;;http://www.caduser.ru/forum/index.php?PAGE_NAME=message&FID=30&TID=16401&PAGEN_1=2
;;;http://www.caduser.ru/forum/index.php?PAGE_NAME=message&FID=4&TID=40408&MID=227420#message227420
;;;http://forum.dwg.ru/showthread.php?t=30595
;;; gile http://www.theswamp.org/index.php?topic=28096.0
  (vlax-for item
                 (vla-get-textstyles
                   (vla-get-activedocument (vlax-get-acad-object))
                 ) ;_ end of vla-get-textstyles
     (setq fname (vla-get-fontfile item))
     (setq fname (vl-string-trim "\" \t\n" fname))
    (if (not
          (vl-filename-extension fname) ;_ end of vl-filename-extension
        ) ;_ end of not
      (setq fname (strcat fname ".shx"))
    ) ;_ end of if
    (if (and
          (setq ext (vl-filename-extension fname))
          (= (strcase ext) ".SHX")
          (= 1
           (logand (cdr (assoc 70 (entget(vlax-vla-object->ename item)))) 1)
          ) ;_ end of =
          (not (findfile fname))
        ) ;_ end of and
      (progn
        (vla-put-fontfile item "ltypeshp.shx")
        (princ "\nChange ")
        (princ fname)
        (princ " on ltypeshp.shx")
      ) ;_ end of princ
    ) ;_ end of if
  ) ;_ end of vlax-for
  (princ)
)
(princ "\nType RSHX in command line to remove missing shape referens")