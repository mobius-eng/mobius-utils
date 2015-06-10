(defun aget (alist key)
  (if-let (p (assoc key alist))
    (values (cdr p) T)
    (values nil nil)))

(defun (setf aget) (new-value alist key)
  (if-let (p (assoc key alist))
    (setf (cdr p) new-value)
    (progn
      (setf alist (cons (cons key new-value) alist))
      new-value)))

(defun mapalist (fun alist &rest more-alists)
  (loop for (k . val) in alist
     collect
       (cons k (apply fun
                      k
                      (cons val
                            (mapcar #'(lambda (alst)
                                        (aget alst k))
                                    more-alists))))))

(defun mapal (fun alist &rest more-alists)
  (loop for (k . v) in alist
     do (apply fun k (cons v (mapcar #'(lambda (alst) (aget alst k))
                                     more-alists)))))
