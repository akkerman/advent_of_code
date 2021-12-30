(defvar sum 0)
(defvar prev 0)
(defvar current 0)
(defvar n1 0)
(defvar n2 0)
(defvar n0 0)

(let ((in (open "./01-input.txt" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
          while line do 
          (setq n0 (parse-integer line))

          (if (and (< 0 n2) (< 0 n1))
            (setq current (+ n2 n1 n0)))

          (if (and (< 0 prev) (< prev current))
            (setq sum (+ sum 1)))

          (setq prev current)
          (setq n2 n1)
          (setq n1 n0))
    (close in)))

(write sum)
