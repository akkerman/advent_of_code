;;; 01.el 2025 Day 1: Secret Entrance -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026
;;
;; Author: Akkerman
;; Maintainer:  Akkerman
;; Created: January 02, 2026
;; Modified: January 02, 2026
;; Version: 0.0.1
;; Homepage: https://github.com/akkerman/2025/01.el
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; 2025 Day 1: Secret Entrance
;;
;;
;;
;;; Code:
;;;

(defun File-lines (file)
  "Return a list of non-empty lines from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun parse-instruction (line)
  "Parse LINE like \"L10\" into (CHAR INT)."
  (list (aref line 0)
        (string-to-number (substring line 1))))

(defun read-instructions (file)
  (mapcar #'parse-instruction
          (file-lines file)))

(defun part-one (instructions)
  (let ((pos 50)
        (zeroes 0))
    (dolist (instr instructions)
      (pcase-let ((`(,d ,n) instr))
        (setq pos (pcase d
                    (?L (mod (- pos n) 100))
                    (?R (mod (+ pos n) 100))
                    (_ pos)))
        (when (= pos 0)
          (setq zeroes (1+ zeroes)))))
    zeroes))

(defun part-two (instructions)
  (let ((pos 50)
        (zeroes 0))
    (dolist (instr instructions)
      (pcase-let ((`(,d ,n) instr))
        (dotimes (_ n)
          (setq pos (pcase d
                      (?L (1- pos))
                      (?R (1+ pos))
                      (_ pos)))
          (setq pos (mod pos 100))
          (when (= pos 0)
            (setq zeroes (1+ zeroes))))))
    zeroes))

(part-one (read-instructions "./01-input.txt"))
(part-two (read-instructions "./01-input.txt"))
;;; 01.el ends here
