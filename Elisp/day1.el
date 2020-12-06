;;; day1.el --- Elisp solution for Advent Of Code Day 1 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Alessandro Wollek
;;
;; Author: Alessandro Wollek <http://github/AlessandroW>
;; Maintainer: Alessandro Wollek <a@wollek.dev>
;; Created: December 06, 2020
;; Modified: December 06, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/AlessandroW/AdventOfCode-2020/2020/Elisp/day1
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Elisp solution for Advent Of Code Day 1
;;
;;; Code:

(defun find-pairs(input)
  "Find the two numbers in INPUT summing to 2020 and return their product."
  (let (value)
    (dolist (element input value)
      (if (member (- 2020 element) input)
          (setq value (* element (- 2020 element)))))))

(defun find-triplets(input)
  "Find the three numbers in INPUT summing to 2020 and return their product."
  (let (value)
    (dolist (first_element input value)
      (dolist (second_element input value)
        (if (member (- 2020 (+ first_element second_element)) input)
            (setq value (* first_element
                           (* second_element
                              (- 2020 (+ first_element second_element))))))))))

;; Taken from http://ergoemacs.org/emacs/elisp_read_file-content.html
;; Thanks to Xah Lee.
(defun read-lines (filePath)
  "Return a list of lines of a file at FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(assert (= (find-pairs '(1721 979 366 299 675 1456)) 514579))
(assert (= (find-triplets '(1721 979 366 299 675 1456)) 241861950))

(setq puzzle_input (mapcar 'string-to-number (read-lines "../day1.txt")))
(find-pairs puzzle_input)
(find-triplets puzzle_input)

(provide 'day1)
;;; day1.el ends here
