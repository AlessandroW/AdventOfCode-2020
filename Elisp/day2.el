;;; day2.el --- My Solution for the second day in the Advent of Code 2020 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Alessandro Wollek
;;
;; Author: Alessandro Wollek <http://github/AlessandroW>
;; Maintainer: Alessandro Wollek <a@wollek.dev>
;; Created: December 07, 2020
;; Modified: December 07, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/AlessandroW/AdventOfCode-2020
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  My Solution for the second day in the Advent of Code 2020
;;
;;; Code:
(require 'seq)

(defun split-policies (policies)
  "Split POLICIES into a list of policies."
  (split-string policies "\n" t))

(defun split-colon (input-list)
  "Split the INPUT-LIST into poicy and password."
  (split-string input-list ":" t " "))

(defun split-space (input-list)
  "Split the INPUT-LIST into both parts of the policy."
  (split-string input-list " " t " "))


(defun count-policy-characters (character input-list)
  "Count the number of CHARACTER in INPUT-LIST."
  (length (concat (seq-filter (lambda (element) (string= (char-to-string element) character)) input-list))))

(defun policy-full-filled (min max count)
  (and (<= min count) (<= count max)))


(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))
;; thanks to “Pascal J Bourguignon” and “TheFlyingDutchman 〔zzbba…@aol.com〕”. 2010-09-02

(defun is-policy-fulfilled (input)
  (let (min max character password (temp input))
    (setq temp (split-colon temp))
    (setq password (car (cdr temp)))
    (setq temp (split-space (car temp)))
    (setq character (car (cdr temp)))
    (setq temp (mapcar 'string-to-number (split-string (car temp) "-")))
    (setq min (car temp))
    (setq max (car (cdr temp)))
    (policy-full-filled min max (count-policy-characters character password))))


(defun character-equal (position password character)
  (string= (char-to-string (elt password (- position 1))) character))

(defun is-policy-fulfilled-part-two (input)
  (let (min max character password (temp input))
    (setq temp (split-colon temp))
    (setq password (car (cdr temp)))
    (setq temp (split-space (car temp)))
    (setq character (car (cdr temp)))
    (setq temp (mapcar 'string-to-number (split-string (car temp) "-")))
    (setq min (car temp))
    (setq max (car (cdr temp)))
    (xor (character-equal min password character) (character-equal max password character))))


(defun solve-part-one (input)
  (length (seq-filter 'is-policy-fulfilled (split-policies input))))


(defun solve-part-two (input)
  (length (seq-filter 'is-policy-fulfilled-part-two (split-policies input))))

(assert (= (solve-part-one "1-3 a: abcde\n1-3 b: cdefg\n 2-9 c: ccccccccc") 2))
(solve-part-one (get-string-from-file "../day2.txt"))
(solve-part-two (get-string-from-file "../day2.txt"))


(provide 'day2)
;;; day2.el ends here
