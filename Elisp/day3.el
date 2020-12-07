;;; day3.el --- My Solution for the 3rd day of the Advent Of Code 2020 -*- lexical-binding: t; -*-
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
;;  My Solution for the 3rd day of the Advent Of Code 2020
;;
;;; Code:
(require 'seq)

(defun count-tree (position line)
  (if (= ?# (elt line position))
      1
    0))

(defun get-next-position (column columns right)
  (let ((next (+ column right)))
    (if (< next columns)
        next
      (- next columns))))

(defun count-all-trees (right down rows columns)
  (let ((trees 0) (row 0) (column 0))
    (while (< row (length rows))
      (setq trees (+ trees (count-tree column (nth row rows))))
      (setq row (+ row down))
      (setq column (get-next-position column columns right)))
    trees))

(defun solve-part-one (right down input)
  (let (columns rows)
     (setq rows (split-string input "\n" t))
     (setq columns (- (length (car rows)))
     (count-all-trees right down rows columns)))

(defun solve-part-two (input)
  (seq-reduce '* (mapcar (lambda (element)
                           (solve-part-one (car element) (car (cdr element)) input ))
                         '((1 1) (3 1) (5 1) (7 1) (1 2)))
              1))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))
;; thanks to “Pascal J Bourguignon” and “TheFlyingDutchman 〔zzbba…@aol.com〕”. 2010-09-02

(assert (= (get-next-position 0 7 3) 3))
(assert (= (get-next-position 3 7 3) 6))
(assert (= (get-next-position 6 7 3) 2))
(assert (= (count-tree 6 ".#....#..#.") 1))
(setq example-input "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#")
(assert (solve-part-one 3 1 example-input) 7)
(assert (solve-part-two example-input) 336)
(solve-part-one 3 1 (get-string-from-file "../day3.txt"))
(solve-part-two (get-string-from-file "../day3.txt"))

(provide 'day3)
;;; day3.el ends here
