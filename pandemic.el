;;; pandemic.el --- calculate infection probabilities for the pandemic board game -*- lexical-binding: t -*-

;; Copyright (C) 2020 Ian Martins

;; Author: Ian Martins <ianxm@jhu.edu>
;; URL: https://github.com/ianxm/pandemic.el
;; Version: 0.0.1
;; Keywords: games
;; Package-Requires: ((emacs "25.2"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; pandemic.el calculate infection probabilities for the pandemic board game

;;; Code:

(require 'seq)

(defconst pandemic-table-headline "cities"
  "The headline containing the cities table.")

(defun pandemic--get-cities ()
  "Get the full list of cities."
  (save-excursion
    (let (field cities)
      (goto-char (org-find-exact-headline-in-buffer pandemic-table-headline))
      (forward-line 7)
      (while (org-at-table-p)
        (forward-char 2)
        (setq field (string-trim (org-table-get-field)))
        (if (not (seq-contains field ?-))
            (setq cities (cons field cities)))
        (forward-line))
      (message "cities %s" cities)
      cities)))

(defun pandemic-reset ()
  "Reset the buffer for a new game."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t)
          (pos (org-find-exact-headline-in-buffer pandemic-table-headline)))
      ;; reset infection rate
      (goto-char pos)
      (org-entry-put pos "infection-step" "1")
      (org-entry-put pos "infection-rate" "2")

      ;; clear hlines
      (forward-line 8)
      (while (re-search-forward "^[-\+\|]+\n" nil t)
        (replace-match ""))

      ;; add an hline at the bottom
      (goto-char pos)
      (forward-line 8)
      (while (save-excursion
               (forward-line)
               (org-at-table-p))
        (forward-line))
      (org-table-insert-hline))))

(defun pandemic-infect ()
  "Infect a city.  Prompts for the city to infect."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t)
          (city (completing-read "City: " (pandemic--get-cities) nil t)))
      (goto-char (point-min))
      (search-forward city nil t)
      (while (save-excursion
               (forward-line)
               (org-at-table-p))
        (org-table-move-row-down)))
    (pandemic--compute-prob)))

(defun pandemic-epidemic ()
  "A new epidemic was pulled.
Add an hline at the bottom of the table to mark off where the infection deck was reshuffled."
  (interactive)
  (save-excursion
    (goto-char (org-find-exact-headline-in-buffer pandemic-table-headline))
    (forward-line 7)
    (while (save-excursion
             (forward-line)
             (org-at-table-p))
      (forward-line))
    (pandemic--increment-rate)
    (let ((inhibit-read-only t))
      (org-table-insert-hline))))

(defun pandemic--compute-prob ()
  "Fill probabilities in to the table."
  (goto-char (org-find-exact-headline-in-buffer pandemic-table-headline))
  (forward-line 7)
  (forward-char 2)
  (let ((inhibit-read-only t)
        (infection-rate (string-to-number (org-entry-get (org-find-exact-headline-in-buffer pandemic-table-headline)
                                                         "infection-rate")))
        (separator-count 0)
        line-num separators
        very-bottom next-bottom)
    ;; find separators
    (while (save-excursion
             (forward-line)
             (org-at-table-p))
      (setq line-num (line-number-at-pos))
      (if (seq-contains (org-table-get-field) ?-)
          (setq separators (cons line-num separators)))
      (forward-line)
      (forward-char 2))
    (setq separators (cons (+ 2 line-num) separators))
    (setq very-bottom (1- (first separators)))
    (setq next-bottom (1- (second separators)))

    ;; fill in probabilities
    (while (>= (length separators) 2)
      (let* ((top (1+ (second separators)))
             (bottom (1- (first separators)))
             prob)
        (dolist (turn (number-sequence 1 5))
          (setq prob (pandemic--calc-prob very-bottom next-bottom top bottom turn infection-rate separator-count))
          (dolist (line (number-sequence top bottom))
            (forward-line (- line (line-number-at-pos))) ; (goto-line line)
            (forward-char 2)
            (org-table-goto-column (1+ turn))
            (org-table-blank-field)
            (insert (format "%.0f" prob))))
        (setq separator-count (1+ separator-count)))
      (setq separators (cdr separators)))
    (org-table-align)))

(defun pandemic--calc-prob (very-bottom next-bottom top bottom turn infection-rate separator-count)
  "Calculate a single probability."
  (if (= bottom very-bottom)
      0
    (let ((size (float (1+ (- bottom top))))
          (new-infections (+ (* infection-rate turn) (1- separator-count)))
          (base-infections (- next-bottom bottom)))
      (* 100
         (/
          (max 0
               (min size
                    (- new-infections base-infections)))
          size)))))

(defun pandemic--increment-rate ()
  "Increment the infection rate."
  (let* ((inhibit-read-only t)
         (pos (org-find-exact-headline-in-buffer pandemic-table-headline))
         (infection-step (1+ (string-to-number (org-entry-get pos "infection-step"))))
         (infection-rate (or (nth (1- infection-step)
                                  (org-entry-get-multivalued-property pos "infection-rate-schedule"))
                             "4")))
    (org-entry-put pos "infection-step" (number-to-string infection-step))
    (org-entry-put pos "infection-rate" infection-rate)))

;;;###autoload
(define-minor-mode pandemic-mode "Toggle Pandemic mode."
  :init-value nil
  :lighter " pandemic"
  :keymap
  `((,(kbd "r") . pandemic-reset)
    (,(kbd "i") . pandemic-infect)
    (,(kbd "e") . pandemic-epidemic))
  :group 'pandemic
  (read-only-mode t))

(provide 'pandemic-mode)

;;; pandemic.el ends here
