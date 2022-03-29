;;; layers.el --- my-org layers File for Spacemacs
;;
;; Copyright (c) 2012-2021 Nathan Cui
;;
;; Author: Nathan Cui <nathan.cui@gmail.com>
;; URL: https://github.com/alohaworld/.spacemacs.private
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This layer add customizable options based on the spacemacs's org layer
;; .emacs.d/layers/+emacs/org
;; !CAUTION! the org layer needs spacemacs-org layer, which resides in
;;   .emacs.d/layers/+spacemacs/spacemacs-org
;; my-org layer depends on org layer
(configuration-layer/declare-layer-dependencies '(org))
