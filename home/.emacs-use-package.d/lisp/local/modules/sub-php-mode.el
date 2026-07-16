;;; sub-php-mode.el --- Force buffers to open in the same window -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/jc-dev
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; php-mode.

;;; Code:

(eval-when-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package php-mode
  :commands php-mode
  :mode
  ("\\.php3\\'" . php-mode)
  ("\\.php\\'" . php-mode))

(provide 'sub-php-mode)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; sub-php-mode.el ends here
