[![MELPA](http://melpa.org/packages/agtags-badge.svg)](http://melpa.org/#/agtags)

agtags
========

Another Emacs frontend to GNU Global. It support **wgrep** in **agtags-grep-mode**.

Setup
-----

Add the following to your `init.el` (after the `agtags.el` in you load-path):

``` el
(require 'agtags)
(require 'agtags-xref) ;; drop it when you don't need xref
```

A simple setup demo

``` el
(when (executable-find "global")
  (defun agtags-mode-on()
    (agtags-mode 1))

  (setq agtags-global-treat-text t)

  (agtags-bind-keys)
  (add-hook 'text-mode-hook 'agtags-mode-on)
  (add-hook 'prog-mode-hook 'agtags-mode-on)
  (add-to-list 'xref-backend-functions 'agtags-xref-backend) ;; drop it when you don't need xref

  (agtags-update-parser))

```

Call **agtags-update-root** When you modify the **Project** directory.

``` el
;; aprodject demo
(after-aproject-change (agtags-update-root aproject-rootdir)))

;; projectile demo
(defun my-switch-project-hook ()
  (agtags-update-root (projectile-project-root)))
(add-hook 'projectile-after-switch-project-hook #'my-switch-project-hook)
```

Variables
-------

**agtags-key-prefix** (Default "C-x t")

It is used for the prefix key of agtags's command.

**agtags-global-ignore-case** (Default nil)

Non-nil if Global should ignore case in the search pattern.

**agtags-global-treat-text** (Default nil)

Non-nil if Global should include matches from text files. This affects 'agtags/find-file' and 'agtags/find-grep'.

Functions
-------

**agtags-bind-keys ()**

Set global key bindings for agtags.

**agtags-update-root (root)**

It simply modify env **GTAGSROOT**.
Set ROOT directory of the project for agtags.  

**agtags-update-parser (&optional parser)**

It simply modify env **GTAGSLABEL**.  
Set parser to PARSER for agtags or smart parser it.  
The order of precedence is 'new-ctags' -> 'pygments' -> 'ctags' -> 'default'.

Bind Keys
-------

**("b" . agtags/update-tags)**

Create or Update tag files (e.g. GTAGS) in directory `GTAGSROOT`.

**("f" . agtags/open-file)**

Input pattern and move to the top of the file.

**("F" . agtags/find-file)**

Input pattern, search file and move to the top of the file.

**("t" . agtags/find-tag)**

Input tag and move to the locations.

**("r" . agtags/find-rtag)**

Input rtags and move to the locations.

**("p" . agtags/find-with-grep)**

Input pattern, search with grep(1) and move to the locations.

**("q" . agtags/switch-dwim)**

Switch to last agtags-*-mode buffer.

License
-------

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
Street, Fifth Floor, Boston, MA 02110-1301, USA.
