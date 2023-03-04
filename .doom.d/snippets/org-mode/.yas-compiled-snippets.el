;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("n.video" "* [ ] Script\n* [ ] Video Recording\n* [ ] Voice Over\n* [ ] Thumbnail\n* [ ] Upload\n* [ ] Links\n  -\n" "new video" nil nil nil "/Users/markolsonse/Dropbox/mho_dot-files/.doom.d/snippets/org-mode/new video" nil nil)
                       (">iobjv" "${1:$$(yas-choose-value '(\"defn\" \"objv\"))}-${2:TagID}\n#+transclude: [[file:~/GitHub/mhoTeX/0-tex/tex_core-$2.tex]]\n- $0" "insert objv" t nil nil "/Users/markolsonse/Dropbox/mho_dot-files/.doom.d/snippets/org-mode/insert objv" nil nil)
                       ("ib.exam" "* Paper 1\n** TZ1\n-\n** TZ2\n-\n* Paper 2\n** TZ1\n-\n** TZ2\n-" "ib exam paper" nil nil nil "/Users/markolsonse/Dropbox/mho_dot-files/.doom.d/snippets/org-mode/ib exam paper" nil nil)))


;;; Do not edit! File generated at Tue Feb 28 10:44:19 2023
