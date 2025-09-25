(defun my/text-scale-adjust-latex-previews (&optional arg)
  "Adjust the size of latex preview fragments when changing the
buffer's text scale."
  (pcase major-mode
    ('latex-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (if (eq (overlay-get ov 'category)
               'preview-overlay)
           (my/text-scale--resize-fragment ov))))
    ('org-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (if (eq (overlay-get ov 'org-overlay-type)
               'org-latex-overlay)
           (my/text-scale--resize-fragment ov))))))

(defun my/text-scale--resize-fragment (ov)
  (overlay-put
   ov 'display
   (cons 'image
         (plist-put
          (cdr (overlay-get ov 'display))
          :scale (* 0.33 (expt text-scale-mode-step text-scale-mode-amount))))))

(advice-add #'org-latex-preview :after #'my/text-scale-adjust-latex-previews)

(add-hook 'text-scale-mode-hook #'my/text-scale-adjust-latex-previews)
