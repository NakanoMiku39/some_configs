;;; org-mode.el -*- lexical-binding: t; -*-

(defface my/org-bold
  '((t (:foreground "#ff6c6b" :weight bold)))
  "Face used for bold text in Org buffers."
  :group 'org-faces)

(defconst my/org-color-palette
  '(("Red" . "#ff6c6b")
    ("Orange" . "#da8548")
    ("Yellow" . "#ECBE7B")
    ("Green" . "#98be65")
    ("Cyan" . "#46D9FF")
    ("Blue" . "#51afef")
    ("Purple" . "#c678dd")
    ("Pink" . "#ff79c6"))
  "Named colors available to `my/org-colorize-region'.")

(defconst my/org-color-name-aliases
  '(("红色" . "Red") ("橙色" . "Orange") ("黄色" . "Yellow")
    ("绿色" . "Green") ("青色" . "Cyan") ("蓝色" . "Blue")
    ("紫色" . "Purple") ("粉色" . "Pink"))
  "Legacy color names kept for existing Org links.")

(defun my/org-color-link-face (color-name)
  "Return the display face for COLOR-NAME."
  (let* ((name (or (cdr (assoc color-name my/org-color-name-aliases))
                   color-name))
         (color (cdr (assoc name my/org-color-palette))))
    (when color `(:foreground ,color))))

(defun my/org-colorize-region (beg end color-name)
  "Wrap the active region from BEG to END in a COLOR-NAME Org link."
  (interactive
   (progn
     (unless (use-region-p)
       (user-error "Select some text first"))
     (list (region-beginning)
           (region-end)
           (completing-read "Text color: " my/org-color-palette nil t))))
  (let ((text (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    (insert (org-link-make-string (concat "color:" color-name) text))))

(with-eval-after-load 'org
  ;; Org 9.6 使用 `bold' face 显示 *粗体*。换成专用 face，
  ;; 避免将 Emacs 其他界面中的粗体也全部染红。
  (setf (cadr (assoc "*" org-emphasis-alist)) 'my/org-bold)

  ;; 文档中保存为 [[color:红色][文字]]，编辑时只显示并着色“文字”。
  (org-link-set-parameters "color" :face #'my/org-color-link-face)
  (define-key org-mode-map (kbd "C-c c") #'my/org-colorize-region)

  ;; 设置标题的字体大小，逐级递减
  (set-face-attribute 'org-level-1 nil :height 1.5 :weight 'bold)
  (set-face-attribute 'org-level-2 nil :height 1.4 :weight 'bold)
  (set-face-attribute 'org-level-3 nil :height 1.3 :weight 'semi-bold)
  (set-face-attribute 'org-level-4 nil :height 1.2 :weight 'semi-bold)
  (set-face-attribute 'org-level-5 nil :height 1.1)
  (set-face-attribute 'org-level-6 nil :height 1.0)
  (set-face-attribute 'org-level-7 nil :height 1.0)
  (set-face-attribute 'org-level-8 nil :height 1.0)
)

(provide 'org-mode)
