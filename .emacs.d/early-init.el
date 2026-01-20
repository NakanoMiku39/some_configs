;;; early-init.el -*- lexical-binding: t; -*-

;; 在 init.el 之前执行，用于最早期的优化

;; 启动时最大化
;; (push '(fullscreen . maximized) default-frame-alist)

;; 禁止调整 frame 大小
;; (setq frame-inhibit-implied-resize t)

;; 禁止 UI 元素（比在 init.el 中设置更快）
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; 禁止自动加载 native-comp 的 eln 文件警告
(setq native-comp-async-report-warnings-errors 'silent)

;; 禁止启动时检查文件处理器（加速文件加载）
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)))


