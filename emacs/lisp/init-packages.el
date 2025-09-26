(require 'package)
(setq package-archives '(("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")

                         ("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")))
(package-initialize)

;;防止反复调用 package-refresh-contents 会影响加载速度
(when (not package-archive-contents)
  (package-refresh-contents))

;; 文件末尾
(provide 'init-packages)
