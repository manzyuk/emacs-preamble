(add-to-list 'load-path "~/.emacs.d/site-lisp/xml-rpc")
(add-to-list 'load-path "~/.emacs.d/site-lisp/metaweblog")
(add-to-list 'load-path "~/.emacs.d/site-lisp/org2blog")

(require 'org2blog-autoloads)

(setq org2blog/wp-blog-alist
      (let ((credentials (netrc-credentials "wordpress")))
        `(("wordpress"
           :url "http://oleksandrmanzyuk.wordpress.com/xmlrpc.php"
           :username ,(car  credentials)
           :password ,(cadr credentials)
           :track-posts nil))))

(setq org2blog/wp-buffer-template
      "# -*- eval: (org2blog/wp-mode 1) -*-
#+DATE: %s
#+TITLE: %s
#+OPTIONS: toc:nil num:nil todo:nil pri:nil tags:nil ^:nil TeX:nil")

(provide 'preamble-org2blog)
