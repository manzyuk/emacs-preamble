(require 'org2blog-autoloads)

(setq org2blog/wp-blog-alist
      (let ((blog (netrc-machine (netrc-parse "~/.netrc") "wordpress" t)))
        `(("wordpress"
           :url "http://oleksandrmanzyuk.wordpress.com/xmlrpc.php"
           :username ,(netrc-get blog "login")
           :password ,(netrc-get blog "password")
           :track-posts nil))))

(setq org2blog/wp-buffer-template
      "# -*- eval: (org2blog/wp-mode 1) -*-
#+DATE: %s
#+TITLE: %s
#+OPTIONs: toc:nil num:nil todo:nil pri:nil tags:nil ^:nil TeX:nil")
