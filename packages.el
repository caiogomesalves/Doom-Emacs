;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

;; Examples of using `el-get' to install packages.
;; https://tapoueh.org/blog/2010/08/el-get/
;; (package! el-get)

(package! magit)
(package! bm)
(package! yafolding)
(package! smartparens)
(package! markdown-mode)
(package! orgalist)
(package! elpy)
(package! imenu-list)
(package! auctex)
(package! ess-view-data)
(package! lsp-treemacs)
(package! hl-prog-extra)
(package! yasnippet)
(package! quarto-mode)

(package! bookmark+
  :recipe (:host github
           :repo "emacsmirror/bookmark-plus"))

;; https://github.com/tecosaur/screenshot
(package! screenshot
  :recipe (:host github
           :repo "tecosaur/screenshot"
           ;; Avoid generate `.elc' that has a bug.
           ;; https://github.com/tecosaur/screenshot/issues/11
           :build (:not compile)))

;; pyls.
;; pip install --user python-language-server[all]

;; pylsp.
;; pip install --user python-lsp-server

;; Microsoft language server.
;; (package! lsp-python-ms)            ;; Turn on.
(package! lsp-python-ms :disable t) ;; Turn off.
;; (package! lsp-pyright)

;; (package! exec-path-from-shell)

;; Needs `sudo apt-get install virtualenv'.
;; (package! lsp-jedi)
;; (package! jedi)

;; Version with Org-struct.
;; https://github.com/bzg/org-mode/releases/tag/release_9.1.14
;; (package! org-mode
;;   :pin "30498ef932bc35c26e3e58278f4987a67480b446"
;;   :recipe (:host github
;;            :repo "bzg/org-mode"))

;; Gets an error about unbalanced expressions.
;; (package! electric-spacing-r
;;   :recipe (:host github
;;            :repo "walmes/electric-spacing"
;;            ;; :branch "master"
;;            :files ("electric-spacing-r.el")
;;            ))

;; Color themes.
;; (package! afternoon-theme)
;; (package! apropospriate-theme)
;; (package! apropospriate-theme)
;; (package! fantom-theme)
;; (package! flatland-theme)
;; (package! flatui-theme)
;; (package! gotham-theme)
;; (package! leuven-theme)
;; (package! material-theme)
;; (package! molokai-theme)
;; (package! monokai-theme)
;; (package! solarized-theme)
;; (package! spacemacs-theme)
;; (package! vscode-dark-plus-theme)
