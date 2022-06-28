(nil . ((eval . (progn
                   (require 'projectile)
                   (puthash (projectile-project-root)
                            "cask exec buttercup -L ."
                            projectile-test-cmd-map))))))
