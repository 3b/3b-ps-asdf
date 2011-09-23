(defsystem :3b-ps-asdf
  :depends-on (:parenscript :hunchentoot)
  :serial t
  :components ((:file "package")
               (:file "ps-asdf")))