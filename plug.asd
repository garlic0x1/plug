(asdf:defsystem "plug"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:alexandria :str)
  :components ((:module "src"
                :components ((:file "utils")
                             (:file "core")))))
