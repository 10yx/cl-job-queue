(asdf:defsystem "cl-job-queue"
 :version "0.1.0"
 :author "ryuei sasaki"
 :license ""
 :depends-on ("bordeaux-threads"
              "logur")
 :components ((:module "src"
               :components ((:file "package")
                            (:file "queue" :depends-on ("package"))
                            (:file "worker" :depends-on ("queue"))
                            (:file "scheduler" :depends-on ("worker"))))))
