(asdf/source-registry:initialize-source-registry
 `(:source-registry
   (:tree (:here "."))
   :inherit-configuration))

(ql:quickload :quicklisp-controller)

(quicklisp-controller::update-what-you-can)


 
