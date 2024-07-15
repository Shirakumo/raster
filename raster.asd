(asdf:defsystem raster
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A library for software rasterisation"
  :homepage "https://shirakumo.github.io/raster/"
  :bug-tracker "https://github.com/shirakumo/raster/issues"
  :source-control (:git "https://github.com/shirakumo/raster.git")
  :serial T
  :components ((:file "package")
               (:file "buffer")
               (:file "sampler")
               (:file "sdf")
               (:file "composite")
               (:file "raster")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :nibbles)
  :in-order-to ((asdf:test-op (asdf:test-op :raster/test))))

(asdf:defsystem raster/test
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Tests for the raster system."
  :homepage "https://shirakumo.github.io/raster/"
  :bug-tracker "https://github.com/shirakumo/raster/issues"
  :source-control (:git "https://github.com/shirakumo/raster.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:raster :parachute :pngload)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.raster.test)))
