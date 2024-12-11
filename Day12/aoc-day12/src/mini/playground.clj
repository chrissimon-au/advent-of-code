(ns mini.playground)

(require '[clojure.repl :refer :all])

; This project has custom configuration.
; See .vscode/settings.json

; If you are new to Calva, you may want to use the command:
; Calva: Create a “Getting Started” REPL project
; which creates a project with a an interactive Calva (and Clojure) guide.

(defn square [x] (* x x))

(println "What is this:" (square 5))

(square 5)

(+ 7654 1234)

; ( 7 + 3 * 4 + 5 ) / 10
(+ 7 (* 3 4) 5)

(find-doc "stacktrace")