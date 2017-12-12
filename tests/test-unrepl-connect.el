;;; test-unrepl-connect.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: unrepl-connect.el
;; Author: Daniel Barreto
;; Copyright (C) 2017 Daniel Barreto
;; Created: Fri Dec  1 10:53:03 2017 (+0100)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Test unrepl connection related functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(add-to-list 'load-path (expand-file-name "../parseclj/"))
;; HACK: this is a temporary meassure while MELPA/package-build.el fixes:
;;       https://github.com/melpa/package-build/commit/063e64ad55ac07a348af0373605fd90348a8bc11#r26028026

(require 'buttercup)
(require 'dash)
(require 'seq)
(require 'with-simulated-input)

(require 'unrepl)

(describe "unrepl--connection-prompt"
  (before-all
    (unrepl-projects-add (unrepl-create-project 'localhost:5555 "/foo/bar/" nil nil))
    (unrepl-projects-add (unrepl-create-project 'localhost:5566 "/root/secret/" nil nil))
    (unrepl-projects-add (unrepl-create-project '159.98.201.23:60100 nil nil nil)))

  (after-all
    (setq unrepl-projects nil))

  
  (it "understands selecting a project by name"
    (mapc (lambda (input-conn-id-kv)
            (let ((input (car input-conn-id-kv))
                  (conn-id (cdr input-conn-id-kv)))
              (expect
               (with-simulated-input input
                 (unrepl--connection-prompt))
               :to-equal (list (unrepl-projects-get conn-id) nil))))
          '(("secr TAB RET" . localhost:5566)
            ("bar TAB RET" . localhost:5555)
            ("159 TAB RET" . 159.98.201.23:60100))))

  
  (it "accepts <host>:<port> values that are not listed in choices"
    (let* ((mocked-proc 'mocked-proc)
           (proc-buffer (get-buffer-create "*proc buffer*")))
      (spy-on 'make-network-process :and-return-value mocked-proc)
      (spy-on 'unrepl-socket--get-network-buffer :and-return-value proc-buffer)
      (spy-on 'process-send-string)
      (spy-on 'set-process-sentinel)
      (spy-on 'process-buffer :and-return-value proc-buffer)
      (spy-on 'delete-process)

      (seq-let [new-project new-p] (with-simulated-input "barreto.tech:12345 RET"
                                     (unrepl--connection-prompt))
        (expect new-p)
        (expect (unrepl-project-id new-project) :to-equal 'barreto.tech:12345)
        (expect (unrepl-project-namespace new-project) :to-equal nil)
        (expect (unrepl-project-dir new-project) :to-equal nil)
        (expect (unrepl-project-socket-repl new-project) :to-equal nil)
        (expect (buffer-name (unrepl-project-repl-buffer new-project))
                :to-equal "UNREPL[barreto.tech:12345]")
        (expect (unrepl-project-conn-pool-get-process new-project :client)
                :to-equal mocked-proc))
      
      (expect 'make-network-process :to-have-been-called-with
              :name "unrepl-client"
              :buffer proc-buffer
              :host "barreto.tech"
              :service 12345
              :filter #'unrepl-loop-handle-proc-message)
      (expect 'unrepl-socket--get-network-buffer :to-have-been-called-with
              :client "barreto.tech" 12345)
      (expect 'process-send-string
              :to-have-been-called-with mocked-proc (unrepl-socket--blob))
      (expect 'set-process-sentinel
              :to-have-been-called-with mocked-proc #'unrepl-socket--client-sentinel)
      (expect 'process-buffer :to-have-been-called-with mocked-proc)

      (with-current-buffer proc-buffer
        (expect (derived-mode-p 'clojure-mode))
        (expect unrepl-conn-id :to-equal 'barreto.tech:12345)
        (expect unrepl-loop-process-type :to-equal :client)
        (expect unrepl-loop-process-dispatcher :to-equal #'unrepl-loop-client-dispatcher))

      (unrepl-project-quit 'barreto.tech:12345)))


  (it "raises `user-error' if not given something that looks like <host>:<port>"
    (expect (with-simulated-input "somecrazything RET"
              (unrepl--connection-prompt))
            :to-throw 'user-error))


  (describe "unrepl--connection-prompt-choices"
    (it "when called with project-dir, returns projects that match first, all ordered decr by `:created'"
      (unrepl-projects-add (unrepl-create-project 'localhost:5544 "/foo/bar/" nil nil))
      (expect (unrepl--connection-prompt-choices "/foo/bar/")
              :to-equal
              '("bar [localhost:5544]"
                "bar [localhost:5555]"
                "159.98.201.23:60100"
                "secret [localhost:5566]"))
      (spy-on 'delete-process)
      (unrepl-project-quit 'localhost:5544))

    (it "when called nil project-dir, returns projects with no dir first"
      (expect (unrepl--connection-prompt-choices nil)
              :to-equal
              '("159.98.201.23:60100"
                "secret [localhost:5566]"
                "bar [localhost:5555]")))))



(describe "unrepl-connect"
  (it "when asked to connect a non clojure-mode buffer, prompts for confirmation"
    (with-temp-buffer
      (with-simulated-input "n RET"
        (call-interactively #'unrepl-connect))))

  (describe "when using it inside a Clojure project"
    :var (real-default-directory
          (project-dir "/random/clj/project/"))

    (before-all
      (spy-on 'unrepl-clojure-dir :and-return-value project-dir)
      (setq real-default-directory default-directory)
      (setq default-directory project-dir))

    (after-all
      (setq default-directory real-default-directory))

    (it "If buffer is already connected, warns the user"
      (with-temp-buffer
        (clojure-mode)
        (setq-local unrepl-conn-id 'localhost:5555)
        (spy-on 'message)
        (call-interactively #'unrepl-connect)

        (expect (spy-calls-count 'message) :to-equal 1)
        (expect (string-match-p "already connected to localhost:5555"
                                (car (spy-calls-args-for 'message 0))))))

    (it "If there's no UNREPL project for this dir, create a new one automatically"
      (spy-on 'unrepl-socket--issue-new-socket-port :and-return-value 12345)
      (spy-on 'start-file-process-shell-command :and-return-value 'mocked-proc)
      (spy-on 'set-process-coding-system)
      (spy-on 'set-process-sentinel)
      (spy-on 'set-process-filter)
      (with-temp-buffer
        (clojure-mode)
        (let ((inhibit-message t))
          (call-interactively #'unrepl-connect)))

      (expect 'start-file-process-shell-command :to-have-been-called-times 1)
      (let ((call-args (spy-calls-args-for 'start-file-process-shell-command 0)))
        (expect (car call-args) :to-equal "unrepl-socket-server")
        (expect (cl-caddr call-args)
                :to-equal
                (unrepl-socket--repl-cmd unrepl-default-socket-repl-command 12345)))
      (expect 'set-process-coding-system
              :to-have-been-called-with'mocked-proc 'utf-8-unix 'utf-8-unix)
      (expect 'set-process-sentinel
              :to-have-been-called-with'mocked-proc 'unrepl-socket--server-sentinel)
      (expect 'set-process-filter :to-have-been-called-times 1))

    (it "If there a matching UNREPL project for this dir, automatically connect to it"
      (let ((project (unrepl-create-project '127.0.0.1:60189 project-dir nil nil)))
        (unrepl-projects-add project)
        (with-temp-buffer
          (clojure-mode)
          (expect (not unrepl-mode))
          (expect unrepl-conn-id :to-equal nil)

          (let ((inhibit-message t))
            (call-interactively #'unrepl-connect))

          (expect unrepl-mode)
          (expect unrepl-conn-id :to-equal (unrepl-project-id project))
          (unrepl-project-quit '127.0.0.1:60189)))))


  (describe "when using it outside a Clojure project"
    (it "prompts the user for a Socket REPL HOST:PORT"
      (spy-on 'unrepl-clojure-dir :and-return-value nil)
      (spy-on 'unrepl--connection-prompt
              :and-return-value (list '((:id . localhost:5555)) nil))
      (with-temp-buffer
        (clojure-mode)
        (let ((inhibit-message t))
          (call-interactively #'unrepl-connect))
        (expect 'unrepl--connection-prompt :to-have-been-called)))))


(describe "When socket connection gets broken"
  (before-each
    (unrepl-projects-add (unrepl-create-project 'localhost:5555 "/foo/bar/" nil nil))
    (let ((mocked-buf-name "*temp proc buffer*"))
      (when (get-buffer mocked-buf-name)
        (kill-buffer mocked-buf-name))
      (let ((mocked-proc-buffer (get-buffer-create mocked-buf-name)))
        (with-current-buffer mocked-proc-buffer
          (setq-local unrepl-conn-id 'localhost:5555))
        (spy-on 'process-buffer :and-return-value mocked-proc-buffer))))

  (after-all
    (setq unrepl-projects nil))

  (it "user gets notified through the REPL"
    (let ((repl-buffer (-> 'localhost:5555
                           (unrepl-projects-get)
                           (unrepl-project-repl-buffer))))
      (unrepl-socket--client-sentinel 'mocked-proc "connection broken by peer.")

      (expect (get-buffer-window repl-buffer))
      (with-current-buffer repl-buffer
        (expect (string-match-p
                 "connection broken by peer.$"
                 (buffer-substring-no-properties (point-min) (point-max)))))))

  (it "project is removed from `unrepl-projects'"
    (unrepl-socket--client-sentinel 'mocked-proc "connection broken by peer.")
    (expect (not unrepl-projects))))


(describe "When trying to connect to a wrong authority"
  (it "let the user know in a `unrepl-connection-error'"
    (spy-on 'make-network-process :and-throw-error 'file-error)
    (expect
     (with-simulated-input "localhost:9999 RET"
       (with-temp-buffer
         (clojure-mode)
         (let ((inhibit-message t))
           (call-interactively #'unrepl-connect))))
     :to-throw 'unrepl-connection-error)))

;;; test-unrepl-connect.el ends here
